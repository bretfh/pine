;;;; The river window manager frontend: bind the global, answer every manage
;;;; and render sequence, and carry policy between the compositor and the
;;;; daemon. The daemon (pine.wm) decides what the chords are and what they
;;;; do; this process registers them with the compositor, reports presses,
;;;; and applies the actions it is told to -- always inside the sequence the
;;;; protocol requires. Tiling here is still the placeholder side-by-side
;;;; policy; design/wm.org's os-window trees replace the bodies of MANAGE and
;;;; RENDER, not the machinery around them.

(defpackage #:pine.wl-wm
  (:use #:cl #:wayflan-client #:pine.river-wm)
  (:local-nicknames (#:xkb-bind #:pine.river-xkb))
  (:export #:run-wm)
  (:documentation "river-window-management-v1 client: pump, bindings, actions."))
(in-package #:pine.wl-wm)

(defstruct (wm (:constructor %make-wm))
  display
  manager
  bindings-global                       ; river_xkb_bindings_v1
  (windows nil)                         ; newest first
  (outputs nil)                         ; newest first
  (seats nil)
  (bindings nil)                        ; chord string -> binding proxy
  ;; two generations of geometry: STAGED is what the daemon last sent,
  ;; RECTS is what the current sequence answers from. Adopting only at
  ;; manage_start keeps a window's proposed size and its position in the
  ;; same generation, so a re-tile never renders new positions against old
  ;; sizes -- one frame of that is a visible flicker.
  (staged nil)
  (rects nil)                           ; (id x y w h) for this generation
  (focus-id nil)                        ; identifier the daemon focused
  (staged-focus nil)
  (dirty nil)                           ; a manage sequence has been asked for
  (pending nil)                         ; thunks to run in the next manage seq
  (inbox nil)                           ; thunks from the daemon's actor thread
  (lock (bordeaux-threads:make-lock))
  (focus nil)
  (ref nil)                             ; daemon client actor
  (sys nil)
  (done nil))

(defstruct win proxy node id title app-id width height)
(defstruct out proxy x y width height)

;;;; The mirror: the arrangement the daemon computed, keyed by the protocol's
;;;; window identifier. Sequences are answered from this and nothing else, so
;;;; the compositor never waits on the daemon.

(defun %rect (wm w)
  "(values X Y WIDTH HEIGHT) for W from the daemon's arrangement, or nil."
  (let ((entry (and (win-id w) (assoc (win-id w) (wm-rects wm) :test #'equal))))
    (when entry
      (values-list (rest entry)))))

(defun %find-win (wm proxy)
  (find proxy (wm-windows wm) :key #'win-proxy :test #'eq))

(defun %screen (wm)
  "The output windows tile onto: the first one announced."
  (first (last (wm-outputs wm))))

(defun %tiled (wm)
  "Windows in tree order (oldest first), the order they tile in."
  (reverse (wm-windows wm)))

;;;; Two queues, for two different reasons.
;;;;
;;;; The inbox crosses threads: messages from the daemon arrive on a sento
;;;; actor thread, and the wayland connection belongs to the thread running
;;;; the dispatch loop. Nothing touches a proxy off that thread; the actor
;;;; only enqueues, exactly as the editor frontend does.
;;;;
;;;; The pending queue crosses sequences: close, focus, and binding enables
;;;; are window management state, which the protocol allows only inside a
;;;; manage sequence, so work arriving between sequences waits for one and
;;;; manage_dirty asks the compositor to start it.

(defun %enqueue (wm thunk)
  "Hand THUNK to the wayland thread."
  (bordeaux-threads:with-lock-held ((wm-lock wm))
    (push thunk (wm-inbox wm))))

(defun %drain-inbox (wm)
  (let (thunks)
    (bordeaux-threads:with-lock-held ((wm-lock wm))
      (setf thunks (nreverse (wm-inbox wm)) (wm-inbox wm) nil))
    (dolist (thunk thunks) (funcall thunk))))

(defun %ask-manage (wm)
  "Ask the compositor for a manage sequence, once: the protocol starts a new
one as soon as the current finishes, so raising it per queued item only asks
for sequences that have nothing left to do."
  (unless (wm-dirty wm)
    (setf (wm-dirty wm) t)
    (river-window-manager-v1.manage-dirty (wm-manager wm))))

(defun %defer (wm thunk)
  (push thunk (wm-pending wm))
  (%ask-manage wm))

(defun %drain (wm)
  (let ((pending (nreverse (wm-pending wm))))
    (setf (wm-pending wm) nil)
    (dolist (thunk pending) (funcall thunk))))

;;;; Bindings. A chord string from the daemon's keymap becomes an xkbcommon
;;;; keysym plus river's modifier mask; the compositor then delivers that
;;;; chord to us instead of to the focused window.

(defun %chord-keysym+modifiers (chord)
  "(values KEYSYM MODIFIERS) for a pine chord string such as \"s-Return\", or
nil when xkbcommon does not know the key name. MODIFIERS is the keyword list
river_seat_v1.modifiers is written in: pine's meta is the protocol's mod1 and
pine's super is its mod4."
  (let* ((key (pine.key:parse-key chord))
         (keysym (xkb:xkb-keysym-from-name (pine.key:key-sym key) '(:no-flags)))
         (modifiers (append (when (pine.key:key-shift key) '(:shift))
                            (when (pine.key:key-ctrl key)  '(:ctrl))
                            (when (pine.key:key-meta key)  '(:mod1))
                            (when (pine.key:key-super key) '(:mod4)))))
    (when (and keysym (plusp keysym))
      (values keysym modifiers))))

(defun %register-binding (wm seat chord command)
  "Create and enable the compositor-side binding for CHORD. Enabling is
window management state, so it runs inside a manage sequence."
  (multiple-value-bind (keysym modifiers) (%chord-keysym+modifiers chord)
    (cond
      ((null keysym)
       (format *error-output* "pine wm: unknown key in chord ~a (for ~a)~%"
               chord command))
      (t
       (let ((binding (xkb-bind:river-xkb-bindings-v1.get-xkb-binding
                       (wm-bindings-global wm) seat keysym modifiers)))
         (setf (gethash chord (wm-bindings wm)) binding)
         (push (lambda (&rest event)
                 (event-case event
                   (:pressed () (%send wm (list :binding :keys chord)))
                   (t (&rest args) (declare (ignore args)))))
               (wl-proxy-hooks binding))
         (%defer wm (lambda () (xkb-bind:river-xkb-binding-v1.enable binding))))))))

(defun %install-bindings (wm table)
  "Replace every registered binding with the daemon's TABLE."
  (unless (wm-bindings wm)
    (setf (wm-bindings wm) (make-hash-table :test 'equal)))
  (maphash (lambda (chord binding)
             (declare (ignore chord))
             (xkb-bind:river-xkb-binding-v1.destroy binding))
           (wm-bindings wm))
  (clrhash (wm-bindings wm))
  (let ((seat (first (wm-seats wm))))
    (cond
      ((null (wm-bindings-global wm))
       (format *error-output* "pine wm: no river_xkb_bindings_v1 global~%"))
      ((null seat)
       (format *error-output* "pine wm: no seat yet, bindings not registered~%"))
      (t
       (loop for (chord . command) in table
             do (%register-binding wm seat chord command))
       (format *error-output* "pine wm: registered ~d binding(s)~%"
               (hash-table-count (wm-bindings wm)))
       (finish-output *error-output*)))))

;;;; Actions the daemon asks for.

(defun %apply-action (wm plist)
  (destructuring-bind (&key action command id &allow-other-keys) plist
    (case action
      (:spawn
       ;; the frontend runs in the compositor's session, so a program it
       ;; launches inherits WAYLAND_DISPLAY and lands in this session
       (uiop:launch-program (list "sh" "-c" command)))
      (:close
       (%defer wm (lambda ()
                    (let ((w (or (find id (wm-windows wm)
                                       :key #'win-id :test #'equal)
                                 (wm-focus wm))))
                      (when w (river-window-v1.close (win-proxy w)))))))
      (:exit
       (river-window-manager-v1.exit-session (wm-manager wm)))
      (t (format *error-output* "pine wm: unknown action ~s~%" action)))))

;;;; The daemon seam.

(defun %send (wm message)
  "Report MESSAGE to the daemon. A message sent before the daemon is reachable
is reported rather than dropped in silence -- losing one of these is how the
output size went missing and the whole arrangement stayed empty."
  (let ((ref (wm-ref wm)))
    (cond
      (ref (sento.actor:tell ref message))
      (t (format *error-output* "pine wm: no daemon yet, dropped ~s~%"
                 (first message))
         (finish-output *error-output*)))))

(defun %report-state (wm)
  "Tell the daemon everything the compositor has already reported. The output
and any existing windows arrive before the attach completes -- and after a
respawn they were never sent at all -- so the frontend replays them whenever
it gains a daemon."
  (let ((screen (%screen wm)))
    (when (and screen (out-width screen))
      (%send wm (list :output :width (out-width screen)
                      :height (out-height screen)))))
  (dolist (w (%tiled wm))
    (when (win-id w)
      (%send wm (list :window-added :id (win-id w)
                      :title (win-title w) :app-id (win-app-id w))))))

(defun handle-daemon (wm message)
  "Runs on the daemon's actor thread: decide nothing here, hand the work to
the wayland thread."
  (case (first message)
    (:attached
     (destructuring-bind (&key id client-uri) (rest message)
       (declare (ignore id))
       (setf (wm-ref wm) (sento.remoting:make-remote-ref (wm-sys wm) client-uri))
       (%enqueue wm (lambda () (%report-state wm)))))
    (:bindings
     (destructuring-bind (&key table) (rest message)
       (%enqueue wm (lambda () (%install-bindings wm table)))))
    (:arrangement
     (destructuring-bind (&key rects focus) (rest message)
       (%enqueue wm (lambda ()
                      ;; staged, not applied: the sequence that adopts it
                      ;; proposes the sizes and places the windows together
                      (setf (wm-staged wm) rects (wm-staged-focus wm) focus)
                      (%ask-manage wm)))))
    (:wm
     (let ((plist (rest message)))
       (%enqueue wm (lambda () (%apply-action wm plist)))))
    (t nil)))

;;;; The protocol pump.

(defun handle-window (wm w &rest event)
  (event-case event
    (:dimensions (width height)
     (setf (win-width w) width (win-height w) height))
    ;; the identifier is the window's name everywhere off this process: the
    ;; daemon's trees and, later, the saved world are keyed by it
    (:identifier (identifier)
     (setf (win-id w) identifier)
     (%send wm (list :window-added :id identifier
                     :title (win-title w) :app-id (win-app-id w))))
    (:title (title)
     (setf (win-title w) title))
    (:app-id (app-id)
     (setf (win-app-id w) app-id))
    (:closed ()
     (setf (wm-windows wm) (remove w (wm-windows wm)))
     (when (eq (wm-focus wm) w)
       (setf (wm-focus wm) (first (wm-windows wm))))
     (when (win-id w) (%send wm (list :window-closed :id (win-id w))))
     (when (win-node w) (river-node-v1.destroy (win-node w)))
     (river-window-v1.destroy (win-proxy w)))
    (t (&rest args) (declare (ignore args)))))

(defun handle-output (wm o &rest event)
  (event-case event
    (:position (x y) (setf (out-x o) x (out-y o) y))
    (:dimensions (width height)
     (setf (out-width o) width (out-height o) height)
     (%send wm (list :output :width width :height height)))
    (:removed ()
     (setf (wm-outputs wm) (remove o (wm-outputs wm)))
     (river-output-v1.destroy (out-proxy o)))
    (t (&rest args) (declare (ignore args)))))

(defun handle-seat (wm seat &rest event)
  (declare (ignore seat))
  (event-case event
    ;; a click is a focus request, not a focus change: focus lives in the
    ;; daemon's tree, so report it and let the next arrangement carry it
    ;; back. Setting it here would be overwritten by that arrangement.
    (:window-interaction (window)
     (let ((w (%find-win wm window)))
       (when (and w (win-id w))
         (%send wm (list :window-focused :id (win-id w))))))
    (t (&rest args) (declare (ignore args)))))

(defun manage (wm)
  "One manage sequence: run whatever was deferred, propose each window the
size the daemon arranged for it, and focus the window the daemon focused. A
window with no rect yet is simply not proposed, so it stays undisplayed until
the daemon's next arrangement -- which is what the protocol expects. Always
finishes."
  (setf (wm-dirty wm) nil)
  (when (wm-staged wm)
    (setf (wm-rects wm) (wm-staged wm)
          (wm-focus-id wm) (wm-staged-focus wm)
          (wm-staged wm) nil))
  (%drain wm)
  (dolist (w (%tiled wm))
    (multiple-value-bind (x y width height) (%rect wm w)
      (declare (ignore x y))
      (when width
        (river-window-v1.propose-dimensions (win-proxy w) width height))))
  (let ((f (or (find (wm-focus-id wm) (wm-windows wm)
                     :key #'win-id :test #'equal)
               (wm-focus wm))))
    (when f
      (setf (wm-focus wm) f)
      (dolist (s (wm-seats wm))
        (river-seat-v1.focus-window s (win-proxy f)))))
  (river-window-manager-v1.manage-finish (wm-manager wm)))

(defun render (wm)
  "One render sequence: put every window where the daemon placed it. Always
finishes."
  (dolist (w (%tiled wm))
    (multiple-value-bind (x y width height) (%rect wm w)
      (declare (ignore width height))
      (when (and x (win-node w))
        (river-node-v1.set-position (win-node w) x y))))
  (river-window-manager-v1.render-finish (wm-manager wm)))

(defun handle-manager (wm &rest event)
  (event-case event
    (:window (id)
     (let ((w (make-win :proxy id :node (river-window-v1.get-node id))))
       (push w (wm-windows wm))
       (setf (wm-focus wm) w)
       (push (lambda (&rest ev) (apply #'handle-window wm w ev))
             (wl-proxy-hooks id))))
    (:output (id)
     (let ((o (make-out :proxy id)))
       (push o (wm-outputs wm))
       (push (lambda (&rest ev) (apply #'handle-output wm o ev))
             (wl-proxy-hooks id))))
    (:seat (id)
     (push id (wm-seats wm))
     (push (lambda (&rest ev) (apply #'handle-seat wm id ev))
           (wl-proxy-hooks id)))
    (:manage-start () (manage wm))
    (:render-start () (render wm))
    (:unavailable ()
     (format *error-output* "pine wm: window management unavailable~%")
     (setf (wm-done wm) t))
    (:finished () (setf (wm-done wm) t))
    (t (&rest args) (declare (ignore args)))))

(defun connect (wm)
  "Bind the window management global and the xkb bindings global."
  (let ((registry (wl-display.get-registry (wm-display wm))))
    (push (evlambda
            (:global (name interface version)
             (cond
               ((string= interface "river_window_manager_v1")
                (let ((mgr (wl-registry.bind registry name
                                             'river-window-manager-v1
                                             (min 4 version))))
                  (setf (wm-manager wm) mgr)
                  (push (lambda (&rest ev) (apply #'handle-manager wm ev))
                        (wl-proxy-hooks mgr))))
               ((string= interface "river_xkb_bindings_v1")
                (setf (wm-bindings-global wm)
                      (wl-registry.bind registry name
                                        'xkb-bind:river-xkb-bindings-v1
                                        (min 3 version)))))))
          (wl-proxy-hooks registry))
    (wl-display-roundtrip (wm-display wm))
    (wl-display-roundtrip (wm-display wm))))

(defun run-wm (&key (host pine.server:*host*) (port pine.server:*port*))
  "Drive the compositor's window management: bind the global, attach to the
daemon for policy, and run the sequence loop until the server finishes with
us. Errors out plainly when the compositor is not river or another window
manager holds the global."
  (let ((wm (%make-wm :display (wl-display-connect))))
    (connect wm)
    (unless (wm-manager wm)
      (wl-display-disconnect (wm-display wm))
      (error "pine wm: no river_window_manager_v1 global -- not river, or ~
another window manager is bound"))
    (let ((sys (sento.actor-system:make-actor-system
                '(:dispatchers (:shared (:workers 2 :strategy :random))))))
      (setf (wm-sys wm) sys)
      (sento.remoting:enable-remoting sys :host pine.server:*host* :port 0)
      (sento.actor-context:actor-of sys :name "display"
        :receive (lambda (msg) (handle-daemon wm msg) nil))
      (let ((daemon-uri (pine.server:daemon-uri "attach" :host host :port port))
            (self-uri (pine.server:local-uri "display"
                                             (sento.remoting:remoting-port sys))))
        (format *error-output* "pine wm: attaching to ~a as ~a~%" daemon-uri self-uri)
        (finish-output *error-output*)
        ;; The attach is a tell, so one sent before the daemon is listening is
        ;; simply lost and the session would run with no bindings. Keep asking
        ;; until it answers: session start order is not ours to control.
        (bordeaux-threads:make-thread
         (lambda ()
           (loop repeat 60
                 until (wm-ref wm)
                 do (pine.attach:attach-to-daemon sys daemon-uri self-uri :kind :wm)
                    (sleep 2))
           (unless (wm-ref wm)
             (format *error-output* "pine wm: no daemon answered at ~a~%" daemon-uri)
             (finish-output *error-output*)))
         :name "pine-wm-attach")))
    (unwind-protect
         ;; the wayland thread: daemon work first, then everything the
         ;; compositor has queued, then idle briefly. Sequences are answered
         ;; from this thread and no other.
         (loop until (wm-done wm) do
           (%drain-inbox wm)
           (loop while (wl-display-listen (wm-display wm))
                 do (wl-display-dispatch-event (wm-display wm)))
           (sleep 0.004))
      (wl-display-disconnect (wm-display wm)))))

(setf pine::*wm-hook* #'run-wm)
