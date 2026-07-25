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
  (pending nil)                         ; thunks to run in the next manage seq
  (inbox nil)                           ; thunks from the daemon's actor thread
  (lock (bordeaux-threads:make-lock))
  (focus nil)
  (ref nil)                             ; daemon client actor
  (sys nil)
  (done nil))

(defstruct win proxy node width height)
(defstruct out proxy x y width height)

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

(defun %defer (wm thunk)
  (push thunk (wm-pending wm))
  (river-window-manager-v1.manage-dirty (wm-manager wm)))

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
  (destructuring-bind (&key action command direction &allow-other-keys) plist
    (case action
      (:spawn
       ;; the frontend runs in the compositor's session, so a program it
       ;; launches inherits WAYLAND_DISPLAY and lands in this session
       (uiop:launch-program (list "sh" "-c" command)))
      (:close
       (%defer wm (lambda ()
                    (let ((f (wm-focus wm)))
                      (when f (river-window-v1.close (win-proxy f)))))))
      (:focus
       (%defer wm (lambda ()
                    (let* ((wins (%tiled wm))
                           (n (length wins)))
                      (when (plusp n)
                        (let* ((i (or (position (wm-focus wm) wins) 0))
                               (step (if (eq direction :prev) -1 1)))
                          (setf (wm-focus wm) (nth (mod (+ i step) n) wins))))))))
      (:exit
       (river-window-manager-v1.exit-session (wm-manager wm)))
      (t (format *error-output* "pine wm: unknown action ~s~%" action)))))

;;;; The daemon seam.

(defun %send (wm message)
  (let ((ref (wm-ref wm)))
    (when ref (sento.actor:tell ref message))))

(defun handle-daemon (wm message)
  "Runs on the daemon's actor thread: decide nothing here, hand the work to
the wayland thread."
  (case (first message)
    (:attached
     (destructuring-bind (&key id client-uri) (rest message)
       (declare (ignore id))
       (setf (wm-ref wm) (sento.remoting:make-remote-ref (wm-sys wm) client-uri))))
    (:bindings
     (destructuring-bind (&key table) (rest message)
       (%enqueue wm (lambda () (%install-bindings wm table)))))
    (:wm
     (let ((plist (rest message)))
       (%enqueue wm (lambda () (%apply-action wm plist)))))
    (t nil)))

;;;; The protocol pump.

(defun handle-window (wm w &rest event)
  (event-case event
    (:dimensions (width height)
     (setf (win-width w) width (win-height w) height))
    (:closed ()
     (setf (wm-windows wm) (remove w (wm-windows wm)))
     (when (eq (wm-focus wm) w)
       (setf (wm-focus wm) (first (wm-windows wm))))
     (when (win-node w) (river-node-v1.destroy (win-node w)))
     (river-window-v1.destroy (win-proxy w)))
    (t (&rest args) (declare (ignore args)))))

(defun handle-output (wm o &rest event)
  (event-case event
    (:position (x y) (setf (out-x o) x (out-y o) y))
    (:dimensions (width height)
     (setf (out-width o) width (out-height o) height))
    (:removed ()
     (setf (wm-outputs wm) (remove o (wm-outputs wm)))
     (river-output-v1.destroy (out-proxy o)))
    (t (&rest args) (declare (ignore args)))))

(defun handle-seat (wm seat &rest event)
  (declare (ignore seat))
  (event-case event
    (:window-interaction (window)
     (let ((w (%find-win wm window)))
       (when w (setf (wm-focus wm) w))))
    (t (&rest args) (declare (ignore args)))))

(defun manage (wm)
  "One manage sequence: run whatever was deferred, propose equal side-by-side
widths on the screen, and focus the current window. Always finishes."
  (%drain wm)
  (let ((screen (%screen wm))
        (wins (%tiled wm)))
    (when (and screen wins (out-width screen))
      (let ((cw (max 1 (floor (out-width screen) (length wins)))))
        (dolist (w wins)
          (river-window-v1.propose-dimensions
           (win-proxy w) cw (out-height screen)))))
    (let ((f (or (wm-focus wm) (first (wm-windows wm)))))
      (when f
        (dolist (s (wm-seats wm))
          (river-seat-v1.focus-window s (win-proxy f))))))
  (river-window-manager-v1.manage-finish (wm-manager wm)))

(defun render (wm)
  "One render sequence: place windows left to right at their actual widths.
Always finishes."
  (let ((screen (%screen wm))
        (wins (%tiled wm)))
    (when screen
      (let ((x (or (out-x screen) 0))
            (y (or (out-y screen) 0)))
        (dolist (w wins)
          (when (and (win-node w) (win-width w))
            (river-node-v1.set-position (win-node w) x y)
            (incf x (win-width w)))))))
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
        (pine.attach:attach-to-daemon sys daemon-uri self-uri :kind :wm)))
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
