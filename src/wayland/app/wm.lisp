(defpackage #:pine.wayland.app.wm
  (:use #:cl #:wayflan-client #:pine.wayland.protocol #:pine.wayland.connection)
  (:export #:run-wm))

(in-package #:pine.wayland.app.wm)

;;;; The river window manager frontend: bind the global, answer every manage
;;;; and render sequence, and carry policy between the compositor and the
;;;; daemon. The daemon (pine.wm) decides what the chords are and what they
;;;; do; this process registers them with the compositor, reports presses,
;;;; and applies the actions it is told to, always inside the sequence the
;;;; protocol requires. design/wm.org is the design.


(defstruct (wm (:constructor %make-wm))
  display
  manager
  bindings-global                       ; river_xkb_bindings_v1
  layer-shell                           ; river_layer_shell_v1
  (layer-focus :none)                   ; :none, :exclusive or :non-exclusive
  (windows nil)                         ; newest first
  (outputs nil)                         ; newest first
  (seats nil)
  (bindings nil)                        ; chord string -> binding proxy
  (staged nil)
  (rects nil)                           ; (id x y w h) for this generation
  (focus-id nil)                        ; identifier the daemon focused
  (border nil)                          ; the daemon's border width and colours
  (staged-focus nil)
  (dirty nil)                           ; a manage sequence has been asked for
  (pending nil)                         ; thunks to run in the next manage seq
  backing                               ; the connection, as something to wait on
  pump                                  ; the queue the daemon's threads fill
  (focus nil)
  (ref nil)                             ; daemon client actor
  (sys nil)
  (done nil))

(defstruct win proxy node id title app-id width height
                hint                    ; the client's decoration_hint
                framed)                 ; whether it has been told to use ssd
(defstruct out proxy x y width height
                shell                   ; river_layer_shell_output_v1
                area)                   ; (X Y WIDTH HEIGHT) left by layer surfaces

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
  "Hand THUNK to the wayland thread and wake it."
  (pine.frontend:enqueue (wm-pump wm) thunk))

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
  (let* ((key (pine.editor.key:parse-key chord))
         (keysym (xkb:xkb-keysym-from-name (pine.editor.key:key-sym key) '(:no-flags)))
         (modifiers (append (when (pine.editor.key:key-shift key) '(:shift))
                            (when (pine.editor.key:key-ctrl key)  '(:ctrl))
                            (when (pine.editor.key:key-meta key)  '(:mod1))
                            (when (pine.editor.key:key-super key) '(:mod4)))))
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
       (let ((binding (river-xkb-bindings-v1.get-xkb-binding
                       (wm-bindings-global wm) seat keysym modifiers)))
         (setf (gethash chord (wm-bindings wm)) binding)
         (push (lambda (&rest event)
                 (event-case event
                   (:pressed () (%send wm (list :binding :keys chord)))
                   (t (&rest args) (declare (ignore args)))))
               (wl-proxy-hooks binding))
         (%defer wm (lambda () (river-xkb-binding-v1.enable binding))))))))

(defun %install-bindings (wm table)
  "Replace every registered binding with the daemon's TABLE."
  (unless (wm-bindings wm)
    (setf (wm-bindings wm) (make-hash-table :test 'equal)))
  (maphash (lambda (chord binding)
             (declare (ignore chord))
             (river-xkb-binding-v1.destroy binding))
           (wm-bindings wm))
  (clrhash (wm-bindings wm))
  (let ((seat (first (wm-seats wm))))
    (cond
      ((null (wm-bindings-global wm))
       (format *error-output* "pine wm: no river_xkb_bindings_v1 global~%"))
      ((null seat)
       (format *error-output* "pine wm: no seat yet, bindings not registered~%"))
      (t
       (loop :for (chord . command) :in table
             :do (%register-binding wm seat chord command))
       (format *error-output* "pine wm: registered ~d binding(s)~%"
               (hash-table-count (wm-bindings wm)))
       (finish-output *error-output*)))))

;;;; Actions the daemon asks for.

(defparameter +build-environment+
  '("GUIX_ENVIRONMENT" "CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"
    "LD_LIBRARY_PATH")
  "Variables that belong to the environment pine itself was built and run in.
A window manager started from inside one, such as a guix shell over the
repo's manifest, would otherwise pass it to everything it launches, and a
terminal would open in pine's build environment rather than the session.")

(defun %spawn (command)
  "Launch COMMAND as the user's own program: their login environment, their
home directory, and the compositor's display. Only WAYLAND_DISPLAY is carried
over from this process."
  (let ((login (format nil "cd \"$HOME\" && exec ~a" command)))
    (uiop:launch-program (list "sh" "-l" "-c" login)
                         :environment
                         (append
                          (remove-if
                           (lambda (entry)
                             (some (lambda (name)
                                     (let ((prefix (concatenate 'string name "=")))
                                       (and (>= (length entry) (length prefix))
                                            (string= prefix entry
                                                     :end2 (length prefix)))))
                                   +build-environment+))
                           (sb-ext:posix-environ))
                          nil))))

(defun %apply-action (wm plist)
  (destructuring-bind (&key action command id &allow-other-keys) plist
    (case action
      (:spawn (%spawn command))
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
is reported rather than dropped in silence."
  (let ((ref (wm-ref wm)))
    (cond
      (ref (sento.actor:tell ref message))
      (t (format *error-output* "pine wm: no daemon yet, dropped ~s~%"
                 (first message))
         (finish-output *error-output*)))))

(defun %area (o)
  "(X Y WIDTH HEIGHT) windows may tile into on output O, or nil.

The area left after layer surfaces take their exclusive zones, so a bar keeps
its strip and windows get the rest. Before river reports one, that is the
whole output."
  (or (out-area o)
      (when (out-width o)
        (list (or (out-x o) 0) (or (out-y o) 0) (out-width o) (out-height o)))))

(defun %report-area (wm)
  "Tell the daemon the area to arrange windows in."
  (let ((area (and (%screen wm) (%area (%screen wm)))))
    (when area
      (destructuring-bind (x y width height) area
        (%send wm (list :output :x x :y y :width width :height height))))))

(defun %report-state (wm)
  "Tell the daemon everything the compositor has already reported. The output
and any existing windows arrive before the attach completes, and after a
respawn were never sent at all, so the frontend replays them whenever it
gains a daemon."
  (%report-area wm)
  (dolist (w (%tiled wm))
    (when (win-id w)
      (%send wm (list :window-added :id (win-id w)
                      :title (win-title w) :app-id (win-app-id w))))))

(defun handle-wm-message (wm message)
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
     (destructuring-bind (&key rects focus border) (rest message)
       (%enqueue wm (lambda ()
                      (setf (wm-staged wm) rects (wm-staged-focus wm) focus
                            (wm-border wm) border)
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
    (:identifier (identifier)
     (setf (win-id w) identifier)
     (%send wm (list :window-added :id identifier
                     :title (win-title w) :app-id (win-app-id w))))
    (:title (title)
     (setf (win-title w) title))
    (:app-id (app-id)
     (setf (win-app-id w) app-id))
    (:decoration-hint (hint)
     (setf (win-hint w) hint (win-framed w) nil))
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
     (%report-area wm))
    (:removed ()
     (setf (wm-outputs wm) (remove o (wm-outputs wm)))
     (when (out-shell o)
       (river-layer-shell-output-v1.destroy (out-shell o))
       (setf (out-shell o) nil))
     (river-output-v1.destroy (out-proxy o)))
    (t (&rest args) (declare (ignore args)))))

(defun handle-layer-output (wm o &rest event)
  (event-case event
    (:non-exclusive-area (x y width height)
     (setf (out-area o) (list x y width height))
     (%report-area wm))
    (t (&rest args) (declare (ignore args)))))

(defun %watch-layer-output (wm o)
  "Take the layer shell state of output O, and make it the output new layer
surfaces land on when they name none themselves."
  (let ((shell (river-layer-shell-v1.get-output (wm-layer-shell wm)
                                                      (out-proxy o))))
    (setf (out-shell o) shell)
    (push (lambda (&rest ev) (apply #'handle-layer-output wm o ev))
          (wl-proxy-hooks shell))
    (%defer wm (lambda ()
                 (when (eq o (%screen wm))
                   (river-layer-shell-output-v1.set-default shell))))))

(defun handle-layer-seat (wm &rest event)
  "Follow which layer surface holds the keyboard, so a manage sequence does not
take focus back from a panel that asked for it."
  (event-case event
    (:focus-exclusive () (setf (wm-layer-focus wm) :exclusive))
    (:focus-non-exclusive () (setf (wm-layer-focus wm) :non-exclusive))
    (:focus-none () (setf (wm-layer-focus wm) :none))
    (t (&rest args) (declare (ignore args)))))

(defun handle-wm-seat (wm seat &rest event)
  (declare (ignore seat))
  (event-case event
    (:window-interaction (window)
     (let ((w (%find-win wm window)))
       (when (and w (win-id w))
         (%send wm (list :window-focused :id (win-id w))))))
    (t (&rest args) (declare (ignore args)))))

;;;; Chrome. Every window is framed the same way: the client is asked to draw
;;;; no decorations of its own, and the compositor draws the border pine's
;;;; theme names. The window's dimensions are its content, and borders are
;;;; drawn outside that, in the room the daemon's arrangement leaves.

(defun %draws-own-chrome-p (w)
  "True when W's client cannot be talked out of decorating itself.

The compositor only carries a decoration mode to a client that bound
xdg-decoration; one that never did is reported as `only_supports_csd', and
asking it to use server side decorations does nothing at all."
  (eq (win-hint w) :only-supports-csd))

(defun %take-chrome (wm w)
  "Ask W's client to leave its decorations to us. Window management state, so
this belongs to a manage sequence."
  (declare (ignore wm))
  (unless (win-framed w)
    (setf (win-framed w) t)
    (if (%draws-own-chrome-p w)
        (river-window-v1.use-csd (win-proxy w))
        (river-window-v1.use-ssd (win-proxy w)))
    (river-window-v1.set-tiled (win-proxy w) '(:top :bottom :left :right))))

(defun %border-rgba (colour)
  "(values R G B A) for an (R G B) theme COLOUR, in the channels set_borders
takes.

The protocol's channels are 32 bit and premultiplied. A theme colour is eight
bit and the border is opaque, so premultiplying is the identity here."
  (destructuring-bind (r g b) colour
    (flet ((scale (c) (floor (* c #xFFFFFFFF) 255)))
      (values (scale r) (scale g) (scale b) #xFFFFFFFF))))

(defun %draw-borders (wm w)
  "Border W in the colour that says whether it holds focus. Rendering state."
  (let ((border (wm-border wm)))
    (when border
      (multiple-value-bind (r g b a)
          (%border-rgba (getf border (if (eq w (wm-focus wm)) :active :inactive)))
        (river-window-v1.set-borders (win-proxy w) '(:top :bottom :left :right)
                                     (getf border :width) r g b a)))))

(defun manage (wm)
  "One manage sequence: run whatever was deferred, propose each window the
size the daemon arranged for it, and focus the window the daemon focused. A
window with no rect yet is simply not proposed, so it stays undisplayed until
the daemon's next arrangement, which is what the protocol expects. Always
finishes."
  (setf (wm-dirty wm) nil)
  (when (wm-staged wm)
    (setf (wm-rects wm) (wm-staged wm)
          (wm-focus-id wm) (wm-staged-focus wm)
          (wm-staged wm) nil))
  (%drain wm)
  (dolist (w (%tiled wm))
    (%take-chrome wm w)
    (multiple-value-bind (x y width height) (%rect wm w)
      (declare (ignore x y))
      (when width
        (river-window-v1.propose-dimensions (win-proxy w) width height))))
  (let ((f (or (find (wm-focus-id wm) (wm-windows wm)
                     :key #'win-id :test #'equal)
               (wm-focus wm))))
    (when f
      (setf (wm-focus wm) f)
      (when (eq (wm-layer-focus wm) :none)
        (dolist (s (wm-seats wm))
          (river-seat-v1.focus-window s (win-proxy f))))))
  (river-window-manager-v1.manage-finish (wm-manager wm)))

(defun render (wm)
  "One render sequence: put every window where the daemon placed it and border
it. Always finishes."
  (dolist (w (%tiled wm))
    (multiple-value-bind (x y width height) (%rect wm w)
      (declare (ignore width height))
      (when x
        (%draw-borders wm w)
        (when (win-node w)
          (river-node-v1.set-position (win-node w) x y)))))
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
             (wl-proxy-hooks id))
       (when (wm-layer-shell wm) (%watch-layer-output wm o))))
    (:seat (id)
     (push id (wm-seats wm))
     (push (lambda (&rest ev) (apply #'handle-wm-seat wm id ev))
           (wl-proxy-hooks id))
     (when (wm-layer-shell wm)
       (let ((seat (river-layer-shell-v1.get-seat (wm-layer-shell wm) id)))
         (push (lambda (&rest ev) (apply #'handle-layer-seat wm ev))
               (wl-proxy-hooks seat)))))
    (:manage-start () (manage wm))
    (:render-start () (render wm))
    (:unavailable ()
     (format *error-output* "pine wm: window management unavailable~%")
     (setf (wm-done wm) t))
    (:finished () (setf (wm-done wm) t))
    (t (&rest args) (declare (ignore args)))))

(defun connect-wm (wm)
  "Bind the window management, xkb bindings and layer shell globals."
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
                                        'river-xkb-bindings-v1
                                        (min 3 version))))
               ((string= interface "river_layer_shell_v1")
                (setf (wm-layer-shell wm)
                      (wl-registry.bind registry name
                                        'river-layer-shell-v1
                                        (min 1 version)))))))
          (wl-proxy-hooks registry))
    (wl-display-roundtrip (wm-display wm))
    (wl-display-roundtrip (wm-display wm))))

(defun run-wm (&key (host pine.core.server:*host*) (port pine.core.server:*port*))
  "Drive the compositor's window management: bind the global, attach to the
daemon for policy, and run the sequence loop until the server finishes with
us. Errors out plainly when the compositor is not river or another window
manager holds the global."
  (let* ((backing (connect-display))
         (wm (%make-wm :display (display backing) :backing backing
                       :pump (pine.frontend:make-pump))))
    (connect-wm wm)
    (unless (wm-manager wm)
      (wl-display-disconnect (wm-display wm))
      (format *error-output*
              "pine wm: no river_window_manager_v1 global; this compositor ~
offers no window management, or another manager holds it~%")
      (finish-output *error-output*)
      (uiop:quit pine:+frontend-unavailable+))
    (setf (wm-sys wm) (pine.frontend:attach
                       :wm (lambda (msg) (handle-wm-message wm msg))
                       :host host :port port
                       :ready (lambda () (and (wm-ref wm) t))))
    (unwind-protect
         (pine.frontend:run (wm-backing wm) (wm-pump wm)
                            :done (lambda () (wm-done wm)))
      (pine.frontend:close-pump (wm-pump wm))
      (pine.frontend:shutdown backing))))

(defmethod pine.core.attach:run-frontend ((app pine.wm::wm-app))
  (declare (ignore app))
  (run-wm))
