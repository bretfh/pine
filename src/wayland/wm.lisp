;;;; The minimal river window manager: bind the global, answer every manage
;;;; and render sequence, tile side by side on the first output, focus the
;;;; newest or last-clicked window. This is the pump half of design/wm.org
;;;; with a placeholder local policy; the daemon-owned tree policy replaces
;;;; the bodies of MANAGE and RENDER, not the machinery around them.

(defpackage #:pine.wm
  (:use #:cl #:wayflan-client #:pine.river-wm)
  (:export #:run-wm)
  (:documentation "Minimal river-window-management-v1 client."))
(in-package #:pine.wm)

(defstruct (wm (:constructor %make-wm))
  display
  manager
  (windows nil)   ; newest first
  (outputs nil)   ; newest first
  (seats nil)
  (focus nil)
  (done nil))

(defstruct win proxy node width height)
(defstruct out proxy x y width height)

(defun %find-win (wm proxy)
  (find proxy (wm-windows wm) :key #'win-proxy :test #'eq))

(defun %screen (wm)
  "The output windows tile onto: the first one announced."
  (first (last (wm-outputs wm))))

(defun handle-window (wm w &rest event)
  (event-case event
    (:dimensions (width height)
     (setf (win-width w) width (win-height w) height))
    (:closed ()
     (setf (wm-windows wm) (remove w (wm-windows wm)))
     (when (eq (wm-focus wm) w)
       (setf (wm-focus wm) (first (wm-windows wm))))
     (when (win-node w) (ignore-errors (river-node-v1.destroy (win-node w))))
     (ignore-errors (river-window-v1.destroy (win-proxy w))))
    (t ())))

(defun handle-output (wm o &rest event)
  (event-case event
    (:position (x y) (setf (out-x o) x (out-y o) y))
    (:dimensions (width height)
     (setf (out-width o) width (out-height o) height))
    (:removed ()
     (setf (wm-outputs wm) (remove o (wm-outputs wm)))
     (ignore-errors (river-output-v1.destroy (out-proxy o))))
    (t ())))

(defun handle-seat (wm seat &rest event)
  (declare (ignore seat))
  (event-case event
    (:window-interaction (window)
     (let ((w (%find-win wm window)))
       (when w (setf (wm-focus wm) w))))
    (t ())))

(defun manage (wm)
  "One manage sequence: equal side-by-side proposals on the screen, focus to
the newest or last-interacted window. Always finishes."
  (let ((screen (%screen wm))
        (wins (reverse (wm-windows wm))))
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
        (wins (reverse (wm-windows wm))))
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
    (t ())))

(defun run-wm ()
  "Connect to the compositor, bind river_window_manager_v1, and run the
sequence loop until the server finishes with us. Errors out plainly when the
compositor is not river or another window manager holds the global."
  (let ((wm (%make-wm :display (wl-display-connect))))
    (let ((registry (wl-display.get-registry (wm-display wm))))
      (push (evlambda
              (:global (name interface version)
               (when (string= interface "river_window_manager_v1")
                 (let ((mgr (wl-registry.bind
                             registry name 'river-window-manager-v1
                             (min 4 version))))
                   (setf (wm-manager wm) mgr)
                   (push (lambda (&rest ev) (apply #'handle-manager wm ev))
                         (wl-proxy-hooks mgr))))))
            (wl-proxy-hooks registry))
      (wl-display-roundtrip (wm-display wm)))
    (unless (wm-manager wm)
      (wl-display-disconnect (wm-display wm))
      (error "pine wm: no river_window_manager_v1 global -- not river, or ~
another window manager is bound"))
    (unwind-protect
        (loop until (wm-done wm)
              do (wl-display-dispatch-event (wm-display wm)))
      (wl-display-disconnect (wm-display wm)))))

(setf pine::*wm-hook* #'run-wm)
