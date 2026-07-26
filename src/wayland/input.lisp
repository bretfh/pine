(in-package #:pine.wayland)

;;;; Pointer input for layer surfaces: bind wl_seat -> wl_pointer, track the
;;;; focused surface and cursor position, and drive the layout engine's own
;;;; hit-testing (pine.layout:node-at / click-thunk) against the retained,
;;;; arranged tree -- so hover highlights and clicks land on exactly what is
;;;; painted. wayflan delivers pointer x/y already in surface pixels.

(defvar *on-hover* nil
  "When set, a function of the hovered node (or nil) called on each hover change
-- the daemon-attached client uses it to push the node's hint to the echo.")

(defconstant +btn-left+ #x110)


;;;; Connect + bind globals, including the seat.

(defun connect-desktop ()
  "Open the display and bind compositor, shm, layer-shell, and seat globals."
  (let* ((backing (connect-display))
         (display (display backing))
         (registry (wl-display.get-registry display))
         (conn (make-wl-conn :display display :backing backing)))
    (push (evlambda
            (:global (name interface version)
             (declare (ignore version))
             (case (a:when-let ((it (find-interface-named interface))) (class-name it))
               (wl-compositor
                (setf (wl-conn-compositor conn) (wl-registry.bind registry name 'wl-compositor 4)))
               (wl-shm
                (setf (wl-conn-shm conn) (wl-registry.bind registry name 'wl-shm 1)))
               (zwlr-layer-shell-v1
                (setf (wl-conn-shell conn) (wl-registry.bind registry name 'zwlr-layer-shell-v1 1)))
               (wl-seat
                (let ((seat (wl-registry.bind registry name 'wl-seat 5)))
                  (setf (wl-conn-seat conn) seat)
                  (push (a:curry #'handle-desktop-seat conn) (wl-proxy-hooks seat)))))))
          (wl-proxy-hooks registry))
    (wl-display-roundtrip display)            ; globals + seat bound
    (wl-display-roundtrip display)            ; seat capabilities -> pointer
    (unless (wl-conn-shell conn)
      (wl-display-disconnect display)
      (error "compositor does not advertise zwlr_layer_shell_v1"))
    conn))

(defun handle-desktop-seat (conn &rest event)
  (event-case event
    (:capabilities (capabilities)
     (if (member :pointer capabilities)
         (unless (wl-conn-pointer conn)
           (let ((pointer (wl-seat.get-pointer (wl-conn-seat conn))))
             (setf (wl-conn-pointer conn) pointer)
             (push (a:curry #'handle-pointer conn) (wl-proxy-hooks pointer))))
         (when (wl-conn-pointer conn)
           (destroy-proxy (wl-conn-pointer conn))
           (setf (wl-conn-pointer conn) nil))))
    (:name (name) (declare (ignore name)))))

;;;; Pointer events -> hover + click.

(defun handle-pointer (conn &rest event)
  (event-case event
    (:enter (serial surface x y)
     (setf (wl-conn-ptr-serial conn) serial
           (wl-conn-focus conn) (conn-surface->ls conn surface)
           (wl-conn-ptr-x conn) x (wl-conn-ptr-y conn) y)
     (update-hover conn))
    (:motion (time-ms x y)
     (declare (ignore time-ms))
     (setf (wl-conn-ptr-x conn) x (wl-conn-ptr-y conn) y)
     (if (wl-conn-drag conn) (drag-to conn) (update-hover conn)))
    (:leave (serial surface)
     (declare (ignore serial surface))
     (clear-hover conn)
     (setf (wl-conn-focus conn) nil))
    (:button (serial time-ms button state)
     (declare (ignore serial time-ms))
     (when (= button +btn-left+)
       (ecase state
         (:pressed  (pointer-press conn))
         (:released (pointer-release conn)))))
    (:axis (time-ms axis delta) (declare (ignore time-ms axis delta)))
    (:frame ())))

(defun pointer-node (conn)
  "The interactive node under the cursor on the focused surface, or nil."
  (a:when-let ((ls (wl-conn-focus conn)))
    (when (ls-tree ls)
      (l:node-at (ls-tree ls) (round (wl-conn-ptr-y conn)) (round (wl-conn-ptr-x conn))))))

(defun clear-hover (conn)
  (a:when-let ((ls (wl-conn-focus conn)))
    (when (ls-hover ls)
      (setf (l:hovered (ls-hover ls)) nil (ls-hover ls) nil)
      (when *on-hover* (funcall *on-hover* nil))
      (paint-surface ls))))

(defun update-hover (conn)
  (a:when-let ((ls (wl-conn-focus conn)))
    (let ((hit (pointer-node conn)))
      (unless (eq hit (ls-hover ls))
        (when (ls-hover ls) (setf (l:hovered (ls-hover ls)) nil))
        (when hit (setf (l:hovered hit) t))
        (setf (ls-hover ls) hit)
        (when *on-hover* (funcall *on-hover* hit))
        (paint-surface ls)))))

(defun pointer-press (conn)
  "Left press: a slider starts a drag (live scrub, fire on release); anything
else runs its action now and rebuilds the surface."
  (let ((hit (pointer-node conn)))
    (cond
      ((typep hit 'l:slider)
       (setf (wl-conn-drag conn) hit)
       (drag-to conn))
      (t (pointer-click conn)))))

(defun drag-to (conn)
  "Move the dragged slider's knob to the cursor and repaint, without firing the
callback (that waits for release, so a scrub does not spam the action)."
  (a:when-let ((slider (wl-conn-drag conn)) (ls (wl-conn-focus conn)))
    (setf (l:value slider) (l:slider-value-at slider (round (wl-conn-ptr-x conn))))
    (paint-surface ls)))

(defun pointer-release (conn)
  "End a drag: fire the slider's on-change once with the final value."
  (a:when-let ((slider (wl-conn-drag conn)))
    (setf (wl-conn-drag conn) nil)
    (a:when-let ((fn (l:on-change slider)))
      (pine.eval:attempt (lambda () (funcall fn (l:value slider)))
                         "slider drag"))))

(defun pointer-click (conn)
  "Run the callback under the cursor, then rebuild + repaint the surface (the
action may have changed the refs the tree reads)."
  (a:when-let ((ls (wl-conn-focus conn)))
    (when (ls-tree ls)
      (let ((thunk (l:click-thunk (ls-tree ls)
                                  (round (wl-conn-ptr-y conn)) (round (wl-conn-ptr-x conn)))))
        (when thunk
          (pine.eval:attempt thunk "widget action")
          (build-tree ls)
          (paint-surface ls))))))
