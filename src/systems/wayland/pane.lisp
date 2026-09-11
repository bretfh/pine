(in-package #:pine/wayland)

(defvar *namespace* "gtk-layer-shell")

(defvar *font-size* 15)
(defparameter +window+ '(900 . 600))

(defclass pane ()
  ((name-of :initarg :name    :reader name-of)
   (shell   :initarg :shell   :reader shell)
   (surface :initform nil     :accessor surface)
   (took    :initform nil     :accessor took)
   (tree    :initarg :tree    :accessor tree :initform nil)
   (width    :initarg :width    :accessor width :initform 0)
   (height    :initarg :height    :accessor height :initform 0)
   (hover   :initform nil     :accessor hover)
   (kind    :initform nil     :accessor kind)
   (node-of :initform nil     :accessor node-of)
   (where   :initform nil     :accessor where)
   (dirty   :initform nil     :accessor dirty)
   (on-resize :initarg :on-resize :accessor on-resize :initform nil)
   (configuredp :initform nil :accessor configuredp)))

(defun chromep (s) (eq :chrome (kind s)))

(defun %sized (s width height)
  (let ((was-width (width s)) (was-height (height s)))
    (unless (zerop width) (setf (width s) width))
    (unless (zerop height) (setf (height s) height))
    (setf (configuredp s) t)
    (when (and (on-resize s)
               (or (/= was-width (width s)) (/= was-height (height s))))
      (funcall (on-resize s) s))
    s))

(defmethod print-object ((s pane) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~dx~d" (name-of s) (width s) (height s))))

(defun %canvas (data width height stride)
  (let ((it (cl-cairo2:create-image-surface-for-data data :argb32 width height
                                                     stride)))
    (values (make-instance 'canvas
                           :context (cl-cairo2:create-context it)
                           :size *font-size*)
            it)))

(defun %blit (s)
  (let ((width (width s)) (height (height s)))
    (when (and (plusp width) (plusp height) (tree s) (surface s)
               (configuredp s))
      (let* ((stride (* width 4))
             (size (* stride height)))
        (shm:with-open-shm-and-mmap* (obj data (:direction :io) (size))
          (let (buffer)
            (with-proxy (pool (wl-shm.create-pool (shm (shell s))
                                                  (shm:shm-fd obj) size))
              (setf buffer (wl-shm-pool.create-buffer pool 0 width height stride
                                                      :argb8888)))
            (multiple-value-bind (m it) (%canvas data width height stride)
              (unwind-protect
                   (progn
                     (with-canvas (m)
                       (cl-cairo2:set-operator :source)
                       (cl-cairo2:set-source-rgba 0d0 0d0 0d0 0d0)
                       (cl-cairo2:paint)
                       (cl-cairo2:set-operator :over))
                     (ui:with-pass
                       (ui:with-faces
                         (ui:dress (tree s))
                         (ui:measure (tree s) m width height)
                         (ui:lay (tree s) m 0 0 width height)
                         (ui:paint (tree s) m))))
                (cl-cairo2:destroy (context m))
                (cl-cairo2:destroy it)))
            (when (uiop:getenv "PINE_FRAME_DUMP")
              (fault:or-nothing "cairo may refuse the buffer it was handed"
               (let ((it (cl-cairo2:create-image-surface-for-data data :argb32
                                                                  width height
                                                                  stride)))
                 (cl-cairo2:surface-write-to-png
                  it (format nil "~a-~a.png" (uiop:getenv "PINE_FRAME_DUMP")
                             (name-of s)))
                 (cl-cairo2:destroy it))))
            (wl-surface.attach (surface s) buffer 0 0)
            (wl-surface.damage-buffer (surface s) 0 0 width height)
            (wl-surface.commit (surface s))
            (push (evelambda (:release () (destroy-proxy buffer)))
                  (wl-proxy-hooks buffer))))))
    s))

(defun paint (s)
  (cond ((chromep s) (setf (dirty s) t) nil)
        (t (%blit s))))

(defun render (s)
  (when (and (chromep s) (took s))
    (river-shell-surface-v1.sync-next-commit (took s))
    (%blit s)
    (let ((node (node-of s))
          (where (where s)))
      (when node
        (destructuring-bind (top right bottom left) (or (getf where :margin)
                                                        '(0 0 0 0))
          (declare (ignore right bottom))
          (river-node-v1.set-position node left top))
        (river-node-v1.place-top node)))
    (setf (dirty s) nil))
  s)

(defun measure (s &key (avail 3840))
  (let* ((it (cl-cairo2:create-image-surface :argb32 1 1))
         (m (make-instance 'canvas :context (cl-cairo2:create-context it)
                                          :size *font-size*)))
    (unwind-protect
         (ui:with-pass
           (ui:with-faces
             (ui:dress (tree s))
             (ui:measure (tree s) m avail avail)))
      (cl-cairo2:destroy it))))

(defun cell (s)
  (declare (ignore s))
  (let* ((it (cl-cairo2:create-image-surface :argb32 1 1))
         (m (make-instance 'canvas :context (cl-cairo2:create-context it)
                                          :size *font-size*)))
    (unwind-protect (ui:text-size m "M" *font-size*)
      (cl-cairo2:destroy it))))

(defun %layer (where)
  (let ((edges (getf where :edges)))
    (cond ((null edges) :top)
          ((= 4 (length edges)) :background)
          ((plusp (or (getf where :reserve) 0)) :top)
          (t :overlay))))

(defun %open-layer (s where)
  (let* ((sh (shell s))
         (surface (wl-compositor.create-surface (compositor sh)))
         (it (zwlr-layer-shell-v1.get-layer-surface
              (layer sh) surface nil (%layer where) *namespace*)))
    (setf (surface s) surface (took s) it)
    (zwlr-layer-surface-v1.set-anchor it (getf where :edges))
    (zwlr-layer-surface-v1.set-size it (or (getf where :width) 0)
                                    (or (getf where :height) 0))
    (zwlr-layer-surface-v1.set-exclusive-zone it (or (getf where :reserve) 0))
    (destructuring-bind (top right bottom left) (or (getf where :margin)
                                                    '(0 0 0 0))
      (zwlr-layer-surface-v1.set-margin it top right bottom left))
    (push (evelambda
            (:configure (serial width height)
             (zwlr-layer-surface-v1.ack-configure it serial)
             (%sized s width height)
             (paint s))
            (:closed () (setf (configuredp s) nil)))
          (wl-proxy-hooks it))
    (wl-surface.commit surface)
    s))

(defun %open-chrome (s where)
  (let* ((sh (shell s))
         (surface (wl-compositor.create-surface (compositor sh)))
         (it (river-window-manager-v1.get-shell-surface (chrome sh)
                                                        surface)))
    (setf (surface s) surface (took s) it (kind s) :chrome
          (node-of s) (river-shell-surface-v1.get-node it)
          (where s) where
          (configuredp s) t
          (dirty s) t)
    s))

(defun %open-window (s title)
  (when (zerop (width s)) (setf (width s) (car +window+)))
  (when (zerop (height s)) (setf (height s) (cdr +window+)))
  (let* ((sh (shell s))
         (surface (wl-compositor.create-surface (compositor sh)))
         (xdg (xdg-wm-base.get-xdg-surface (toplevel sh) surface))
         (top (xdg-surface.get-toplevel xdg)))
    (setf (surface s) surface (took s) top)
    (push (evelambda
            (:configure (serial)
             (xdg-surface.ack-configure xdg serial)
             (setf (configuredp s) t)
             (paint s)))
          (wl-proxy-hooks xdg))
    (push (evlambda
            (:configure (width height states)
             (declare (ignore states))
             (%sized s width height))
            (:close () (setf (configuredp s) nil)))
          (wl-proxy-hooks top))
    (xdg-toplevel.set-title top title)
    (xdg-toplevel.set-app-id top "pine")
    (wl-surface.commit surface)
    s))

(defun open-pane (s where &key (windowp nil) (title "pine"))
  (let ((sh (shell s)))
    (when (plusp (or (getf where :width) 0)) (setf (width s) (getf where :width)))
    (when (plusp (or (getf where :height) 0)) (setf (height s) (getf where :height)))
    (setf (where s) where)
    (cond (windowp (setf (kind s) :window) (%open-window s title))
          ((layer sh) (setf (kind s) :layer) (%open-layer s where))
          ((chrome sh) (%open-chrome s where))
          (t (setf (kind s) :window) (%open-window s title)))
    (show sh (surface s) s)
    s))

(defun close-pane (s)
  (let ((it (took s)))
    (when it
      (fault:or-nothing "a surface the compositor has dropped is dropped"
       (typecase it
         (zwlr-layer-surface-v1 (zwlr-layer-surface-v1.destroy it))
         (river-shell-surface-v1 (river-shell-surface-v1.destroy it))
         (t (xdg-toplevel.destroy it))))))
  (when (surface s) (fault:or-nothing "and so is the surface under it"
                      (wl-surface.destroy (surface s))))
  (unshow (shell s) s)
  (setf (surface s) nil (took s) nil (configuredp s) nil)
  s)

(defun resize (s width height)
  (unless (and (= width (width s)) (= height (height s)))
    (setf (width s) width (height s) height)
    (when (and (took s) (typep (took s) 'zwlr-layer-surface-v1))
      (zwlr-layer-surface-v1.set-size (took s) width height))
    (paint s))
  s)

(defun place (s where)
  (let ((it (took s)))
    (when (and it (typep it 'zwlr-layer-surface-v1))
      (zwlr-layer-surface-v1.set-anchor it (getf where :edges))
      (zwlr-layer-surface-v1.set-exclusive-zone it (or (getf where :reserve) 0))
      (destructuring-bind (top right bottom left) (or (getf where :margin)
                                                      '(0 0 0 0))
        (zwlr-layer-surface-v1.set-margin it top right bottom left))
      (wl-surface.commit (surface s))))
  s)

(defun at (s line col)
  (and (tree s) (ui:under (tree s) line col)))

(defun clicked (s line col)
  (let ((found (at s line col)))
    (and found (values (ui:clicked found col) found))))

(defun hint-of (found) (or (and found (ui:hint found)) ""))
