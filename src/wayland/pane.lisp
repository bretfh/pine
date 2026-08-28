(defpackage #:pine/wayland/pane
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell #:pine/wayland/protocol)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:canvas #:pine/paint/canvas)
                    (#:shm #:posix-shm) (#:shell #:pine/wayland/shell)
                    (#:fault #:pine/run/fault))
  (:export #:pane #:open-pane #:close-pane #:paint #:render #:resize #:measure
           #:on-resize
           #:name-of #:tree #:wide #:tall #:hover #:configuredp #:took
           #:kind #:dirty #:chromep #:clicked #:hint-of #:cell
           #:*namespace* #:*font*))
(in-package #:pine/wayland/pane)

(defvar *namespace* "gtk-layer-shell"
  "What the compositor knows this surface as. Its blur and shadow rules commonly
match on this one.")

(defvar *font* 15)
(defparameter +window+ '(900 . 600)
  "How big a window is until the compositor says otherwise. A toplevel is sized by
whoever is showing it, and until it does the client picks.")

(defclass pane ()
  ((name-of :initarg :name    :reader name-of)
   (shell   :initarg :shell   :reader shell)
   (surface :initform nil     :accessor surface)
   (took    :initform nil     :accessor took)
   (tree    :initarg :tree    :accessor tree :initform nil)
   (wide    :initarg :wide    :accessor wide :initform 0)
   (tall    :initarg :tall    :accessor tall :initform 0)
   (hover   :initform nil     :accessor hover)
   (kind    :initform nil     :accessor kind)
   (node-of :initform nil     :accessor node-of)
   (where   :initform nil     :accessor where)
   (dirty   :initform nil     :accessor dirty)
   (on-resize :initarg :on-resize :accessor on-resize :initform nil)
   (configuredp :initform nil :accessor configuredp))
  (:documentation "One of the daemon's surfaces, on screen. TOOK is what the
compositor gave it, and KIND says which: a layer surface where the compositor has
a layer shell, an xdg toplevel for a window, and where pine is the window manager
itself, one of the surfaces it hands out for its own furniture.

Which one is what the role said, not something this file decides."))

(defun chromep (s) (eq :chrome (kind s)))

(defun %sized (s width height)
  "The compositor said how big this is. What is laid out for it is laid out
somewhere else, so that somewhere has to be told: a frame worked out for the size
before is a frame that does not fill the surface it lands in."
  (let ((was-wide (wide s)) (was-tall (tall s)))
    (unless (zerop width) (setf (wide s) width))
    (unless (zerop height) (setf (tall s) height))
    (setf (configuredp s) t)
    (when (and (on-resize s)
               (or (/= was-wide (wide s)) (/= was-tall (tall s))))
      (funcall (on-resize s) s))
    s))

(defmethod print-object ((s pane) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~dx~d" (name-of s) (wide s) (tall s))))

(defun %canvas (data width height stride)
  (let ((it (cl-cairo2:create-image-surface-for-data data :argb32 width height
                                                     stride)))
    (values (make-instance 'canvas:canvas
                           :context (cl-cairo2:create-context it)
                           :size *font*)
            it)))

(defun %blit (s)
  "The buffer: draw the tree into shared memory and hand it over. The tree is
arranged as it is painted, so what a click lands on is what was drawn there."
  (let ((width (wide s)) (height (tall s)))
    (when (and (plusp width) (plusp height) (tree s) (surface s)
               (configuredp s))
      (let* ((stride (* width 4))
             (size (* stride height)))
        (shm:with-open-shm-and-mmap* (obj data (:direction :io) (size))
          (let (buffer)
            (with-proxy (pool (wl-shm.create-pool (shell:shm (shell s))
                                                  (shm:shm-fd obj) size))
              (setf buffer (wl-shm-pool.create-buffer pool 0 width height stride
                                                      :argb8888)))
            (multiple-value-bind (m it) (%canvas data width height stride)
              (unwind-protect
                   (progn
                     (canvas:with-canvas (m)
                       (cl-cairo2:set-operator :source)
                       (cl-cairo2:set-source-rgba 0d0 0d0 0d0 0d0)
                       (cl-cairo2:paint)
                       (cl-cairo2:set-operator :over))
                     (ui:with-pass
                       (ui:with-faces
                         (ui:dress (tree s))
                         (ui:measure (tree s) m width height)
                         (ui:arrange (tree s) m 0 0 width height)
                         (ui:paint (tree s) m))))
                (cl-cairo2:destroy (canvas:context m))
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
  "Show what this pane holds now.

Furniture pine hands itself is not painted here: a window manager's own surfaces
are rendering state, and rendering state is committed inside a render sequence and
nowhere else. This says it wants one; RENDER is what runs there."
  (cond ((chromep s) (setf (dirty s) t) nil)
        (t (%blit s))))

(defun render (s)
  "Commit this pane inside a render sequence. Only furniture is: everything else
answers to the compositor rather than to us."
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
  "How big the tree wants to be, measured the way it will be painted."
  (let* ((it (cl-cairo2:create-image-surface :argb32 1 1))
         (m (make-instance 'canvas:canvas :context (cl-cairo2:create-context it)
                                          :size *font*)))
    (unwind-protect
         (ui:with-pass
           (ui:with-faces
             (ui:dress (tree s))
             (ui:measure (tree s) m avail avail)))
      (cl-cairo2:destroy it))))

(defun cell (s)
  "How big one character cell is on this surface, so what is laid out in cells
lands in them."
  (declare (ignore s))
  (let* ((it (cl-cairo2:create-image-surface :argb32 1 1))
         (m (make-instance 'canvas:canvas :context (cl-cairo2:create-context it)
                                          :size *font*)))
    (unwind-protect (ui:text-size m "M" *font*)
      (cl-cairo2:destroy it))))

(defun %layer (where)
  "Which layer an anchoring goes on. A surface anchored to every edge is the
background; one that keeps a strip for itself is furniture; anything else floats."
  (let ((edges (getf where :edges)))
    (cond ((null edges) :top)
          ((= 4 (length edges)) :background)
          ((plusp (or (getf where :keeps) 0)) :top)
          (t :overlay))))

(defun %open-layer (s where)
  (let* ((sh (shell s))
         (surface (wl-compositor.create-surface (shell:compositor sh)))
         (it (zwlr-layer-shell-v1.get-layer-surface
              (shell:layer sh) surface nil (%layer where) *namespace*)))
    (setf (surface s) surface (took s) it)
    (zwlr-layer-surface-v1.set-anchor it (getf where :edges))
    (zwlr-layer-surface-v1.set-size it (or (getf where :wide) 0)
                                    (or (getf where :tall) 0))
    (zwlr-layer-surface-v1.set-exclusive-zone it (or (getf where :keeps) 0))
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
  "Furniture on a compositor that has no layer shell of its own: pine is the
window manager there, and a window manager's own surfaces come from it.

Nothing configures one, so it is drawn at the size it measured to. Nothing is
committed here either: that happens in a render sequence."
  (let* ((sh (shell s))
         (surface (wl-compositor.create-surface (shell:compositor sh)))
         (it (river-window-manager-v1.get-shell-surface (shell:chrome sh)
                                                        surface)))
    (setf (surface s) surface (took s) it (kind s) :chrome
          (node-of s) (river-shell-surface-v1.get-node it)
          (where s) where
          (configuredp s) t
          (dirty s) t)
    s))

(defun %open-window (s title)
  (when (zerop (wide s)) (setf (wide s) (car +window+)))
  (when (zerop (tall s)) (setf (tall s) (cdr +window+)))
  (let* ((sh (shell s))
         (surface (wl-compositor.create-surface (shell:compositor sh)))
         (xdg (xdg-wm-base.get-xdg-surface (shell:toplevel sh) surface))
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
  "Put a pane up. WHERE is what the role answered: which edges it is anchored to,
how big, and what strip it keeps. A window is anchored to nothing, so the
compositor sizes it.

The pane is made before this, wherever the asking was done, and only the telling
happens here: the thread that owns the compositor is the only one that may."
  (let ((sh (shell s)))
    (when (plusp (or (getf where :wide) 0)) (setf (wide s) (getf where :wide)))
    (when (plusp (or (getf where :tall) 0)) (setf (tall s) (getf where :tall)))
    (setf (where s) where)
    (cond (windowp (setf (kind s) :window) (%open-window s title))
          ((shell:layer sh) (setf (kind s) :layer) (%open-layer s where))
          ((shell:chrome sh) (%open-chrome s where))
          (t (setf (kind s) :window) (%open-window s title)))
    (shell:show sh (surface s) s)
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
  (shell:unshow (shell s) s)
  (setf (surface s) nil (took s) nil (configuredp s) nil)
  s)

(defun resize (s width height)
  (unless (and (= width (wide s)) (= height (tall s)))
    (setf (wide s) width (tall s) height)
    (when (and (took s) (typep (took s) 'zwlr-layer-surface-v1))
      (zwlr-layer-surface-v1.set-size (took s) width height))
    (paint s))
  s)

(defun place (s where)
  "Put a layer surface where the role now says it goes."
  (let ((it (took s)))
    (when (and it (typep it 'zwlr-layer-surface-v1))
      (zwlr-layer-surface-v1.set-anchor it (getf where :edges))
      (zwlr-layer-surface-v1.set-exclusive-zone it (or (getf where :keeps) 0))
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
