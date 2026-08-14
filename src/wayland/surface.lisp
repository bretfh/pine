(defpackage #:pine.wayland.surface
  (:use #:cl #:wayflan-client #:pine.wayland.protocol #:pine.wayland.connection)
  (:local-nicknames (#:c #:cl-cairo2) (#:shm #:posix-shm)
                    (#:paint #:pine.cairo.paint))
  (:export

   #:wl-conn #:wl-conn-p #:make-wl-conn
   #:wl-conn-display #:wl-conn-backing #:wl-conn-compositor #:wl-conn-shm
   #:wl-conn-shell #:wl-conn-seat #:wl-conn-pointer #:wl-conn-surfaces

   #:wl-conn-focus #:wl-conn-ptr-x #:wl-conn-ptr-y #:wl-conn-ptr-serial
   #:wl-conn-drag #:conn-surface->ls

   #:layer-surface #:open-layer-surface #:measure-panel
   #:ls-conn #:ls-wl-surface #:ls-layer-surf #:ls-width #:ls-height
   #:ls-tree-fn #:ls-tree #:ls-hover #:ls-said #:ls-on-closed
   #:build-tree #:paint-surface #:*rebuilt*))

(in-package #:pine.wayland.surface)

(defvar *rebuilt* nil
  "Told a surface whose tree was just built again, so hover can be found on it.")

(defstruct wl-conn
  display backing compositor shm shell seat pointer
  (surfaces nil)
  focus (ptr-x 0) (ptr-y 0) (ptr-serial 0)
  drag)

(defun conn-surface->ls (conn surface)
  (cdr (assoc surface (wl-conn-surfaces conn))))

(defclass layer-surface ()
  ((conn       :initarg :conn       :reader ls-conn)
   (wl-surface :accessor ls-wl-surface)
   (layer-surf :accessor ls-layer-surf)
   (width      :initarg :width  :accessor ls-width  :initform 0)
   (height     :initarg :height :accessor ls-height :initform 0)
   (tree-fn    :initarg :tree-fn :accessor ls-tree-fn)
   (tree       :initform nil :accessor ls-tree)
   (hover      :initform nil :accessor ls-hover)
   (said       :initform nil :accessor ls-said)
   (on-closed  :initarg :on-closed :accessor ls-on-closed :initform nil)))

(defun build-tree (ls)
  "(Re)build LS's node tree from its builder. The pointer has not moved because
a surface was pushed again, so what it is over is found again on the new tree
rather than dropped: a bar that repaints twice a second would otherwise light
up under the pointer only while the pointer is moving."
  (setf (ls-hover ls) nil
        (ls-tree ls) (funcall (ls-tree-fn ls)))
  (when *rebuilt* (funcall *rebuilt* ls))
  ls)

(defun paint-surface (ls)
  "Render LS's retained tree into a fresh shm buffer and commit it. Builds the
tree first if there is none. Leaves it arranged so hit-testing is exact."
  (with-accessors ((surface ls-wl-surface) (width ls-width) (height ls-height)) ls
    (let ((shm (wl-conn-shm (ls-conn ls))))
      (when (and (plusp width) (plusp height))
        (unless (ls-tree ls) (build-tree ls))
        (let* ((stride (* width 4)) (size (* stride height)))
          (shm:with-open-shm-and-mmap* (obj data (:direction :io) (size))
            (let (buffer)
              (with-proxy (pool (wl-shm.create-pool shm (shm:shm-fd obj) size))
                (setf buffer (wl-shm-pool.create-buffer pool 0 width height stride :argb8888)))
              (c:with-surface-and-context
                  (surf (c:create-image-surface-for-data data :argb32 width height stride))
                (paint:with-cairo-layout
                  (c:set-operator :source)
                  (c:set-source-rgba 0d0 0d0 0d0 0d0) (c:paint)
                  (c:set-operator :over)
                  (paint:paint-tree (ls-tree ls) width height)))
              (wl-surface.attach surface buffer 0 0)
              (wl-surface.damage-buffer surface 0 0 width height)
              (wl-surface.commit surface)
              (push (evelambda (:release () (destroy-proxy buffer))) (wl-proxy-hooks buffer)))))))))

(defun open-layer-surface (conn tree-fn
                           &key (layer :top) (anchor '(:top :left))
                                (width 0) (height 0) (exclusive 0)
                                (namespace "gtk-layer-shell") (on-closed nil)
                                (margin '(0 0 0 0)))
  "Create an anchored layer surface. LAYER is :background/:bottom/:top/:overlay,
ANCHOR a list of :top/:bottom/:left/:right. WIDTH/HEIGHT 0 on an axis anchored to
both edges lets the compositor size it. TREE-FN returns the layout node to paint.
NAMESPACE defaults to gtk-layer-shell, which compositor blur and shadow
rules commonly match on."
  (let* ((ls (make-instance 'layer-surface :conn conn
                            :width width :height height :tree-fn tree-fn
                            :on-closed on-closed))
         (surface (wl-compositor.create-surface (wl-conn-compositor conn)))
         (lsurf (zwlr-layer-shell-v1.get-layer-surface
                 (wl-conn-shell conn) surface nil layer namespace)))
    (setf (ls-wl-surface ls) surface (ls-layer-surf ls) lsurf)
    (push (cons surface ls) (wl-conn-surfaces conn))
    (zwlr-layer-surface-v1.set-anchor lsurf anchor)
    (zwlr-layer-surface-v1.set-size lsurf width height)
    (zwlr-layer-surface-v1.set-exclusive-zone lsurf exclusive)
    (destructuring-bind (top right bottom left) margin
      (zwlr-layer-surface-v1.set-margin lsurf top right bottom left))
    (push (evelambda
            (:configure (serial w h)
             (zwlr-layer-surface-v1.ack-configure lsurf serial)
             (unless (zerop w) (setf (ls-width ls) w))
             (unless (zerop h) (setf (ls-height ls) h))
             (paint-surface ls))
            (:closed ()
             (when (ls-on-closed ls) (funcall (ls-on-closed ls)))))
          (wl-proxy-hooks lsurf))
    (wl-surface.commit surface)
    ls))

(defun measure-panel (tree-fn &key (avail-w 460))
  "Measure the tree a builder produces, for sizing an overlay to its content."
  (paint:with-cairo-layout (paint:measure-tree (funcall tree-fn) avail-w)))
