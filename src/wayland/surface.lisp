(in-package #:pine.wayland)

;;;; A wayland layer surface painted by the cairo backend. We bind
;;;; zwlr_layer_shell_v1 directly, ask the compositor to anchor us (bar on the
;;;; left, echo at the bottom, panels as overlays), and
;;;; paint the pine.ui.node tree into an shm buffer with cairo -- the same tree
;;;; the daemon builds and the same paint pass the headless PNGs use. Input
;;;; (pointer hit-testing) lives in input.lisp.

;;;; The connection: the display plus the bound globals, a registry of
;;;; surface->layer-surface for pointer focus, and the live pointer state.

(defstruct wl-conn
  display backing compositor shm shell seat pointer
  (surfaces nil)                        ; alist (wl-surface-proxy . layer-surface)
  focus (ptr-x 0) (ptr-y 0) (ptr-serial 0)
  drag)                                 ; a slider being scrubbed, or nil

(defun conn-surface->ls (conn surface)
  (cdr (assoc surface (wl-conn-surfaces conn))))

;;;; A layer surface retains the node tree it last built, so pointer hit-testing
;;;; and hover run against exactly what is on screen; a rebuild happens only when
;;;; an action may have changed it.

(defclass layer-surface ()
  ((conn       :initarg :conn       :reader ls-conn)
   (wl-surface :accessor ls-wl-surface)
   (layer-surf :accessor ls-layer-surf)
   (width      :initarg :width  :accessor ls-width  :initform 0)
   (height     :initarg :height :accessor ls-height :initform 0)
   (tree-fn    :initarg :tree-fn :accessor ls-tree-fn)   ; () -> layout node
   (tree       :initform nil :accessor ls-tree)          ; the built root
   (hover      :initform nil :accessor ls-hover)         ; hovered node, or nil
   (on-closed  :initarg :on-closed :accessor ls-on-closed :initform nil)))

(defun build-tree (ls)
  "(Re)build LS's node tree from its builder, dropping stale hover."
  (setf (ls-hover ls) nil
        (ls-tree ls) (funcall (ls-tree-fn ls))))

;;;; Painting: the layout tree -> shm buffer via cairo.

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
                  (c:set-operator :source)              ; start fully transparent
                  (c:set-source-rgba 0d0 0d0 0d0 0d0) (c:paint)
                  (c:set-operator :over)
                  (paint:paint-tree (ls-tree ls) width height)))
              (wl-surface.attach surface buffer 0 0)
              (wl-surface.damage-buffer surface 0 0 width height)
              (wl-surface.commit surface)
              (push (evelambda (:release () (destroy-proxy buffer))) (wl-proxy-hooks buffer)))))))))

;;;; Opening a layer surface.

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
    (wl-surface.commit surface)                          ; first commit: no buffer
    ls))

(defun measure-panel (tree-fn &key (avail-w 460))
  "Measure the tree a builder produces, for sizing an overlay to its content."
  (paint:with-cairo-layout (paint:measure-tree (funcall tree-fn) avail-w)))
