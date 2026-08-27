(defpackage #:pine/paint/shot
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:w #:pine/ui/widget) (#:grid #:pine/ui/grid)
                    (#:face #:pine/ui/face) (#:layout #:pine/ui/layout)
                    (#:surface #:pine/ui/surface)
                    (#:canvas #:pine/paint/canvas))
  (:export #:shot #:draw #:every-surface))
(in-package #:pine/paint/shot)

(defparameter +background+ '(30 30 46))

(defun measure (tree &key (width 800) (height 600) (font 14))
  "How big TREE wants to be in pixels, and the canvas it was measured on. The
measurement is cairo's, so it is the one the paint will use."
  (let* ((s (cl-cairo2:create-image-surface :argb32 1 1))
         (m (make-instance 'canvas:canvas
                           :context (cl-cairo2:create-context s)
                           :size font)))
    (unwind-protect
         (layout:with-pass
           (face:with-faces
             (layout:dress tree)
             (multiple-value-bind (cw ch) (layout:measure tree m width height)
               (values cw ch m))))
      (cl-cairo2:destroy s))))

(defun draw (tree path &key (width 800) (height 600) (font 14)
                            (background +background+))
  "Draw TREE onto a PNG. No display and no compositor: this is what would land on
the screen, in a file you can look at."
  (let* ((s (cl-cairo2:create-image-surface :argb32 width height))
         (context (cl-cairo2:create-context s))
         (m (make-instance 'canvas:canvas :context context :size font)))
    (unwind-protect
         (progn
           (canvas:with-canvas (m)
             (canvas:rgb background)
             (cl-cairo2:paint))
           (layout:with-pass
             (face:with-faces
               (layout:dress tree)
               (layout:measure tree m width height)
               (layout:arrange tree m 0 0 width height)
               (layout:paint tree m)))
           (ensure-directories-exist path)
           (cl-cairo2:surface-write-to-png s (namestring path))
           (unless (probe-file path)
             (error "~a was not written" (namestring path)))
           (namestring path))
      (cl-cairo2:destroy context)
      (cl-cairo2:destroy s))))

(defun every-surface (&key (into "/tmp/") (width 800) (height 600))
  "Every surface that is up, each as a PNG at the size it asked for. A bar is drawn
as tall and as narrow as it measured, rather than stretched to fill a window it
would never be given."
  (loop :for each :in (surface:surfaces)
        :when (surface:shown each)
          :collect (let ((tree (node:contents each)))
                     (when tree
                       (multiple-value-bind (cw ch) (measure tree :width width
                                                                  :height height)
                         (draw tree (merge-pathnames
                                     (format nil "pine-~a.png" (node:name each))
                                     into)
                               :width (max 16 (min width cw))
                               :height (max 16 (min height ch))))))))

(defun shot (tree &key (path "/tmp/pine.png") (width 800) (height 600) (font 14))
  (draw tree path :width width :height height :font font))
