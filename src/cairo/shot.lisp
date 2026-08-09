(defpackage #:pine.cairo.shot
  (:use #:cl)
  (:local-nicknames (#:cairo #:cl-cairo2) (#:paint #:pine.cairo.paint)
                    (#:grid #:pine.cairo.grid) (#:layout #:pine.ui.layout)
                    (#:face #:pine.ui.face) (#:render #:pine.edit.render)
                    (#:buffer #:pine.edit.buffer) (#:world #:pine.world.world)
                    (#:node #:pine.fs.node))
  (:export #:rows #:window #:shot #:*text*))

(in-package #:pine.cairo.shot)

(defparameter *text* "(defun greet (who &optional (times 1))
  \"say hello\"
  (dotimes (i times)
    (format t \"~&hello, ~a!~%\" who)))")

(defun %ready (text)
  (unless world:*world* (pine:start))
  (when text
    (setf (node:contents (buffer:current)) text)
    (buffer:goto! (buffer:current) 2 4))
  (buffer:current))

(defun rows (&key (path "/tmp/pine-rows.png") (cols 84) (lines 24) (text *text*))
  (%ready text)
  (grid:render-grid-to-png (mapcar #'identity (render:rows :width cols :height lines))
                           path)
  path)

(defun %cell ()
  (let ((px (face:metric :font-px 15)))
    (multiple-value-bind (w h) (layout:text-size "M" px)
      (values (max 1 w) (max 1 h)))))

(defun window (&key (path "/tmp/pine-window.png") (width 900) (height 560)
                    (text *text*))
  (%ready text)
  (face:with-faces
    (paint:with-cairo-layout
      (let ((surface (cairo:create-image-surface :argb32 width height)))
        (cairo:with-context ((cairo:create-context surface))
          (cairo:set-source-rgba 0d0 0d0 0d0 0d0)
          (cairo:paint)
          (multiple-value-bind (cw ch) (%cell)
            (paint:paint-tree (render:frame-tree :cols (max 1 (floor width cw))
                                                 :rows (max 2 (floor height ch)))
                              width height)))
        (cairo:surface-write-to-png surface path))))
  path)

(defun shot (&key (dir "/tmp") (text *text*))
  (list (rows :path (format nil "~a/pine-rows.png" dir) :text text)
        (window :path (format nil "~a/pine-window.png" dir) :text text)))
