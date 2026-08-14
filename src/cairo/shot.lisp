(defpackage #:pine.cairo.shot
  (:use #:cl)
  (:local-nicknames (#:cairo #:cl-cairo2) (#:paint #:pine.cairo.paint)
                    (#:grid #:pine.cairo.grid) (#:layout #:pine.ui.layout)
                    (#:face #:pine.ui.face) (#:render #:pine.edit.render)
                    (#:buffer #:pine.edit.buffer) (#:world #:pine.world.world)
                    (#:node #:pine.fs.node))
  (:export #:rows #:window #:surfaces #:as-frontend #:shot #:*text*))

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
  (pine.ts.parser:wait (buffer:current))
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

(defun %surface-size (as w h)
  (case as
    (:bar (values w 1080))
    (:echo (values 1600 h))
    (t (values w h))))

(defun surface (s &key (dir "/tmp"))
  (let ((path (format nil "~a/pine-~a.png" dir (node:name s)))
        (tree (node:contents s)))
    (paint:with-cairo-layout
      (multiple-value-bind (mw mh) (paint:measure-tree tree 480)
        (multiple-value-bind (w h) (%surface-size (pine.app.surface:as s) mw mh)
          (let ((surface (cairo:create-image-surface :argb32 (max 1 w) (max 1 h))))
            (cairo:with-context ((cairo:create-context surface))
              (cairo:set-source-rgba 0d0 0d0 0d0 0d0)
              (cairo:paint)
              (paint:paint-tree tree w h))
            (cairo:surface-write-to-png surface path)
            (list :w w :h h :path path)))))))

(defun surfaces (&key (dir "/tmp") (config (pine:config-file)))
  (unless world:*world* (pine:start))
  (when config (pine:load-config config))
  (face:with-faces
    (loop :for s :in (pine.app.surface:surfaces)
          :for shot := (ignore-errors (surface s :dir dir))
          :collect (or shot (list :failed (node:name s))))))

(defun as-frontend (name &key (dir "/tmp") (width 900) (height 560)
                             (config (pine:config-file)))
  (unless world:*world* (pine:start))
  (when config (pine:load-config config))
  (let* ((s (pine.app.surface:surface-named name))
         (wire (and s (let ((render:*cols* (max 1 (floor width 9)))
                            (render:*rows* (max 2 (floor height 18))))
                        (pine.ui.wire:node->wire
                         (pine.fs.computed:recompute s)))))
         (styles (pine.ui.css:styles))
         (path (format nil "~a/pine-frontend-~a.png" dir name)))
    (when wire
      (setf world:*world* nil)
      (pine.ui.css:install styles)
      (let ((tree (pine.ui.wire:wire->node
                   wire :on-action (lambda (id) (declare (ignore id))
                                     (lambda (&rest a) (declare (ignore a)) nil)))))
        (face:with-faces
          (paint:with-cairo-layout
            (let ((surface (cairo:create-image-surface :argb32 width height)))
              (cairo:with-context ((cairo:create-context surface))
                (cairo:set-source-rgba 0d0 0d0 0d0 0d0)
                (cairo:paint)
                (if (pine.ui.wire:arranged-p tree)
                    (paint:paint-arranged tree)
                    (paint:paint-tree tree width height)))
              (cairo:surface-write-to-png surface path))))
        (list :w width :h height :path path)))))

(defun shot (&key (dir "/tmp") (text *text*))
  (list (rows :path (format nil "~a/pine-rows.png" dir) :text text)
        (window :path (format nil "~a/pine-window.png" dir) :text text)))
