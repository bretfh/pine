(in-package #:pine/wayland)

(defparameter +background+ '(30 30 46))

(defun %measured (tree &key (width 800) (height 600) (font 14))
  (let* ((s (cl-cairo2:create-image-surface :argb32 1 1))
         (m (make-instance 'canvas
                           :context (cl-cairo2:create-context s)
                           :size font)))
    (unwind-protect
         (ui:with-pass
           (ui:with-faces
             (ui:dress tree)
             (multiple-value-bind (cw ch) (ui:measure tree m width height)
               (values cw ch m))))
      (cl-cairo2:destroy s))))

(defun draw (tree path &key (width 800) (height 600) (font 14)
                            (background +background+))
  (let* ((s (cl-cairo2:create-image-surface :argb32 width height))
         (context (cl-cairo2:create-context s))
         (m (make-instance 'canvas :context context :size font)))
    (unwind-protect
         (progn
           (with-canvas (m)
             (rgb background)
             (cl-cairo2:paint))
           (ui:with-pass
             (ui:with-faces
               (ui:dress tree)
               (ui:measure tree m width height)
               (ui:lay tree m 0 0 width height)
               (ui:paint tree m)))
           (ensure-directories-exist path)
           (cl-cairo2:surface-write-to-png s (namestring path))
           (unless (probe-file path)
             (error "~a was not written" (namestring path)))
           (namestring path))
      (cl-cairo2:destroy context)
      (cl-cairo2:destroy s))))

(defun every-surface (&key (into "/tmp/") (width 800) (height 600))
  (loop :for each :in (ui:surfaces)
        :when (ui:shown each)
          :collect (let ((tree (ui:tree each)))
                     (when tree
                       (multiple-value-bind (cw ch) (%measured tree :width width
                                                                  :height height)
                         (draw tree (merge-pathnames
                                     (format nil "pine-~a.png" (fs:name each))
                                     into)
                               :width (max 16 (min width cw))
                               :height (max 16 (min height ch))))))))

(defun shot (tree &key (path "/tmp/pine.png") (width 800) (height 600) (font 14))
  (draw tree path :width width :height height :font font))
