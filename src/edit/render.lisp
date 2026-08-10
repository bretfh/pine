(defpackage #:pine.edit.render
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node) (#:build #:pine.ui.build)
                    (#:cells #:pine.ui.cells) (#:layout #:pine.ui.layout)
                    (#:raster #:pine.ui.raster) (#:face #:pine.ui.face)
                    (#:buffer #:pine.edit.buffer) (#:window #:pine.edit.window)
                    (#:mode #:pine.repl.mode) (#:parser #:pine.ts.parser)
                    (#:prompt #:pine.edit.prompt))
  (:export #:buffer-tree #:window-tree #:frame-tree #:rows #:modeline #:echo-tree
           #:visible-lines #:scroll-to-point #:highlights-for #:indent-for
           #:*cols* #:*rows*))

(in-package #:pine.edit.render)

(defparameter +candidates-shown+ 12)
(defvar *cols* 80)
(defvar *rows* 24)

(defun visible-lines (b from height)
  (loop :for n :from from :below (min (buffer:line-count b) (+ from height))
        :collect (buffer:line b n)))

(defun scroll-to-point (w)
  (let* ((b (or (window:buffer-of w) (buffer:current) (buffer:scratch)))
         (line (buffer:point-line b))
         (from (window:scroll-of w))
         (height (max 1 (window:height-of w))))
    (setf (window:scroll-of w)
          (cond ((< line from) line)
                ((>= line (+ from height)) (1+ (- line height)))
                (t from)))))

(defun highlights-for (b)
  (let ((found (make-hash-table :test 'eql)))
    (dolist (run (parser:highlights b) found)
      (destructuring-bind (line from to face) run
        (push (list from to face) (gethash line found))))))

(defun %face-at (highlights line col)
  (loop :for (from to face) :in (gethash line highlights)
        :when (and (>= col from) (< col to))
          :do (return face)))

(defun %cell-face (b highlights line col)
  (or (getf (first (buffer:properties-at b line col)) :face)
      (%face-at highlights line col)
      :default))

(defun %region-span (b)
  (let ((mark (buffer:mark b)))
    (when mark
      (destructuring-bind (line col) mark
        (let ((at-line (buffer:point-line b)) (at-col (buffer:point-col b)))
          (if (or (< line at-line) (and (= line at-line) (<= col at-col)))
              (list line col at-line at-col)
              (list at-line at-col line col)))))))

(defun %paint-region (r b from height width)
  (let ((span (%region-span b))
        (bg (face:face-bg :selection)))
    (when (and span bg)
      (destructuring-bind (start-line start-col end-line end-col) span
        (destructuring-bind (br bg bb) bg
          (loop :for line :from (max start-line from)
                  :to (min end-line (1- (+ from height)))
                :do (loop :for col :from (if (= line start-line) start-col 0)
                            :below (if (= line end-line) end-col width)
                          :do (raster:raster-put-bg r (- line from) col br bg bb))))))))

(defun %carets-here (w)
  (and (eq w (window:focused)) (not (prompt:asking-p))))

(defun buffer-tree (w)
  (let* ((b (or (window:buffer-of w) (buffer:current) (buffer:scratch)))
         (from (window:scroll-of w))
         (width (max 1 (window:width-of w)))
         (height (max 1 (window:height-of w)))
         (highlights (highlights-for b))
         (r (raster:make-raster width height))
         (caret (%carets-here w)))
    (loop :for text :in (visible-lines b from height)
          :for row :from 0
          :for line :from from
          :do (loop :for col :from 0 :below (min width (length text))
                    :do (raster:raster-put r row col (char text col)
                                           (%cell-face b highlights line col))))
    (%paint-region r b from height width)
    (build:cells (cells:rows-of r)
                 :class "editor-view" :expand 1
                 :crow (if caret (min (1- height) (max 0 (- (buffer:point-line b) from))) -1)
                 :ccol (if caret (min (1- width) (buffer:point-col b)) -1))))

(defun modeline (w)
  (let ((b (or (window:buffer-of w) (buffer:current) (buffer:scratch))))
    (build:label
     (format nil " ~:[  ~;**~] ~a  ~a~{ ~a~}  L~d C~d"
             (buffer:modified b)
             (node:name b)
             (or (mode:setting (buffer:mode-of b) :indicator) (buffer:mode-of b))
             (remove nil (mapcar (lambda (name)
                                   (mode:setting (mode:mode-named name) :indicator))
                                 (buffer:minors-of b)))
             (1+ (buffer:point-line b))
             (buffer:point-col b))
     :class "modeline" :face :modeline)))

(defun window-tree (w)
  (scroll-to-point w)
  (build:column :align :stretch :class "window" :expand 1
                (buffer-tree w)
                (modeline w)))

(defun candidates-shown ()
  (let ((p (prompt:asking)))
    (when p
      (let* ((found (prompt:matching p))
             (n (length found))
             (from (pine.ui.wire:scroll-to-selection (prompt:chosen p) 0
                                                     +candidates-shown+)))
        (values (subseq found (min from n) (min (+ from +candidates-shown+) n))
                from)))))

(defun %candidate-rows (found from width)
  (let ((chosen (prompt:chosen (prompt:asking)))
        (r (raster:make-raster (max 1 width) (max 1 (length found)))))
    (loop :for each :in found
          :for row :from 0
          :for text := (prompt:shows each width)
          :for face := (if (= (+ row from) chosen) :completion-selected :completion)
          :do (loop :for col :from 0 :below width
                    :do (raster:raster-put r row col
                                           (if (< col (length text)) (char text col) #\space)
                                           face)))
    (cells:rows-of r)))

(defun echo-tree (width &optional echo)
  (let* ((p (and (null echo) (prompt:asking)))
         (question (if p (prompt:question p) ""))
         (text (or echo (prompt:showing)))
         (r (raster:make-raster (max 1 width) 1)))
    (loop :for col :from 0 :below (min width (length text))
          :do (raster:raster-put r 0 col (char text col)
                                 (if (< col (length question)) :prompt :echo)))
    (multiple-value-bind (found from) (and p (candidates-shown))
      (let ((rows (cells:rows-of r)))
        (build:cells (if found
                         (append (%candidate-rows found from width) rows)
                         rows)
                     :class "echo" :base 1
                     :crow (if p 0 -1)
                     :ccol (if p
                               (min (1- width)
                                    (+ (length question)
                                       (buffer:point-col (prompt:answer-buffer))))
                               -1))))))

(defun frame-tree (&key (cols *cols*) (rows *rows*) echo)
  (let* ((windows (window:windows))
         (share (max 2 (floor (max 2 (1- rows)) (max 1 (length windows))))))
    (dolist (w windows)
      (setf (window:width-of w) (max 1 cols)
            (window:height-of w) (max 1 (1- share))))
    (apply #'build:column :align :stretch :class "editor"
           (append (mapcar #'window-tree windows)
                   (list (echo-tree cols echo))))))

(defun rows (&key (width 80) (height 24) echo)
  (cells:render (frame-tree :cols width :rows height :echo echo)
                width :height height))

(defun indent-for (b line)
  (let ((width (or (mode:setting (buffer:mode-of b) :indent) 2)))
    (or (parser:indent b line :width width)
        (and (plusp line) (buffer:indent-of b (1- line)))
        0)))
