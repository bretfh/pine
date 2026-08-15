(defpackage #:pine/edit/render
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node) (#:build #:pine/ui/build)
                    (#:cells #:pine/ui/cells) (#:layout #:pine/ui/layout)
                    (#:raster #:pine/ui/raster) (#:face #:pine/ui/face)
                    (#:buffer #:pine/edit/buffer) (#:window #:pine/edit/window)
                    (#:mode #:pine/repl/mode) (#:parser #:pine/ts/parser)
                    (#:prompt #:pine/edit/prompt) (#:widget #:pine/ui/node))
  (:export #:shown #:modelinep #:buffer-tree #:window-tree #:frame-tree
           #:rows #:modeline #:echo-tree
           #:shown-line #:shown-col
           #:visible-lines #:scroll-to-point #:hscroll-to-point #:caret-col #:highlights-for #:indent-for
           #:*cols* #:*rows* #:*font-px*))
(in-package #:pine/edit/render)

(defparameter +candidates-shown+ 12)
(defvar *cols* 80)
(defvar *rows* 24)
(defvar *font-px* nil)

(defun visible-lines (b from height)
  (loop :for n :from from :below (min (buffer:line-count b) (+ from height))
        :collect (buffer:line b n)))

(defun shown-line (text width)
  "TEXT as it is drawn: a tab takes you to the next stop rather than one cell.
Answers the text and the column each character landed in."
  (let ((out (make-string-output-stream))
        (at 0)
        (where (make-array (length text) :element-type 'fixnum)))
    (loop :for ch :across text
          :for i :from 0
          :do (setf (aref where i) at)
              (cond ((char= ch #\Tab)
                     (let ((to (* width (1+ (floor at width)))))
                       (dotimes (n (- to at)) (write-char #\Space out))
                       (setf at to)))
                    (t (write-char ch out) (incf at))))
    (values (get-output-stream-string out) where)))

(defun shown-col (where col)
  (cond ((zerop (length where)) col)
        ((< col (length where)) (aref where col))
        (t (1+ (aref where (1- (length where)))))))

(defun %buffer-of (w)
  "The buffer W is showing, or the one it stands in for while it shows
something else."
  (let ((it (window:buffer-of w)))
    (or (and (stringp it) (buffer:buffer-named it))
        (and (typep it 'buffer:buffer) it)
        (buffer:current)
        (buffer:scratch))))

(defun scroll-to-point (w)
  (let* ((b (%buffer-of w))
         (line (buffer:point-line b))
         (from (window:scroll-of w))
         (height (max 1 (window:height-of w))))
    (setf (window:scroll-of w)
          (cond ((< line from) line)
                ((>= line (+ from height)) (1+ (- line height)))
                (t from)))
    (hscroll-to-point w)))

(defun caret-col (b)
  "The column point is drawn in: where it lands once tabs are expanded."
  (multiple-value-bind (drawn where)
      (shown-line (buffer:line b (buffer:point-line b))
                  (max 1 (or (buffer:setting b :tab-width) 8)))
    (declare (ignore drawn))
    (shown-col where (buffer:point-col b))))

(defun hscroll-to-point (w)
  "Keep point in the window sideways too: a long line scrolls under it rather
than putting the caret where the text is not."
  (let* ((b (%buffer-of w))
         (col (caret-col b))
         (left (window:hscroll-of w))
         (width (max 1 (window:width-of w))))
    (setf (window:hscroll-of w)
          (cond ((< col left) col)
                ((>= col (+ left width)) (1+ (- col width)))
                (t left)))))

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

(defun %paint-region (r b from height width &optional (left 0))
  (let ((span (%region-span b))
        (bg (face:face-bg :selection)))
    (when (and span bg)
      (destructuring-bind (start-line start-col end-line end-col) span
        (destructuring-bind (br bg bb) bg
          (loop :for line :from (max start-line from)
                  :to (min end-line (1- (+ from height)))
                :do (loop :for col :from (if (= line start-line) start-col 0)
                            :below (if (= line end-line) end-col (+ left width))
                          :when (>= col left)
                            :do (raster:raster-put-bg r (- line from) (- col left)
                                                      br bg bb))))))))

(defun %at-col (where drawn)
  "Which character of the text is being drawn at this column."
  (or (position drawn where) (max 0 (1- (length where)))))

(defun %carets-here (w)
  (and (eq w (window:focused)) (not (prompt:asking-p))))

(defgeneric shown (content w)
  (:documentation "The cells window W shows of what it holds. A buffer, a
widget tree and the name of a buffer are all things a window can hold, and
another kind of content is a method someone else writes: nothing here has to
know about it.")
  (:method (content w)
    (declare (ignore content))
    (build:cells (cells:rows-of (raster:make-raster (max 1 (window:width-of w))
                                                    (max 1 (window:height-of w))))
                 :class "editor-view" :expand 1 :font-px *font-px*
                 :crow -1 :ccol -1)))

(defmethod shown ((content string) w)
  "A name is the buffer it names."
  (shown (or (buffer:buffer-named content) (buffer:scratch)) w))

(defmethod shown ((content widget:node) w)
  "A widget tree renders to cells like any surface does, so a config can put
one in a window beside a buffer."
  (build:cells (cells:render content (max 1 (window:width-of w))
                             :height (max 1 (window:height-of w)))
               :class "editor-view" :expand 1 :font-px *font-px*
               :crow -1 :ccol -1))

(defmethod shown ((b buffer:buffer) w)
  (let* ((from (window:scroll-of w))
         (left (window:hscroll-of w))
         (width (max 1 (window:width-of w)))
         (height (max 1 (window:height-of w)))
         (highlights (highlights-for b))
         (r (raster:make-raster width height))
         (caret (%carets-here w)))
    (loop :with tab := (max 1 (or (buffer:setting b :tab-width) 8))
          :for text :in (visible-lines b from height)
          :for row :from 0
          :for line :from from
          :do (multiple-value-bind (drawn where) (shown-line text tab)
                (loop :for col :from left :below (min (+ left width) (length drawn))
                      :do (raster:raster-put r row (- col left) (char drawn col)
                                             (%cell-face b highlights line
                                                         (%at-col where col))))
                (let ((said (buffer:overlays-at b line)))
                  (when said
                    (let ((at (min (1- width) (max 0 (- (+ 2 (length drawn)) left)))))
                      (loop :for props :in said
                            :for text := (getf props :after)
                            :do (loop :for i :from 0
                                        :below (min (- width at) (length text))
                                      :do (raster:raster-put r row (+ at i)
                                                             (char text i)
                                                             (or (getf props :face)
                                                                 :comment)))))))))
    (%paint-region r b from height width left)
    (build:cells (cells:rows-of r)
                 :class "editor-view" :expand 1 :font-px *font-px*
                 :crow (if caret (min (1- height) (max 0 (- (buffer:point-line b) from))) -1)
                 :ccol (if caret
                           (min (1- width) (max 0 (- (caret-col b) left)))
                           -1))))

(defun buffer-tree (w)
  "What window W is showing, whatever it holds."
  (shown (or (window:buffer-of w) (buffer:current) (buffer:scratch)) w))

(defun modelinep (w)
  "Whether what this window holds has a modeline: a buffer says what line you
are on and a widget tree has nothing of the sort to say."
  (let ((it (window:buffer-of w)))
    (or (null it) (typep it 'buffer:buffer)
        (and (stringp it) (buffer:buffer-named it)))))

(defun modeline (w)
  (let* ((b (%buffer-of w))
         (width (max 1 (window:width-of w)))
         (text (format nil " ~:[  ~;**~] ~a  ~a~{ ~a~}  L~d C~d"
                       (buffer:modified b)
                       (node:name b)
                       (or (mode:setting (buffer:mode-of b) :indicator)
                           (buffer:mode-of b))
                       (remove nil
                               (mapcar (lambda (name)
                                         (mode:setting (mode:mode-named name)
                                                       :indicator))
                                       (buffer:minors-of b)))
                       (1+ (buffer:point-line b))
                       (buffer:point-col b)))
         (r (raster:make-raster width 1)))
    (loop :for col :from 0 :below width
          :do (raster:raster-put r 0 col
                                 (if (< col (length text)) (char text col) #\space)
                                 :modeline))
    (build:cells (cells:rows-of r) :class "modeline" :font-px *font-px*)))

(defun window-tree (w)
  (scroll-to-point w)
  (if (modelinep w)
      (build:column :align :stretch :class "window" :expand 1
                    (buffer-tree w)
                    (modeline w))
      (build:column :align :stretch :class "window" :expand 1
                    (buffer-tree w))))

(defun candidates-shown ()
  (let ((p (prompt:asking)))
    (when p
      (let* ((found (prompt:matching p))
             (n (length found))
             (from (pine/ui/wire:scroll-to-selection (prompt:chosen p) 0
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
                     :class "echo" :base 1 :font-px *font-px*
                     :crow (if p 0 -1)
                     :ccol (if p
                               (min (1- width)
                                    (+ (length question)
                                       (buffer:point-col (prompt:answer-buffer))))
                               -1))))))

(defun frame-tree (&key (cols *cols*) (rows *rows*) echo)
  (let* ((windows (window:windows))
         (weight (reduce #'+ windows :key #'window:weight-of :initial-value 0))
         (room (max 2 (1- rows))))
    (dolist (w windows)
      (setf (window:width-of w) (max 1 cols)
            (window:height-of w)
            (let ((share (max 2 (floor (* room (window:weight-of w))
                                       (max 1 weight)))))
              (max 1 (if (modelinep w) (1- share) share)))))
    (apply #'build:column :align :stretch :class "editor"
           (append (loop :for w :in windows
                         :for first := t :then nil
                         :append (if first
                                     (list (window-tree w))
                                     (list (build:rule :face :border-inactive)
                                           (window-tree w))))
                   (list (echo-tree cols echo))))))

(defun rows (&key (width 80) (height 24) echo)
  (cells:render (frame-tree :cols width :rows height :echo echo)
                width :height height))

(defun indent-for (b line)
  (let ((width (or (mode:setting (buffer:mode-of b) :indent) 2)))
    (or (parser:indent b line :width width)
        (and (plusp line) (buffer:indent-of b (1- line)))
        0)))
