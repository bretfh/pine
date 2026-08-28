(in-package #:pine/ui)

(defvar *styles* nil
  "Widget to resolved style, for one pass. Resolution answers a style and layout
reads it; nothing writes back into the tree it was handed.")

(defvar *sizes* nil
  "Widget to its measured (w h), for one pass. A container arranges by measuring its
parts, and it has just measured them.")

(defvar *hover* nil
  "A face painted as the background of the hovered widget, or nothing.")

(defparameter +filled+ (code-char #x2588))
(defparameter +empty+  (code-char #x2500))

(defgeneric text-size (medium text font)
  (:documentation "How big TEXT is on this medium, as (values w h). A grid answers
in cells and a canvas in pixels: that is the whole of the difference between them,
and it lives here rather than in every widget.")
  (:method ((m grid) text font)
    (declare (ignore font))
    (values (length text) 1)))

(defun line-height (m font) (nth-value 1 (text-size m "M" font)))

(defmacro with-pass (&body body)
  "One layout pass: styles resolved once, sizes measured once."
  `(let ((*styles* (make-hash-table :test 'eq))
         (*sizes* (make-hash-table :test 'eq)))
     ,@body))

(defun styled (w)
  (or (and *styles* (gethash w *styles*)) (d:no-map)))

(defun dress (root &optional chain)
  "Resolve the style of every widget under ROOT against its chain of class-sets.
Nothing is written into the widgets: what a config authored stays what it authored,
and the style is what the sheet says on top of it."
  (labels ((walk (w chain)
             (let* ((classes (append (classes (css-class w))
                                     (and (chosen w) (list "sel"))))
                    (full (append chain (list classes))))
               (when *styles*
                 (setf (gethash w *styles*) (resolve full :hover (hovered w))))
               (dolist (part (parts w)) (walk part full)))))
    (walk root chain))
  root)

(defun %pad-x (w)
  (let ((p (pad w)) (s (d:lookup (styled w) :pad)))
    (cond ((consp p) (car p)) ((realp p) p) ((consp s) (car s)) (t 0))))

(defun %pad-y (w)
  (let ((p (pad w)) (s (d:lookup (styled w) :pad)))
    (cond ((consp p) (cdr p)) ((realp p) p) ((consp s) (cdr s)) (t 0))))

(defun %margin (w) (or (margin w) (d:lookup (styled w) :margin)))
(defun %margin-x (w) (let ((m (%margin w))) (if m (+ (fourth m) (second m)) 0)))
(defun %margin-y (w) (let ((m (%margin w))) (if m (+ (first m) (third m)) 0)))
(defun %min-w (w) (max (min-w w) (or (d:lookup (styled w) :min-w) 0)))
(defun %min-h (w) (max (min-h w) (or (d:lookup (styled w) :min-h) 0)))
(defun %font (w) (or (font w) (d:lookup (styled w) :font)))

(defgeneric measure (widget medium avail-w avail-h)
  (:documentation "The widget's natural (values w h) in the space it is given.")
  (:method ((w widget) m aw ah) (declare (ignore m aw ah)) (values 0 1)))

(defmethod measure :around ((w widget) m aw ah)
  "The css box model around the intrinsic measure: content is measured in what is
left after margin and padding, and the answer adds padding back, floors at the
minimum, then adds margin."
  (let ((had (and *sizes* (gethash w *sizes*))))
    (if had
        (values (first had) (second had))
        (multiple-value-bind (cw ch)
            (call-next-method w m
                              (max 0 (- aw (* 2 (%pad-x w)) (%margin-x w)))
                              (max 0 (- ah (* 2 (%pad-y w)) (%margin-y w))))
          (let ((out-w (+ (max (%min-w w) (+ cw (* 2 (%pad-x w)))) (%margin-x w)))
                (out-h (+ (max (%min-h w) (+ ch (* 2 (%pad-y w)))) (%margin-y w))))
            (when *sizes* (setf (gethash w *sizes*) (list out-w out-h)))
            (values out-w out-h))))))

(defgeneric arrange (widget medium x y w h)
  (:documentation "Give the widget this rect, and place what it holds.")
  (:method ((w widget) m x y width height)
    (declare (ignore m))
    (setf (left w) x (top w) y
          (right w) (+ x width) (bottom w) (+ y (max 0 (1- height))))))

(defmethod arrange :around ((w widget) m x y width height)
  (let ((mg (%margin w)))
    (if mg
        (call-next-method w m (+ x (fourth mg)) (+ y (first mg))
                          (max 0 (- width (%margin-x w)))
                          (max 0 (- height (%margin-y w))))
        (call-next-method))))

(defun %inner (w x y width height)
  (values (+ x (%pad-x w)) (+ y (%pad-y w))
          (max 0 (- width (* 2 (%pad-x w))))
          (max 0 (- height (* 2 (%pad-y w))))))

(defgeneric paint (widget medium)
  (:documentation "Draw the widget into its arranged rect. One generic; which
medium you were handed is what says whether that is cells or pixels.")
  (:method ((w widget) m) (declare (ignore m)) nil))

(defun %fill (w m)
  (let ((f (if (and (hovered w) *hover*) *hover* (face w))))
    (when f
      (multiple-value-bind (fr fg fb br bg bb attr) (ink f)
        (declare (ignore fr fg fb attr))
        (when (>= br 0)
          (loop :for line :from (top w) :to (bottom w)
                :do (loop :for col :from (left w) :below (right w)
                          :do (put-bg m line col br bg bb))))))))

(defmethod paint :before ((w widget) (m grid)) (%fill w m))

(defmethod paint ((w widget) (m grid))
  (dolist (part (parts w)) (paint part m)))

(defmethod measure ((w label) m aw ah)
  (declare (ignore aw ah))
  (text-size m (content w) (%font w)))

(defmethod paint ((w label) (m grid))
  (let ((s (content w)) (n (width w)))
    (loop :for i :from 0 :below (min (length s) n)
          :do (put m (top w) (+ (left w) i) (char s i) (face w)))))

(defmethod measure ((w rule) m aw ah)
  "A rule's natural size is its thickness; its length comes from the container's
arrange, never from the space available -- answering that here would eat the axis."
  (declare (ignore aw ah))
  (let ((thick (max 1 (nth-value 1 (text-size m "M" nil)))))
    (if (upright w)
        (values (if (typep m 'grid) 1 thick) 1)
        (values 1 (if (typep m 'grid) 1 thick)))))

(defmethod paint ((w rule) (m grid))
  (if (upright w)
      (loop :for line :from (top w) :to (bottom w)
            :do (put m line (left w) (glyph w) (face w)))
      (loop :for col :from (left w) :below (right w)
            :do (put m (top w) col (glyph w) (face w)))))

(defmethod measure ((w gap) m aw ah) (declare (ignore m aw ah)) (values 0 0))

(defmethod measure ((w cells) m aw ah)
  (declare (ignore aw ah))
  (let* ((all (rows-of w))
         (cols (reduce #'max all :initial-value 1 :key (lambda (r) (length (car r)))))
         (n (max 1 (- (length all) (or (over w) 0)))))
    (multiple-value-bind (cw ch) (text-size m "M" (%font w))
      (values (* cols cw) (* n ch)))))

(defmethod paint ((w cells) (m grid))
  (let ((up (or (over w) 0)))
    (loop :for row :in (rows-of w)
          :for line :from (- (top w) up)
          :do (blit m line (left w) (car row) (cdr row)))))

(defmethod measure ((w slider) m aw ah)
  (declare (ignore aw ah))
  (if (typep m 'grid)
      (values (track w) 1)
      (values (* 8 (track w)) (line-height m (%font w)))))

(defmethod paint ((w slider) (m grid))
  (let* ((n (track w))
         (upto (round (* (fraction w) n))))
    (loop :for i :from 0 :below n
          :do (put m (top w) (+ (left w) i)
                        (if (< i upto) +filled+ +empty+)
                        (if (< i upto) :function-name :comment)))))

(defmethod measure ((w calendar) m aw ah)
  "Seven days across and eight lines down: the month, the day names, and the six
weeks a month can fall across."
  (declare (ignore aw ah))
  (multiple-value-bind (cw ch) (text-size m "MM " (%font w))
    (if (typep m 'grid)
        (values (* 7 3) 8)
        (values (* 7 cw) (* 8 ch)))))

(defmethod measure ((w ring) m aw ah)
  (declare (ignore m aw ah))
  (let ((d (max (diameter w) (%min-w w) (%min-h w))))
    (values d d)))

(defmethod arrange ((w ring) m x y width height)
  (call-next-method)
  (let ((part (first (parts w))))
    (when part
      (multiple-value-bind (ix iy iw ih) (%inner w x y width height)
        (multiple-value-bind (cw ch) (measure part m iw ih)
          (arrange part m (+ ix (floor (- iw cw) 2)) (+ iy (floor (- ih ch) 2))
                   cw ch))))))

(defun %stacked (w m aw ah down)
  (let ((wide 0) (high 0) (all (parts w)))
    (dolist (part all)
      (multiple-value-bind (cw ch) (measure part m aw ah)
        (if down
            (progn (setf wide (max wide cw)) (incf high ch))
            (progn (incf wide cw) (setf high (max high ch))))))
    (let ((gaps (* (spacing w) (max 0 (1- (length all))))))
      (if down (values wide (+ high gaps)) (values (+ wide gaps) high)))))

(defmethod measure ((w column) m aw ah) (%stacked w m aw ah t))
(defmethod measure ((w row) m aw ah) (%stacked w m aw ah nil))

(defun %lay (w m x y width height down)
  (let* ((all (parts w))
         (sizes (mapcar (lambda (p) (multiple-value-list (measure p m width height)))
                        all))
         (natural (+ (reduce #'+ sizes :initial-value 0
                                       :key (if down #'second #'first))
                     (* (spacing w) (max 0 (1- (length all))))))
         (slack (max 0 (- (if down height width) natural)))
         (weight (reduce #'+ all :initial-value 0 :key #'expand))
         (acc 0) (given 0)
         (at (if down y x)))
    (loop :for part :in all
          :for (cw ch) :in sizes
          :for extra := (if (plusp weight)
                            (progn (incf acc (* slack (/ (expand part) weight)))
                                   (prog1 (- (round acc) given)
                                     (setf given (round acc))))
                            0)
          :do (if down
                  (let ((fh (+ ch extra))
                        (cx (ecase (align w)
                              ((:start :stretch) x)
                              (:center (+ x (floor (- width cw) 2)))
                              (:end (+ x (- width cw))))))
                    (arrange part m cx at (if (eq (align w) :stretch) width cw) fh)
                    (setf at (+ at fh (spacing w))))
                  (let ((fw (+ cw extra))
                        (cy (ecase (align w)
                              ((:start :stretch) y)
                              (:center (+ y (floor (- height ch) 2)))
                              (:end (+ y (- height ch))))))
                    (arrange part m at cy fw (if (eq (align w) :stretch) height ch))
                    (setf at (+ at fw (spacing w))))))))

(defmethod arrange ((w column) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (%lay w m x y width height t)))

(defmethod arrange ((w row) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (%lay w m x y width height nil)))

(defmethod measure ((w stack) m aw ah)
  (let ((wide 0) (high 0))
    (dolist (part (parts w) (values wide high))
      (multiple-value-bind (cw ch) (measure part m aw ah)
        (setf wide (max wide cw) high (max high ch))))))

(defmethod arrange ((w stack) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (dolist (part (parts w)) (arrange part m x y width height))))

(defmethod measure ((w box) m aw ah)
  (declare (ignore aw))
  (let ((part (first (parts w))))
    (values (wide w) (if part (nth-value 1 (measure part m (wide w) ah)) 1))))

(defmethod arrange ((w box) m x y width height)
  (declare (ignore width))
  (call-next-method w m x y (wide w) height)
  (let ((part (first (parts w))))
    (when part
      (multiple-value-bind (cw ch) (measure part m (wide w) height)
        (arrange part m
                 (ecase (align w)
                   (:left x)
                   (:right (+ x (- (wide w) cw)))
                   (:center (+ x (floor (- (wide w) cw) 2))))
                 y cw (max 1 ch))))))

(defmethod measure ((w center) m aw ah)
  (let ((part (first (parts w))))
    (if part (measure part m aw ah) (values 0 0))))

(defmethod arrange ((w center) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (let ((part (first (parts w))))
      (when part
        (multiple-value-bind (cw ch) (measure part m width height)
          (arrange part m (+ x (floor (- width cw) 2))
                   (+ y (floor (- height ch) 2)) cw ch))))))

(defmethod measure ((w centerbox) m aw ah)
  (let ((wide 0) (high 0))
    (dolist (part (parts w) (values wide high))
      (multiple-value-bind (cw ch) (measure part m aw ah)
        (setf wide (max wide cw))
        (incf high ch)))))

(defmethod arrange ((w centerbox) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (let* ((s (start w)) (c (middle w)) (e (end w))
           (sh (if s (nth-value 1 (measure s m width height)) 0))
           (eh (if e (nth-value 1 (measure e m width height)) 0)))
      (when s (arrange s m x y width sh))
      (when e (arrange e m x (+ y (- height eh)) width eh))
      (when c
        (let ((ch (nth-value 1 (measure c m width height)))
              (room (max 0 (- height sh eh))))
          (arrange c m x (+ y sh (max 0 (floor (- room ch) 2))) width ch))))))

(defmethod measure ((w scroll) m aw ah)
  (declare (ignore ah))
  (let ((part (first (parts w))))
    (values (if part (nth-value 0 (measure part m aw 100000)) 0) (tall w))))

(defmethod arrange ((w scroll) m x y width height)
  (declare (ignore height))
  (call-next-method w m x y width (tall w))
  (let ((part (first (parts w))))
    (when part
      (let ((ch (nth-value 1 (measure part m width 100000))))
        (arrange part m x (- y (offset w)) width ch)))))

(defmethod paint ((w scroll) (m grid))
  (let ((part (first (parts w))))
    (when part
      (with-clip (m (left w) (top w) (right w) (+ (top w) (tall w)))
        (paint part m)))))

(defmethod measure ((w choice) m aw ah)
  (let ((mark (if (chosen w) (before w) (after w)))
        (part (first (parts w))))
    (multiple-value-bind (cw ch) (if part (measure part m aw ah) (values 0 1))
      (values (+ (length mark) cw) ch))))

(defmethod arrange ((w choice) m x y width height)
  (call-next-method)
  (let ((mark (if (chosen w) (before w) (after w)))
        (part (first (parts w))))
    (when part
      (arrange part m (+ x (length mark)) y (- width (length mark)) height))))

(defmethod paint ((w choice) (m grid))
  (let ((mark (if (chosen w) (before w) (after w))))
    (loop :for i :from 0 :below (length mark)
          :do (put m (top w) (+ (left w) i) (char mark i) (face w)))
    (dolist (part (parts w)) (paint part m))))

(defmethod measure ((w action) m aw ah)
  (let ((part (first (parts w))))
    (if part (measure part m aw ah) (values 0 1))))

(defmethod arrange ((w action) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (let ((part (first (parts w))))
      (when part
        (multiple-value-bind (cw ch) (measure part m width height)
          (arrange part m (+ x (floor (- width cw) 2))
                   (+ y (floor (- height ch) 2)) cw ch))))))

(pine/word:lends "measure" "paint")
