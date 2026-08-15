(defpackage #:pine.ui.style
  (:use #:cl)
  (:export #:resolve #:style
           #:st-bg #:st-gradient #:st-fg #:st-border-w #:st-border-color
           #:st-radius #:st-pad-x #:st-pad-y #:st-min-w #:st-min-h
           #:st-font-px #:st-bold #:st-inset #:st-margin #:st-shadow
           #:st-opacity))

(in-package #:pine.ui.style)

(defstruct (style (:conc-name st-))
  bg gradient fg (border-w 0) border-color (radius 0) pad-x pad-y min-w min-h
  font-px bold inset margin shadow opacity)

(defun split-ws (s)
  (remove "" (uiop:split-string s :separator '(#\space #\tab)) :test #'string=))

(defun parse-segment (s)
  (cond ((string= s "*") (list :universal t))
        ((find #\. s)
         (let* ((colon (position #\: s))
                (pseudo (and colon (subseq s (1+ colon))))
                (body   (subseq s 0 (or colon (length s))))
                (classes (remove "" (uiop:split-string body :separator ".") :test #'string=)))
           (list :classes classes :pseudo pseudo)))
        (t (list :element t))))

(defun parse-selector (s)
  (mapcar #'parse-segment (split-ws s)))

(defun parse-rule-selectors (sel-string)
  (mapcar (lambda (g) (parse-selector (string-trim " " g)))
          (uiop:split-string sel-string :separator '(#\,))))

(defun compiled-rules ()
  (pine.ui.face:memo
   :stylesheet
   (lambda ()
     (loop :for (sel props) :in (pine.ui.css:stylesheet)
           :collect (cons (parse-rule-selectors sel) props)))))

(defun reset-rules ()
  (let* ((w pine.world.world:*world*)
         (n (and w (pine.world.world:at w "memo/stylesheet"))))
    (when n (pine/fs/node:invalidate n)))
  nil)

(defun pseudo-ok (pseudo hover)
  (or (null pseudo) (and (string= pseudo "hover") hover)))

(defun seg-match (seg classes hover)
  (cond ((getf seg :element) nil)
        ((getf seg :universal) (pseudo-ok (getf seg :pseudo) hover))
        (t (and (subsetp (getf seg :classes) classes :test #'string=)
                (pseudo-ok (getf seg :pseudo) hover)))))

(defun ancestors-match (segs ancestors)
  "Each seg matches some ancestor, left to right, ancestors root-first."
  (if (null segs)
      t
      (loop for tail on ancestors
            when (seg-match (first segs) (first tail) nil)
              do (return (ancestors-match (rest segs) (rest tail)))
            finally (return nil))))

(defun selector-match (segments chain hover)
  "CHAIN is a list of class-sets root..node; the last segment matches the node
(with HOVER), the earlier ones match ancestors in order."
  (and segments chain
       (seg-match (car (last segments)) (car (last chain)) hover)
       (ancestors-match (butlast segments) (butlast chain))))

(defun matched-props (chain hover)
  "The props of every rule matching CHAIN, merged in source order, later wins."
  (let ((merged nil))
    (loop for (selectors . props) in (compiled-rules)
          when (some (lambda (sel) (selector-match sel chain hover)) selectors)
            do (loop for (k v) on props by #'cddr
                     do (setf (getf merged k) v)))
    merged))

(defun parts-nth (parts i) (string-trim " " (nth i parts)))

(defun parse-rgb-func (s n)
  (let* ((open (position #\( s)) (close (position #\) s))
         (parts (uiop:split-string (subseq s (1+ open) close) :separator '(#\,))))
    (when (>= (length parts) n)
      (flet ((num (i) (read-from-string (parts-nth parts i))))
        (values (/ (num 0) 255.0) (/ (num 1) 255.0) (/ (num 2) 255.0)
                (if (= n 4) (float (num 3)) 1.0))))))

(defun parse-color (s)
  "(values r g b a) in 0..1, or NIL for transparent/none/unparsable."
  (cond
    ((or (null s) (string= s "transparent") (string= s "none")) nil)
    ((and (plusp (length s)) (char= (char s 0) #\#))
     (multiple-value-bind (r g b) (pine.ui.face:hex-rgb s)
       (when r (values (/ r 255.0) (/ g 255.0) (/ b 255.0) 1.0))))
    ((eql 0 (search "rgba(" s)) (parse-rgb-func s 4))
    ((eql 0 (search "rgb(" s))  (parse-rgb-func s 3))
    (t nil)))

(defgeneric parse-lengths (s)
  (:documentation "The px integers of a length value: a number is that many px,
a list of numbers likewise, a string is parsed (\"10px 12px\"; rem and % are
ignored). A value shape a back end brings of its own is a method.")
  (:method (s) (declare (ignore s)) nil))

(defmethod parse-lengths ((s real)) (list (round s)))

(defmethod parse-lengths ((s cons)) (mapcar #'round s))

(defmethod parse-lengths ((s string))
  (loop :for tok :in (split-ws s)
        :for n = (and (plusp (length tok))
                      (digit-char-p (char tok 0))
                      (not (search "rem" tok)) (not (find #\% tok))
                      (parse-integer tok :junk-allowed t))
        :when n :collect n))

(defun box-xy (s)
  "(values pad-x pad-y) from a CSS box shorthand, averaging asymmetric sides."
  (let* ((l (parse-lengths s)))
    (case (length l)
      (0 (values nil nil))
      (1 (values (first l) (first l)))
      (2 (values (second l) (first l)))
      (3 (values (second l) (round (+ (first l) (third l)) 2)))
      (t (values (round (+ (second l) (fourth l)) 2)
                 (round (+ (first l) (third l)) 2))))))

(defun parse-radius (s)
  (cond ((null s) 0)
        ((realp s) (round s))
        ((and (stringp s) (search "100%" s)) :round)
        (t (or (first (parse-lengths s)) 0))))

(defun parse-gradient (s)
  "A CSS linear-gradient -> (list (r g b) (r g b)) start/end colours, or NIL."
  (when (and s (search "linear-gradient" s))
    (let* ((open (position #\( s)) (close (position #\) s :from-end t))
           (parts (uiop:split-string (subseq s (1+ open) close) :separator '(#\,)))
           (cols (loop for pt in parts
                       for c = (multiple-value-list (parse-color (string-trim " " pt)))
                       when (first c) collect (subseq c 0 3))))
      (when (>= (length cols) 2) (list (first cols) (second cols))))))

(defun parse-inset (s)
  "A CSS inset box-shadow (inset <w> 0 0 0 <colour>) -> (list r g b width), or
NIL. Used for the left accent bar on a selected row; the blurred drop-shadow
form is the separate PARSE-SHADOW."
  (when (and s (search "inset" s))
    (let* ((toks (split-ws s))
           (width (or (first (parse-lengths s)) 3)))
      (multiple-value-bind (r g b) (parse-color (car (last toks)))
        (when r (list r g b width))))))

(defun parse-shadow (s)
  "A CSS drop shadow (<ox> <oy> <blur> <spread> <colour>) -> (list ox oy blur
(r g b a)), or NIL. The inset form is PARSE-INSET's; this is the panel float."
  (when (and s (not (search "inset" s)) (search "px" s))
    (let* ((lens (parse-lengths s))
           (toks (split-ws s))
           (c (multiple-value-list (parse-color (car (last toks))))))
      (when (and (>= (length lens) 3) (first c))
        (list (or (first lens) 0) (or (second lens) 0) (or (third lens) 0) c)))))

(defun parse-box4 (s)
  "The four side lengths (top right bottom left) of a CSS box shorthand, px only,
expanded per CSS rules; NIL when none."
  (let ((l (parse-lengths s)))
    (case (length l)
      (0 nil)
      (1 (list (first l) (first l) (first l) (first l)))
      (2 (list (first l) (second l) (first l) (second l)))
      (3 (list (first l) (second l) (third l) (second l)))
      (t (subseq l 0 4)))))

(defun resolve-margin (prop)
  "Merge the :margin shorthand with any per-side :margin-* overrides into
(top right bottom left); NIL when every side is zero."
  (let ((m (or (parse-box4 (funcall prop :margin)) (list 0 0 0 0))))
    (flet ((side (k i) (let ((v (first (parse-lengths (funcall prop k)))))
                         (when v (setf (nth i m) v)))))
      (side :margin-top 0) (side :margin-right 1)
      (side :margin-bottom 2) (side :margin-left 3))
    (unless (every #'zerop m) m)))

(defun parse-opacity (s)
  "An :opacity value as a float 0..1: a real directly, or a string parsed."
  (let ((v (if (realp s)
               s
               (and (stringp s)
                    (ignore-errors
                     (with-standard-io-syntax
                       (let ((*read-eval* nil)) (read-from-string s))))))))
    (when (realp v) (float (max 0 (min 1 v)) 1.0))))

(defun resolve (chain &key hover)
  "Resolve the merged style for a node given its class CHAIN (root..node)."
  (let* ((p (matched-props chain hover))
         (st (make-style)))
    (flet ((prop (k) (getf p k)))
      (multiple-value-bind (r g b a) (parse-color (prop :background-color))
        (when r (setf (st-bg st) (list r g b a))))
      (setf (st-gradient st) (parse-gradient (prop :background-image)))
      (setf (st-inset st) (parse-inset (prop :box-shadow)))
      (setf (st-shadow st) (parse-shadow (prop :box-shadow)))
      (setf (st-margin st) (resolve-margin #'prop))
      (multiple-value-bind (r g b) (parse-color (prop :color))
        (when r (setf (st-fg st) (list r g b))))
      (when (equal (prop :border-style) "solid")
        (setf (st-border-w st) (or (first (parse-lengths (prop :border-width))) 1))
        (multiple-value-bind (r g b) (parse-color (prop :border-color))
          (when r (setf (st-border-color st) (list r g b)))))
      (setf (st-radius st) (parse-radius (prop :border-radius)))
      (multiple-value-bind (px py) (box-xy (prop :padding))
        (setf (st-pad-x st) px (st-pad-y st) py))
      (setf (st-min-w st) (first (parse-lengths (prop :min-width)))
            (st-min-h st) (first (parse-lengths (prop :min-height)))
            (st-font-px st) (first (parse-lengths (prop :font-size)))
            (st-bold st) (equal (prop :font-weight) "bold")
            (st-opacity st) (parse-opacity (prop :opacity))))
    st))
