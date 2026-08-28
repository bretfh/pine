(in-package #:pine/ui)

(defclass widget ()
  ((parts   :initarg :parts   :accessor parts   :initform nil)
   (key     :initarg :key     :accessor key     :initform nil)
   (of      :initarg :of      :accessor of      :initform nil)
   (chosen  :initarg :chosen  :accessor chosen  :initform nil)
   (face    :initarg :face    :accessor face    :initform nil)
   (css-class :initarg :class :accessor css-class :initform nil)
   (hint    :initarg :hint    :accessor hint    :initform nil)
   (hovered :initform nil     :accessor hovered)
   (radius  :initarg :radius  :accessor radius  :initform 0)
   (fill-color :initarg :fill :accessor fill-color :initform nil)
   (grad    :initarg :grad    :accessor grad    :initform nil)
   (font    :initarg :font    :accessor font    :initform nil)
   (pad     :initarg :pad     :accessor pad     :initform nil)
   (margin  :initarg :margin  :accessor margin  :initform nil)
   (expand  :initarg :expand  :accessor expand  :initform 0)
   (min-w   :initarg :min-w   :accessor min-w   :initform 0)
   (min-h   :initarg :min-h   :accessor min-h   :initform 0)
   (top     :initform 0 :accessor top)
   (left    :initform 0 :accessor left)
   (bottom  :initform 0 :accessor bottom)
   (right   :initform 0 :accessor right))
  (:documentation "An immutable value that crosses a wire, not a node: nothing you
draw is in the namespace, only the surface that works it out.

PARTS is what it holds, and it is one slot rather than a method per container, so
measure, arrange, paint, hit-testing and the wire cannot disagree about it. TOP LEFT
BOTTOM RIGHT is where it was arranged."))

(defclass label (widget)
  ((content :initarg :content :accessor content :initform "")
   (changed :initarg :changed :accessor changed :initform nil))
  (:documentation "A run of text. With CHANGED it is a field: what is typed into it
is written back where it came from."))

(defclass rule (widget)
  ((glyph   :initarg :glyph   :accessor glyph   :initform (code-char #x2500))
   (upright :initarg :upright :accessor upright :initform nil)))

(defclass gap (widget) ())

(defclass column (widget)
  ((spacing :initarg :spacing :accessor spacing :initform 0)
   (align   :initarg :align   :accessor align   :initform :start)))

(defclass row (widget)
  ((spacing :initarg :spacing :accessor spacing :initform 1)
   (align   :initarg :align   :accessor align   :initform :start)))

(defclass stack (widget) ()
  (:documentation "Parts in one place: each is given the whole rect and they are
painted in the order they were written, so the last is on top."))

(defclass box (widget)
  ((wide  :initarg :wide  :accessor wide  :initform 0)
   (align :initarg :align :accessor align :initform :left))
  (:documentation "A cell of a fixed width."))

(defclass center (widget) ())

(defclass centerbox (widget)
  ((upright :initarg :upright :accessor upright :initform t)
   (start   :initarg :start   :accessor start   :initform nil)
   (middle  :initarg :middle  :accessor middle  :initform nil)
   (end     :initarg :end     :accessor end     :initform nil))
  (:documentation "Start at one end, end at the other, and the middle in what is
left between them. Centring the middle in the whole box instead is what puts a bar's
apps on top of its workspaces once there are enough workspaces."))

(defclass scroll (widget)
  ((offset :initarg :offset :accessor offset :initform 0)
   (tall   :initarg :tall   :accessor tall   :initform 10))
  (:documentation "A clipped window onto something taller."))

(defclass action (widget)
  ((click :initarg :click :accessor click :initform nil))
  (:documentation "Does something when it is clicked."))

(defclass choice (widget)
  ((click  :initarg :click  :accessor click  :initform nil)
   (before :initarg :before :accessor before :initform "> ")
   (after  :initarg :after  :accessor after  :initform "  "))
  (:documentation "A row of a listing: it can be chosen, and it stands for
something -- OF is what."))

(defclass slider (widget)
  ((value   :initarg :value   :accessor value   :initform 0)
   (low     :initarg :low     :accessor low     :initform 0)
   (high    :initarg :high    :accessor high    :initform 100)
   (track   :initarg :track   :accessor track   :initform 16)
   (changed :initarg :changed :accessor changed :initform nil)))

(defclass ring (widget)
  ((value     :initarg :value     :accessor value     :initform 0)
   (low       :initarg :low       :accessor low       :initform 0)
   (high      :initarg :high      :accessor high      :initform 100)
   (thickness :initarg :thickness :accessor thickness :initform 5)
   (diameter  :initarg :diameter  :accessor diameter  :initform 56)))

(defclass calendar (widget)
  ((year  :initarg :year  :accessor year  :initform 2000)
   (month :initarg :month :accessor month :initform 1)
   (day   :initarg :day   :accessor day   :initform 1)))

(defclass picture (widget)
  ((path :initarg :path :accessor path :initform "")))

(defclass cells (widget)
  ((rows    :initarg :rows    :accessor rows-of :initform nil)
   (caret   :initarg :caret   :accessor caret   :initform nil)
   (over    :initarg :over    :accessor over    :initform nil)
   (opacity :initarg :opacity :accessor opacity :initform 1.0))
  (:documentation "A leaf holding rows that are already laid out, each (text .
runs). Measure and arrange are one step and paint blits them. CARET is (row . col)
or nothing; OVER is how many leading rows are drawn above the rect rather than in
it, which is what floats a completion list over the buffer."))

(defmethod parts ((w centerbox))
  (remove nil (list (start w) (middle w) (end w))))

(defun placed (w)
  "Whether W carries an arranged rect."
  (or (plusp (right w)) (plusp (bottom w))))

(defun width (w) (- (right w) (left w)))

(defun height (w) (1+ (- (bottom w) (top w))))

(defun fraction (w)
  "The filled part of a slider or a ring, 0..1, clamped."
  (let* ((span (max 1 (- (high w) (low w))))
         (v (max 0 (min span (- (or (value w) 0) (low w))))))
    (/ v span)))

(defmethod initialize-instance :after ((w gap) &key)
  (when (zerop (expand w)) (setf (expand w) 1)))

(defmethod initialize-instance :after ((w rule) &key)
  (when (and (upright w) (char= (glyph w) (code-char #x2500)))
    (setf (glyph w) (code-char #x2502))))

(defmethod initialize-instance :after ((w slider) &key)
  (unless (numberp (value w)) (setf (value w) 0))
  (unless (numberp (low w))   (setf (low w) 0))
  (unless (numberp (high w))  (setf (high w) 100)))

(defmethod initialize-instance :after ((w ring) &key)
  (unless (numberp (value w)) (setf (value w) 0))
  (unless (numberp (low w))   (setf (low w) 0))
  (unless (numberp (high w))  (setf (high w) 100)))

(pine/word:lends "widget" "parts")
