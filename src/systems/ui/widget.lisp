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
   (right   :initform 0 :accessor right)))

(defclass label (widget)
  ((content :initarg :content :accessor content :initform "")
   (on-change :initarg :on-change :accessor on-change :initform nil)))

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

(defclass stack (widget) ())

(defclass box (widget)
  ((fixed-width :initarg :fixed-width :accessor fixed-width :initform 0)
   (align :initarg :align :accessor align :initform :left)))

(defclass center (widget) ())

(defclass centerbox (widget)
  ((upright :initarg :upright :accessor upright :initform t)
   (start   :initarg :start   :accessor start   :initform nil)
   (middle  :initarg :middle  :accessor middle  :initform nil)
   (end     :initarg :end     :accessor end     :initform nil)))

(defclass scroll (widget)
  ((offset :initarg :offset :accessor offset :initform 0)
   (fixed-height :initarg :fixed-height :accessor fixed-height :initform 10)))

(defclass action (widget)
  ((on-click :initarg :on-click :accessor on-click :initform nil)))

(defclass choice (widget)
  ((on-click  :initarg :on-click  :accessor on-click  :initform nil)
   (before :initarg :before :accessor before :initform "> ")
   (after  :initarg :after  :accessor after  :initform "  ")))

(defclass slider (widget)
  ((value   :initarg :value   :accessor held    :initform 0)
   (low     :initarg :low     :accessor low     :initform 0)
   (high    :initarg :high    :accessor high    :initform 100)
   (track   :initarg :track   :accessor track   :initform 16)
   (on-change :initarg :on-change :accessor on-change :initform nil)))

(defclass ring (widget)
  ((value     :initarg :value     :accessor held      :initform 0)
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
   (opacity :initarg :opacity :accessor opacity :initform 1.0)))

(defmethod parts ((w centerbox))
  (remove nil (list (start w) (middle w) (end w))))

(defun placed (w)
  (or (plusp (right w)) (plusp (bottom w))))

(defun width (w) (- (right w) (left w)))

(defun height (w) (- (bottom w) (top w)))

(defun fraction (w)
  (let* ((span (max 1 (- (high w) (low w))))
         (v (max 0 (min span (- (or (held w) 0) (low w))))))
    (/ v span)))

(defmethod initialize-instance :after ((w gap) &key)
  (when (zerop (expand w)) (setf (expand w) 1)))

(defmethod initialize-instance :after ((w rule) &key)
  (when (and (upright w) (char= (glyph w) (code-char #x2500)))
    (setf (glyph w) (code-char #x2502))))

(defmethod initialize-instance :after ((w slider) &key)
  (unless (numberp (held w)) (setf (held w) 0))
  (unless (numberp (low w))   (setf (low w) 0))
  (unless (numberp (high w))  (setf (high w) 100)))

(defmethod initialize-instance :after ((w ring) &key)
  (unless (numberp (held w)) (setf (held w) 0))
  (unless (numberp (low w))   (setf (low w) 0))
  (unless (numberp (high w))  (setf (high w) 100)))

