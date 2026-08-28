(defpackage #:pine/paint/canvas
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:fault #:pine/run/fault))
  (:export
   #:canvas #:context #:with-canvas #:rgb))
(in-package #:pine/paint/canvas)

(defvar *font* "Maple Mono NF")

(defparameter +months+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defparameter +days+ #("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))

(defclass canvas (ui:medium)
  ((context :initarg :context :reader context)
   (font    :initarg :font    :accessor font    :initform *font*)
   (size    :initarg :size    :accessor size    :initform 14))
  (:documentation "Pixels, through cairo. The other medium PAINT dispatches on: a
widget is measured, arranged and painted the same way, and what it lands on is what
says whether that is cells or pixels."))

(defmacro with-canvas ((it) &body body)
  "Draw onto a canvas, with cairo's context bound for the extent of it."
  `(let ((cl-cairo2:*context* (context ,it)))
     ,@body))

(defun rgb (colour &optional (alpha 1.0))
  (when colour
    (destructuring-bind (r g b) (subseq colour 0 3)
      (if (< alpha 1.0)
          (cl-cairo2:set-source-rgba (/ r 255.0) (/ g 255.0) (/ b 255.0) alpha)
          (cl-cairo2:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)))
      t)))

(defun %size (m w) (float (or (ui:font w) (d:lookup (ui:styled w) :font) (size m))
                          1d0))

(defun %face (m w)
  (cl-cairo2:select-font-face (font m) :normal :normal)
  (cl-cairo2:set-font-size (%size m w)))

(defmethod ui:text-size ((m canvas) text font)
  (with-canvas (m)
    (cl-cairo2:select-font-face (font m) :normal :normal)
    (cl-cairo2:set-font-size (float (or font (size m)) 1d0))
    (let ((extents (cl-cairo2:get-font-extents)))
      (multiple-value-bind (xb yb width height advance) (cl-cairo2:text-extents text)
        (declare (ignore xb yb width height))
        (values (max 1 (ceiling advance))
                (max 1 (ceiling (+ (cl-cairo2:font-ascent extents)
                                   (cl-cairo2:font-descent extents)))))))))

(defun %rect (widget)
  (values (ui:left widget) (ui:top widget)
          (ui:width widget) (ui:height widget)))

(defun %rounded (x y width height radius)
  (let ((x (float x 1d0)) (y (float y 1d0))
        (width (float width 1d0)) (height (float height 1d0))
        (r (float (min radius (/ (min width height) 2d0)) 1d0)))
    (if (<= r 0d0)
        (cl-cairo2:rectangle x y width height)
        (progn
          (cl-cairo2:new-sub-path)
          (cl-cairo2:arc (+ x width (- r)) (+ y r) r (* -0.5d0 pi) 0d0)
          (cl-cairo2:arc (+ x width (- r)) (+ y height (- r)) r 0d0 (* 0.5d0 pi))
          (cl-cairo2:arc (+ x r) (+ y height (- r)) r (* 0.5d0 pi) pi)
          (cl-cairo2:arc (+ x r) (+ y r) r pi (* 1.5d0 pi))
          (cl-cairo2:close-path)))))

(defun %radius (widget style width height)
  (let ((r (or (d:lookup style :radius) (ui:radius widget) 0)))
    (cond ((eq r :round) (/ (min width height) 2.0))
          ((numberp r) r)
          (t 0))))

(defun %gradient (x y width height stops)
  (let ((p (cl-cairo2:create-linear-pattern (float x 1d0) (float y 1d0)
                                            (float (+ x width) 1d0)
                                            (float (+ y height) 1d0))))
    (destructuring-bind ((r0 g0 b0) (r1 g1 b1)) (subseq stops 0 2)
      (cl-cairo2:pattern-add-color-stop-rgb p 0d0 (/ r0 255.0) (/ g0 255.0)
                                            (/ b0 255.0))
      (cl-cairo2:pattern-add-color-stop-rgb p 1d0 (/ r1 255.0) (/ g1 255.0)
                                            (/ b1 255.0)))
    (cl-cairo2:set-source p)
    (cl-cairo2:fill-path)
    (cl-cairo2:destroy p)))

(defun %shadow (style x y width height radius)
  "A feathered drop shadow, drawn as low-alpha rings around the shape, so a panel
reads as floating over what is behind it."
  (let ((said (d:lookup style :shadow)))
    (when said
      (destructuring-bind (ox oy blur colour) said
        (let ((steps (max 1 (min 12 (round blur)))))
          (loop :for i :downfrom steps :to 1
                :for grow := (float i 1d0)
                :do (rgb colour (* 0.14 (or (fourth colour) 1.0)))
                    (%rounded (- (+ x ox) grow) (- (+ y oy) grow)
                              (+ width (* 2 grow)) (+ height (* 2 grow))
                              (+ radius grow))
                    (cl-cairo2:fill-path)))))))

(defun %ink (widget style)
  "The colour this widget's content is drawn in: what its style says, else what its
face says, else the plain one."
  (or (d:lookup style :fg)
      (multiple-value-bind (fr fg fb) (ui:ink (or (ui:face widget) :default))
        (list fr fg fb))))

(defmethod ui:paint :before ((widget ui:widget) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (when (and (plusp width) (plusp height))
      (let* ((style (ui:styled widget))
             (radius (%radius widget style width height))
             (grad (or (ui:grad widget) (d:lookup style :grad)))
             (fill (or (ui:fill-color widget) (d:lookup style :bg)))
             (border (d:lookup style :border))
             (opacity (or (d:lookup style :opacity) 1.0)))
        (with-canvas (m)
          (%shadow style x y width height radius)
          (cond (grad
                 (%rounded x y width height radius)
                 (%gradient x y width height grad))
                (fill
                 (%rounded x y width height radius)
                 (rgb fill (* opacity (or (fourth fill) 1.0)))
                 (cl-cairo2:fill-path)))
          (when border
            (destructuring-bind (thickness colour) border
              (when (rgb colour)
                (cl-cairo2:set-line-width (float thickness 1d0))
                (%rounded (+ x 0.5) (+ y 0.5) (- width 1) (- height 1) radius)
                (cl-cairo2:stroke)))))))))

(defmethod ui:paint ((widget ui:widget) (m canvas))
  (dolist (part (ui:parts widget)) (ui:paint part m)))

(defun %text (m widget text x y)
  (with-canvas (m)
    (%face m widget)
    (let ((extents (cl-cairo2:get-font-extents)))
      (cl-cairo2:move-to (float x 1d0)
                         (float (+ y (cl-cairo2:font-ascent extents)) 1d0))
      (cl-cairo2:show-text text))))

(defmethod ui:paint ((widget ui:label) (m canvas))
  (multiple-value-bind (x y) (%rect widget)
    (with-canvas (m)
      (rgb (%ink widget (ui:styled widget))))
    (%text m widget (ui:content widget) x y)))

(defmethod ui:paint ((widget ui:rule) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (with-canvas (m)
      (when (rgb (%ink widget (ui:styled widget)))
        (cl-cairo2:set-line-width 1d0)
        (if (ui:upright widget)
            (progn (cl-cairo2:move-to (+ x 0.5d0) (float y 1d0))
                   (cl-cairo2:line-to (+ x 0.5d0) (float (+ y height) 1d0)))
            (progn (cl-cairo2:move-to (float x 1d0) (+ y 0.5d0))
                   (cl-cairo2:line-to (float (+ x width) 1d0) (+ y 0.5d0))))
        (cl-cairo2:stroke)))))

(defmethod ui:paint ((widget ui:slider) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (let ((style (ui:styled widget))
          (upto (round (* (ui:fraction widget) width))))
      (with-canvas (m)
        (rgb (or (d:lookup style :bg) '(60 60 60)))
        (%rounded x (+ y (floor height 3)) width (max 2 (floor height 3))
                  (floor height 6))
        (cl-cairo2:fill-path)
        (when (plusp upto)
          (rgb (%ink widget style))
          (%rounded x (+ y (floor height 3)) upto (max 2 (floor height 3))
                    (floor height 6))
          (cl-cairo2:fill-path))))))

(defmethod ui:paint ((widget ui:ring) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (let* ((style (ui:styled widget))
           (r (/ (- (min width height) (ui:thickness widget)) 2.0))
           (cx (+ x (/ width 2.0)))
           (cy (+ y (/ height 2.0))))
      (with-canvas (m)
        (cl-cairo2:set-line-width (float (ui:thickness widget) 1d0))
        (rgb (or (d:lookup style :bg) '(60 60 60)))
        (cl-cairo2:arc (float cx 1d0) (float cy 1d0) (float r 1d0) 0d0 (* 2 pi))
        (cl-cairo2:stroke)
        (rgb (%ink widget style))
        (cl-cairo2:arc (float cx 1d0) (float cy 1d0) (float r 1d0)
                       (* -0.5d0 pi)
                       (+ (* -0.5d0 pi) (* 2 pi (ui:fraction widget))))
        (cl-cairo2:stroke))))
  (dolist (part (ui:parts widget)) (ui:paint part m)))

(defmethod ui:paint ((widget ui:cells) (m canvas))
  "Rows that are already laid out. A run carries its own colours, so this is the
one place a canvas paints what a grid worked out."
  (multiple-value-bind (x y) (%rect widget)
    (multiple-value-bind (cw ch) (ui:text-size m "M" (%size m widget))
      (with-canvas (m)
        (let ((up (or (ui:over widget) 0)))
          (loop :for row :in (ui:rows-of widget)
                :for line :from (- up)
                :for top := (+ y (* line ch))
                :do (destructuring-bind (text . runs) row
                      (loop :for (run . more) :on runs
                            :do (destructuring-bind (col fr fg fb br bg bb attr)
                                    run
                                  (declare (ignore attr))
                                  (let* ((end (if more
                                                  (car (first more))
                                                  (length text)))
                                         (part (subseq text col
                                                       (min end (length text)))))
                                    (when (>= br 0)
                                      (rgb (list br bg bb))
                                      (cl-cairo2:rectangle
                                       (float (+ x (* col cw)) 1d0)
                                       (float top 1d0)
                                       (float (* (length part) cw) 1d0)
                                       (float ch 1d0))
                                      (cl-cairo2:fill-path))
                                    (rgb (list fr fg fb))
                                    (%text m widget part (+ x (* col cw))
                                           top))))))))))
  (let ((caret (ui:caret widget)))
    (when caret
      (multiple-value-bind (x y) (%rect widget)
        (multiple-value-bind (cw ch) (ui:text-size m "M" (%size m widget))
          (with-canvas (m)
            (rgb (%ink widget (ui:styled widget)))
            (cl-cairo2:rectangle (float (+ x (* (cdr caret) cw)) 1d0)
                                 (float (+ y (* (car caret) ch)) 1d0)
                                 (max 1d0 (float (round cw 8) 1d0))
                                 (float ch 1d0))
            (cl-cairo2:fill-path)))))))

(defmethod ui:paint ((widget ui:picture) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (let ((path (ui:path widget)))
      (when (and path (probe-file path))
        (with-canvas (m)
          (let ((image (fault:or-nothing "the file may not be a png"
                         (cl-cairo2:image-surface-create-from-png
                          (namestring path)))))
            (when image
              (unwind-protect
                   (let ((iw (cl-cairo2:image-surface-get-width image))
                         (ih (cl-cairo2:image-surface-get-height image)))
                     (cl-cairo2:save)
                     (cl-cairo2:translate (float x 1d0) (float y 1d0))
                     (when (and (plusp iw) (plusp ih))
                       (cl-cairo2:scale (/ width (float iw 1d0))
                                        (/ height (float ih 1d0))))
                     (cl-cairo2:set-source-surface image 0d0 0d0)
                     (cl-cairo2:paint)
                     (cl-cairo2:restore))
                (cl-cairo2:destroy image)))))))))

(defun %weekday (year month day)
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 12 day month year))))

(defun %days-in (year month)
  (let ((n (aref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))))
    (if (and (= month 2)
             (zerop (mod year 4))
             (or (plusp (mod year 100)) (zerop (mod year 400))))
        29
        n)))

(defmethod ui:paint ((widget ui:calendar) (m canvas))
  (multiple-value-bind (x y width) (%rect widget)
    (multiple-value-bind (cw ch) (ui:text-size m "M" (%size m widget))
      (let* ((year (ui:year widget))
             (month (ui:month widget))
             (today (ui:day widget))
             (first-day (%weekday year month 1))
             (days (%days-in year month))
             (cell (max cw (floor width 7))))
        (with-canvas (m) (rgb (%ink widget (ui:styled widget))))
        (%text m widget (format nil "~a ~d" (aref +months+ (1- month)) year) x y)
        (loop :for i :from 0 :below 7
              :do (%text m widget (aref +days+ i) (+ x (* i cell)) (+ y ch)))
        (loop :for day :from 1 :to days
              :for at := (+ first-day (1- day))
              :do (let ((col (mod at 7))
                        (row (floor at 7)))
                    (with-canvas (m)
                      (rgb (if (= day today)
                               (or (d:lookup (ui:styled widget) :bg) '(255 255 255))
                               (%ink widget (ui:styled widget)))))
                    (%text m widget (format nil "~2d" day)
                           (+ x (* col cell)) (+ y (* (+ row 2) ch)))))))))

(defmethod ui:paint ((widget ui:choice) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (when (ui:chosen widget)
      (with-canvas (m)
        (when (rgb (or (d:lookup (ui:styled widget) :bg)
                       (multiple-value-list (ui:ink :completion-selected))))
          (cl-cairo2:rectangle (float x 1d0) (float y 1d0)
                               (float width 1d0) (float height 1d0))
          (cl-cairo2:fill-path)))))
  (dolist (part (ui:parts widget)) (ui:paint part m)))

(defmethod ui:paint ((widget ui:scroll) (m canvas))
  (multiple-value-bind (x y width height) (%rect widget)
    (with-canvas (m)
      (cl-cairo2:save)
      (cl-cairo2:rectangle (float x 1d0) (float y 1d0)
                           (float width 1d0) (float height 1d0))
      (cl-cairo2:clip)))
  (dolist (part (ui:parts widget)) (ui:paint part m))
  (with-canvas (m) (cl-cairo2:restore)))
