(in-package #:pine/wm)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defclass layout ()
  ((share :initarg :share :accessor share :initform 1/2)
   (gaps  :initarg :gaps  :accessor gaps  :initform 0)))

(defclass tall (layout) ())

(defclass wide (layout) ())

(defclass full (layout) ())

(defclass stacked (layout) ())

(defun layouts ()
  (labels ((under (class)
             (c2mop:ensure-finalized class)
             (cons class (mapcan #'under (c2mop:class-direct-subclasses class)))))
    (remove (find-class 'layout) (under (find-class 'layout)))))

(defun layout (name)
  (let ((found (find (string-downcase (princ-to-string name)) (layouts)
                     :key (lambda (c) (string-downcase (symbol-name (class-name c))))
                     :test #'equal)))
    (when found (make-instance (class-name found)))))

(defclass area ()
  ((x    :initarg :x    :reader x-of    :initform 0)
   (y    :initarg :y    :reader y-of    :initform 0)
   (width :initarg :width :reader width-of :initform 0)
   (height :initarg :height :reader height-of :initform 0)))

(defclass placed ()
  ((id    :initarg :id    :reader id-of)
   (x     :initarg :x     :reader x-of    :initform 0)
   (y     :initarg :y     :reader y-of    :initform 0)
   (width  :initarg :width  :reader width-of :initform 0)
   (height  :initarg :height  :reader height-of :initform 0)
   (clip  :initarg :clip  :reader clip-of  :initform nil)
   (stack :initarg :stack :reader stack-of :initform nil)))

(defmethod print-object ((p placed) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~d,~d ~dx~d" (id-of p) (x-of p) (y-of p)
            (width-of p) (height-of p))))

(defun area (&key (x 0) (y 0) (width 0) (height 0))
  (make-instance 'area :x x :y y :width width :height height))

(defun placed (id &key (x 0) (y 0) (width 0) (height 0) clip stack)
  (make-instance 'placed :id id :x x :y y :width width :height height
                         :clip clip :stack stack))

(defun %at (l id x y width height)
  (let ((g (gaps l)))
    (placed id :x (+ x g) :y (+ y g)
               :width (max 1 (- width (* 2 g)))
               :height (max 1 (- height (* 2 g))))))

(defgeneric arrange (layout windows area)
  (:method ((l layout) windows (a area))
    (loop :for id :in windows
          :collect (%at l id (x-of a) (y-of a) (width-of a) (height-of a)))))

(defmethod arrange ((l stacked) windows (a area))
  (loop :for id :in windows
        :collect (%at l id (x-of a) (y-of a) (width-of a) (height-of a))))

(defmethod arrange ((l full) windows (a area))
  (loop :for id :in windows
        :for first := t :then nil
        :when first
          :collect (%at l id (x-of a) (y-of a) (width-of a) (height-of a))))

(defmethod arrange ((l tall) windows (a area))
  (let ((n (length windows))
        (x (x-of a)) (y (y-of a)) (width (width-of a)) (height (height-of a)))
    (cond ((zerop n) nil)
          ((= n 1) (list (%at l (first windows) x y width height)))
          (t (let* ((main (max 1 (round (* width (share l)))))
                    (rest (max 1 (- width main)))
                    (each (max 1 (floor height (1- n)))))
               (cons (%at l (first windows) x y main height)
                     (loop :for id :in (cdr windows)
                           :for i :from 0
                           :collect (%at l id (+ x main) (+ y (* i each)) rest
                                         (if (= i (- n 2))
                                             (- height (* i each))
                                             each)))))))))

(defmethod arrange ((l wide) windows (a area))
  (let ((n (length windows))
        (x (x-of a)) (y (y-of a)) (width (width-of a)) (height (height-of a)))
    (cond ((zerop n) nil)
          ((= n 1) (list (%at l (first windows) x y width height)))
          (t (let* ((main (max 1 (round (* height (share l)))))
                    (rest (max 1 (- height main)))
                    (each (max 1 (floor width (1- n)))))
               (cons (%at l (first windows) x y width main)
                     (loop :for id :in (cdr windows)
                           :for i :from 0
                           :collect (%at l id (+ x (* i each)) (+ y main)
                                         (if (= i (- n 2))
                                             (- width (* i each))
                                             each)
                                         rest))))))))

(defclass tiles (module)
  ((layout-of :initarg :layout :accessor layout-of
              :initform (make-instance 'tall))
   (watching  :initform nil :accessor watching)))

(defun %system ()
  (at /proc/tiles))

(defclass chosen (derived) ())

(defmethod fs:volatile-p ((n chosen) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n chosen))
  (let ((s (%system)))
    (when s (string-downcase (class-name (class-of (layout-of s)))))))

(defmethod fs:takes ((n chosen) value)
  (let ((s (%system)) (l (layout value)))
    (when (and s l)
      (setf (layout-of s) l)
      (%placed s))))

(defun %layout ()
  (make-instance 'chosen :name "layout" :describes "which layout is in force"))

(defun %area (s)
  (declare (ignore s))
  (let ((c (current)))
    (destructuring-bind (&optional (x 0) (y 0) (width 1920) (height 1080))
        (or (getf (first (outputs c)) :area) (list))
      (area :x x :y y :width width :height height))))

(defun %ids (s)
  (declare (ignore s))
  (ids (current)))

(defun %plainly (p)
  (append (list (id-of p) (x-of p) (y-of p) (width-of p) (height-of p))
          (when (clip-of p) (list :clip (clip-of p)))
          (when (stack-of p) (list :stack (stack-of p)))))

(defun %placed (s)
  (let ((n (at /wm/placement)))
    (when n
      (setf (contents n)
            (mapcar #'%plainly (arrange (layout-of s) (%ids s) (%area s)))))))

(defcommand "wm-layout" (&optional name)
    (:describes "how windows are laid out")
  (let ((n (at /wm/layout)))
    (when (and n name) (setf (contents n) (princ-to-string name)))
    (and n (contents n))))

(defcommand "wm-layouts" () (:describes "every layout there is")
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
          (layouts)))

(defmethod start ((s tiles))
  (let ((c (current)))
    (unless c (error "no compositor: use the wm system before this one."))
    (mount (%layout) "/wm/layout")
    (let ((said (fs:at "/wm/said")))
      (when said
        (setf (watching s)
              (list (watch said
                           (lambda (of value)
                             (declare (ignore of value))
                             (attempt (lambda () (%placed s)) "tiles"))
                           :tells-when :always :poll nil :name "tiles<-wm/said")))))
    (%placed s))
  s)

(defmethod stop ((s tiles))
  (dolist (w (watching s)) (attempt (lambda () (unwatch w)) "letting a watch go"))
  (setf (watching s) nil)
  (erase "/wm/layout")
  s)

