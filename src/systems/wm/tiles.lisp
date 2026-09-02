(defpackage #:pine/wm/tiles
  (:use #:pine/user)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export
   #:layout #:tall #:wide #:full #:stacked
   #:arrange #:named #:layouts
   #:area #:placed #:id-of #:x-of #:y-of #:wide-of #:tall-of
   #:clip-of #:stack-of))
(in-package #:pine/wm/tiles)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defclass layout ()
  ((share :initarg :share :accessor share :initform 1/2)
   (gaps  :initarg :gaps  :accessor gaps  :initform 0))
  (:documentation "How windows are laid out on an output. A class, so a config
writes one ARRANGE method and pine has a new layout: nothing else knows the names
of these."))

(defclass tall (layout) ()
  (:documentation "One window down the left, the rest stacked down the right."))

(defclass wide (layout) ()
  (:documentation "One window across the top, the rest side by side under it."))

(defclass full (layout) ()
  (:documentation "One window at a time, filling the output."))

(defclass stacked (layout) ()
  (:documentation "Every window filling the output, in the order they arrived."))

(defun layouts ()
  "Every layout class there is, so a config's own is offered like the rest."
  (labels ((under (class)
             (c2mop:ensure-finalized class)
             (cons class (mapcan #'under (c2mop:class-direct-subclasses class)))))
    (remove (find-class 'layout) (under (find-class 'layout)))))

(defun named (name)
  (let ((found (find (string-downcase (princ-to-string name)) (layouts)
                     :key (lambda (c) (string-downcase (symbol-name (class-name c))))
                     :test #'equal)))
    (when found (make-instance (class-name found)))))

(defclass area ()
  ((x    :initarg :x    :reader x-of    :initform 0)
   (y    :initarg :y    :reader y-of    :initform 0)
   (wide :initarg :wide :reader wide-of :initform 0)
   (tall :initarg :tall :reader tall-of :initform 0))
  (:documentation "The room a layout is given: where it starts and how big it is.
What the compositor left after the bars took their strip."))

(defclass placed ()
  ((id    :initarg :id    :reader id-of)
   (x     :initarg :x     :reader x-of    :initform 0)
   (y     :initarg :y     :reader y-of    :initform 0)
   (wide  :initarg :wide  :reader wide-of :initform 0)
   (tall  :initarg :tall  :reader tall-of :initform 0)
   (clip  :initarg :clip  :reader clip-of  :initform nil)
   (stack :initarg :stack :reader stack-of :initform nil))
  (:documentation "One window, placed. CLIP is (X Y WIDE TALL) of it to show, and
STACK is :TOP, :BOTTOM or the id of a window to sit above.

Those two are why this is a class. It was a list of five, and what shows a window
has always read a list of five and two keywords after it -- so a layout could not
say either, and the clipping and stacking a compositor was already told how to do
could never be asked for. Nothing said so, because every shape of that list is a
list."))

(defmethod print-object ((p placed) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~d,~d ~dx~d" (id-of p) (x-of p) (y-of p)
            (wide-of p) (tall-of p))))

(defun area (&key (x 0) (y 0) (wide 0) (tall 0))
  (make-instance 'area :x x :y y :wide wide :tall tall))

(defun placed (id &key (x 0) (y 0) (wide 0) (tall 0) clip stack)
  (make-instance 'placed :id id :x x :y y :wide wide :tall tall
                         :clip clip :stack stack))

(defun %at (l id x y wide tall)
  "One window placed, with the layout's gaps taken out of it."
  (let ((g (gaps l)))
    (placed id :x (+ x g) :y (+ y g)
               :wide (max 1 (- wide (* 2 g)))
               :tall (max 1 (- tall (* 2 g))))))

(defgeneric arrange (layout windows area)
  (:documentation "Where each window goes: a PLACED per window, in the order they
were given. AREA is the room there is.")
  (:method ((l layout) windows (a area))
    (loop :for id :in windows
          :collect (%at l id (x-of a) (y-of a) (wide-of a) (tall-of a)))))

(defmethod arrange ((l stacked) windows (a area))
  (loop :for id :in windows
        :collect (%at l id (x-of a) (y-of a) (wide-of a) (tall-of a))))

(defmethod arrange ((l full) windows (a area))
  (loop :for id :in windows
        :for first := t :then nil
        :when first
          :collect (%at l id (x-of a) (y-of a) (wide-of a) (tall-of a))))

(defmethod arrange ((l tall) windows (a area))
  (let ((n (length windows))
        (x (x-of a)) (y (y-of a)) (wide (wide-of a)) (tall (tall-of a)))
    (cond ((zerop n) nil)
          ((= n 1) (list (%at l (first windows) x y wide tall)))
          (t (let* ((main (max 1 (round (* wide (share l)))))
                    (rest (max 1 (- wide main)))
                    (each (max 1 (floor tall (1- n)))))
               (cons (%at l (first windows) x y main tall)
                     (loop :for id :in (cdr windows)
                           :for i :from 0
                           :collect (%at l id (+ x main) (+ y (* i each)) rest
                                         (if (= i (- n 2))
                                             (- tall (* i each))
                                             each)))))))))

(defmethod arrange ((l wide) windows (a area))
  (let ((n (length windows))
        (x (x-of a)) (y (y-of a)) (wide (wide-of a)) (tall (tall-of a)))
    (cond ((zerop n) nil)
          ((= n 1) (list (%at l (first windows) x y wide tall)))
          (t (let* ((main (max 1 (round (* tall (share l)))))
                    (rest (max 1 (- tall main)))
                    (each (max 1 (floor wide (1- n)))))
               (cons (%at l (first windows) x y wide main)
                     (loop :for id :in (cdr windows)
                           :for i :from 0
                           :collect (%at l id (+ x (* i each)) (+ y main)
                                         (if (= i (- n 2))
                                             (- wide (* i each))
                                             each)
                                         rest))))))))

(defclass tiles (system)
  ((layout-of :initarg :layout :accessor layout-of
              :initform (make-instance 'tall))
   (watching  :initform nil :accessor watching))
  (:documentation "One of the window managers pine ships: it reads what the
compositor handed over and writes where each window goes.

Nothing in the substrate knows this is here. Dropping it takes /wm/layout and its
commands away, and pine places nothing again."))


(defun %system ()
  "The system itself: a system is a node, and /system/tiles is where it stands."
  (at /system/tiles))

(defun %layout ()
  "Which layout is in force, by name: pine write /wm/layout wide."
  (make-instance 'place :name "layout"
              :reads (lambda ()
                       (let ((s (%system)))
                         (when s
                           (string-downcase
                            (class-name (class-of (layout-of s)))))))
              :writes (lambda (value)
                        (let ((s (%system)) (l (named value)))
                          (when (and s l)
                            (setf (layout-of s) l)
                            (%placed s))))
              :describes "which layout is in force"))

(defun %area (s)
  (declare (ignore s))
  (let ((c (at /wm)))
    (destructuring-bind (&optional (x 0) (y 0) (wide 1920) (tall 1080))
        (or (getf (first (outputs c)) :area) (list))
      (area :x x :y y :wide wide :tall tall))))

(defun %ids (s)
  (declare (ignore s))
  (ids (at /wm)))

(defun %plainly (p)
  "One PLACED as plain data. Whatever shows a window may be another pine, so what
crosses is a list and a couple of keywords -- said here, at the edge, and not by
ARRANGE, which answers to whoever wrote the layout."
  (append (list (id-of p) (x-of p) (y-of p) (wide-of p) (tall-of p))
          (when (clip-of p) (list :clip (clip-of p)))
          (when (stack-of p) (list :stack (stack-of p)))))

(defun %placed (s)
  "Work out where the windows go and say so. Writing /wm/placement is the whole
of being the window manager here."
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
  (let ((c (at /wm)))
    (unless c (error "no /wm: use the wm system before this one."))
    (node:attach (%layout) c)
    (let ((said (node:resolve c "said")))
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

