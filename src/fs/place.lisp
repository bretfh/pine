(in-package #:pine/fs/node)

(defclass place (live)
  ((reads     :initarg :reads     :accessor reads     :initform nil)
   (writes    :initarg :writes    :accessor writes    :initform nil)
   (names     :initarg :names     :reader  names-of   :initform nil)
   (each      :initarg :each      :reader  each-of    :initform nil)
   (listing   :initarg :nodes     :reader  nodes-of   :initform nil)
   (announces :initarg :announces :reader  announces  :initform nil)
   (refreshes :initarg :refreshes :reader  refreshes  :initform nil))
  (:documentation "Somewhere the world answers, through the closures it was given.

READS answers what it holds and WRITES says what writing it means. NAMES says what
is under it and EACH makes one, and what EACH made is kept, so the same name is
the same node every time; with NODES the children are nodes something else already
keeps.

A device is one of these and each of its readings is a closure pair, which is why
a device is a table of rows rather than a class per reading."))

(defun answers (name &rest initargs &key reads writes (class 'place)
                &allow-other-keys)
  "A leaf the world answers. Nothing is remembered; DERIVE is the half that does.

CLASS where the place has something of its own to answer -- a place in another pine
says how it is watched -- and PLACE where it has not."
  (declare (ignore reads writes))
  (apply #'make-instance class :name (princ-to-string name)
         (alexandria:remove-from-plist initargs :class)))

(defun lists (name &rest initargs &key names each nodes (class 'place)
              &allow-other-keys)
  "A branch the world fills. NAMES with EACH, or NODES something else keeps."
  (declare (ignore names each nodes))
  (apply #'make-instance class :name (princ-to-string name)
         (alexandria:remove-from-plist initargs :class)))


(defun %kid (n name)
  (child n name
         (lambda ()
           (let ((it (funcall (each-of n) name)))
             (when it (setf (parent it) n))
             it))))

(defun %listed (n) (mapcar #'princ-to-string (funcall (names-of n))))

(defmethod nodes ((n place))
  (cond ((nodes-of n) (funcall (nodes-of n)))
        ((names-of n)
         (remove nil (mapcar (lambda (each) (%kid n each)) (%listed n))))
        (t (call-next-method))))

(defmethod resolve ((n place) name)
  (let ((name (princ-to-string name)))
    (cond ((nodes-of n)
           (find name (funcall (nodes-of n)) :key #'name :test #'equal))
          ((each-of n) (%kid n name))
          (t (call-next-method)))))

(defmethod make-child ((n place) name)
  "One worked out has none to make. Attached here it would sit where NODES and
RESOLVE never look, so the write would be taken and not be there to read."
  (if (or (each-of n) (nodes-of n))
      (error "~a works out what is under it; ~a is not a place to write."
             (full-name n) name)
      (call-next-method)))

(defmethod contents ((n place))
  (cond ((reads n) (funcall (reads n)))
        ((names-of n) (%listed n))
        ((nodes-of n) (mapcar #'name (funcall (nodes-of n))))
        (t nil)))

(defmethod (setf contents) (v (n place))
  (unless (writes n)
    (error "~a holds nothing that can be written." (full-name n)))
  (funcall (writes n) v)
  v)
