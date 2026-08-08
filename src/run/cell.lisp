(defpackage #:pine.run.cell
  (:use #:cl)
  (:export #:cell #:cellp #:held #:swap #:cas #:put))

(in-package #:pine.run.cell)

(defclass cell ()
  ((value :initarg :value :initform nil :accessor %value)))

(defmethod print-object ((c cell) stream)
  (print-unreadable-object (c stream :type t)
    (prin1 (%value c) stream)))

(defun cell (&optional value)
  (make-instance 'cell :value value))

(defun cellp (x) (typep x 'cell))

(defgeneric held (cell)
  (:method ((c cell)) (%value c)))

(defgeneric cas (cell old new)
  (:method ((c cell) old new)
    (eq old (sb-ext:compare-and-swap (slot-value c 'value) old new))))

(defgeneric swap (cell function &rest arguments)
  (:method ((c cell) function &rest arguments)
    (loop :for old := (%value c)
          :for new := (apply function old arguments)
          :when (cas c old new)
            :do (return new))))

(defgeneric put (cell value)
  (:method ((c cell) value)
    (swap c (lambda (old) (declare (ignore old)) value))))
