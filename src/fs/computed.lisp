(defpackage #:pine.fs.computed
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node))
  (:export #:computed-node #:computed #:thunk #:reads #:recompute #:staleness))

(in-package #:pine.fs.computed)

(defparameter +unread+ '#:unread)

(defclass computed-node (node:node)
  ((thunk  :initarg :thunk :accessor thunk)
   (cached :initform (d:box +unread+) :reader cached)
   (reads  :initform (d:box nil) :accessor reads)))

(defun computed (name thunk &rest initargs)
  (apply #'make-instance 'computed-node :name name :thunk thunk initargs))

(defun recompute (n)
  (let ((reading (cons :reading nil)))
    (let* ((node:*reading* reading)
           (value (funcall (thunk n))))
      (d:put! (reads n) (cdr reading))
      (dolist (on (cdr reading))
        (unless (eq on n) (node:depend n on)))
      (d:put! (cached n) value)
      value)))

(defmethod node:contents ((n computed-node))
  (let ((v (d:held (cached n))))
    (if (eq v +unread+) (recompute n) v)))

(defmethod node:invalidate ((n computed-node))
  (unless (eq (d:held (cached n)) +unread+)
    (d:put! (cached n) +unread+)
    (d:do-each (d (d:held (node:dependents n)))
      (node:invalidate d)))
  n)

(defmethod node:persistp ((n computed-node)) nil)

(defmethod (setf node:contents) (value (n computed-node))
  (setf (thunk n) (lambda () value))
  (d:put! (cached n) +unread+)
  (node:invalidate n)
  value)

(defun staleness (n)
  (eq (d:held (cached n)) +unread+))
