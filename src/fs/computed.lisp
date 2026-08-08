(defpackage #:pine.fs.computed
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:node #:pine.fs.node))
  (:export #:computed-node #:computed #:thunk #:reads #:recompute #:staleness))

(in-package #:pine.fs.computed)

(defvar +unread+ '#:unread)

(defclass computed-node (node:node)
  ((thunk  :initarg :thunk :accessor thunk)
   (cached :initform (c:cell nil) :reader cached)
   (reads  :initform (c:cell nil) :accessor reads)))

(defun computed (name thunk &rest initargs)
  (let ((n (apply #'make-instance 'computed-node :name name :thunk thunk
                  initargs)))
    (c:put (cached n) +unread+)
    n))

(defun recompute (n)
  (let ((reading (cons :reading nil)))
    (let* ((node:*reading* reading)
           (value (funcall (thunk n))))
      (c:put (reads n) (cdr reading))
      (dolist (on (cdr reading))
        (unless (eq on n) (node:depend n on)))
      (c:put (cached n) value)
      value)))

(defmethod node:contents ((n computed-node))
  (let ((v (c:held (cached n))))
    (if (eq v +unread+) (recompute n) v)))

(defmethod node:invalidate ((n computed-node))
  (unless (eq (c:held (cached n)) +unread+)
    (c:put (cached n) +unread+)
    (dolist (d (c:held (node:dependents n)))
      (node:invalidate d)))
  n)

(defmethod node:persistp ((n computed-node)) nil)

(defmethod (setf node:contents) (value (n computed-node))
  (setf (thunk n) (lambda () value))
  (c:put (cached n) +unread+)
  (node:invalidate n)
  value)

(defun staleness (n)
  (eq (c:held (cached n)) +unread+))
