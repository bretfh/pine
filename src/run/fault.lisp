(defpackage #:pine.run.fault
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell))
  (:export #:fault #:faults #:report #:attempt #:with-debugger #:forget-faults
           #:condition-of #:label #:backtrace-of #:at-time #:*kept* #:*on-fault*))

(in-package #:pine.run.fault)

(defvar *kept* 50)
(defvar *faults* (c:cell nil))
(defvar *on-fault* nil)
(defvar *debugging* nil)

(defclass fault ()
  ((condition-of :initarg :condition :reader condition-of)
   (label        :initarg :label     :reader label     :initform nil)
   (backtrace-of :initarg :backtrace :reader backtrace-of :initform "")
   (at-time      :initform (get-universal-time) :reader at-time)))

(defmethod print-object ((f fault) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~@[~a: ~]~a" (label f) (condition-of f))))

(defun faults () (c:held *faults*))

(defun forget-faults () (c:put *faults* nil))

(defun %backtrace ()
  (handler-case
      (with-output-to-string (s) (sb-debug:print-backtrace :stream s :count 25))
    (error () "")))

(defun report (condition &optional label)
  (let ((f (make-instance 'fault :condition condition :label label
                                 :backtrace (%backtrace))))
    (c:swap *faults*
            (lambda (all)
              (let ((next (cons f all)))
                (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
    (when *on-fault* (ignore-errors (funcall *on-fault* f)))
    f))

(defun attempt (thunk &optional label)
  (handler-case (funcall thunk)
    (error (c) (report c label) nil)))

(defmacro with-debugger (&body body)
  `(let ((*debugging* t)) ,@body))
