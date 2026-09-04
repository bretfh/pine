(defpackage #:pine/fs/log
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:export
   #:note #:*to* #:last-said))
(in-package #:pine/fs/log)

(defvar *kept* 500)
(defvar *to* nil)
(defvar *said* nil)
(defvar *node* nil)

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (d:swap *said* #'d:capped line *kept*)
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    (when *node* (fs:moved *node*))
    line))

(defun said () *said*)

(defun last-said () (first *said*))

(defun forget () (setf *said* nil))

(defclass saying (fs:derived) ()
  (:documentation "/log: what pine said. Writing nothing here forgets it."))

(defmethod fs:works ((n saying)) (said))

(defmethod fs:takes ((n saying) value) (unless value (forget)))

(fs:mount (lambda () (setf *node* (make-instance 'saying :describes "what pine said")))
          "/log")
