(defpackage #:pine/fs/log
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:export
   #:note #:*to* #:last-said))
(in-package #:pine/fs/log)

(defvar *lines-kept* 500)
(defvar *to* nil)
(defvar *said* nil)
(defvar *node* nil)

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (sb-ext:atomic-update *said* (lambda (old) (d:capped old line *lines-kept*)))
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    (when *node* (fs:touch *node*))
    line))

(defun said () *said*)

(defun last-said () (first *said*))

(defun forget () (setf *said* nil))

(defclass saying (fs:derived) ())

(defmethod fs:works ((n saying)) (said))

(defmethod fs:takes ((n saying) value) (unless value (forget)))

(fs:mount (lambda () (setf *node* (make-instance 'saying :describes "what pine said")))
          "/log")
