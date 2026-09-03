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

(defun %attach (root)
  (setf *node* (fs:attach (make-instance 'fs:derived :name "log"
                                         :reads #'said
                                         :writes (lambda (value)
                                                   (unless value (forget)))
                                         :describes "what pine said")
                          root)))

(fs:builder #'%attach)
