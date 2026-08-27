(defpackage #:pine/run/log
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node))
  (:export #:note #:said #:forget #:attach
           #:*kept* #:*to* #:*on-note* #:last-said))
(in-package #:pine/run/log)

(defvar *kept* 500)
(defvar *to* nil)
(defvar *said* nil)
(defvar *on-note* nil
  "Told that something was said, so a place holding it can say it moved.")

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (d:swap *said* #'d:capped line *kept*)
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    (when *on-note* (funcall *on-note* line))
    line))

(defun said () *said*)

(defun last-said () (first *said*))

(defun forget () (setf *said* nil))

(defun attach (root)
  (let ((n (node:attach (node:place "log"
                                    :reads #'said
                                    :writes (lambda (value) (unless value (forget)))
                                    :describes "what pine said")
                        root)))
    (setf *on-note* (lambda (line) (declare (ignore line)) (node:stir n)))
    n))
