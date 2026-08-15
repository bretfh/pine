(defpackage #:pine/run/log
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export #:note #:said #:forget #:*kept* #:*to* #:last-said))
(in-package #:pine/run/log)

(defvar *kept* 500)
(defvar *to* nil)
(defvar *said* (d:box nil))

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (d:swap! *said*
            (lambda (all)
              (let ((next (cons line all)))
                (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    line))

(defun said () (d:held *said*))

(defun last-said () (first (d:held *said*)))

(defun forget () (d:put! *said* nil))
