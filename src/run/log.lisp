(defpackage #:pine.run.log
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell))
  (:export #:note #:said #:forget #:*kept* #:*to* #:last-said))

(in-package #:pine.run.log)

(defvar *kept* 500)
(defvar *to* nil)
(defvar *said* (c:cell nil))

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (c:swap *said*
            (lambda (all)
              (let ((next (cons line all)))
                (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    line))

(defun said () (c:held *said*))

(defun last-said () (first (c:held *said*)))

(defun forget () (c:put *said* nil))
