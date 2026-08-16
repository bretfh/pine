(defpackage #:pine/run/log
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export #:note #:said #:forget #:*kept* #:*to* #:*on-note* #:last-said))
(in-package #:pine/run/log)

(defvar *kept* 500)
(defvar *to* nil)
(defvar *said* (d:box nil))
(defvar *on-note* nil
  "Told that something was said, so a place holding it can say it moved.")

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (d:swap! *said*
            (lambda (all)
              (let ((next (cons line all)))
                (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    (when *on-note* (funcall *on-note* line))
    line))

(defun said () (d:held *said*))

(defun last-said () (first (d:held *said*)))

(defun forget () (d:put! *said* nil))
