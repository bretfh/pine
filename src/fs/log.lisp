(defpackage #:pine/fs/log
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node))
  (:export
   #:note #:*to* #:last-said))
(in-package #:pine/fs/log)

(defvar *kept* 500)
(defvar *to* nil)
(defvar *said* nil)
(defvar *node* nil
  "Where the log stands, once it stands anywhere. Adding a line is writing that
place, so whatever is showing the log is worked out again -- which the log has to
say itself, because nothing else can see *SAID* move.")

(defun note (format &rest arguments)
  (let ((line (apply #'format nil format arguments)))
    (d:swap *said* #'d:capped line *kept*)
    (when *to*
      (format *to* "~&~a~%" line)
      (force-output *to*))
    (when *node* (node:moved *node*))
    line))

(defun said () *said*)

(defun last-said () (first *said*))

(defun forget () (setf *said* nil))

(defun %attach (root)
  (setf *node* (node:attach (node:answers "log"
                                        :reads #'said
                                        :writes (lambda (value)
                                                  (unless value (forget)))
                                        :describes "what pine said")
                            root)))


(pine/fs/tree:builder #'%attach)
