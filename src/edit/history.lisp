(defpackage #:pine.edit.history
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) )
  (:export #:history #:remember #:undo #:redo #:undoable #:redoable #:forget
           #:*kept* #:state #:state-lines #:state-line #:state-col))

(in-package #:pine.edit.history)

(defvar *kept* 200)

(defstruct (state (:constructor state (lines line col)))
  lines line col)

(defclass history ()
  ((done   :initform (d:box nil) :reader done)
   (undone :initform (d:box nil) :reader undone)))

(defmethod print-object ((h history) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "~d/~d" (length (d:held (done h)))
            (length (d:held (undone h))))))

(defun history () (make-instance 'history))

(defun remember (h lines line col)
  (d:swap! (done h)
          (lambda (all)
            (let ((next (cons (state lines line col) all)))
              (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
  (d:put! (undone h) nil)
  h)

(defun undoable (h) (and (d:held (done h)) t))
(defun redoable (h) (and (d:held (undone h)) t))

(defun undo (h now)
  (let ((all (d:held (done h))))
    (when all
      (d:put! (done h) (rest all))
      (d:swap! (undone h) (lambda (u) (cons now u)))
      (first all))))

(defun redo (h now)
  (let ((all (d:held (undone h))))
    (when all
      (d:put! (undone h) (rest all))
      (d:swap! (done h) (lambda (u) (cons now u)))
      (first all))))

(defun forget (h)
  (d:put! (done h) nil)
  (d:put! (undone h) nil)
  h)
