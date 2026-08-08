(defpackage #:pine.edit.history
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell))
  (:export #:history #:remember #:undo #:redo #:undoable #:redoable #:forget
           #:*kept* #:state #:state-lines #:state-line #:state-col))

(in-package #:pine.edit.history)

(defvar *kept* 200)

(defstruct (state (:constructor state (lines line col)))
  lines line col)

(defclass history ()
  ((done   :initform (c:cell nil) :reader done)
   (undone :initform (c:cell nil) :reader undone)))

(defmethod print-object ((h history) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "~d/~d" (length (c:held (done h)))
            (length (c:held (undone h))))))

(defun history () (make-instance 'history))

(defun remember (h lines line col)
  (c:swap (done h)
          (lambda (all)
            (let ((next (cons (state lines line col) all)))
              (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
  (c:put (undone h) nil)
  h)

(defun undoable (h) (and (c:held (done h)) t))
(defun redoable (h) (and (c:held (undone h)) t))

(defun undo (h now)
  (let ((all (c:held (done h))))
    (when all
      (c:put (done h) (rest all))
      (c:swap (undone h) (lambda (u) (cons now u)))
      (first all))))

(defun redo (h now)
  (let ((all (c:held (undone h))))
    (when all
      (c:put (undone h) (rest all))
      (c:swap (done h) (lambda (u) (cons now u)))
      (first all))))

(defun forget (h)
  (c:put (done h) nil)
  (c:put (undone h) nil)
  h)
