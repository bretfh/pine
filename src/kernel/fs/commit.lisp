(in-package #:pine/fs)

(defvar *listening* nil)
(defvar *forgetting* nil)
(defvar *moving* nil)

(defun %tell (tells said)
  (handler-case (funcall tells said)
    (error (c) (when *broke* (funcall *broke* c nil)) nil)))

(defun %went (path)
  (dolist (each *forgetting* path)
    (%tell (cdr each) path)))

(defun on-commit (key) (cdr (assoc key *listening*)))

(defun (setf on-commit) (tells key)
  (setf *listening* (remove key *listening* :key #'car))
  (when tells (push (cons key tells) *listening*))
  tells)

(defun on-forget (key) (cdr (assoc key *forgetting*)))

(defun (setf on-forget) (tells key)
  (setf *forgetting* (remove key *forgetting* :key #'car))
  (when tells (push (cons key tells) *forgetting*))
  tells)

(defun forget-listeners ()
  (setf *listening* nil *forgetting* nil))

(defun %told (touch)
  (when touch
    (let ((touch (remove-duplicates (reverse touch))))
      (loop :for (nil . tells) :in *listening*
            :do (%tell tells touch)
            :finally (return touch)))))

(defun %announce (x)
  (if *moving*
      (push x (cdr *moving*))
      (%told (list x)))
  x)

(defmacro writing (&body body)
  (let ((mine (gensym "MOVING")) (outer (gensym "OUTER")) (batch (gensym "BATCH")))
    `(let* ((,outer *moving*)
            (,mine (or ,outer (cons :moving nil)))
            (,batch (if ,outer *store-batch* (cons :keep nil))))
       (unwind-protect
            (let ((*moving* ,mine) (*store-batch* ,batch)) ,@body)
         (unless ,outer
           (%told (cdr ,mine))
           (store-flush (cdr ,batch)))))))
