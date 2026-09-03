(in-package #:pine/fs)

(defvar *listening* nil)
(defvar *forgetting* nil)
(defvar *moving* nil)

(defun %tell (tells said)
  "Tell one listener, and let it break on its own: a write is not wrong because
somebody listening to it is."
  (handler-case (funcall tells said)
    (error (c) (when *broke* (funcall *broke* c nil)) nil)))

(defun %went (path)
  "Say PATH and everything under it went, to whoever keeps a copy of the tree."
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

(defun %told (moved)
  (when moved
    (let ((moved (remove-duplicates (reverse moved))))
      (loop :for (nil . tells) :in *listening*
            :do (%tell tells moved)
            :finally (return moved)))))

(defun %announce (x)
  (if *moving*
      (push x (cdr *moving*))
      (%told (list x)))
  x)

(defmacro writing (&body body)
  "Batch every write inside BODY into one telling."
  (let ((mine (gensym "MOVING")) (outer (gensym "OUTER")))
    `(let* ((,outer *moving*)
            (,mine (or ,outer (cons :moving nil))))
       (unwind-protect
            (let ((*moving* ,mine)) ,@body)
         (unless ,outer (%told (cdr ,mine)))))))
