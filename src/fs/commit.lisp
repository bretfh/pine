(defpackage #:pine/fs/commit
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export
   #:forget #:writing #:announce #:on-commit #:on-forget
   #:forget-listeners #:*broke*))
(in-package #:pine/fs/commit)

(defvar *listening* (d:table))
(defvar *forgetting* (d:table)
  "Who to tell that a path went, by name. A write and an erasure are two things
that happen to a place, so they are two lists and not one with a tag on it.")
(defvar *moving* nil)
(defvar *broke* nil
  "What to do about a listener that would not run. Filled in by whatever keeps
faults, because this layer loads before there is one.")

(defun %tell (tells said)
  "Tell one listener, and let it break on its own. A write is not wrong because
somebody listening to it is, and the listeners after it are still owed the news."
  (handler-case (funcall tells said)
    (error (c) (when *broke* (funcall *broke* c)) nil)))

(defun forget (place)
  "Say PLACE and everything under it went. A value goes with the node that held it,
so there is nothing to take out here; what is left is telling whoever keeps a copy
of the tree, which is why nothing lower down has to know who that is."
  (dolist (tells (d:vals (d:all *forgetting*)) place)
    (%tell tells place)))

(defun listeners () (d:all *listening*))

(defun on-commit (key) (d:lookup (listeners) key))

(defun (setf on-commit) (tells key)
  (if tells (d:keep! *listening* key tells) (d:drop! *listening* key))
  tells)

(defun on-forget (key) (d:lookup (d:all *forgetting*) key))

(defun (setf on-forget) (tells key)
  (if tells (d:keep! *forgetting* key tells) (d:drop! *forgetting* key))
  tells)

(defun forget-listeners ()
  (d:clear! *listening*)
  (d:clear! *forgetting*))

(defun %told (moved)
  (when moved
    (let ((moved (remove-duplicates (reverse moved))))
      (loop :for tells :in (d:vals (listeners))
            :do (%tell tells moved)
            :finally (return moved)))))

(defun announce (n)
  (if *moving*
      (push n (cdr *moving*))
      (%told (list n)))
  n)

(defmacro writing (&body body)
  (let ((mine (gensym "MOVING")) (outer (gensym "OUTER")))
    `(let* ((,outer *moving*)
            (,mine (or ,outer (cons :moving nil))))
       (unwind-protect
            (let ((*moving* ,mine)) ,@body)
         (unless ,outer (%told (cdr ,mine)))))))
