(defpackage #:pine/kernel/tell
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export
   #:moved #:went #:listeners #:on-move #:on-go #:forget-all
   #:together #:heldp #:*broke*))
(in-package #:pine/kernel/tell)

(defvar *listening* (d:table)
  "Who to tell that places moved, by name.")

(defvar *going* (d:table)
  "Who to tell that a name went. A write and an erasure are two things that
happen to a place, so they are two lists and not one with a tag on it.")

(defvar *holding* nil
  "What has moved since the news was held back, or nothing where it is not being
held. Bound, never assigned, so a thread that holds the news holds only its own.")

(defvar *broke* nil
  "What to do about a listener that would not run. Filled in by whatever keeps
faults, because this layer loads before there is one.")

(defun %tell (tells said)
  "Tell one listener, and let it break on its own. A write is not wrong because
somebody listening to it is, and the listeners after it are still owed the news."
  (handler-case (funcall tells said)
    (error (c) (when *broke* (funcall *broke* c)) nil)))

(defun listeners () (d:all *listening*))

(defun on-move (key) (d:lookup (listeners) key))

(defun (setf on-move) (tells key)
  (if tells (d:keep! *listening* key tells) (d:drop! *listening* key))
  tells)

(defun on-go (key) (d:lookup (d:all *going*) key))

(defun (setf on-go) (tells key)
  (if tells (d:keep! *going* key tells) (d:drop! *going* key))
  tells)

(defun forget-all ()
  (d:clear! *listening*)
  (d:clear! *going*))

(defun heldp () (and *holding* t))

(defun %told (all)
  (when all
    (let ((all (remove-duplicates (reverse all))))
      (dolist (tells (d:vals (listeners)) all)
        (%tell tells all)))))

(defun moved (place)
  "Say PLACE moved. Held back where the news is being held, told at once where it
is not."
  (if *holding*
      (push place (cdr *holding*))
      (%told (list place)))
  place)

(defun went (name)
  "Say NAME and everything under it went. A value goes with the place that held
it, so there is nothing to take out here; what is left is telling whoever keeps a
copy of the tree, which is why nothing lower down has to know who that is."
  (dolist (tells (d:vals (d:all *going*)) name)
    (%tell tells name)))

(defmacro together (&body body)
  "Hold the news until BODY is done, then tell it once.

Two places written one after the other are two pieces of news, and anything
worked out from both of them is worked out once from the first write and once
from the second -- and in between, from a pair that stood together for no longer
than it took to write them. Held, there is one telling, so there is no such pair
to be worked out from.

Re-entrant: the innermost is the one that tells, so a command that groups its
writes still counts as one piece of news inside a config that grouped the whole
of itself."
  (let ((mine (gensym "HOLDING")) (outer (gensym "OUTER")))
    `(let* ((,outer *holding*)
            (,mine (or ,outer (cons :holding nil))))
       (unwind-protect
            (let ((*holding* ,mine)) ,@body)
         (unless ,outer (%told (cdr ,mine)))))))
