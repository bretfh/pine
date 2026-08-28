(defpackage #:pine/fs/commit
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export #:held #:held-at #:change #:forget #:clear
           #:writing #:announce #:on-commit #:on-forget #:listeners
           #:forget-listeners))
(in-package #:pine/fs/commit)

(defvar *now* (d:no-map))
(defvar *listening* (d:table))
(defvar *forgetting* (d:table)
  "Who to tell that a path went, by name. A write and an erasure are two things
that happen to a place, so they are two lists and not one with a tag on it.")
(defvar *moving* nil)

(defun held () *now*)

(defun held-at (place &optional default)
  "What stands at PLACE now. HELD is the whole of it; this is one place in it."
  (d:lookup (held) place default))

(defun change (changes &key when)
  (labels ((refused (had)
             (d:do-map (place value when nil)
               (unless (d:same value (d:lookup had place))
                 (return-from refused place))))
           (moving (had)
             (let ((out (d:no-map)))
               (d:do-map (place value changes out)
                 (unless (d:same value (d:lookup had place))
                   (setf out (d:with out place value))))))
           (next (had)
             (let ((out had))
               (d:do-map (place value changes out)
                 (setf out (d:with out place value))))))
    (loop :for had := (held)
          :when (and when (refused had)) :return nil
          :do (let ((moved (moving had)))
                (when (d:emptyp moved) (return moved))
                (when (d:cas *now* had (next had))
                  (return moved))))))

(defun forget (place)
  "Take PLACE and everything under it out of what stands, and say it went. Whatever
keeps a copy of the tree hears it here, which is why nothing lower down has to be
told who that is."
  (let ((under (concatenate 'string place "/")))
    (flet ((underp (each)
             (and (> (length each) (length under))
                  (string= under each :end2 (length under)))))
      (d:swap *now*
               (lambda (had)
                 (let ((out had))
                   (d:do-map (each value had out)
                     (when (or (equal each place) (underp each))
                       (setf out (d:without out each))))))))
    (dolist (tells (d:vals (d:all *forgetting*)) place)
      (funcall tells place))))

(defun clear ()
  (setf *now* (d:no-map))
  t)

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
            :do (funcall tells moved)
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
