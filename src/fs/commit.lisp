(defpackage #:pine/fs/commit
  (:use #:cl)
  (:shadow #:at)
  (:local-nicknames (#:d #:pine/data))
  (:export #:held #:at #:change #:forget #:clear
           #:roots #:root-at #:was #:*kept*
           #:writing #:announce #:on-commit #:listeners #:forget-listeners))
(in-package #:pine/fs/commit)

(defparameter *kept* 200)

(defvar *now* (d:no-map))
(defvar *was* nil)
(defvar *listening* (d:table))
(defvar *moving* nil)

(defun held () *now*)

(defun at (place &optional default) (d:at (held) place default))

(defun roots () *was*)

(defun root-at (n) (nth n (roots)))

(defun was (place n)
  (let ((it (root-at n))) (and it (d:at it place))))

(defun change (changes &key when)
  (labels ((refused (had)
             (d:do-map (place value when nil)
               (unless (d:same value (d:at had place))
                 (return-from refused place))))
           (moving (had)
             (let ((out (d:no-map)))
               (d:do-map (place value changes out)
                 (unless (d:same value (d:at had place))
                   (setf out (d:with out place value))))))
           (next (had)
             (let ((out had))
               (d:do-map (place value changes out)
                 (setf out (d:with out place value)))))
           (superseded (had) (d:swap *was* #'d:capped had *kept*)))
    (loop :for had := (held)
          :when (and when (refused had)) :return nil
          :do (let ((moved (moving had)))
                (when (d:emptyp moved) (return moved))
                (when (d:cas *now* had (next had))
                  (superseded had)
                  (return moved))))))

(defun forget (place)
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
    place))

(defun clear ()
  (setf *now* (d:no-map))
  (setf *was* nil)
  t)

(defun listeners () (d:all *listening*))

(defun on-commit (key) (d:at (listeners) key))

(defun (setf on-commit) (tells key)
  (if tells (d:keep! *listening* key tells) (d:drop! *listening* key))
  tells)

(defun forget-listeners () (d:clear! *listening*))

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
