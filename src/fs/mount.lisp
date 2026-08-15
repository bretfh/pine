(defpackage #:pine/fs/mount
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node))
  (:export #:file-node #:directory-node #:mount #:unmount #:truename-of
           #:mounted #:refresh #:place))
(in-package #:pine/fs/mount)

(defvar *mounts* (d:table))

(defclass file-node (node:node)
  ((truename-of :initarg :truename :reader truename-of)))

(defclass directory-node (node:node)
  ((truename-of :initarg :truename :reader truename-of)))

(defmethod node:persistp ((n file-node)) nil)
(defmethod node:persistp ((n directory-node)) nil)
(defmethod node:livep ((n file-node)) t)
(defmethod node:livep ((n directory-node)) t)

(defmethod node:contents ((n file-node))
  (when (probe-file (truename-of n))
    (with-open-file (in (truename-of n) :external-format :utf-8)
      (let ((text (make-string (file-length in))))
        (subseq text 0 (read-sequence text in))))))

(defmethod (setf node:contents) (value (n file-node))
  (with-open-file (out (truename-of n) :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
    (write-string (princ-to-string value) out))
  value)

(defmethod node:leafp ((n file-node)) t)

(defun %entries (where)
  (append (directory (merge-pathnames "*.*" where))
          (directory (merge-pathnames "*/" where))))

(defun %named (path)
  (if (pathname-name path)
      (file-namestring path)
      (car (last (pathname-directory path)))))

(defun %branchp (name)
  (let ((n (length name)))
    (and (plusp n) (char= #\/ (char name (1- n))))))

(defun %bare (name)
  (if (%branchp name) (subseq name 0 (1- (length name))) name))

(defun %under (n name)
  "Where NAME is on the disk under N, or NIL when nothing is there."
  (let ((where (truename-of n)))
    (or (probe-file (merge-pathnames (%bare name) where))
        (probe-file (merge-pathnames (concatenate 'string (%bare name) "/")
                                     where)))))

(defun %node-for (n path name)
  "The node N keeps for PATH, made once, so what reads it can be recomputed."
  (node:child n name
              (lambda ()
                (if (pathname-name path)
                    (make-instance 'file-node :name name :parent n
                                              :truename path)
                    (make-instance 'directory-node :name name :parent n
                                                   :truename path)))))

(defmethod node:nodes ((n directory-node))
  (let ((seen (make-hash-table :test 'equal)))
    (loop :for path :in (%entries (truename-of n))
          :for name := (%named path)
          :unless (or (null name) (gethash name seen))
            :do (setf (gethash name seen) t)
            :and :collect (%node-for n path name))))

(defmethod node:resolve ((n directory-node) name)
  (let ((path (%under n name)))
    (when path (%node-for n path (%bare name)))))

(defmethod node:contents ((n directory-node))
  (mapcar #'node:name (node:nodes n)))

(defmethod node:make-child ((n directory-node) name)
  "Make NAME on the disk. A name that ends in / is a directory."
  (let* ((where (truename-of n))
         (path (merge-pathnames (if (%branchp name) name (%bare name)) where)))
    (if (%branchp name)
        (ensure-directories-exist path)
        (let ((stream (open path :direction :output :if-exists nil
                                 :if-does-not-exist :create)))
          (when stream (close stream))))
    (node:invalidate n)
    (%node-for n (probe-file path) (%bare name))))

(defmethod node:erase-child ((n directory-node) name)
  "Take NAME off the disk. A directory has to be empty first, so removing one
node cannot cost a tree nobody looked at."
  (let ((path (%under n name)))
    (when path
      (if (pathname-name path)
          (delete-file path)
          (uiop:delete-empty-directory path))
      (d:drop! (node:kept n) (%bare name))
      (node:invalidate n))
    path))

(defun place (n name)
  "The node N keeps for NAME, whether or not anything stands there yet. Reading
one that is not there answers nothing and writing it makes it, which is what
opening a file that does not exist is: a buffer on a place, not on a file."
  (or (node:resolve n name)
      (%node-for n (merge-pathnames (%bare name) (truename-of n)) (%bare name))))

(defun mount (where into name)
  (let* ((truename (truename (pathname where)))
         (n (make-instance 'directory-node :name name :truename truename
                                           :describes (namestring truename))))
    (node:attach n into)
    (d:keep! *mounts* name n)
    n))

(defun unmount (name from)
  (d:drop! *mounts* name)
  (node:detach from name))

(defun mounted () (d:keys (d:all *mounts*)))

(defun refresh (n)
  (node:invalidate n)
  n)
