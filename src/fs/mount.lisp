(defpackage #:pine/fs/mount
  (:use #:cl)
  (:shadow #:directory)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree))
  (:export #:mount #:truename-of #:place
           #:file #:directory))
(in-package #:pine/fs/mount)


(defclass mount (node:node) ((livep :initform t))
  (:documentation "A namespace from somewhere else, grafted into this one. What is
behind it keeps its own contents, so nothing here is snapshotted and everything
here answers differently without being written."))

(defclass file (mount)
  ((truename-of :initarg :truename :reader truename-of))
  (:documentation "A file on this machine."))

(defclass directory (mount)
  ((truename-of :initarg :truename :reader truename-of))
  (:documentation "A directory on this machine."))

(defgeneric mount (what into name)
  (:documentation "Graft the namespace WHAT stands for into INTO, under NAME.

A pathname is a directory on this machine. A peer is another pine, here or on
another machine. The method differs; nothing above it does."))

(defmethod mount ((what pathname) into name)
  (let* ((it (truename what))
         (n (make-instance 'directory :name name :truename it
                                      :describes (namestring it))))
    (node:attach n into)
    n))

(defmethod mount ((what string) into name)
  (mount (pathname what) into name))

(defmethod node:contents ((n file))
  (when (probe-file (truename-of n))
    (with-open-file (in (truename-of n) :external-format :utf-8)
      (let ((text (make-string (file-length in))))
        (subseq text 0 (read-sequence text in))))))

(defmethod (setf node:contents) (value (n file))
  (with-open-file (out (truename-of n) :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
    (write-string (princ-to-string value) out))
  value)

(defun %entries (where)
  (append (cl:directory (merge-pathnames "*.*" where))
          (cl:directory (merge-pathnames "*/" where))))

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
  (let ((where (truename-of n)))
    (or (probe-file (merge-pathnames (%bare name) where))
        (probe-file (merge-pathnames (concatenate 'string (%bare name) "/")
                                     where)))))

(defun %node-for (n path name)
  "The node N keeps for PATH, made once, so what reads it can be worked out again."
  (node:child n name
              (lambda ()
                (make-instance (if (pathname-name path) 'file 'directory)
                               :name name :over n :truename path))))

(defmethod node:nodes ((n directory))
  (let ((seen (make-hash-table :test 'equal)))
    (loop :for path :in (%entries (truename-of n))
          :for name := (%named path)
          :unless (or (null name) (gethash name seen))
            :do (setf (gethash name seen) t)
            :and :collect (%node-for n path name))))

(defmethod node:resolve ((n directory) name)
  "Asked of the disk rather than derived from NODES: finding one file by listing a
directory of ten thousand is what a mount cannot afford."
  (let ((path (%under n name)))
    (when path (%node-for n path (%bare name)))))

(defmethod node:contents ((n directory))
  (mapcar #'node:name (node:nodes n)))

(defmethod node:make-child ((n directory) name)
  "Make NAME on the disk. A name that ends in / is a directory."
  (let* ((where (truename-of n))
         (path (merge-pathnames (if (%branchp name) name (%bare name)) where)))
    (if (%branchp name)
        (ensure-directories-exist path)
        (let ((stream (open path :direction :output :if-exists nil
                                 :if-does-not-exist :create)))
          (when stream (close stream))))
    (node:stir n)
    (%node-for n (probe-file path) (%bare name))))

(defmethod node:erase-child ((n directory) name)
  "Take NAME off the disk. A directory has to be empty first, so removing one node
cannot cost a tree nobody looked at."
  (let ((path (%under n name)))
    (when path
      (if (pathname-name path)
          (delete-file path)
          (uiop:delete-empty-directory path))
      (d:drop! (node:memo n) (%bare name))
      (node:stir n))
    path))

(defun place (n name)
  "The node N keeps for NAME, whether or not anything stands there yet. Reading one
that is not there answers nothing and writing it makes it, which is what opening a
file that does not exist is: a buffer on a place, not on a file."
  (or (node:resolve n name)
      (%node-for n (merge-pathnames (%bare name) (truename-of n)) (%bare name))))
