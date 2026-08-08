(defpackage #:pine.fs.mount
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:tree #:pine.fs.tree))
  (:export #:file-node #:directory-node #:mount #:unmount #:truename-of
           #:mounted #:refresh))

(in-package #:pine.fs.mount)

(defvar *mounts* nil)

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

(defmethod node:nodes ((n directory-node))
  (let ((seen (make-hash-table :test 'equal)))
    (loop :for path :in (%entries (truename-of n))
          :for name := (%named path)
          :unless (or (null name) (gethash name seen))
            :do (setf (gethash name seen) t)
            :and :collect (%node-for path name n))))

(defun %node-for (path name into)
  (if (pathname-name path)
      (make-instance 'file-node :name name :parent into :truename path)
      (make-instance 'directory-node :name name :parent into :truename path)))

(defmethod node:resolve ((n directory-node) name)
  (find name (node:nodes n) :key #'node:name :test #'equal))

(defmethod node:contents ((n directory-node))
  (mapcar #'node:name (node:nodes n)))

(defun mount (where into name)
  (let* ((truename (truename (pathname where)))
         (n (make-instance 'directory-node :name name :truename truename
                                           :describes (namestring truename))))
    (node:attach n into)
    (push (cons name n) *mounts*)
    n))

(defun unmount (name from)
  (setf *mounts* (remove name *mounts* :key #'car :test #'equal))
  (node:detach from name))

(defun mounted () (mapcar #'car *mounts*))

(defun refresh (n)
  (node:invalidate n)
  n)
