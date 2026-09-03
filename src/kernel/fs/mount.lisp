(defpackage #:pine/fs/mount
  (:use #:cl)
  (:shadow #:directory)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:export
   #:mount #:truename-of #:node-for #:file))
(in-package #:pine/fs/mount)

(defclass mount ()
  ((truename-of :initarg :truename :reader truename-of))
  (:documentation "Somewhere on this machine's disk, grafted into the tree."))

(defclass file (mount fs:derived) ())

(defclass directory (mount fs:dir) ())

(defmethod fs:livep ((n file)) t)
(defmethod fs:livep ((n directory)) t)

(defgeneric mount (what into name)
  (:documentation "Graft the namespace WHAT stands for into INTO, under NAME."))

(defmethod mount ((what pathname) into name)
  (let* ((it (truename what))
         (n (make-instance 'directory :name name :truename it
                                      :describes (namestring it))))
    (fs:attach n into)
    n))

(defmethod mount ((what string) into name)
  (mount (pathname what) into name))

(defmethod fs:works ((n file))
  (when (probe-file (truename-of n))
    (with-open-file (in (truename-of n) :external-format :utf-8)
      (let ((text (make-string (file-length in))))
        (subseq text 0 (read-sequence text in))))))

(defmethod fs:takes ((n file) value)
  "Written beside itself and then over itself, so the name answers either the
whole of what was written or exactly what it held before."
  (uiop:with-staging-pathname (staged (truename-of n))
    (with-open-file (out staged :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
      (write-string (princ-to-string value) out)))
  value)

(defun %entries (where)
  (append (cl:directory (merge-pathnames "*.*" where))
          (cl:directory (merge-pathnames "*/" where))))

(defun %leaf-name (path)
  (if (pathname-name path)
      (file-namestring path)
      (car (last (pathname-directory path)))))

(defun %under (n name)
  (let ((where (truename-of n)))
    (or (probe-file (merge-pathnames name where))
        (probe-file (merge-pathnames (concatenate 'string name "/") where)))))

(defun %node-for (n path name)
  (fs:child n name
            (lambda ()
              (make-instance (if (pathname-name path) 'file 'directory)
                             :name name :parent n :truename path))))

(defmethod fs:entries ((n directory))
  (let ((seen (make-hash-table :test 'equal)))
    (loop :for path :in (%entries (truename-of n))
          :for name := (%leaf-name path)
          :unless (or (null name) (gethash name seen))
            :do (setf (gethash name seen) t)
            :and :collect (%node-for n path name))))

(defmethod fs:entry ((n directory) name)
  "Asked of the disk rather than listed: finding one file by listing a directory of
ten thousand is what a mount cannot afford."
  (let* ((name (princ-to-string name))
         (path (%under n name)))
    (when path (%node-for n path name))))

(defmethod fs:make-entry ((n directory) name kind)
  (let* ((name (princ-to-string name))
         (where (truename-of n))
         (path (merge-pathnames (if (eq kind :dir)
                                    (concatenate 'string name "/")
                                    name)
                                where)))
    (if (eq kind :dir)
        (ensure-directories-exist path)
        (let ((stream (open path :direction :output :if-exists nil
                                 :if-does-not-exist :create)))
          (when stream (close stream))))
    (fs:moved n)
    (%node-for n (probe-file path) name)))

(defmethod fs:erase-entry ((n directory) name)
  "A directory has to be empty first, so removing one entry cannot cost a tree
nobody looked at."
  (let* ((name (princ-to-string name))
         (path (%under n name)))
    (when path
      (if (pathname-name path)
          (delete-file path)
          (uiop:delete-empty-directory path))
      (d:swap (slot-value n 'fs::memo) #'d:without name)
      (fs:moved n))
    path))

(defun node-for (n name)
  "The entry N keeps for NAME, whether or not anything stands there yet: a buffer
on a place, not on a file."
  (let ((name (princ-to-string name)))
    (or (fs:entry n name)
        (%node-for n (merge-pathnames name (truename-of n)) name))))
