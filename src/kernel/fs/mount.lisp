(in-package #:pine/fs)

(defclass directory (mount)
  ((truename-of :initarg :truename :reader truename-of))
  (:documentation "A directory on this machine's disk, serving what is in it."))

(defclass file (derived)
  ((truename-of :initarg :truename :reader truename-of))
  (:documentation "One file on the disk."))

(defmethod livep ((n file) &optional name) (declare (ignore name)) t)
(defmethod livep ((n directory) &optional name) (declare (ignore name)) t)

(defmethod mount ((what pathname) where)
  (let ((it (truename what)))
    (mount (make-instance 'directory :truename it :describes (namestring it))
           where)))

(defmethod works ((n file))
  (when (probe-file (truename-of n))
    (with-open-file (in (truename-of n) :external-format :utf-8)
      (let ((text (make-string (file-length in))))
        (subseq text 0 (read-sequence text in))))))

(defmethod takes ((n file) value)
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

(defun %under-disk (n name)
  (let ((where (truename-of n)))
    (or (probe-file (merge-pathnames name where))
        (probe-file (merge-pathnames (concatenate 'string name "/") where)))))

(defun %node-for (n path name)
  (child n name
         (lambda ()
           (make-instance (if (pathname-name path) 'file 'directory)
                          :name name :parent n :truename path))))

(defmethod entries ((n directory))
  (let ((seen (make-hash-table :test 'equal)))
    (loop :for path :in (%entries (truename-of n))
          :for name := (%leaf-name path)
          :unless (or (null name) (gethash name seen))
            :do (setf (gethash name seen) t)
            :and :collect (%node-for n path name))))

(defmethod entry ((n directory) name)
  "Asked of the disk rather than listed: finding one file by listing a directory of
ten thousand is what a mount cannot afford."
  (let* ((name (princ-to-string name))
         (path (%under-disk n name)))
    (when path (%node-for n path name))))

(defmethod make-entry ((n directory) name kind)
  (let* ((name (princ-to-string name))
         (where (truename-of n))
         (path (merge-pathnames (if (eq kind :mount)
                                    (concatenate 'string name "/")
                                    name)
                                where)))
    (if (eq kind :mount)
        (ensure-directories-exist path)
        (let ((stream (open path :direction :output :if-exists nil
                                 :if-does-not-exist :create)))
          (when stream (close stream))))
    (moved n)
    (%node-for n (probe-file path) name)))

(defmethod erase-entry ((n directory) name)
  "A directory has to be empty first, so removing one entry cannot cost a tree
nobody looked at."
  (let* ((name (princ-to-string name))
         (path (%under-disk n name)))
    (when path
      (if (pathname-name path)
          (delete-file path)
          (uiop:delete-empty-directory path))
      (d:swap (slot-value n 'memo) #'d:without name)
      (moved n))
    path))

(defun node-for (n name)
  "The entry N keeps for NAME, whether or not anything stands there yet: a buffer
on a place, not on a file."
  (let ((name (princ-to-string name)))
    (or (entry n name)
        (%node-for n (merge-pathnames name (truename-of n)) name))))
