(in-package #:pine/fs)

(defclass directory (mount)
  ((truename-of :initarg :truename :reader truename-of)))

(defclass file (derived)
  ((truename-of :initarg :truename :reader truename-of)))

(defmethod volatile-p ((n file) &optional name) (declare (ignore name)) t)
(defmethod volatile-p ((n directory) &optional name) (declare (ignore name)) t)

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
  (ensure-child n name
         (lambda ()
           (make-instance (if (pathname-name path) 'file 'directory)
                          :name name :parent n :truename path))))

(defmethod children ((n directory))
  (let ((seen (make-hash-table :test 'equal)))
    (loop :for path :in (%entries (truename-of n))
          :for name := (%leaf-name path)
          :unless (or (null name) (gethash name seen))
            :do (setf (gethash name seen) t)
            :and :collect (%node-for n path name))))

(defmethod child ((n directory) name)
  (let* ((name (princ-to-string name))
         (path (%under-disk n name)))
    (when path (%node-for n path name))))

(defmethod create ((n directory) name kind)
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
    (touch n)
    (%node-for n (probe-file path) name)))

(defmethod unlink ((n directory) name)
  (let* ((name (princ-to-string name))
         (path (%under-disk n name)))
    (when path
      (if (pathname-name path)
          (delete-file path)
          (uiop:delete-empty-directory path))
      (sb-ext:atomic-update (slot-value n 'dentries) (lambda (old) (d:without old name)))
      (touch n))
    path))

(defun node-for (n name)
  (let ((name (princ-to-string name)))
    (or (child n name)
        (%node-for n (merge-pathnames name (truename-of n)) name))))
