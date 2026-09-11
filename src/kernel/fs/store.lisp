(defpackage #:pine/fs/store
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:serial #:pine/serial) (#:log #:pine/fs/log))
  (:export
   #:open-store #:close-store #:keeping #:store #:*store*))
(in-package #:pine/fs/store)

(defvar *schema*
  "create table if not exists node (path text primary key, value text not null,
                                    at integer)")
(defvar *store* nil)

(defclass store ()
  ((file-of :initarg :file :reader file-of)
   (db      :initarg :db   :reader db)
   (lock    :initform (bordeaux-threads:make-recursive-lock "store") :reader lock)))

(defmethod print-object ((s store) stream)
  (print-unreadable-object (s stream :type t)
    (write-string (princ-to-string (file-of s)) stream)))

(defmacro with-store ((s) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock ,s)) ,@body))

(defun %trouble (what)
  (log:note "~a" what)
  nil)

(defun open-store (file)
  (ensure-directories-exist file)
  (let ((db (sqlite:connect file)))
    (sqlite:execute-non-query db "pragma journal_mode = wal")
    (sqlite:execute-non-query db "pragma busy_timeout = 2000")
    (sqlite:execute-non-query db *schema*)
    (setf *store* (make-instance 'store :file file :db db))))

(defun close-store (s)
  (when (eq s fs:*backing-store*) (setf fs:*backing-store* nil))
  (sqlite:disconnect (db s))
  (when (eq s *store*) (setf *store* nil))
  s)

(defun keeping (&optional (s *store*))
  (setf fs:*backing-store* s
        (fs:on-forget :store) (when s (lambda (path) (fs:store-delete s path))))
  s)

(defun written (value)
  (let ((*print-readably* nil) (*print-circle* nil)
        (*print-length* nil) (*print-level* nil)
        (*package* (find-package :keyword)))
    (prin1-to-string (serial:encode value))))

(defun read-back (text)
  (let ((*read-eval* nil) (*package* (find-package :keyword)))
    (handler-case (values (serial:decode (read-from-string text)) t)
      (error (c)
        (%trouble (format nil "a value in the store will not read back: ~a" c))
        (values nil nil)))))

(defmethod fs:store-get ((s store) path)
  (let ((row (with-store (s)
               (sqlite:execute-single (db s)
                                      "select value from node where path = ?" path))))
    (if row (read-back row) (values nil nil))))

(defmethod (setf fs:store-get) (value (s store) path)
  (handler-case
      (with-store (s)
        (sqlite:execute-non-query
         (db s) "insert or replace into node (path, value, at) values (?, ?, ?)"
         path (written value) (get-universal-time)))
    (error (c)
      (%trouble (format nil "~a did not reach the store: ~a" path c))))
  value)

(defun %like (text)
  (with-output-to-string (out)
    (loop :for ch :across text
          :do (when (find ch "%_\\") (write-char #\\ out))
              (write-char ch out))))

(defmethod fs:store-list ((s store) path)
  (let* ((prefix (if (string= path "/") "/" (concatenate 'string path "/")))
         (at (length prefix))
         (out nil))
    (dolist (row (with-store (s)
                   (sqlite:execute-to-list
                    (db s) "select path from node where path like ? escape '\\'"
                    (concatenate 'string (%like prefix) "%")))
             (nreverse out))
      (let* ((said (first row))
             (rest (subseq said at))
             (cut (position #\/ rest))
             (name (if cut (subseq rest 0 cut) rest)))
        (when (and (plusp (length name)) (not (member name out :test #'equal)))
          (push name out))))))

(defmethod fs:store-transaction ((s store) thunk)
  (with-store (s) (sqlite:with-transaction (db s) (funcall thunk))))

(defmethod fs:store-any-p ((s store) path)
  (let ((prefix (if (string= path "/") "/" (concatenate 'string path "/"))))
    (and (with-store (s)
           (sqlite:execute-single
            (db s) "select 1 from node where path like ? escape '\\' limit 1"
            (concatenate 'string (%like prefix) "%")))
         t)))

(defmethod fs:store-delete ((s store) path)
  (with-store (s)
    (sqlite:execute-non-query
     (db s)
     "delete from node where path = ? or path like ? escape '\\'"
     path (concatenate 'string (%like path) "/%")))
  path)

(defclass persisting (fs:derived) ())

(defmethod fs:works ((n persisting))
  (let ((s fs:*backing-store*)) (and s (princ-to-string (file-of s)))))

(fs:mount (lambda ()
            (make-instance 'persisting :describes "where this pine persists"))
          "/store")
