(defpackage #:pine/fs/store
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:said #:pine/said) (#:log #:pine/fs/log))
  (:export
   #:open-store #:close-store #:snapshot #:restore #:stale
   #:keeping #:*store*))
(in-package #:pine/fs/store)

(defvar *schema*
  "create table if not exists node (path text primary key, value text not null,
                                    at integer)")
(defvar *store* nil)
(defvar *putting-back* nil
  "Whether this thread is putting values back out of the store. Bound rather than
turned off, so a write from another thread while a restore runs is still heard.")

(defclass store ()
  ((file-of :initarg :file :reader file-of)
   (db      :initarg :db   :reader db)))

(defmethod print-object ((s store) stream)
  (print-unreadable-object (s stream :type t)
    (write-string (princ-to-string (file-of s)) stream)))

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
  (sqlite:disconnect (db s))
  (when (eq s *store*) (setf *store* nil))
  s)

(defun storablep (value) (said:sayablep value))

(defun written (value)
  "Printed as if from the keyword package, so every symbol that is not one carries
the package it is in."
  (let ((*print-readably* nil) (*print-circle* nil)
        (*package* (find-package :keyword)))
    (prin1-to-string (said:said value))))

(defun read-back (text)
  (let ((*read-eval* nil) (*package* (find-package :keyword)))
    (handler-case (values (said:took (read-from-string text)) t)
      (error (c)
        (%trouble (format nil "a value in the store will not read back: ~a" c))
        (values nil nil)))))

(defun snapshot (s &optional (root (fs:root)))
  "Write down every saved value standing now. Nothing is taken out here: what went
was taken out as it went."
  (let ((n 0))
    (sqlite:with-transaction (db s)
      (fs:walk root
               (lambda (each)
                 (when (and (fs:savedp each) (storablep (fs:contents each)))
                   (sqlite:execute-non-query
                    (db s)
                    "insert or replace into node (path, value, at) values (?, ?, ?)"
                    (fs:full-name each) (written (fs:contents each))
                    (get-universal-time))
                   (incf n)))))
    n))

(defun keep (n)
  "Write this value where it will be found again, now rather than at shutdown."
  (let ((s *store*))
    (when (and s (fs:savedp n) (storablep (fs:contents n)))
      (handler-case
          (sqlite:execute-non-query
           (db s) "insert or replace into node (path, value, at) values (?, ?, ?)"
           (fs:full-name n) (written (fs:contents n)) (get-universal-time))
        (error (c)
          (%trouble (format nil "~a did not reach the store: ~a"
                            (fs:full-name n) c))))
      n)))

(defun %like (text)
  (with-output-to-string (out)
    (loop :for ch :across text
          :do (when (find ch "%_\\") (write-char #\\ out))
              (write-char ch out))))

(defun forget (path)
  (let ((s *store*))
    (when s
      (sqlite:execute-non-query
       (db s)
       "delete from node where path = ? or path like ? escape '\\'"
       path (concatenate 'string (%like path) "/%")))
    path))

(defun kept (moved)
  (let ((s *store*))
    (when (and s (not *putting-back*))
      (sqlite:with-transaction (db s)
        (loop :for n :in moved
              :when (fs:kind n) :do (keep n))))))

(defun keeping (&optional (s *store*))
  (setf (fs:on-commit :store) (when s #'kept)
        (fs:on-forget :store) (when s #'forget))
  s)

(defun restore (s &optional (root (fs:root)))
  "Put values back into what already stands. A path nothing stands at any more is
left in the store rather than conjured: what it stood for is what knew how to read
it."
  (let ((n 0)
        (*putting-back* t))
    (loop :for (path text) :in (sqlite:execute-to-list
                                (db s) "select path, value from node")
          :do (multiple-value-bind (value read) (read-back text)
                (let* ((names (fs:split-name path))
                       (at (and names read (apply #'fs:at root names))))
                  (when (and at (fs:savedp at))
                    (setf (fs:contents at) (fs:as-value value))
                    (incf n))))
          :finally (return n))))

(defun stale (s &optional (root (fs:root)))
  (loop :for (path) :in (sqlite:execute-to-list (db s) "select path from node")
        :for names := (fs:split-name path)
        :unless (and names (apply #'fs:at root names))
          :collect path))

(fs:mount (lambda ()
            (make-instance 'fs:derived
                           :reads (lambda ()
                                    (let ((s *store*))
                                      (and s (princ-to-string (file-of s)))))
                           :writes (lambda (value)
                                     (declare (ignore value))
                                     (and *store* (snapshot *store*)))
                           :describes "where this pine persists, and writing it
writes the tree down"))
          "/store")
