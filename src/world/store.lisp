(defpackage #:pine.world.store
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node)
                    (#:tree #:pine.fs.tree) (#:world #:pine.world.world)
                    )
  (:export #:store #:open-store #:close-store #:snapshot #:restore #:file-of
           #:storablep #:written #:read-back #:keep #:keeping #:*store*))

(in-package #:pine.world.store)

(defvar *schema*
  "create table if not exists node (path text primary key, value text not null,
                                    at integer)")
(defvar *store* nil)

(defclass store ()
  ((file-of :initarg :file :reader file-of)
   (db      :initarg :db   :reader db)))

(defmethod print-object ((s store) stream)
  (print-unreadable-object (s stream :type t)
    (write-string (princ-to-string (file-of s)) stream)))

(defun open-store (file)
  (let ((db (sqlite:connect file)))
    (sqlite:execute-non-query db "pragma journal_mode = wal")
    (sqlite:execute-non-query db "pragma busy_timeout = 2000")
    (sqlite:execute-non-query db *schema*)
    (setf *store* (make-instance 'store :file file :db db))))

(defun close-store (s)
  (sqlite:disconnect (db s))
  (when (eq s *store*) (setf *store* nil))
  s)

(defun storablep (value)
  (typecase value
    ((or null number string character keyword) t)
    (symbol t)
    (cons (and (storablep (car value)) (storablep (cdr value))))
    ((and vector (not string)) (every #'storablep value))
    (t (and (d:collectionp value)
            (every #'storablep (d:as :list (d:keys value)))
            (every #'storablep (d:as :list (d:vals value)))))))

(defun said (value)
  "VALUE as something that reads back. A map, a seq and a set are written as
what they are, because prin1 cannot say them and read cannot take them."
  (cond ((d:mapp value)
         (list* :map (loop :for (k . v) :in (d:pairs value)
                           :append (list (said k) (said v)))))
        ((d:setp value) (list* :set (mapcar #'said (d:as :list value))))
        ((d:seqp value) (list* :seq (mapcar #'said (d:as :list value))))
        ((consp value) (cons (said (car value)) (said (cdr value))))
        (t value)))

(defun took (form)
  (cond ((and (consp form) (eq :map (car form)))
         (loop :with m := (d:no-map)
               :for (k v) :on (rest form) :by #'cddr
               :do (setf m (d:with m (took k) (took v)))
               :finally (return m)))
        ((and (consp form) (eq :seq (car form)))
         (d:as :seq (mapcar #'took (rest form))))
        ((and (consp form) (eq :set (car form)))
         (d:as :set (mapcar #'took (rest form))))
        ((consp form) (cons (took (car form)) (took (cdr form))))
        (t form)))

(defun written (value)
  (let ((*print-readably* nil) (*print-circle* nil))
    (prin1-to-string (said value))))

(defun read-back (text)
  (let ((*read-eval* nil))
    (handler-case (took (read-from-string text)) (error () nil))))

(defun snapshot (w s)
  (let ((n 0))
    (sqlite:with-transaction (db s)
      (sqlite:execute-non-query (db s) "delete from node")
      (tree:walk (world:root w)
                 (lambda (each)
                   (when (and (node:persistp each)
                              (node:contents each)
                              (storablep (node:contents each)))
                     (sqlite:execute-non-query
                      (db s)
                      "insert or replace into node (path, value, at) values (?, ?, ?)"
                      (node:full-name each) (written (node:contents each))
                      (get-universal-time))
                     (incf n)))))
    n))

(defun keep (n)
  "Write this node where it will be found again, now rather than at shutdown.
A crash must not cost what was written before it."
  (let ((s *store*))
    (when (and s (node:persistp n) (storablep (node:contents n)))
      (ignore-errors
       (sqlite:execute-non-query
        (db s) "insert or replace into node (path, value, at) values (?, ?, ?)"
        (node:full-name n) (written (node:contents n)) (get-universal-time)))
      n)))

(defun keeping (&optional (s *store*))
  "Write every node through as it is written, so what is in the store is what
the tree says rather than what it said when it last stopped."
  (setf node:*on-write* (when s #'keep))
  s)

(defun restore (w s)
  (let ((n 0))
    (loop :for (path text) :in (sqlite:execute-to-list
                                (db s) "select path, value from node")
          :do (let ((names (tree:split-name path)))
                (when names
                  (world:place w names (read-back text))
                  (incf n))))
    n))
