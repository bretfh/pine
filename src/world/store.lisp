(defpackage #:pine.world.store
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:world #:pine.world.world))
  (:export #:store #:open-store #:close-store #:snapshot #:restore #:file-of
           #:storablep #:written))

(in-package #:pine.world.store)

(defvar *schema*
  "create table if not exists node (path text primary key, value text not null)")

(defclass store ()
  ((file-of :initarg :file :reader file-of)
   (db      :initarg :db   :reader db)))

(defmethod print-object ((s store) stream)
  (print-unreadable-object (s stream :type t)
    (write-string (princ-to-string (file-of s)) stream)))

(defun open-store (file)
  (let ((db (sqlite:connect file)))
    (sqlite:execute-non-query db *schema*)
    (make-instance 'store :file file :db db)))

(defun close-store (s)
  (sqlite:disconnect (db s))
  s)

(defun storablep (value)
  (typecase value
    ((or null number string character keyword) t)
    (symbol t)
    (cons (and (storablep (car value)) (storablep (cdr value))))
    ((and vector (not string)) (every #'storablep value))
    (t nil)))

(defun written (value)
  (let ((*print-readably* nil) (*print-circle* nil))
    (prin1-to-string value)))

(defun %read-back (text)
  (let ((*read-eval* nil))
    (handler-case (read-from-string text) (error () nil))))

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
                      (db s) "insert or replace into node (path, value) values (?, ?)"
                      (node:full-name each) (written (node:contents each)))
                     (incf n)))))
    n))

(defun restore (w s)
  (let ((n 0))
    (loop :for (path text) :in (sqlite:execute-to-list
                                (db s) "select path, value from node")
          :do (let ((names (tree:split-name path)))
                (when names
                  (world:place w names (%read-back text))
                  (incf n))))
    n))
