(defpackage #:pine.state.store
  (:use #:cl)
  (:shadow #:open #:close #:push #:read #:write)
  (:export #:open #:close #:with-db #:read #:write #:id
           #:value #:push #:items #:clear #:forget)
  (:documentation "The sqlite file under pine.state.world: the connection, the
lock around it, and the printing rules that keep every value readable back.
world is the only caller."))

(in-package #:pine.state.store)

;;;; One file, two tables: kv holds durable values, log holds bounded ordered
;;;; lists (newest = highest rowid). Two tables is the whole schema, because a
;;;; feature that wants durable state names it in world rather than adding a
;;;; table. Keys and values are readable lisp, printed under standard io syntax
;;;; so an unreadable object errors at the write, and read back with *read-eval*
;;;; off. All statements run under one lock on one connection; with nothing
;;;; open, reads answer their default and writes are no-ops, so images that
;;;; never open one (frontends, agents, benches) pay nothing.

(defvar *db* nil)
(defvar *lock* (bordeaux-threads:make-lock "pine-store"))

(defun read (string)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (values (read-from-string string)))))

(defun write (value)
  "VALUE printed to store text. *print-readably* stays off: it would encode a
base-string as an #A form while a character string prints as \"...\", splitting
one logical key into two spellings. The fail-loud guarantee is a read-back
check instead: a value whose printed form does not read errors at the write."
  (let ((s (with-standard-io-syntax
             (let ((*print-readably* nil))
               (prin1-to-string value)))))
    (handler-case (read s)
      (error () (error "pine.state.store: unstorable value ~s" value)))
    s))

(defmacro with-db ((var) &body body)
  "Run BODY with VAR bound to the open connection, under the store's lock.
With nothing open, BODY does not run and the answer is NIL."
  `(bordeaux-threads:with-lock-held (*lock*)
     (when *db*
       (let ((,var *db*))
         ,@body))))

(defun open (&optional path)
  "Open the store at PATH, by default XDG data home pine/store.db.

\":memory:\" opens a transient one. Idempotent: an already-open store is
closed first. Returns the path opened."
  (close)
  (let ((target (if path
                    (if (pathnamep path) (namestring path) path)
                    (namestring (uiop:xdg-data-home "pine/store.db")))))
    (unless (string= target ":memory:")
      (ensure-directories-exist target))
    (bordeaux-threads:with-lock-held (*lock*)
      (let ((db (sqlite:connect target)))
        (sqlite:execute-single db "PRAGMA journal_mode=WAL")
        (sqlite:execute-non-query db "PRAGMA busy_timeout=2000")
        (sqlite:execute-non-query
         db "CREATE TABLE IF NOT EXISTS kv (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
        (sqlite:execute-non-query
         db "CREATE TABLE IF NOT EXISTS log (name TEXT NOT NULL, value TEXT NOT NULL)")
        (sqlite:execute-non-query
         db "CREATE INDEX IF NOT EXISTS log_name ON log (name)")
        (setf *db* db)))
    target))

(defun close ()
  "Close the store if open. Safe to call with none open."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (ignore-errors (sqlite:disconnect *db*))
      (setf *db* nil))))

(defun id ()
  "A fresh 128-bit identity, from sqlite's own entropy. Unique across hosts, so
two images' buffers of the same name are not the same thing."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (sqlite:execute-single *db* "SELECT lower(hex(randomblob(16)))"))))

(defun value (key &optional default)
  "The durable value under KEY, or DEFAULT when absent or nothing is open."
  (bordeaux-threads:with-lock-held (*lock*)
    (if *db*
        (let ((row (sqlite:execute-single
                    *db* "SELECT value FROM kv WHERE key = ?" (write key))))
          (if row (read row) default))
        default)))

(defun (setf value) (new key &optional default)
  (declare (ignore default))
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (sqlite:execute-non-query
       *db* "INSERT OR REPLACE INTO kv (key, value) VALUES (?, ?)"
       (write key) (write new))))
  new)

(defun forget (key)
  "Drop the durable value under KEY."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (sqlite:execute-non-query
       *db* "DELETE FROM kv WHERE key = ?" (write key)))))

(defun push (name new &key (max 100) (unique t))
  "Append NEW to the bounded list NAME. With UNIQUE an equal entry moves to the
front instead of duplicating; the list is trimmed to its newest MAX entries."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (let ((n (write name)) (v (write new)))
        (when unique
          (sqlite:execute-non-query
           *db* "DELETE FROM log WHERE name = ? AND value = ?" n v))
        (sqlite:execute-non-query
         *db* "INSERT INTO log (name, value) VALUES (?, ?)" n v)
        (sqlite:execute-non-query
         *db* "DELETE FROM log WHERE name = ? AND rowid NOT IN
               (SELECT rowid FROM log WHERE name = ? ORDER BY rowid DESC LIMIT ?)"
         n n max))))
  new)

(defun items (name &key limit)
  "The list NAME, newest first, LIMIT entries at most."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (let ((n (write name)))
        (mapcar (lambda (row) (read (first row)))
                (if limit
                    (sqlite:execute-to-list
                     *db* "SELECT value FROM log WHERE name = ? ORDER BY rowid DESC LIMIT ?"
                     n limit)
                    (sqlite:execute-to-list
                     *db* "SELECT value FROM log WHERE name = ? ORDER BY rowid DESC"
                     n)))))))

(defun clear (name)
  "Drop the whole list NAME."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (sqlite:execute-non-query
       *db* "DELETE FROM log WHERE name = ?" (write name)))))
