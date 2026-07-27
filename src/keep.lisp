(defpackage #:pine.keep
  (:use #:cl)
  (:shadow #:open #:close)
  (:local-nicknames (#:p #:pine.path) (#:ns #:pine.ns))
  (:export #:open #:close #:restore #:revert #:storablep #:*changes-kept*))

(in-package #:pine.keep)
(named-readtables:in-readtable pine.path:syntax)

;;;; What outlives the daemon, and the one file it lives in.
;;;;
;;;; Nothing here is asked for. A path is live when a provider reads it, derived
;;;; when an expression computes it from other paths, and held when someone
;;;; wrote it and nothing else determines it. Only held is stored, which is why
;;;; a buffer visiting a file keeps its point and its mode and not its text:
;;;; the text came from the file.

(defvar *db* nil)
(defvar *lock* (bordeaux-threads:make-lock "pine-keep"))
(defvar *restoring* nil "True while reading the file back, so it is not rewritten.")
(defparameter *changes-kept* 5000
  "How many changes the file remembers, which is how far back a revert reaches.")

(defvar *since-trim* 0)

(defun storablep (value)
  "Whether VALUE is data the file can hold.

Code is not: a command, a provider and a view are functions, and whatever
declared them declares them again at boot. Refusing quietly would lose state
without saying so, so this decides before the write rather than after."
  (typecase value
    ((or number string character symbol) t)
    (p:path t)
    (cons (and (storablep (car value)) (storablep (cdr value))))
    (t (cond ((fset:map? value)
              (let ((ok t))
                (fset:do-map (k v value)
                  (unless (and (storablep k) (storablep v)) (setf ok nil)))
                ok))
             ((fset:seq? value)
              (let ((ok t))
                (fset:do-seq (x value) (unless (storablep x) (setf ok nil)))
                ok))
             ((fset:set? value)
              (let ((ok t))
                (fset:do-set (x value) (unless (storablep x) (setf ok nil)))
                ok))
             (t nil)))))

(defun %out (value) (pine.data:serialize value))
(defun %in (text) (pine.data:deserialize text 'p:data))

;;;; The file

(defun open (&optional path)
  "Open the file at PATH, by default XDG data home pine/pine.db, read every held
path back into the namespace, and keep it written through from here on.

\":memory:\" opens one that lives as long as the image."
  (close)
  (let ((target (if path
                    (if (pathnamep path) (namestring path) path)
                    (namestring (uiop:xdg-data-home "pine/pine.db")))))
    (unless (string= target ":memory:")
      (ensure-directories-exist target))
    (bordeaux-threads:with-lock-held (*lock*)
      (let ((db (sqlite:connect target)))
        (sqlite:execute-single db "PRAGMA journal_mode=WAL")
        (sqlite:execute-non-query db "PRAGMA busy_timeout=2000")
        (sqlite:execute-non-query db "
CREATE TABLE IF NOT EXISTS held (
  path TEXT NOT NULL, seq INTEGER NOT NULL DEFAULT 0,
  value TEXT NOT NULL, at INTEGER NOT NULL,
  PRIMARY KEY (path, seq))")
        (sqlite:execute-non-query db "
CREATE TABLE IF NOT EXISTS changes (
  n INTEGER PRIMARY KEY, path TEXT NOT NULL,
  old TEXT, new TEXT, at INTEGER NOT NULL)")
        (sqlite:execute-non-query db "
CREATE INDEX IF NOT EXISTS changes_path ON changes (path)")
        (setf *db* db)))
    (restore)
    (setf ns:*after-commit* #'record)
    (ns:write /history (history-provider))
    (ns:write /was (was-provider))
    target))

(defun close ()
  "Close the file. Safe with none open."
  (setf ns:*after-commit* nil)
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (sqlite:disconnect *db*)
      (setf *db* nil))))

;;;; Write through

(defun %now () (get-universal-time))

(defun %trim (db)
  (when (> (incf *since-trim*) 100)
    (setf *since-trim* 0)
    (sqlite:execute-non-query
     db "DELETE FROM changes WHERE n NOT IN
         (SELECT n FROM changes ORDER BY n DESC LIMIT ?)" *changes-kept*)))

(defun record (moved)
  "Put every held change in the file. Installed as the namespace's commit hook."
  (unless *restoring*
    (bordeaux-threads:with-lock-held (*lock*)
      (when *db*
        (dolist (change moved)
          (destructuring-bind (path old new) change
            (when (and (eq :held (ns:kind path))
                       (ns:setting path :keep t)
                       (storablep new)
                       (storablep old))
              (let ((text (p:text path)))
                (if (null new)
                    (sqlite:execute-non-query
                     *db* "DELETE FROM held WHERE path = ?" text)
                    (sqlite:execute-non-query
                     *db* "INSERT OR REPLACE INTO held (path, seq, value, at)
                           VALUES (?, 0, ?, ?)"
                     text (%out new) (%now)))
                (sqlite:execute-non-query
                 *db* "INSERT INTO changes (path, old, new, at) VALUES (?, ?, ?, ?)"
                 text (and old (%out old)) (and new (%out new)) (%now))
                (%trim *db*)))))))))

(defun restore ()
  "Write every held path in the file back into the namespace. A stored value
wins over the one a config seeded, which is what makes it durable."
  (let ((rows (bordeaux-threads:with-lock-held (*lock*)
                (when *db*
                  (sqlite:execute-to-list
                   *db* "SELECT path, value FROM held ORDER BY path")))))
    (let ((*restoring* t))
      (dolist (row rows)
        (destructuring-bind (text value) row
          (ns:write (p:parse text) (%in value)))))
    (length rows)))

;;;; History, as paths

(defun %changes (&optional limit)
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (if limit
          (sqlite:execute-to-list
           *db* "SELECT n, path, old, new, at FROM changes ORDER BY n DESC LIMIT ?"
           limit)
          (sqlite:execute-to-list
           *db* "SELECT n, path, old, new, at FROM changes ORDER BY n DESC")))))

(defun %row (row)
  (destructuring-bind (n text old new at) row
    (fset:map (:n n) (:path (p:parse text)) (:at at)
              (:old (and old (%in old))) (:new (and new (%in new))))))

(defun %at-change (text n)
  "The value TEXT's path held as of change N.

The newest change to it at or before N, or the oldest one after N read
backwards, or -- when the file remembers no change to it -- what it holds now."
  (bordeaux-threads:with-lock-held (*lock*)
    (when *db*
      (let ((newer (sqlite:execute-to-list
                    *db* "SELECT new FROM changes WHERE path = ? AND n <= ?
                          ORDER BY n DESC LIMIT 1" text n)))
        (if newer
            (values (and (first (first newer)) (%in (first (first newer)))) t)
            (let ((older (sqlite:execute-to-list
                          *db* "SELECT old FROM changes WHERE path = ? AND n > ?
                                ORDER BY n ASC LIMIT 1" text n)))
              (if older
                  (values (and (first (first older)) (%in (first (first older)))) t)
                  (values nil nil))))))))

(defun revert (n)
  "Undo every change after N, newest first, so the namespace reads as it did."
  (let ((rows (bordeaux-threads:with-lock-held (*lock*)
                (when *db*
                  (sqlite:execute-to-list
                   *db* "SELECT path, old FROM changes WHERE n > ? ORDER BY n DESC"
                   n)))))
    (dolist (row rows (length rows))
      (destructuring-bind (text old) row
        (ns:write (p:parse text) (and old (%in old)))))))

;;;; A provider only ever answers under where it is mounted, so the log and the
;;;; view back through it are two.

(defun history-provider ()
  (ns:provider
   (/history {:read (pine.data:fn []
                      (fset:convert 'fset:seq (mapcar #'%row (%changes 200))))
              :verbs {:revert (pine.data:fn [n] (revert n))}
              :doc "every change the file remembers, newest first"})))

(defun was-provider ()
  (ns:provider
   (/was/?n/?@rest
    {:read (pine.data:fn []
             (multiple-value-bind (value known)
                 (%at-change (p:text (apply #'p:path rest))
                             (parse-integer n :junk-allowed t))
               (if known value (ns:read (apply #'p:path rest)))))
     :doc "what a path held as of a change"})))
