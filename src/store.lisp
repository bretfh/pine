(defpackage #:pine.store
  (:use #:cl)
  (:shadow #:open #:close)
  (:local-nicknames (#:p #:pine.path) (#:ns #:pine.ns))
  (:export #:store #:open #:close #:restore #:revert #:storablep
           #:*changes-kept*))

(in-package #:pine.store)
(named-readtables:in-readtable pine.path:syntax)

;;;; What outlives the daemon, and the one file it lives in.
;;;;
;;;; Nothing here is asked for. A path is live when a provider reads it, derived
;;;; when an expression computes it from other paths, and held when someone
;;;; wrote it and nothing else determines it. Only held is stored, which is why
;;;; a buffer visiting a file keeps its point and its mode and not its text:
;;;; the text came from the file.
;;;;
;;;; One file per pine. A pine is a space, an image may hold several, so the
;;;; connection belongs to the store OPEN answers and not to the image.

(defvar *restoring* nil "True while reading the file back, so it is not rewritten.")

(defparameter *changes-kept* 5000
  "How many changes the file remembers when /history/kept says nothing. How far
back a revert reaches is a decision about this pine, so it is a held path and
the default is only what it starts at.")

(defun %kept ()
  (or (ns:read /history/kept) *changes-kept*))

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

;;;; The connection is an agent's state, so every statement runs on its one
;;;; thread and nothing here holds a lock.
;;;;
;;;; RECORD tells, so a namespace write answers as soon as the value has moved
;;;; and the file catches up behind it. Everything that needs an answer asks,
;;;; and the one mailbox orders those behind the records already told, so a read
;;;; of the log never misses a write that preceded it.
;;;;
;;;; Nothing that runs on that thread asks anything, which is what makes the
;;;; wait graph acyclic: a statement here touches sqlite and nothing else.

(defstruct (store (:constructor %store (agent)) (:copier nil) (:predicate nil))
  agent)

(defun %answer (answer)
  "ANSWER as the caller's value. A condition raised on the agent's thread comes
back as a value; it is signalled here instead, where the call was made."
  (if (and (consp answer) (eq :handler-error (car answer)))
      (error (cdr answer))
      answer))

(defun %ask (store fn)
  (%answer (sento.agent:agent-get (store-agent store) fn)))

(defun %change (store fn)
  (%answer (sento.agent:agent-update-and-get (store-agent store) fn)))

(defun %db (state) (fset:lookup state :db))

;;;; The file

(defun %connect (target)
  (let ((db (sqlite:connect target)))
    (sqlite:execute-single db "PRAGMA journal_mode=WAL")
    (sqlite:execute-non-query db "PRAGMA busy_timeout=2000")
    (sqlite:execute-non-query db "
CREATE TABLE IF NOT EXISTS held (
  path TEXT NOT NULL, seq INTEGER NOT NULL DEFAULT 0,
  value TEXT NOT NULL, at INTEGER NOT NULL, bound INTEGER,
  PRIMARY KEY (path, seq))")
    ;; COMMIT groups the rows one write moved. A transaction is one new root, so
    ;; going back to before it means putting all of its rows back, and without
    ;; the group that has to be guessed at from the order.
    (sqlite:execute-non-query db "
CREATE TABLE IF NOT EXISTS changes (
  n INTEGER PRIMARY KEY, commits INTEGER NOT NULL DEFAULT 0,
  path TEXT NOT NULL, old TEXT, new TEXT, at INTEGER NOT NULL)")
    (sqlite:execute-non-query db "
CREATE INDEX IF NOT EXISTS changes_path ON changes (path)")
    db))

(defun open (&optional path)
  "Open the file at PATH, by default XDG data home pine/pine.db, read every held
path back into the current space, and keep it written through from here on.
Answers the store, which is what closes it.

\":memory:\" opens one that lives as long as the image."
  (let ((target (if path
                    (if (pathnamep path) (namestring path) path)
                    (namestring (uiop:xdg-data-home "pine/pine.db"))))
        (store (%store (sento.agent:make-agent (lambda () (fset:empty-map))))))
    (unless (string= target ":memory:")
      (ensure-directories-exist target))
    (%change store (lambda (state)
                     (declare (ignore state))
                     {:db (%connect target) :since-trim 0}))
    (restore store)
    (setf (ns:on-commit) (lambda (moved) (record store moved)))
    (ns:write /history (history-provider store))
    (ns:write /was (was-provider store))
    store))

(defun close (store)
  "Close STORE and let its thread go. The space stops writing through, and the
paths it served come back off."
  (setf (ns:on-commit) nil)
  (ns:write /history nil)
  (ns:write /was nil)
  (%change store (lambda (state)
                   (let ((db (%db state)))
                     (when db (sqlite:disconnect db)))
                   (fset:empty-map)))
  (sento.agent:agent-stop (store-agent store))
  nil)

;;;; Write through

(defun %now () (get-universal-time))

(defun %trim (state kept)
  "STATE after one more change, trimming the log to KEPT when enough have gone
by."
  (let ((since (1+ (or (fset:lookup state :since-trim) 0))))
    (cond ((<= since 100) (fset:with state :since-trim since))
          (t (sqlite:execute-non-query
              (%db state) "DELETE FROM changes WHERE n NOT IN
                           (SELECT n FROM changes ORDER BY n DESC LIMIT ?)"
              kept)
             (fset:with state :since-trim 0)))))

(defun %rows (moved)
  "The rows MOVED asks the file to write, decided against the space the write
landed on rather than whenever the store reaches them."
  (loop :for (path old new) :in moved
        :when (and (eq :held (ns:kind path))
                   (ns:setting path :keep t)
                   (storablep new)
                   (storablep old))
          :collect (list (p:text path) old new (ns:setting path :max))))

(defun %write (state rows kept)
  "STATE after ROWS -- one commit's worth -- have gone in, keeping KEPT changes
in the log."
  (let ((db (%db state)))
    (if (null db)
        state
        (let ((commit (1+ (or (fset:lookup state :commit) 0))))
          (dolist (row rows (fset:with state :commit commit))
            (destructuring-bind (text old new bound) row
              (if (null new)
                  (sqlite:execute-non-query db "DELETE FROM held WHERE path = ?" text)
                  (sqlite:execute-non-query
                   db "INSERT OR REPLACE INTO held (path, seq, value, at, bound)
                       VALUES (?, 0, ?, ?, ?)"
                   text (%out new) (%now) bound))
              (sqlite:execute-non-query
               db "INSERT INTO changes (commits, path, old, new, at)
                   VALUES (?, ?, ?, ?, ?)"
               commit text (and old (%out old)) (and new (%out new)) (%now))
              (setf state (%trim state kept))))))))

(defun record (store moved)
  "Put every held change in the file. The space tells this each commit.

What to write and how much log to keep are both decided here, on the thread the
write landed on, so the agent's own thread reads nothing but sqlite."
  (unless *restoring*
    (let ((rows (%rows moved))
          (kept (%kept)))
      (when rows
        (sento.agent:agent-update (store-agent store)
                                  (lambda (state) (%write state rows kept)))))))

(defun restore (store)
  "Write every held path in the file back into the current space. A stored value
wins over the one a config seeded, which is what makes it durable."
  (let ((rows (%ask store
                    (lambda (state)
                      (let ((db (%db state)))
                        (when db
                          (sqlite:execute-to-list
                           db "SELECT path, value, bound FROM held ORDER BY path")))))))
    (let ((*restoring* t))
      (dolist (row rows)
        (destructuring-bind (text value bound) row
          (let ((path (p:parse text)))
            ;; the whole ring comes back as it stood, so it is set rather than
            ;; pushed; the bound is put back beside it
            (ns:write path (%in value))
            (when bound (setf (ns:setting path :max) bound))))))
    (length rows)))

;;;; History, as paths

(defun %changes (store &optional limit)
  (%ask store
        (lambda (state)
          (let ((db (%db state)))
            (when db
              (if limit
                  (sqlite:execute-to-list
                   db "SELECT n, commits, path, old, new, at FROM changes ORDER BY n DESC LIMIT ?"
                   limit)
                  (sqlite:execute-to-list
                   db "SELECT n, commits, path, old, new, at FROM changes ORDER BY n DESC")))))))

(defun %row (row)
  (destructuring-bind (n commit text old new at) row
    (fset:map (:n n) (:commit commit) (:path (p:parse text)) (:at at)
              (:old (and old (%in old))) (:new (and new (%in new))))))

(defun %at-change (store text n)
  "The value TEXT's path held as of change N.

The newest change to it at or before N, or the oldest one after N read
backwards, or -- when the file remembers no change to it -- what it holds now.
An ask answers one value, so the row and whether there was one travel together."
  (let ((found (%ask store
                     (lambda (state)
                       (let ((db (%db state)))
                         (when db
                           (let ((newer (sqlite:execute-to-list
                                         db "SELECT new FROM changes
                                             WHERE path = ? AND n <= ?
                                             ORDER BY n DESC LIMIT 1" text n)))
                             (if newer
                                 (list (first (first newer)))
                                 (let ((older (sqlite:execute-to-list
                                               db "SELECT old FROM changes
                                                   WHERE path = ? AND n > ?
                                                   ORDER BY n ASC LIMIT 1" text n)))
                                   (and older (list (first (first older)))))))))))))
    (if found
        (values (and (first found) (%in (first found))) t)
        (values nil nil))))

(defun revert (store n)
  "Undo every change after N, newest first, so the space reads as it did."
  (let ((rows (%ask store
                    (lambda (state)
                      (let ((db (%db state)))
                        (when db
                          (sqlite:execute-to-list
                           db "SELECT path, old FROM changes WHERE n > ?
                               ORDER BY n DESC" n)))))))
    (dolist (row rows (length rows))
      (destructuring-bind (text old) row
        (ns:write (p:parse text) (and old (%in old)))))))

;;;; A provider only ever answers under where it is mounted, so the log and the
;;;; view back through it are two.

(defun history-provider (store)
  (ns:provider
   (/history {:read (pine.data:fn []
                      (fset:convert 'fset:seq
                                    (mapcar #'%row (%changes store 200))))
              :verbs {:revert (pine.data:fn [n] (revert store n))}
              :doc "every change the file remembers, newest first"})))

(defun was-provider (store)
  (ns:provider
   (/was/?n/?@rest
    {:read (pine.data:fn []
             (multiple-value-bind (value known)
                 (%at-change store (p:text (apply #'p:path rest))
                             (parse-integer n :junk-allowed t))
               (if known value (ns:read (apply #'p:path rest)))))
     :doc "what a path held as of a change"})))
