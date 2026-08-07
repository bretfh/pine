(defpackage #:pine.store
  (:use #:cl)
  (:shadow #:open #:close)
  (:local-nicknames (#:p #:pine.path) (#:ns #:pine.ns))
  (:export #:store #:open #:close #:restore #:storablep))

(in-package #:pine.store)
(named-readtables:in-readtable pine.path:syntax)

;;;; What outlives the daemon, and the one file it lives in. Only a held path
;;;; is stored, which is why a buffer visiting a file keeps its point and its
;;;; mode and not its text: the text came from the file.
;;;;
;;;; The connection is an agent's state, so every statement runs on its one
;;;; thread and nothing here holds a lock. Nothing on that thread asks anything,
;;;; so the wait graph stays acyclic.

(defvar *restoring* nil "True while reading the file back, so it is not rewritten.")

(defstruct (store (:constructor %store (agent)) (:copier nil) (:predicate nil))
  agent)

(defgeneric storablep (value)
  (:documentation "Whether VALUE is data the file can hold. Code is not: a
command, a provider and a view are functions, and whatever declared them
declares them again at boot.")
  (:method (value) (declare (ignore value)) nil))

(defmethod storablep ((value number)) t)
(defmethod storablep ((value string)) t)
(defmethod storablep ((value character)) t)
(defmethod storablep ((value symbol)) t)
(defmethod storablep ((value p:path)) t)

(defmethod storablep ((value cons))
  (and (storablep (car value)) (storablep (cdr value))))

(defmethod storablep ((value fset:map))
  (let ((ok t))
    (fset:do-map (k v value)
      (unless (and (storablep k) (storablep v)) (setf ok nil)))
    ok))

(defmethod storablep ((value fset:seq))
  (let ((ok t))
    (fset:do-seq (x value) (unless (storablep x) (setf ok nil)))
    ok))

(defmethod storablep ((value fset:set))
  (let ((ok t))
    (fset:do-set (x value) (unless (storablep x) (setf ok nil)))
    ok))

(defun %out (value) (pine.data:serialize value))
(defun %in (text) (pine.data:deserialize text 'p:data))
(defun %now () (get-universal-time))

(defun %answer (answer)
  "ANSWER as the caller's value. A condition raised on the agent's thread comes
back as a value; it is signalled here, where the call was made."
  (if (and (consp answer) (eq :handler-error (car answer)))
      (error (cdr answer))
      answer))

(defun %ask (store fn)
  (%answer (sento.agent:agent-get (store-agent store) fn)))

(defun %change (store fn)
  (%answer (sento.agent:agent-update-and-get (store-agent store) fn)))

(defun %db (state) (fset:lookup state :db))

(defun %connect (target)
  (let ((db (sqlite:connect target)))
    (sqlite:execute-single db "PRAGMA journal_mode=WAL")
    (sqlite:execute-non-query db "PRAGMA busy_timeout=2000")
    (sqlite:execute-non-query db "
CREATE TABLE IF NOT EXISTS held (
  path TEXT NOT NULL PRIMARY KEY,
  value TEXT NOT NULL, at INTEGER NOT NULL, bound INTEGER)")
    db))

(defun %rows (moved)
  "The rows MOVED asks the file to write. :KEEP is opt-in."
  (loop :for (path old new) :in moved
        :when (and (ns:setting path :keep)
                   (storablep new)
                   (storablep old))
          :collect (list (p:text path) new (ns:setting path :max))))

(defun %write (state rows)
  "STATE after ROWS have gone in."
  (let ((db (%db state)))
    (if (null db)
        state
        (progn
          (dolist (row rows)
            (destructuring-bind (text new bound) row
              (if (null new)
                  (sqlite:execute-non-query db "DELETE FROM held WHERE path = ?" text)
                  (sqlite:execute-non-query
                   db "INSERT OR REPLACE INTO held (path, value, at, bound)
                       VALUES (?, ?, ?, ?)"
                   text (%out new) (%now) bound))))
          state))))

(defun record (store moved)
  "Put every kept change in the file. The space tells this each commit, and it
tells rather than asks, so a write answers as soon as the value has moved."
  (unless *restoring*
    (let ((rows (%rows moved)))
      (when rows
        (sento.agent:agent-update (store-agent store)
                                  (lambda (state) (%write state rows)))))))

(defun restore (store)
  "Write every held path in the file back into the current space. A stored
value wins over the one a config seeded."
  (let ((rows (%ask store
                    (lambda (state)
                      (let ((db (%db state)))
                        (when db
                          (sqlite:execute-to-list
                           db "SELECT path, value, bound FROM held ORDER BY path")))))))
    (let ((*restoring* t))
      (dolist (row rows)
        (destructuring-bind (text value bound) row
          (let ((path (p:parse text))
                (held (%in value)))
            (cond
              ;; a ring comes back the way it was made: the bound, then each
              ;; entry pushed oldest to newest
              ((and bound (fset:seq? held))
               (setf (ns:setting path :max) bound)
               (loop :for i :from (1- (fset:size held)) :downto 0
                     :do (ns:write path (fset:lookup held i))))
              (t (ns:write path held)
                 (when bound (setf (ns:setting path :max) bound))))))))
    (length rows)))

(defun open (&optional path)
  "Open the file at PATH, by default XDG data home pine/pine.db, read every held
path back into the current space, and keep it written through from here on.
Answers the store, which is what closes it. \":memory:\" opens one that lives
as long as the image."
  (let ((target (if path
                    (if (pathnamep path) (namestring path) path)
                    (namestring (uiop:xdg-data-home "pine/pine.db"))))
        (store (%store (sento.agent:make-agent (lambda () (fset:empty-map))))))
    (unless (string= target ":memory:")
      (ensure-directories-exist target))
    (%change store (lambda (state)
                     (declare (ignore state))
                     {:db (%connect target)}))
    (restore store)
    (setf (ns:on-commit :store) (lambda (moved) (record store moved)))
    store))

(defun close (store)
  "Close STORE and let its thread go. The space stops writing through."
  (setf (ns:on-commit :store) nil)
  (%change store (lambda (state)
                   (let ((db (%db state)))
                     (when db (sqlite:disconnect db)))
                   (fset:empty-map)))
  (sento.agent:agent-stop (store-agent store))
  nil)

(ns:serve :store
  ;; last, because restoring writes into every subtree that keeps a path, and a
  ;; stored value has to win over the one a config seeded
  {:after [:mode :win :cmd :key :buf :echo :theme]
   :doc "what outlives the daemon, and the one file it lives in. A sqlite
handle is a thing and not a value, so the space keeps it: one pine is one file."
   ;; open the file, read every held path back, and keep it written through
   :up (lambda () (open (ns:given :store-path)))
   :down (lambda (store) (when store (close store)))})
