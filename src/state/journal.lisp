(defpackage #:pine.state.journal
  (:use #:cl)
  (:export
   #:*origin*
   #:*snapshot-source*
   #:*flush-interval*
   #:*compact-after*
   #:record!
   #:snapshot!
   #:flush-journal
   #:start-journal-writer
   #:stop-journal-writer
   #:buffer-snapshot
   #:journal-since
   #:journal-count
   #:journaled-buffers
   #:forget-buffer
   #:restore-state)
  (:documentation "Working state that survives the daemon.

Restarting the daemon is the repair tool: you restart it when the image is
wedged or a definition went bad, which is exactly when you most want your
buffers. Until now that cost every unsaved one.

An edit is a row, a snapshot is compaction, and recovery is the newest snapshot
plus the rows after it replayed through the ordinary verb path. The same rows are
what a second pine would need to follow this one, so durability and replication
are one mechanism rather than two."))

(in-package #:pine.state.journal)

(defparameter *origin* (machine-instance)
  "Which daemon produced a row. Rows from elsewhere can share the table without
being mistaken for this image's own.")

(defparameter *flush-interval* 0.3
  "Seconds of quiet before pending edits are committed. One transaction per
flush: a row costs a microsecond, a commit costs a millisecond, so the batching
is the whole difference.")

(defparameter *compact-after* 400
  "Journal rows for one buffer before its content is snapshotted and the rows
dropped. Bounds both the file and how much replay a restart does.")

;;;; The writer. Edits arrive from buffer actors, which must not wait for a
;;;; disk, so they leave a row here and go. One thread owns the writing.

(defvar *pending* nil "Rows not yet committed, newest first.")
(defvar *lock* (bordeaux-threads:make-lock "pine-journal"))
(defvar *wake* (bordeaux-threads:make-condition-variable))
(defvar *writer* nil)
(defvar *stop* nil)
(defvar *snapshot-source* nil
  "Function of a buffer id answering (values name meta content tick), installed
by the layer that owns buffers. Without one, compaction has nothing to write and
the journal simply grows.")

(defun record! (id tick verb)
  "Note that VERB took buffer ID to TICK. Returns immediately: the row is
committed by the writer, on its own thread."
  (when id
    (bordeaux-threads:with-lock-held (*lock*)
      (push (list id tick *origin* verb) *pending*)
      (bordeaux-threads:condition-notify *wake*))))

(defun %take-pending ()
  (bordeaux-threads:with-lock-held (*lock*)
    (prog1 (nreverse *pending*) (setf *pending* nil))))

(defun flush-journal ()
  "Commit the pending rows in one transaction. Answers how many were written."
  (let ((rows (%take-pending)))
    (when rows
      (pine.state.store:with-db (db)
        (sqlite:with-transaction db
          (dolist (row rows)
            (destructuring-bind (id tick origin verb) row
              (sqlite:execute-non-query
               db "INSERT INTO journal (id, tick, origin, verb) VALUES (?,?,?,?)"
               (pine.state.store:write-value id) tick
               (pine.state.store:write-value origin)
               (pine.state.store:write-value verb))))))
      (%compact-if-needed (remove-duplicates (mapcar #'first rows) :test #'equal)))
    (length rows)))

(defun journal-count (id)
  "Rows held for buffer ID."
  (or (pine.state.store:with-db (db)
        (sqlite:execute-single db "SELECT count(*) FROM journal WHERE id = ?"
                               (pine.state.store:write-value id)))
      0))

(defun %compact-if-needed (ids)
  (dolist (id ids)
    (when (and *snapshot-source* (>= (journal-count id) *compact-after*))
      (multiple-value-bind (name meta content tick) (funcall *snapshot-source* id)
        (when name (snapshot! id name meta content tick))))))

(defun %meta->list (meta)
  "META as readable pairs. An fset map does not print readably, and the store
refuses anything it cannot read back, which is the guarantee worth keeping."
  (when meta
    (let (pairs)
      (fset:do-map (key value meta)
        ;; a builder closure or a node tree belongs to a live layout, not to a
        ;; buffer's identity, so it is not carried into the store
        (unless (member key '(:layout-builder :layout-tree :layout-rows :overlays))
          (push (cons key value) pairs)))
      (nreverse pairs))))

(defun %list->meta (pairs)
  (let ((map (fset:empty-map)))
    (dolist (pair pairs map)
      (setf map (fset:with map (car pair) (cdr pair))))))

(defun snapshot! (id name meta content tick)
  "Write ID's content as of TICK and drop the rows it covers. Compaction: the
snapshot says what the journal no longer has to."
  (pine.state.store:with-db (db)
    (sqlite:with-transaction db
      (sqlite:execute-non-query
       db "INSERT OR REPLACE INTO buffer (id, name, origin, tick, meta, content)
           VALUES (?,?,?,?,?,?)"
       (pine.state.store:write-value id)
       (pine.state.store:write-value name)
       (pine.state.store:write-value *origin*)
       tick
       (pine.state.store:write-value (%meta->list meta))
       content)
      (sqlite:execute-non-query
       db "DELETE FROM journal WHERE id = ? AND tick <= ?"
       (pine.state.store:write-value id) tick)))
  id)

(defun buffer-snapshot (id)
  "(values NAME META CONTENT TICK) for ID, or NIL when it has never been
snapshotted."
  (pine.state.store:with-db (db)
    (multiple-value-bind (name meta content tick)
        (sqlite:execute-one-row-m-v
         db "SELECT name, meta, content, tick FROM buffer WHERE id = ?"
         (pine.state.store:write-value id))
      (when name
        (values (pine.state.store:read-value name)
                (%list->meta (pine.state.store:read-value meta))
                content
                tick)))))

(defun journal-since (id tick)
  "ID's rows after TICK, oldest first, as (TICK ORIGIN VERB)."
  (pine.state.store:with-db (db)
    (mapcar (lambda (row)
              (destructuring-bind (rtick origin verb) row
                (list rtick
                      (pine.state.store:read-value origin)
                      (pine.state.store:read-value verb))))
            (sqlite:execute-to-list
             db "SELECT tick, origin, verb FROM journal WHERE id = ? AND tick > ?
                 ORDER BY tick ASC, rowid ASC"
             (pine.state.store:write-value id) tick))))

(defun journaled-buffers ()
  "Every buffer id the store knows, with its name, as (ID . NAME)."
  (pine.state.store:with-db (db)
    (mapcar (lambda (row)
              (cons (pine.state.store:read-value (first row))
                    (pine.state.store:read-value (second row))))
            (sqlite:execute-to-list db "SELECT id, name FROM buffer ORDER BY name"))))

(defun forget-buffer (id)
  "Drop ID's snapshot and rows."
  (pine.state.store:with-db (db)
    (sqlite:with-transaction db
      (sqlite:execute-non-query db "DELETE FROM buffer WHERE id = ?"
                                (pine.state.store:write-value id))
      (sqlite:execute-non-query db "DELETE FROM journal WHERE id = ?"
                                (pine.state.store:write-value id)))))

(defun restore-state (id)
  "(values NAME META CONTENT TICK VERBS) for ID: its snapshot and the verbs
recorded after it. The caller replays VERBS through the buffer's own dispatch, so
recovery uses the editing path rather than a second implementation of it."
  (multiple-value-bind (name meta content tick) (buffer-snapshot id)
    (when name
      (values name meta content tick
              (mapcar #'third (journal-since id tick))))))

;;;; The writer thread

(defun start-journal-writer ()
  "Start the thread that commits pending edits. Idempotent."
  (unless (and *writer* (bordeaux-threads:thread-alive-p *writer*))
    (setf *stop* nil
          *writer*
          (bordeaux-threads:make-thread
           (lambda ()
             (loop :until *stop*
                   :do (bordeaux-threads:with-lock-held (*lock*)
                         (unless *pending*
                           (bordeaux-threads:condition-wait *wake* *lock*
                                                            :timeout *flush-interval*)))
                       (handler-case (flush-journal)
                         (error (c)
                           (pine.core.eval:report-failure c "writing the buffer journal")
                           ;; a store that cannot be written to would otherwise
                           ;; spin this thread as fast as the errors arrive
                           (sleep 1)))))
           :name "pine-journal")))
  *writer*)

(defun stop-journal-writer ()
  "Stop the writer after one last flush."
  (setf *stop* t)
  (bordeaux-threads:with-lock-held (*lock*)
    (bordeaux-threads:condition-notify *wake*))
  (when (and *writer* (bordeaux-threads:thread-alive-p *writer*))
    (ignore-errors (bordeaux-threads:join-thread *writer* :timeout 2)))
  (setf *writer* nil)
  (flush-journal))
