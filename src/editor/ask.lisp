(defpackage #:pine.editor.ask
  (:use #:cl)
  (:export #:ask #:tell)
  (:documentation "Ask and tell: the scripting surface over the live system.
ASK queries the server, the client or a buffer; TELL messages a buffer. Above
modes and commands, so it can read the registries that hold them."))

(in-package #:pine.editor.ask)
(named-readtables:in-readtable pine.path:syntax)

(defun tell (target tag &rest plist)
  "Do TAG to TARGET's buffer. Returns the buffer, or nil when there is none.

A local is a place, so setting one is a write; everything else is a verb on the
buffer's text. Both have landed when this answers."
  (let ((buf (pine.editor.frame:buffer target)))
    (when buf
      (case tag
        ((:set-local :set-meta)
         (pine.text.buffer:put buf (getf plist :key) (getf plist :value)))
        (:move-point
         (pine.text.buffer:put-point buf (getf plist :line) (getf plist :col)))
        (:insert (pine.text.buffer:edit buf (fset:seq :insert (getf plist :text))))
        (:backspace (pine.text.buffer:delete-back buf))
        ((:newline :undo :redo) (pine.text.buffer:edit buf (fset:seq tag)))
        (:delete-region
         (pine.text.buffer:edit
          buf (fset:seq :delete
                        (fset:seq (getf plist :start-line) (getf plist :start-col))
                        (fset:seq (getf plist :end-line) (getf plist :end-col)))))
        (:replace-content
         (pine.ns:write (pine.text.buffer:at (pine.text.buffer:name-of buf) :text)
                        (getf plist :content)))
        ;; the verbs /buf does not serve yet -- indenting a region, projecting a
        ;; layout -- are still the buffer's own
        (t (sento.actor:tell buf (list* tag plist)))))
    buf))

(defparameter +server-verbs+
  '(:buffers :clients :modes :commands :faces :actor-system :describe))

(defparameter +client-verbs+
  '(:current-buffer :focused-window :windows :kill-ring :last-command
    :pending-keys :describe))

(defparameter +buffer-verbs+
  '(:state :snapshot :text :meta :name :mode :pathname :point :line :local
    :describe))

(defun %ask-server (spec)
  (let ((srv (pine.editor.frame:server-of (pine.editor.frame:current-client)))
        (query (first spec)))
    (case query
      (:buffers     (loop for k being the hash-keys of (pine.text.buffer:buffer-table srv)
                          collect k))
      (:clients     (pine.core.server:clients srv))
      (:modes       (pine.mode:names))
      (:commands    (pine.editor.command:all-command-names))
      (:faces       (pine.ui.face:faces-table))
      (:actor-system (pine.core.server:actor-system srv))
      (:describe    +server-verbs+)
      (t (error "unknown :server query ~s; known: ~s" query +server-verbs+)))))

(defun %ask-client (spec)
  (let ((c (pine.editor.frame:current-client))
        (query (first spec)))
    (case query
      (:current-buffer (pine.editor.frame:current-buffer c))
      (:focused-window (pine.editor.frame:focused-window c))
      (:windows        (pine.editor.frame:windows c))
      (:kill-ring      (fset:convert (quote list) (or (pine.ns:held /kill) (fset:empty-seq))))
      (:last-command   (pine.editor.frame:last-command c))
      (:pending-keys   (car (pine.editor.frame:pending-keys c)))
      (:describe       +client-verbs+)
      (t (error "unknown :client query ~s; known: ~s" query +client-verbs+)))))

(defun %ask-buffer (buf spec)
  "What BUF says. Every one of these is a read of the buffer's leaves, so
nothing waits on the actor and this answers just as well with no actor at all."
  (when buf
    (let ((query (first spec))
          (args  (rest spec)))
      (case query
        (:state    (pine.text.buffer:state-of buf))
        (:snapshot (pine.text.buffer:snapshot-of buf))
        (:text     (pine.text.buffer:text-of buf))
        (:meta     (pine.text.buffer:meta (pine.text.buffer:state-of buf)))
        (:name     (pine.text.buffer:name-of buf))
        (:mode     (pine.text.buffer:buffer-local
                    (pine.text.buffer:state-of buf) :mode))
        (:pathname (pine.text.buffer:buffer-local
                    (pine.text.buffer:state-of buf) :pathname))
        (:point    (let ((s (pine.text.buffer:snapshot-of buf)))
                     (values (pine.text.buffer:point-line s)
                             (pine.text.buffer:point-col s))))
        (:line     (fset:@ (pine.text.buffer:lines (pine.text.buffer:snapshot-of buf))
                           (first args)))
        (:local    (pine.text.buffer:buffer-local
                    (pine.text.buffer:state-of buf)
                    (first args) (getf (rest args) :default)))
        (:describe +buffer-verbs+)
        (t (error "unknown buffer query ~s; known: ~s" query +buffer-verbs+))))))

(defun ask (target &rest spec)
  "Synchronous query. TARGET is :server, :client, or anything coercible
via BUFFER. SPEC is (query &rest args). Use (ask TARGET :describe) for
the verb list."
  (case target
    (:server (%ask-server spec))
    (:client (%ask-client spec))
    (t (%ask-buffer (pine.editor.frame:buffer target) spec))))
