(defpackage #:pine.editor.ask
  (:use #:cl)
  (:export #:ask #:tell)
  (:documentation "Ask and tell: the scripting surface over the live system.
ASK queries the server, the client or a buffer; TELL messages a buffer. Above
modes and commands, so it can read the registries that hold them."))

(in-package #:pine.editor.ask)

(defun tell (target tag &rest plist)
  "Send (tag . plist) to TARGET (coerced via BUFFER). Returns TARGET.
Silently no-ops on nil target."
  (let ((buf (pine.editor.frame:buffer target)))
    (when buf
      (sento.actor:tell buf (list* tag plist)))
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
      (:modes       (pine.editor.mode:all-mode-names))
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
      (:kill-ring      (pine.editor.frame:kill-ring c))
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
