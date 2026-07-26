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
  (when buf
    (let ((query (first spec))
          (args  (rest spec))
          (timeout 5))
      (case query
        (:state    (pine.core.actor:ask buf '(:get-state) :timeout timeout))
        (:snapshot (pine.core.actor:ask buf '(:get-snapshot) :timeout timeout))
        (:text     (pine.core.actor:ask buf '(:get-text) :timeout timeout))
        (:meta     (pine.text.buffer:meta (pine.core.actor:ask buf '(:get-state)
                                            :timeout timeout)))
        (:name     (pine.text.buffer:name (pine.core.actor:ask buf '(:get-snapshot)
                                            :timeout timeout)))
        (:mode     (pine.text.buffer:buffer-local
                    (pine.core.actor:ask buf '(:get-state) :timeout timeout)
                    :mode))
        (:pathname (pine.text.buffer:buffer-local
                    (pine.core.actor:ask buf '(:get-state) :timeout timeout)
                    :pathname))
        (:point    (let ((s (pine.core.actor:ask buf '(:get-snapshot)
                                               :timeout timeout)))
                     (values (pine.text.buffer:point-line s) (pine.text.buffer:point-col s))))
        (:line     (let* ((n (first args))
                          (s (pine.core.actor:ask buf '(:get-snapshot)
                                                :timeout timeout)))
                     (fset:@ (pine.text.buffer:lines s) n)))
        (:local    (let ((key (first args))
                         (default (getf (rest args) :default)))
                     (pine.text.buffer:buffer-local
                      (pine.core.actor:ask buf '(:get-state) :timeout timeout)
                      key default)))
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
