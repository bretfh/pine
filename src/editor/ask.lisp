(in-package :pine.ask)

;;;; Ask and tell: the scripting surface over the live system. ASK queries
;;;; the server, the client or a buffer; TELL messages a buffer. It sits
;;;; above modes and commands so that (ask :server :modes) and
;;;; (ask :server :commands) can read the registries that actually hold them.

(defun tell (target tag &rest plist)
  "Send (tag . plist) to TARGET (coerced via BUFFER). Returns TARGET.
Silently no-ops on nil target."
  (let ((buf (pine.client:buffer target)))
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
  (let ((srv (pine.client:server-of (pine.client:current-client)))
        (query (first spec)))
    (case query
      (:buffers     (loop for k being the hash-keys of (pine.buffer:buffer-table srv)
                          collect k))
      (:clients     (pine.server:clients srv))
      (:modes       (pine.mode:all-mode-names))
      (:commands    (pine.command:all-command-names))
      (:faces       (pine.server:faces srv))
      (:actor-system (pine.server:actor-system srv))
      (:describe    +server-verbs+)
      (t (error "unknown :server query ~s; known: ~s" query +server-verbs+)))))

(defun %ask-client (spec)
  (let ((c (pine.client:current-client))
        (query (first spec)))
    (case query
      (:current-buffer (pine.client:current-buffer c))
      (:focused-window (pine.client:focused-window c))
      (:windows        (pine.client:windows c))
      (:kill-ring      (pine.client:kill-ring c))
      (:last-command   (pine.client:last-command c))
      (:pending-keys   (car (pine.client:pending-keys c)))
      (:describe       +client-verbs+)
      (t (error "unknown :client query ~s; known: ~s" query +client-verbs+)))))

(defun %ask-buffer (buf spec)
  (when buf
    (let ((query (first spec))
          (args  (rest spec))
          (timeout 5))
      (case query
        (:state    (sento.actor:ask-s buf '(:get-state) :time-out timeout))
        (:snapshot (sento.actor:ask-s buf '(:get-snapshot) :time-out timeout))
        (:text     (sento.actor:ask-s buf '(:get-text) :time-out timeout))
        (:meta     (pine.buffer:meta (sento.actor:ask-s buf '(:get-state)
                                            :time-out timeout)))
        (:name     (pine.buffer:name (sento.actor:ask-s buf '(:get-snapshot)
                                            :time-out timeout)))
        (:mode     (pine.buffer:buffer-local
                    (sento.actor:ask-s buf '(:get-state) :time-out timeout)
                    :mode))
        (:pathname (pine.buffer:buffer-local
                    (sento.actor:ask-s buf '(:get-state) :time-out timeout)
                    :pathname))
        (:point    (let ((s (sento.actor:ask-s buf '(:get-snapshot)
                                               :time-out timeout)))
                     (values (pine.buffer:point-line s) (pine.buffer:point-col s))))
        (:line     (let* ((n (first args))
                          (s (sento.actor:ask-s buf '(:get-snapshot)
                                                :time-out timeout)))
                     (fset:@ (pine.buffer:lines s) n)))
        (:local    (let ((key (first args))
                         (default (getf (rest args) :default)))
                     (pine.buffer:buffer-local
                      (sento.actor:ask-s buf '(:get-state) :time-out timeout)
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
    (t (%ask-buffer (pine.client:buffer target) spec))))
