(in-package :pine.actor)

(defstruct agent-info
  name type actor meta port)

(defun start-agent-registry (server)
  (let ((sys (pine.server:actor-system server)))
    (setf (pine.server:agent-registry server)
          (ac:actor-of sys
            :name "agent-registry"
            :state (make-hash-table :test 'equal)
            :receive
            (lambda (msg)
              (case (first msg)
                (:register
                 (destructuring-bind (&key info) (rest msg)
                   (setf (gethash (agent-info-name info) act:*state*) info)
                   (reply info)))
                (:register-remote
                 (destructuring-bind (&key name host port) (rest msg)
                   (let* ((uri (pine.server:daemon-uri "agent" :host host :port port))
                          (ref (rem:make-remote-ref sys uri))
                          (info (make-agent-info :name name :type :process
                                                 :actor ref :port port)))
                     (setf (gethash name act:*state*) info)
                     (reply info))))
                (:unregister
                 (destructuring-bind (&key name) (rest msg)
                   (remhash name act:*state*)
                   (reply t)))
                (:lookup
                 (destructuring-bind (&key name) (rest msg)
                   (reply (gethash name act:*state*))))
                (:list
                 (let ((agents nil))
                   (maphash (lambda (k v) (declare (ignore k)) (push v agents))
                            act:*state*)
                   (reply (nreverse agents))))
                (t (reply (list :error :unknown msg)))))))))

(defun register-agent (server name type actor &key meta port)
  (act:ask-s (pine.server:agent-registry server)
             (list :register :info (make-agent-info :name name :type type
                                                    :actor actor :meta meta :port port))
             :time-out 5))

(defun unregister-agent (server name)
  (act:ask-s (pine.server:agent-registry server) (list :unregister :name name) :time-out 5))

(defun find-agent (server name)
  (act:ask-s (pine.server:agent-registry server) (list :lookup :name name) :time-out 5))

(defun list-agents (server)
  (act:ask-s (pine.server:agent-registry server) '(:list) :time-out 5))

(defun resolve-agent (server agent-or-name)
  "The actor ref for AGENT-OR-NAME (an agent-info, a registered name, or a ref)."
  (etypecase agent-or-name
    (agent-info (agent-info-actor agent-or-name))
    (string (let ((info (find-agent server agent-or-name)))
              (unless info (error "No agent named ~s" agent-or-name))
              (agent-info-actor info)))
    (t agent-or-name)))

(defun agent-eval (server agent-or-name form-string &key on-done package bindings)
  "Evaluate FORM-STRING in AGENT-OR-NAME through pine.eval, off the agent's
mailbox thread. Returns immediately; the result reaches ON-DONE and any error
reaches the shared debugger surface."
  (act:tell (resolve-agent server agent-or-name)
            (list :eval :form form-string :package package
                  :bindings bindings :on-done on-done)))

(defun agent-compile (server agent-or-name text &key on-done package bindings)
  (act:tell (resolve-agent server agent-or-name)
            (list :compile :text text :package package
                  :bindings bindings :on-done on-done)))

(defun agent-run (server agent-or-name thunk &key on-done package)
  "Run THUNK (a live in-image closure) in AGENT-OR-NAME through pine.eval, off the
agent's mailbox thread. The desktop's widget-click path -- one addressable eval
path, :local by default."
  (act:tell (resolve-agent server agent-or-name)
            (list :run :thunk thunk :package package :on-done on-done)))


(defvar *local-agent* nil
  "The local agent's actor, cached when it starts, so callers route eval through
it (agent-eval :local) by ref without a blocking registry lookup on a hot path.")

(defun start-local-agent (server)
  (let* ((sys (pine.server:actor-system server))
         (local (ac:actor-of sys
                  :name "local-agent"
                  :receive
                  (lambda (msg)
                    (case (first msg)
                      ;; Evaluation runs through pine.eval on its own thread, so
                      ;; a looping or erroring form can neither block this actor's
                      ;; mailbox (and the shared dispatcher behind it) nor drop to
                      ;; a console debugger. Fire-and-forget: the result reaches
                      ;; ON-DONE and errors reach the shared *on-debug* surface,
                      ;; not a synchronous reply.
                      ((:eval :compile)
                       (destructuring-bind (&key form text package bindings on-done)
                           (rest msg)
                         (let ((source (or form text)))
                           (when source
                             (pine.eval:evaluate-string
                              source
                              :package (or (and package (find-package package))
                                           (find-package :cl-user))
                              :bindings bindings
                              :on-done on-done)))
                         (reply :started)))
                      ;; A thunk job (a live closure, in-image): the desktop's
                      ;; click path routes here, so a widget click is agent-eval
                      ;; :local like every other eval -- off this mailbox thread,
                      ;; through the one pine.eval engine.
                      (:run
                       (destructuring-bind (&key thunk package on-done) (rest msg)
                         (when thunk
                           (pine.eval:evaluate-thunk
                            thunk
                            :package (or (and package (find-package package))
                                         (find-package :cl-user))
                            :on-done on-done))
                         (reply :started)))
                      (:set-package
                       (destructuring-bind (&key name) (rest msg)
                         (let ((pkg (find-package name)))
                           (if pkg
                               (progn (setf *package* pkg)
                                      (reply (list :ok (package-name pkg))))
                               (reply (list :error (format nil "No package ~s" name)))))))
                      (:ping (reply :pong))
                      (:shutdown (reply :ok))
                      (t (reply (list :error :unknown-message (first msg)))))))))
    (register-agent server "local" :local local)
    (setf *local-agent* local)
    local))


(defvar *agent-port* 18100
  "Next remoting port for a spawned process agent.")

(defun %agent-command (name master-port self-port)
  "The argv for a spawned SBCL process agent: load :pine, connect back to this
daemon over the same pine.eval engine (shipping errors' restarts home by name),
and idle. Passed as --eval forms, so nothing is written to disk. Isolated: it can
loop, block, or crash in its own image without touching the daemon."
  (list "sbcl" "--non-interactive"
        "--eval" "(require :asdf)"
        "--eval" (format nil "(push #P~s asdf:*central-registry*)"
                         (namestring (asdf:system-source-directory :pine)))
        "--eval" "(asdf:load-system :pine)"
        "--eval" (format nil "(pine.agent:connect :name ~s :master-host ~s :master-port ~d :self-port ~d)"
                         name pine.server:*host* master-port self-port)
        "--eval" "(loop (sleep 3600))"))

(defvar *agent-debug-hook* nil
  "Called (message) for each :agent-debug / :agent-result from a process agent.
The editor (the helm) installs this to show the restart menu and drive resume.")

(defun start-agent-debug (server)
  "The master's receiver for cross-image errors: a process agent's error ships
its restart list here, by name, and the helm drives the choice back."
  (sento.actor-context:actor-of (pine.server:actor-system server)
    :name "agent-debug"
    :receive (lambda (msg)
               (when *agent-debug-hook* (ignore-errors (funcall *agent-debug-hook* msg)))
               nil)))

(defun spawn-agent (server name)
  "Spawn a real SBCL process agent: it enables remoting, evals in its own image,
and registers back to this daemon. It can loop, block, or crash in isolation
without touching the daemon or any app. Returns the agent-info once it connects."
  (let ((master-port (pine.server:remoting-port server)))
    (unless master-port (error "Cannot spawn agent: remoting not enabled."))
    (let ((self-port (incf *agent-port*)))
      (uiop:launch-program (%agent-command name master-port self-port)
                           :output nil :error-output nil)
      (loop for i from 0 below 400
            for info = (ignore-errors (find-agent server name))
            when info return (progn (supervise-agent name) info)
            do (sleep 0.25)
            finally (error "Agent ~s did not connect in time." name)))))

;;;; Supervision. The registry watches process agents: it pings each supervised
;;;; agent on an interval and, when one is dead (ping fails / no info), respawns
;;;; it. The check runs on its own dedicated thread, never the shared pool. Let
;;;; it crash: an isolated agent can die and be brought back without the daemon
;;;; noticing.

(defvar *supervised* (make-hash-table :test 'equal)
  "process-agent name -> t: agents the supervisor keeps alive.")

(defun supervise-agent (name) (setf (gethash name *supervised*) t))
(defun unsupervise-agent (name) (remhash name *supervised*))

(defun agent-alive-p (server name)
  (let ((info (ignore-errors (find-agent server name))))
    (and info
         (ignore-errors (eq :pong (act:ask-s (agent-info-actor info) '(:ping) :time-out 2))))))

(defun start-agent-supervisor (server &key (interval 3))
  "Watch every supervised process agent; respawn any that has died. Runs on its
own thread."
  (bordeaux-threads:make-thread
   (lambda ()
     (loop
       (sleep interval)
       (let (names)
         (maphash (lambda (k v) (declare (ignore v)) (push k names)) *supervised*)
         (dolist (name names)
           (unless (agent-alive-p server name)
             (ignore-errors (spawn-agent server name)))))))
   :name "pine-agent-supervisor"))

(defun request (server capability name)
  "Broker a capability from the registry by name: an agent ref or a buffer actor.
Apps ask the registry for a tool rather than reinventing it."
  (ecase capability
    (:agent  (let ((info (find-agent server name))) (and info (agent-info-actor info))))
    (:buffer (gethash name (pine.server:buffer-table server)))))

(defun kill-agent (server name)
  (let ((info (find-agent server name)))
    (when info
      (handler-case
          (act:ask-s (agent-info-actor info) '(:shutdown) :time-out 5)
        (error () nil))
      (unregister-agent server name))))
