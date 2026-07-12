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
                   (let* ((uri (format nil "sento://~a:~d/user/agent" host port))
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
    local))


(defvar *agent-port* 18100
  "Next remoting port for a spawned process agent.")

(defun %agent-script (name master-port self-port)
  "The source a spawned SBCL process agent runs: a fresh actor-system with
remoting and a /user/agent actor that evals forms (isolated in its own image),
then registers back with the master's agent-registry and stays up."
  (format nil
"(require :asdf)
(asdf:load-system :sento)
(asdf:load-system :sento-remoting)
(defvar *sys* (sento.actor-system:make-actor-system '(:dispatchers (:shared (:workers 2 :strategy :random)))))
(sento.remoting:enable-remoting *sys* :host \"127.0.0.1\" :port ~d)
(sento.actor-context:actor-of *sys* :name \"agent\"
  :receive (lambda (msg)
    (case (first msg)
      (:ping (sento.actor:reply :pong))
      (:eval (sento.actor:reply
              (handler-case (eval (read-from-string (getf (rest msg) :form)))
                (error (e) (list :err (princ-to-string e))))))
      (:crash (sb-ext:exit :abort t))
      (t (sento.actor:reply :unknown)))))
(sento.actor:tell
  (sento.remoting:make-remote-ref *sys* \"sento://127.0.0.1:~d/user/agent-registry\")
  (list :register-remote :name ~s :host \"127.0.0.1\" :port ~d))
(loop (sleep 3600))"
          self-port master-port name self-port))

(defun spawn-agent (server name)
  "Spawn a real SBCL process agent: it enables remoting, evals in its own image,
and registers back to this daemon. It can loop, block, or crash in isolation
without touching the daemon or any app. Returns the agent-info once it connects."
  (let ((master-port (pine.server:remoting-port server)))
    (unless master-port (error "Cannot spawn agent: remoting not enabled."))
    (let* ((self-port (incf *agent-port*))
           (tmp (format nil "/tmp/pine-agent-~a.lisp" name)))
      (with-open-file (s tmp :direction :output :if-exists :supersede)
        (write-string (%agent-script name master-port self-port) s))
      (uiop:launch-program (list "sbcl" "--non-interactive" "--load" tmp)
                           :output nil :error-output nil)
      (loop for i from 0 below 400
            for info = (ignore-errors (find-agent server name))
            when info return info
            do (sleep 0.25)
            finally (error "Agent ~s did not connect in time." name)))))

(defun kill-agent (server name)
  (let ((info (find-agent server name)))
    (when info
      (handler-case
          (act:ask-s (agent-info-actor info) '(:shutdown) :time-out 5)
        (error () nil))
      (unregister-agent server name))))
