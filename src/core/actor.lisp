(defpackage #:pine.core.actor
  (:use :cl :ac :act :asys :rem)
  (:shadow #:ask)
  (:export
   #:agent-info
   #:agent-info-name #:agent-info-type #:agent-info-actor
   #:agent-info-meta #:agent-info-port
   #:start-agent-registry
   #:register-agent #:unregister-agent #:find-agent #:list-agents
   #:agent-eval #:agent-compile #:agent-run #:*local-agent*
   #:start-local-agent #:start-agent-debug
   #:remote #:remote-agent #:remote-eval-id
   #:agent-alive-p #:request
   #:spawn-agent #:kill-agent
   #:ask #:in-actor-p #:blocking-ask-in-receive
   #:blocking-ask-target #:blocking-ask-message))

(in-package #:pine.core.actor)

;;;; An actor's receive runs on a thread that owes its mailbox an answer, so it
;;;; must never wait for one. ASK is the one blocking query and it refuses from
;;;; inside a receive rather than hanging there.
;;;;
;;;; Which agents there are is therefore a value in a cell, not an actor's
;;;; private table. The registry actor stays as the endpoint a spawned image
;;;; registers itself to; what it does is swap the cell.

(define-condition blocking-ask-in-receive (error)
  ((target :initarg :target :reader blocking-ask-target)
   (message :initarg :message :reader blocking-ask-message))
  (:report (lambda (c stream)
             (format stream "Blocking ask of ~a from inside an actor's receive: ~s.
Read the state the receive was handed, or tell and take the reply as a message."
                     (blocking-ask-target c) (blocking-ask-message c)))))

(defstruct agent-info
  name type actor meta port)

(defclass remote (pine.err:fault)
  ((server :initarg :server :reader remote-server)
   (agent :initarg :agent :reader remote-agent)
   (eval-id :initarg :eval-id :reader remote-eval-id))
  (:documentation "A fault in a process agent, recorded at this image's /err.
The decision goes back to the image whose thread is standing in it."))

(defvar *local-agent* nil
  "The local agent's actor, cached when it starts, so agent-eval :local routes
by ref rather than through a registry lookup on a hot path.")

(defun in-actor-p ()
  "True on a thread inside an actor's receive. Read in value position: act:*self*
is a symbol macro, so BOUNDP answers about the macro name and is false anywhere."
  (and act:*self* t))

(defun ask (target message &key (timeout 5))
  "Send MESSAGE to TARGET and wait up to TIMEOUT seconds for its reply. Signals
BLOCKING-ASK-IN-RECEIVE from inside a receive, which is the one place this may
not be used."
  (when (in-actor-p)
    (error 'blocking-ask-in-receive :target target :message message))
  (act:ask-s target message :time-out timeout))

(defun %agents (server)
  (sento.atomic:atomic-get (pine.core.server:agent-registry server)))

(defun %remember (server info)
  (sento.atomic:atomic-swap
   (pine.core.server:agent-registry server)
   (lambda (all) (fset:with all (agent-info-name info) info)))
  info)

(defun register-agent (server name type actor &key meta port)
  (%remember server (make-agent-info :name name :type type :actor actor
                                     :meta meta :port port)))

(defun unregister-agent (server name)
  (sento.atomic:atomic-swap (pine.core.server:agent-registry server)
                            (lambda (all) (fset:less all name)))
  t)

(defun find-agent (server name)
  (fset:lookup (%agents server) name))

(defun list-agents (server)
  (let ((acc nil))
    (fset:do-map (name info (%agents server))
      (declare (ignore name))
      (push info acc))
    (nreverse acc)))

(defun start-agent-registry (server)
  (let ((sys (pine.core.server:actor-system server)))
    (setf (pine.core.server:agent-registry server)
          (sento.atomic:make-atomic-reference :value (fset:empty-map)))
    (ac:actor-of sys
      :name "agent-registry"
      :dispatcher :pinned
      :receive
      (lambda (msg)
        (case (first msg)
          (:register
           (destructuring-bind (&key info) (rest msg)
             (reply (%remember server info))))
          (:register-remote
           (destructuring-bind (&key name host port) (rest msg)
             (let ((uri (pine.core.server:daemon-uri "agent" :host host :port port)))
               (reply (%remember server
                                 (make-agent-info :name name :type :process
                                                  :actor (rem:make-remote-ref sys uri)
                                                  :port port))))))
          (:unregister
           (destructuring-bind (&key name) (rest msg)
             (unregister-agent server name)
             (reply t)))
          (:lookup
           (destructuring-bind (&key name) (rest msg)
             (reply (find-agent server name))))
          (:list (reply (list-agents server)))
          (t (reply (list :error :unknown msg))))))))

(defgeneric resolve-agent (server agent-or-name)
  (:documentation "The actor ref AGENT-OR-NAME names: an agent-info, a
registered name, or a ref already.")
  (:method (server x) (declare (ignore server)) x))

(defmethod resolve-agent (server (x agent-info))
  (declare (ignore server))
  (agent-info-actor x))

(defmethod resolve-agent (server (x string))
  (let ((info (find-agent server x)))
    (unless info (error "No agent named ~s" x))
    (agent-info-actor info)))

(defun agent-eval (server agent-or-name form-string &key on-done package
                                                        readtable bindings)
  "Evaluate FORM-STRING in AGENT-OR-NAME through pine.err, off the agent's
mailbox thread. Returns at once; the result reaches ON-DONE and a failure lands
at /err.

READTABLE is a named-readtable name rather than a readtable: a name is a symbol,
so it crosses to another image the way everything else on the wire does."
  (act:tell (resolve-agent server agent-or-name)
            (list :eval :form form-string :package package :readtable readtable
                  :bindings bindings :on-done on-done)))

(defun agent-compile (server agent-or-name text &key on-done package
                                                     readtable bindings)
  (act:tell (resolve-agent server agent-or-name)
            (list :compile :text text :package package :readtable readtable
                  :bindings bindings :on-done on-done)))

(defun agent-run (server agent-or-name thunk &key on-done package)
  "Run THUNK, a live in-image closure, in AGENT-OR-NAME through pine.err. One
addressable eval path: a widget click goes here like anything else."
  (act:tell (resolve-agent server agent-or-name)
            (list :run :thunk thunk :package package :on-done on-done)))

(defun start-local-agent (server)
  (let* ((sys (pine.core.server:actor-system server))
         (local (ac:actor-of sys
                  :name "local-agent"
                  :dispatcher :pinned
                  :receive
                  (lambda (msg)
                    (case (first msg)
                      ((:eval :compile)
                       (destructuring-bind (&key form text package readtable
                                            bindings on-done)
                           (rest msg)
                         (let ((source (or form text)))
                           (when source
                             (pine.err:evaluate-string
                              source
                              :package (or (and package (find-package package))
                                           (find-package :cl-user))
                              :readtable readtable
                              :bindings bindings
                              :on-done on-done)))
                         (reply :started)))
                      (:run
                       (destructuring-bind (&key thunk package on-done) (rest msg)
                         (when thunk
                           (pine.err:evaluate-thunk
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

(defmethod pine.err:offers ((f remote))
  (pine.err:fault-offered f))

(defmethod pine.err:described ((f remote))
  (fset:with (fset:with (call-next-method) :agent (remote-agent f))
             :eval-id (remote-eval-id f)))

(defmethod pine.err:resume ((f remote) name)
  (let ((info (find-agent (remote-server f) (remote-agent f))))
    (when info
      (act:tell (agent-info-actor info)
                (list :resume :eval-id (remote-eval-id f) :restart (string name))))
    (pine.err:forget f)
    (and info t)))

(defun %remote-fault (agent eval-id)
  (find-if (lambda (f)
             (and (typep f 'remote)
                  (equal agent (remote-agent f))
                  (eql eval-id (remote-eval-id f))))
           (pine.err:faults)))

(defun %note-remote (server msg)
  "Put an agent's fault at this image's /err, unless it is already there. An
agent reports what it is stopped at rather than what has just happened."
  (destructuring-bind (&key agent eval-id condition restarts &allow-other-keys)
      (rest msg)
    (unless (%remote-fault agent eval-id)
      (pine.err:note
       (make-instance 'remote
                      :server server :agent agent :eval-id eval-id
                      :label (format nil "agent ~a" agent)
                      :condition (or condition "")
                      :offered (mapcar (lambda (r) (list r nil))
                                       (remove nil restarts)))))))

(defun start-agent-debug (server)
  "The master's receiver for cross-image errors: a process agent's fault ships
its restart list here, by name, and joins /err beside the local ones."
  (sento.actor-context:actor-of (pine.core.server:actor-system server)
    :name "agent-debug"
    :dispatcher :pinned
    :receive (lambda (msg)
               (case (first msg)
                 (:agent-debug
                  (pine.err:attempt (lambda () (%note-remote server msg))
                                    "a fault from an agent"))
                 (:agent-faults
                  (destructuring-bind (&key agent ids &allow-other-keys) (rest msg)
                    (dolist (f (pine.err:faults))
                      (when (and (typep f 'remote)
                                 (equal agent (remote-agent f))
                                 (not (member (remote-eval-id f) ids)))
                        (pine.err:forget f))))))
               nil)))

(defun %agent-command (name master-port self-port)
  "The argv for a spawned SBCL process agent: load :pine, connect back to this
daemon, and idle. Passed as --eval forms, so nothing is written to disk."
  (list "sbcl" "--non-interactive" "--no-userinit"
        "--eval" "(require :asdf)"
        "--eval" (format nil "(push #P~s asdf:*central-registry*)"
                         (namestring (asdf:system-source-directory :pine)))
        "--eval" "(asdf:load-system :pine)"
        "--eval" (format nil "(pine.core.agent:connect :name ~s :master-host ~s :master-port ~d :self-port ~d)"
                         name pine.core.server:*host* master-port self-port)
        "--eval" "(loop (sleep 3600))"))

(defun spawn-agent (server name)
  "Spawn an SBCL process agent: it enables remoting, evals in its own image, and
registers back here. Answers the agent-info once it connects."
  (let ((master-port (pine.core.server:remoting-port server)))
    (unless master-port (error "Cannot spawn agent: remoting not enabled."))
    (let ((self-port (pine.core.server:next-agent-port server)))
      (uiop:launch-program (%agent-command name master-port self-port)
                           :output nil :error-output nil)
      (loop for i from 0 below 400
            for info = (ignore-errors (find-agent server name))
            when info return info
            do (sleep 0.25)
            finally (error "Agent ~s did not connect in time." name)))))

(defun agent-alive-p (server name)
  (let ((info (ignore-errors (find-agent server name))))
    (and info
         (ignore-errors (eq :pong (ask (agent-info-actor info) '(:ping) :timeout 2))))))

(defun request (server capability name)
  "Broker a capability from the registry by name: an agent ref or a buffer actor."
  (ecase capability
    (:agent  (let ((info (find-agent server name))) (and info (agent-info-actor info))))
    (:buffer name)))

(defun kill-agent (server name)
  (let ((info (find-agent server name)))
    (when info
      (handler-case
          (ask (agent-info-actor info) '(:shutdown) :timeout 5)
        (error () nil))
      (unregister-agent server name))))
