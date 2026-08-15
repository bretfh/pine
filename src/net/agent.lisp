(defpackage #:pine/net/agent
  (:use #:cl)
  (:local-nicknames (#:endpoint #:pine/run/agent) (#:d #:pine/data) (#:server #:pine/net/server)
                    (#:attach #:pine/net/attach) (#:fault #:pine/run/fault)
                    (#:session #:pine/repl/session) (#:log #:pine/run/log))
  (:export #:remote-session #:open-remote #:answer-for #:serve #:agents
           #:agent #:agent-named #:name #:there #:evaluate-there
           #:register #:forget #:*agents* #:listen-for-agents))
(in-package #:pine/net/agent)

(defvar *agents* (d:table))
(defvar *timeout* 30)

(defclass agent ()
  ((name  :initarg :name  :reader name)
   (there :initarg :there :accessor there)
   (uri   :initarg :uri   :accessor uri :initform nil)))

(defmethod print-object ((a agent) stream)
  (print-unreadable-object (a stream :type t)
    (write-string (name a) stream)))

(defclass remote-session (session:session)
  ((agent :initarg :agent :reader agent)))

(defun register (name there &key uri)
  (let ((a (make-instance 'agent :name name :there there :uri uri)))
    (d:keep! *agents* name a)
    a))

(defun forget (name)
  (d:drop! *agents* name)
  name)

(defun agents () (d:vals (d:all *agents*)))

(defun agent-named (name) (d:at (d:all *agents*) name))

(defun open-remote (a &rest initargs)
  (apply #'make-instance 'remote-session :agent a :name (name a) initargs))

(defmethod session:evaluate ((s remote-session) form)
  (let ((answer (evaluate-there (agent s) form)))
    (make-instance 'session:evaluation
                   :form form
                   :answered (and (not (getf answer :fault)) (getf answer :answered))
                   :fault (getf answer :fault)
                   :said (or (getf answer :said) ""))))

(defun evaluate-there (a form &key (timeout *timeout*))
  (let ((reply (sento.actor:ask-s (there a) (list :evaluate form) :time-out timeout)))
    (if (and (consp reply) (eq :ok (car reply)))
        (cdr reply)
        (list :fault (format nil "~a" reply)))))

(defun %evaluate-here (form)
  (let ((said (make-string-output-stream)))
    (handler-case
        (let* ((*standard-output* said)
               (values (multiple-value-list (eval form))))
          (list :answered values :said (get-output-stream-string said)))
      (error (e)
        (list :fault (princ-to-string e)
              :said (get-output-stream-string said))))))

(defun answer-for (sys &key (name "agent"))
  (endpoint:agent name
                  (lambda (message)
                    (case (first message)
                      (:evaluate (cons :ok (%evaluate-here (second message))))
                      (:ping (cons :ok :pong))
                      (t (cons :ok nil))))
                  :dispatcher :pinned :in sys))

(defun serve (sys &key (name "agent") master-host master-port self-port)
  (answer-for sys :name name)
  (when (and master-host master-port)
    (let ((home (sento.remoting:make-remote-ref
                 sys (server:daemon-uri "agents" :host master-host
                                                 :port master-port))))
      (sento.actor:tell home (list :here :name name
                                   :uri (server:local-uri name self-port
                                                          :host master-host)))
      home)))

(defun listen-for-agents (s)
  (endpoint:agent "agents"
    (lambda (message)
      (case (first message)
        (:here (destructuring-bind (&key name uri) (rest message)
                 (let ((there (sento.remoting:make-remote-ref
                               (server:actor-system s) uri)))
                   (register name there :uri uri)
                   (log:note "agent ~a is here at ~a" name uri))))
        (:gone (forget (getf (rest message) :name)))
        (t nil)))
    :dispatcher :pinned :in (server:actor-system s)))
