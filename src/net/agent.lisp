(defpackage #:pine/net/agent
  (:use #:cl)
  (:local-nicknames (#:endpoint #:pine/run/endpoint) (#:d #:pine/data) (#:server #:pine/net/server)
                    (#:attach #:pine/net/attach) (#:fault #:pine/run/fault)
                    (#:elsewhere #:pine/proc/elsewhere)
                    (#:session #:pine/repl/session) (#:log #:pine/run/log))
  (:export #:remote-session #:open-remote #:answer-for #:serve #:agents
           #:agent #:agent-named #:name #:ref
           #:register #:forget #:*agents* #:listen-for-agents))
(in-package #:pine/net/agent)

(defvar *agents* (d:table))
(defvar *timeout* 30)

(defclass agent ()
  ((name :initarg :name :reader name)
   (ref  :initarg :ref  :accessor ref)
   (uri  :initarg :uri  :accessor uri :initform nil)))

(defmethod print-object ((a agent) stream)
  (print-unreadable-object (a stream :type t)
    (write-string (name a) stream)))

(defclass remote-session (session:session)
  ((agent :initarg :agent :reader agent)))

(defun register (name ref &key uri)
  (let ((a (make-instance 'agent :name name :ref ref :uri uri)))
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
  (multiple-value-bind (answered fault offers said)
      (elsewhere:evaluate (agent s) form)
    (declare (ignore offers))
    (make-instance 'session:evaluation
                   :form form :answered answered :fault fault
                   :said (or said ""))))

(defmethod elsewhere:evaluate ((a agent) form &key (timeout *timeout*))
  "Work in a pine reached over remoting. What it says crosses as data, so the
image on the other end may be on another machine.

A fault there unwinds there: this transport carries what broke and not the
restarts it was standing in, which is why OFFERS is empty. Taking one is
RESUME-THERE's, and it needs the far side to stand still rather than answer."
  (let ((reply (sento.actor:ask-s (ref a) (list :evaluate form)
                                  :time-out timeout)))
    (if (and (consp reply) (eq :ok (car reply)))
        (let ((answer (cdr reply)))
          (values (getf answer :answered) (getf answer :fault) nil
                  (or (getf answer :said) "")))
        (values nil (format nil "~a" reply) nil ""))))

(defmethod elsewhere:resume-there ((a agent) name)
  "Nothing to take: the far side answered and unwound rather than standing in
the fault, so by the time a restart is chosen here there is no thread there
holding it. Standing still over remoting is what SERVE would have to do, and
nothing calls SERVE yet."
  (declare (ignore a name))
  nil)

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
  (endpoint:endpoint name
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
  (endpoint:endpoint "agents"
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
