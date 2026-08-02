(defpackage #:pine.core.server
  (:use :cl)
  (:export
   #:server
   #:*server*
   #:*app-actor-config* #:*workers* #:actor-config
   #:actor-system
   #:agent-registry
   #:store
   #:ts-runtime
   #:proc
   #:clients
   #:remoting-port
   #:start-server
   #:stop-server
   #:next-client-id
   #:next-agent-port
   #:*host*
   #:*port*
   #:read-environment
   #:daemon-uri
   #:local-uri))

(in-package #:pine.core.server)

;;;; The image's actor system and what it has handed out. DAEMON-URI names an
;;;; actor on the daemon; LOCAL-URI names one on an attaching client's own
;;;; ephemeral system.

(defvar *server* nil
  "The daemon's server, for server-scoped state reached without a client.")

(defparameter *host* "127.0.0.1"
  "The address the daemon binds and clients dial.")

(defun %env-int (name)
  (parse-integer (or (uiop:getenv name) "") :junk-allowed t))

(defun %cores ()
  (or (ignore-errors
       (parse-integer (uiop:run-program '("nproc") :output '(:string :stripped t))))
      4))

(defparameter *port* (or (%env-int "PINE_PORT") 17000)
  "The daemon's remoting port: where editor, desktop and agents attach. From
PINE_PORT when that is set, which is how a daemon on any other port tells the
frontends it spawns where to find it.")

(defparameter *workers* (or (%env-int "PINE_WORKERS") (max 2 (1- (%cores))))
  "How many threads a shared dispatcher draws from. From PINE_WORKERS when that
is set, else one fewer than the machine has: pine's pinned actors want a core.")

(defparameter +agent-port-from+ 18100
  "Where a spawned process agent's remoting port is counted from.")

(defun actor-config (&key (workers *workers*) (scheduler t))
  "The actor system configuration an image starts with."
  (list :dispatchers (list :shared (list :workers workers :strategy :random))
        :timeout-timer (list :resolution 1000 :max-size 500)
        :scheduler (list :enabled (if scheduler :true :false))))

(defparameter *app-actor-config* (actor-config)
  "The actor system of an image that attaches to the daemon: a frontend or a
process agent. The wheel timer is what every interval in pine runs on.")

(defun read-environment ()
  "Take the settings the environment carries, now.

A saved image answered these when it was built: the port PINE_PORT named on the
build machine, and that machine's core count. A binary asks again before it does
anything, so `PINE_PORT=17123 pine daemon' listens where it was told and a
machine with more cores than the builder uses them."
  (setf *port* (or (%env-int "PINE_PORT") 17000)
        *workers* (or (%env-int "PINE_WORKERS") (max 2 (1- (%cores))))
        *app-actor-config* (actor-config))
  (values *port* *workers*))

(defun daemon-uri (actor &key (host *host*) (port *port*))
  "The sento URI of a well-known actor on the daemon, e.g. \"attach\"."
  (format nil "sento://~a:~d/user/~a" host port actor))

(defun local-uri (actor port &key (host *host*))
  "The sento URI of an actor on a client's own remoting system at PORT."
  (format nil "sento://~a:~d/user/~a" host port actor))

(defclass server ()
  ((actor-system    :initarg :actor-system    :accessor actor-system    :initform nil)
   (agent-registry  :initarg :agent-registry  :accessor agent-registry  :initform nil)
   (store           :initarg :store           :accessor store           :initform nil)
   (ts-runtime      :initarg :ts-runtime      :accessor ts-runtime      :initform nil)
   (proc            :initarg :proc            :accessor proc            :initform nil)
   (clients         :initarg :clients         :accessor clients         :initform nil)
   (remoting-port   :initarg :remoting-port   :accessor remoting-port   :initform nil)
   ;; counting is a read and a write: two apps attaching at once must not be
   ;; given one id, and two agents spawning at once must not be given one port
   (client-ids  :reader client-ids
                :initform (sento.atomic:make-atomic-reference :value 0))
   (agent-ports :reader agent-ports
                :initform (sento.atomic:make-atomic-reference
                           :value +agent-port-from+)))
  (:documentation "One image's actor system, what it serves and what it has
handed out."))

(defun next-client-id (srv)
  (sento.atomic:atomic-swap (client-ids srv) #'1+))

(defun next-agent-port (srv)
  (sento.atomic:atomic-swap (agent-ports srv) #'1+))

(defun start-server (&key (workers *workers*) (remoting-port nil))
  (let* ((sys (sento.actor-system:make-actor-system
               (actor-config :workers workers)))
         (srv (make-instance 'server :actor-system sys)))
    (when remoting-port
      (sento.remoting:enable-remoting sys :host *host* :port remoting-port)
      (setf (remoting-port srv) (sento.remoting:remoting-port sys)))
    srv))

(defun stop-server (srv)
  "Stop the actor system. What SRV serves comes off separately: a space outlives
any one server, so LOWER-ALL is the caller's to make against the space it raised."
  (let ((sys (actor-system srv)))
    (when (and sys (sento.remoting:remoting-enabled-p sys))
      (sento.remoting:disable-remoting sys))
    (when sys
      (sento.actor-context:shutdown sys :wait t)))
  (setf (actor-system srv) nil
        (clients srv) nil)
  srv)
