(defpackage #:pine.core.server
  (:use :cl)
  (:export
   #:server
   #:*server*
   #:*app-actor-config*
   #:actor-system
   #:agent-registry
   #:buffer-registry
   #:buffer-table
   #:ts-runtime
   #:clients
   #:remoting-port
   #:start-server
   #:stop-server
   #:*host*
   #:*port*
   #:daemon-uri
   #:local-uri))

(in-package #:pine.core.server)

(defvar *server* nil
  "The daemon's server, for server-scoped state (faces) reached without a client.
A headless daemon has no client, so registry accessors fall back to this.")

;;;; One place the loopback address and the daemon's remoting port live, and one
;;;; builder for the sento URIs everything used to hardcode. DAEMON-URI names an
;;;; actor on the daemon (attach / control / agent-registry / agent-debug);
;;;; LOCAL-URI names an actor on an attaching client's own ephemeral system.

(defparameter *host* "127.0.0.1"
  "The address the daemon binds and clients dial. Loopback by default.")

(defparameter *app-actor-config*
  (list :dispatchers (list :shared (list :workers 2 :strategy :random))
        :timeout-timer (list :resolution 1000 :max-size 500)
        :scheduler (list :enabled :true))
  "The actor system of an image that attaches to the daemon: a frontend or a
process agent. Two workers, and the wheel timer every interval in pine runs on
rather than a thread of its own.")

(defparameter *port*
  (or (parse-integer (or (uiop:getenv "PINE_PORT") "") :junk-allowed t) 17000)
  "The daemon's remoting port: where editor, desktop, and agents attach.

Read from PINE_PORT when that is set, which is how a daemon running on any
other port tells the frontends it spawns where to find it. Without it a
frontend would attach to whichever daemon holds the default port.")

(defun daemon-uri (actor &key (host *host*) (port *port*))
  "The sento URI of a well-known actor on the daemon, e.g. \"attach\"."
  (format nil "sento://~a:~d/user/~a" host port actor))

(defun local-uri (actor port &key (host *host*))
  "The sento URI of an actor on a client's own remoting system at PORT."
  (format nil "sento://~a:~d/user/~a" host port actor))

(defclass server ()
  ((actor-system    :initarg :actor-system    :accessor actor-system    :initform nil)
   (agent-registry  :initarg :agent-registry  :accessor agent-registry  :initform nil)
   (buffer-registry :initarg :buffer-registry :accessor buffer-registry :initform nil)
   (buffer-table    :initarg :buffer-table    :accessor buffer-table    :initform nil)
   (ts-runtime      :initarg :ts-runtime      :accessor ts-runtime      :initform nil)
   (clients         :initarg :clients         :accessor clients         :initform nil)
   (remoting-port   :initarg :remoting-port   :accessor remoting-port   :initform nil)))

(defun start-server (&key (workers 4) (remoting-port nil))
  (let* ((sys (sento.actor-system:make-actor-system
               `(:dispatchers
                 (:shared (:workers ,workers :strategy :random))
                 :timeout-timer (:resolution 500 :max-size 500)
                 :scheduler (:enabled :true))))
         (srv (make-instance 'server
                :actor-system sys
                :buffer-table (make-hash-table :test 'equal))))
    (when remoting-port
      (sento.remoting:enable-remoting sys :host "127.0.0.1" :port remoting-port)
      (setf (remoting-port srv) (sento.remoting:remoting-port sys)))
    srv))

(defun stop-server (srv)
  (let ((sys (actor-system srv)))
    (when (and sys (sento.remoting:remoting-enabled-p sys))
      (sento.remoting:disable-remoting sys))
    (when sys
      (sento.actor-context:shutdown sys :wait t)))
  (setf (actor-system srv) nil
        (clients srv) nil)
  srv)
