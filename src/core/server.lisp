(in-package :pine.server)

(defvar *server* nil
  "The daemon's server, for server-scoped state (faces) reached without a client.
A headless daemon has no client, so registry accessors fall back to this.")

;;;; One place the loopback address and the daemon's remoting port live, and one
;;;; builder for the sento URIs everything used to hardcode. DAEMON-URI names an
;;;; actor on the daemon (attach / control / agent-registry / agent-debug);
;;;; LOCAL-URI names an actor on an attaching client's own ephemeral system.

(defparameter *host* "127.0.0.1"
  "The address the daemon binds and clients dial. Loopback by default.")

(defparameter *port* 17000
  "The daemon's remoting port -- where editor, desktop, and agents attach.")

(defun daemon-uri (actor &key (host *host*) (port *port*))
  "The sento URI of a well-known actor on the daemon, e.g. \"attach\"."
  (format nil "sento://~a:~d/user/~a" host port actor))

(defun local-uri (actor port &key (host *host*))
  "The sento URI of an actor on a client's own remoting system at PORT."
  (format nil "sento://~a:~d/user/~a" host port actor))

(defclass server ()
  ((actor-system    :initarg :actor-system    :accessor actor-system    :initform nil)
   (event-bus       :initarg :event-bus       :accessor event-bus       :initform nil)
   (agent-registry  :initarg :agent-registry  :accessor agent-registry  :initform nil)
   (buffer-registry :initarg :buffer-registry :accessor buffer-registry :initform nil)
   (layouts         :initarg :layouts         :accessor layouts         :initform nil)
   (buffer-table    :initarg :buffer-table    :accessor buffer-table    :initform nil)
   (commands        :initarg :commands        :accessor commands        :initform nil)
   (global-keymap   :initarg :global-keymap   :accessor global-keymap   :initform nil)
   (faces           :initarg :faces           :accessor faces           :initform nil)
   (ts-runtime      :initarg :ts-runtime      :accessor ts-runtime      :initform nil)
   (modes           :initarg :modes           :accessor modes           :initform nil)
   (clients         :initarg :clients         :accessor clients         :initform nil)
   (remoting-port   :initarg :remoting-port   :accessor remoting-port   :initform nil)))

(defun start-server (&key (workers 4) (remoting-port nil))
  (let* ((sys (sento.actor-system:make-actor-system
               `(:dispatchers
                 (:shared (:workers ,workers :strategy :random))
                 :timeout-timer (:resolution 50 :max-size 500)
                 :scheduler (:enabled :true :resolution 100 :max-size 500))))
         (srv (make-instance 'server
                :actor-system sys
                :commands (make-hash-table :test 'equal)
                :global-keymap (make-hash-table :test 'equal)
                :faces (make-hash-table :test 'eq)
                :layouts (make-hash-table :test 'equal)
                :buffer-table (make-hash-table :test 'equal)
                :modes (make-hash-table :test 'eq))))
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
