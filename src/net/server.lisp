(defpackage #:pine.net.server
  (:use #:cl)
  (:local-nicknames (#:timer #:pine.run.timer) (#:d #:pine.data))
  (:export #:server #:start-server #:stop-server #:*server* #:*host* #:*port*
           #:actor-system #:remoting-port #:clients #:next-client-id
           #:read-environment #:daemon-uri #:local-uri #:workers #:*workers*))

(in-package #:pine.net.server)

(defvar *server* nil)
(defvar *host* "127.0.0.1")
(defvar *port* 17000)
(defvar *workers*
  (max 2 (1- (or (ignore-errors
                  (parse-integer (uiop:run-program '("nproc") :output '(:string :stripped t))))
                 4))))

(defclass server ()
  ((actor-system  :initarg :actor-system  :accessor actor-system  :initform nil)
   (remoting-port :initarg :remoting-port :accessor remoting-port :initform nil)
   (clients       :initform nil           :accessor clients)
   (client-ids    :initform (d:box 0)    :reader client-ids)
   (workers       :initarg :workers       :reader workers :initform 4)))

(defmethod print-object ((s server) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~@[remoting ~d~] ~d client~:p"
            (remoting-port s) (length (clients s)))))

(defun %env-int (name)
  (let ((text (uiop:getenv name)))
    (when text (parse-integer text :junk-allowed t))))

(defun read-environment ()
  (setf *port* (or (%env-int "PINE_PORT") 17000)
        *workers* (or (%env-int "PINE_WORKERS") *workers*))
  (values *port* *workers*))

(defun next-client-id (s) (d:swap! (client-ids s) #'1+))

(defun daemon-uri (actor &key (host *host*) (port *port*))
  (format nil "sento://~a:~d/user/~a" host port actor))

(defun local-uri (actor port &key (host *host*))
  (format nil "sento://~a:~d/user/~a" host port actor))

(defun %config (workers)
  (list :dispatchers (list :shared (list :workers workers :strategy :random))
        :scheduler (list :enabled :true :max-size 1000
                         :resolution (round (* 1000 timer:*soonest*)))))

(defun start-server (&key (workers *workers*) (remoting-port nil))
  (let* ((sys (sento.actor-system:make-actor-system (%config workers)))
         (s (make-instance 'server :actor-system sys :workers workers)))
    (when remoting-port
      (sento.remoting:enable-remoting sys :host *host* :port remoting-port)
      (setf (remoting-port s) (sento.remoting:remoting-port sys)))
    s))

(defun stop-server (s)
  (let ((sys (actor-system s)))
    (when (and sys (sento.remoting:remoting-enabled-p sys))
      (sento.remoting:disable-remoting sys))
    (when sys (sento.actor-context:shutdown sys :wait t)))
  (setf (actor-system s) nil (clients s) nil)
  s)
