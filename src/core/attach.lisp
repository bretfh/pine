(in-package #:pine.attach)

;;;; The attach seam. An app process (editor, desktop) connects to the headless
;;;; daemon over sento remoting and becomes a session. The protocol is all
;;;; async tells, both directions -- never a blocking remote ask, so neither side
;;;; can hang the other:
;;;;
;;;;   app  -> daemon "attach" actor : (:attach :display-uri U :kind K)
;;;;   daemon creates a client (bound to a remote-ref to the app's display) and a
;;;;   per-client input actor, then
;;;;   daemon -> app display : (:attached :id N :client-uri V)
;;;;   app makes a remote-ref to V and sends input to it:
;;;;   app  -> daemon client-N   : (:key ...) (:resize ...) ...
;;;;   daemon -> app display     : (:widgets ...) (:panel ...) ...
;;;;
;;;; Only plain data crosses (the sexp wire): lists, numbers, strings, keywords,
;;;; vectors. fset/CLOS values are rendered to plain data at the edge.

(defun daemon-base-uri (server)
  (pine.server:daemon-uri "" :port (pine.server:remoting-port server)))

(defstruct attached-client id kind display input session)

(defvar *clients* nil "Daemon-side list of attached-client.")
(defvar *client-counter* 0)
(defvar *app-handlers* (make-hash-table :test 'eq)
  "app-kind -> (on-attach . on-input). Each app kind (editor, desktop) registers
its session logic here, so kinds coexist on one daemon.")

(defun register-app-kind (kind &key on-attach on-input)
  "Register session logic for an app KIND. ON-ATTACH (client) sets up the session;
ON-INPUT (client message) handles input from that app."
  (setf (gethash kind *app-handlers*) (cons on-attach on-input)))

(defun on-client-input (client msg)
  (let ((h (gethash (attached-client-kind client) *app-handlers*)))
    (when (and h (cdr h)) (ignore-errors (funcall (cdr h) client msg)))))

(defun push-to-app (client &rest message)
  "Daemon -> app: tell the app's display actor MESSAGE (async, plain data)."
  (let ((d (attached-client-display client)))
    (when d (sento.actor:tell d message))))

(defun start-attach-listener (server)
  "Daemon-side: the actor apps connect to in order to attach. Returns it."
  (let ((sys (pine.server:actor-system server)))
    (sento.actor-context:actor-of sys
      :name "attach"
      :receive
      (lambda (msg)
        (case (first msg)
          (:attach
           (destructuring-bind (&key display-uri kind) (rest msg)
             (let* ((id (incf *client-counter*))
                    (display (sento.remoting:make-remote-ref sys display-uri))
                    (client (make-attached-client :id id :kind kind :display display)))
               (setf (attached-client-input client)
                     (sento.actor-context:actor-of sys
                       :name (format nil "client-~d" id)
                       :receive (lambda (m) (on-client-input client m))))
               (push client *clients*)
               (let ((h (gethash kind *app-handlers*)))
                 (when (and h (car h)) (ignore-errors (funcall (car h) client))))
               (sento.actor:tell display
                 (list :attached :id id
                       :client-uri (format nil "~aclient-~d" (daemon-base-uri server) id)))
               nil)))
          (t nil))))))

(defun attach-to-daemon (app-sys daemon-attach-uri display-uri &key (kind :app))
  "App-side: send the attach request to the daemon. The reply (:attached ...)
arrives asynchronously at the app's own display actor (display-uri), which then
makes a remote-ref to the client actor for sending input. Returns the remote-ref
to the daemon's attach actor (kept so it is not GC'd)."
  (let ((attach (sento.remoting:make-remote-ref app-sys daemon-attach-uri)))
    (sento.actor:tell attach (list :attach :display-uri display-uri :kind kind))
    attach))
