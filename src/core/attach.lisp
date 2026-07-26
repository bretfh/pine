(defpackage #:pine.core.attach
  (:use #:cl)
  (:export #:start-attach-listener #:attach-listener
           #:attach-to-daemon
           #:app #:app-kind #:register-app #:find-app
           #:attached #:received #:detached #:run-frontend
           #:attached-client #:attached-client-id #:attached-client-kind
           #:attached-client-display #:attached-client-input #:attached-client-session
           #:*clients* #:push-to-app #:client-alive-p #:reap-clients))

(in-package #:pine.core.attach)

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
  (pine.core.server:daemon-uri "" :port (pine.core.server:remoting-port server)))

(defstruct attached-client id kind display uri input session)

(defvar *clients* nil "Daemon-side list of attached-client.")
(defvar *client-counter* 0)
;;;; An app is a kind of frontend: the editor, the desktop, the window
;;;; manager. The daemon holds one instance per kind and drives it through
;;;; the protocol below; the kind keyword is what the wire carries.

(defclass app ()
  ((kind :initarg :kind :reader app-kind
         :documentation "The keyword a frontend attaches as."))
  (:documentation "A kind of frontend, from the daemon's side."))

(defgeneric attached (app client)
  (:documentation "A frontend of APP's kind has arrived. Build its session.")
  (:method ((app app) client) (declare (ignore client)) nil))

(defgeneric received (app client message)
  (:documentation "MESSAGE arrived from an attached frontend of APP's kind.")
  (:method ((app app) client message) (declare (ignore client message)) nil))

(defgeneric detached (app client)
  (:documentation "The frontend is gone. Tear its session down.")
  (:method ((app app) client) (declare (ignore client)) nil))

(defgeneric run-frontend (app)
  (:documentation "Run APP's frontend in this image, until it exits.

Defined by whichever backing is loaded; without one, `pine editor' and its
siblings have nothing to run and say so.")
  (:method ((app app))
    (format t "pine: no ~(~a~) frontend in this build~%" (app-kind app))))

(defvar *apps* (make-hash-table :test 'eq)
  "kind -> app instance, so kinds coexist on one daemon.")

(defun register-app (app)
  "Make APP the daemon's handler for its kind."
  (setf (gethash (app-kind app) *apps*) app))

(defun find-app (kind)
  (gethash kind *apps*))

(defun %app-of (client)
  (find-app (attached-client-kind client)))

(defun on-client-input (client msg)
  "Hand the message to the app. An error here is reported, never swallowed: a
handler that fails silently makes the app look dead while the daemon looks
healthy."
  (let ((app (%app-of client)))
    (when app
      (handler-case (received app client msg)
        (error (c)
          (format *error-output* "pine: ~a input handler failed on ~s: ~a~%"
                  (attached-client-kind client) (first msg) c)
          (finish-output *error-output*))))))

(defun on-client-detach (client)
  "Tell the app its frontend is gone."
  (let ((app (%app-of client)))
    (when app
      (handler-case (detached app client)
        (error (c)
          (format *error-output* "pine: ~a detach handler failed: ~a~%"
                  (attached-client-kind client) c)
          (finish-output *error-output*))))))

(defun uri-endpoint (uri)
  "The host and port of a sento URI, as two values, or NIL when it has neither."
  (let ((mark (search "//" uri)))
    (when mark
      (let* ((start (+ mark 2))
             (authority (subseq uri start (position #\/ uri :start start)))
             (colon (position #\: authority)))
        (when colon
          (values (subseq authority 0 colon)
                  (parse-integer authority :start (1+ colon) :junk-allowed t)))))))

(defun client-alive-p (client)
  "True while the app's own remoting port still answers.

Remoting reports nothing when a peer dies, so an app that crashed or was killed
stays on `*clients*' and its kind looks taken forever. The port belongs to the
app's process, so a refused connection is the app being gone."
  (multiple-value-bind (host port) (uri-endpoint (or (attached-client-uri client) ""))
    (if (and host port)
        (handler-case
            (let ((socket (usocket:socket-connect host port :timeout 1)))
              (usocket:socket-close socket)
              t)
          (error () nil))
        t)))

(defun reap-clients ()
  "Drop the apps that are gone and return them, so their kind can start again."
  (let ((dead (remove-if #'client-alive-p *clients*)))
    (dolist (client dead)
      (setf *clients* (remove client *clients*))
      (on-client-detach client)
      (let ((input (attached-client-input client)))
        (when input (sento.actor:tell input :stop))))
    dead))

(defun push-to-app (client &rest message)
  "Daemon -> app: tell the app's display actor MESSAGE (async, plain data)."
  (let ((d (attached-client-display client)))
    (when d (sento.actor:tell d message))))

(defun start-attach-listener (server)
  "Daemon-side: the actor apps connect to in order to attach. Returns it."
  (let ((sys (pine.core.server:actor-system server)))
    (sento.actor-context:actor-of sys
      :name "attach"
      :receive
      (lambda (msg)
        (case (first msg)
          (:attach
           (destructuring-bind (&key display-uri kind) (rest msg)
             (let* ((id (incf *client-counter*))
                    (display (sento.remoting:make-remote-ref sys display-uri))
                    (client (make-attached-client :id id :kind kind :display display
                                                  :uri display-uri)))
               (setf (attached-client-input client)
                     (sento.actor-context:actor-of sys
                       :name (format nil "client-~d" id)
                       :receive (lambda (m) (on-client-input client m))))
               (push client *clients*)
               (let ((app (%app-of client)))
                 (when app
                   (handler-case (attached app client)
                     (error (c)
                       (format *error-output* "pine: ~a attach handler failed: ~a~%"
                               kind c)
                       (finish-output *error-output*)))))
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
