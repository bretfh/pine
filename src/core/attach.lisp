(defpackage #:pine.core.attach
  (:use #:cl)
  (:export #:start-attach-listener
           #:protocol-version #:version-accepted-p #:+wire-generation+
           #:attach-to-daemon
           #:app #:app-kind #:register-app #:find-app
           #:attached #:received #:detached #:run-frontend
           #:attached-client #:attached-client-id #:attached-client-kind
           #:attached-client-display #:attached-client-input #:attached-client-session
           #:*clients* #:push-to-app #:client-alive-p #:reap-clients
           #:accept-attached))

(in-package #:pine.core.attach)

;;;; The attach seam. An app process connects to the headless daemon over sento
;;;; remoting and becomes a session. Every message is an async tell, both ways,
;;;; so neither side can hang the other:
;;;;
;;;;   app    -> daemon "attach" : (:attach :display-uri U :kind K :version V)
;;;;   daemon -> app display     : (:attached :id N :client-uri V :version V)
;;;;                               (:refused :reason R :version V)
;;;;   app    -> daemon client-N : (:key ...) (:resize ...)
;;;;   daemon -> app display     : (:widgets ...) (:panel ...)
;;;;
;;;; Only plain data crosses: lists, numbers, strings, keywords, vectors. fset
;;;; and CLOS values are rendered to plain data at the edge.

(defstruct attached-client id kind display uri input session)

(defclass app ()
  ((kind :initarg :kind :reader app-kind
         :documentation "The keyword a frontend attaches as."))
  (:documentation "A kind of frontend, from the daemon's side. The daemon holds
one instance per kind, and the kind keyword is what the wire carries."))

(defvar *clients* nil "Daemon-side list of attached-client.")

(defvar *apps* (pine.data:table)
  "Kind to app instance, so kinds coexist on one daemon.")

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
  (:documentation "Run APP's frontend in this image, until it exits. Defined by
whichever backing is loaded.")
  (:method ((app app))
    (format t "pine: no ~(~a~) frontend in this build~%" (app-kind app))))

(defparameter +wire-generation+ "ns1"
  "What this protocol is, as against what release it shipped in.

The release number says nothing about the wire: two trees can both call
themselves 0.0.1 and encode a widget's props one as an fset map and the other as
a plist. A frontend that attaches to the wrong one then paints a screen of
condition reports instead of being told plainly that it does not belong there.
Bump this whenever the codec, the tags, or the message set change.")

(defun protocol-version ()
  "The version two images must agree on to talk: the release, and the wire
generation, because agreeing on the first says nothing about the second."
  (format nil "~a/~a"
          (or (asdf:component-version (asdf:find-system :pine nil)) "unknown")
          +wire-generation+))

(defun version-accepted-p (theirs)
  "Whether an app reporting THEIRS may attach. Exact match while the protocol is
still moving; a range belongs here once it stops."
  (equal theirs (protocol-version)))

(defun daemon-base-uri (server)
  (pine.core.server:daemon-uri "" :port (pine.core.server:remoting-port server)))

(defun register-app (app)
  "Make APP the daemon's handler for its kind."
  (pine.data:put *apps* (app-kind app) app))

(defun find-app (kind)
  (pine.data:at *apps* kind))

(defun %app-of (client)
  (find-app (attached-client-kind client)))

(defun on-client-input (client msg)
  (let ((app (%app-of client)))
    (when app
      (handler-case (received app client msg)
        (error (c)
          (format *error-output* "pine: ~a input handler failed on ~s: ~a~%"
                  (attached-client-kind client) (first msg) c)
          (finish-output *error-output*))))))

(defun on-client-detach (client)
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
  "True while the app's own remoting port still answers. Remoting reports
nothing when a peer dies, and the port belongs to the app's process, so a
refused connection is the app being gone."
  (multiple-value-bind (host port) (uri-endpoint (or (attached-client-uri client) ""))
    (if (and host port)
        (handler-case
            (let ((socket (usocket:socket-connect host port :timeout 1)))
              (usocket:socket-close socket)
              t)
          (error () nil))
        t)))

(defun %note-attached (kind there)
  (pine.ns:write (pine.path:child (pine.path:parse "/attached")
                                  (string-downcase (string kind)))
                 there))

(defun reap-clients ()
  "Drop the apps that are gone and answer them, so their kind can start again."
  (let ((dead (remove-if #'client-alive-p *clients*)))
    (dolist (client dead)
      (setf *clients* (remove client *clients*))
      (%note-attached (attached-client-kind client) nil)
      (on-client-detach client)
      (let ((input (attached-client-input client)))
        (when input (sento.actor:tell input :stop))))
    dead))

(defun push-to-app (client &rest message)
  (let ((d (attached-client-display client)))
    (when d (sento.actor:tell d message))))

(defun %refuse-attach (display kind version)
  (format *error-output*
          "pine: refused a ~(~a~) attach: it speaks ~a, this daemon ~a~%"
          kind (or version "no version") (protocol-version))
  (finish-output *error-output*)
  (sento.actor:tell display
    (list :refused
          :reason (format nil "this daemon speaks protocol ~a, you speak ~a"
                          (protocol-version) (or version "none"))
          :version (protocol-version))))

(defun %accept-attach (sys server display display-uri kind)
  (let* ((id (pine.core.server:next-client-id server))
         (client (make-attached-client :id id :kind kind :display display
                                       :uri display-uri)))
    (setf (attached-client-input client)
          (sento.actor-context:actor-of sys
            :name (format nil "client-~d" id)
            ;; one app's input never queues behind another's
            :dispatcher :pinned
            :receive (lambda (m) (on-client-input client m))))
    (push client *clients*)
    (%note-attached kind t)
    (let ((app (%app-of client)))
      (when app
        (handler-case (attached app client)
          (error (c)
            (format *error-output* "pine: ~a attach handler failed: ~a~%" kind c)
            (finish-output *error-output*)))))
    (sento.actor:tell display
      (list :attached :id id
            :client-uri (format nil "~aclient-~d" (daemon-base-uri server) id)
            :version (protocol-version)))
    (pine.log:note "~(~a~) attached as client ~d" kind id)
    nil))

(defun start-attach-listener (server)
  "Daemon-side: the actor apps connect to in order to attach."
  (let ((sys (pine.core.server:actor-system server)))
    (sento.actor-context:actor-of sys
      :name "attach"
      :dispatcher :pinned
      :receive
      (lambda (msg)
        (case (first msg)
          (:attach
           (destructuring-bind (&key display-uri kind version) (rest msg)
             (let ((display (sento.remoting:make-remote-ref sys display-uri)))
               (if (version-accepted-p version)
                   (%accept-attach sys server display display-uri kind)
                   (%refuse-attach display kind version)))))
          (t nil))))))

(defun accept-attached (sys message)
  "The client ref an :ATTACHED reply names, read here rather than in each
frontend: a key added on the daemon's side would otherwise kill three display
actors, and a display actor that dies never sends input and never takes a frame."
  (destructuring-bind (&key client-uri &allow-other-keys) (rest message)
    (when client-uri
      (sento.remoting:make-remote-ref sys client-uri))))

(defun attach-to-daemon (app-sys daemon-attach-uri display-uri &key (kind :app))
  "App-side: send the attach request. The (:attached ...) reply arrives at the
app's own display actor, which then makes a ref to the client actor for input.
Answers the ref to the daemon's attach actor, kept so it is not collected."
  (let ((attach (sento.remoting:make-remote-ref app-sys daemon-attach-uri)))
    (sento.actor:tell attach (list :attach :display-uri display-uri :kind kind
                                           :version (protocol-version)))
    attach))
