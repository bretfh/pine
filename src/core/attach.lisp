(defpackage #:pine.core.attach
  (:use #:cl)
  (:export #:start-attach-listener
           #:protocol-version #:version-accepted-p #:+wire-generation+
           #:attach-to-daemon
           #:app #:frontend #:kinds
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

(defvar *clients* nil "Daemon-side list of attached-client.")

(defvar *apps* (pine.data:table)
  "Kind to what that kind is, so kinds coexist on one daemon.")

(defun app (kind declaration)
  "Say what a frontend of KIND is, from the daemon's side, and answer KIND.

DECLARATION holds :ATTACHED, run when one arrives and answering its session;
:RECEIVED, handed each message it sends; :DETACHED, run when it is gone; and
:DOC. How it runs in this image is FRONTEND, because that is a different
image's business and a build without a back end has none."
  (pine.data:put *apps* kind (fset:with declaration :kind kind))
  kind)

(defun frontend (kind run)
  "Say how a frontend of KIND runs in this image. The back end that can run one
says so from its own file, so a build without it has nothing here and the CLI
says as much rather than guessing."
  (pine.data:put *apps* kind
                 (fset:with (or (pine.data:at *apps* kind) (fset:empty-map))
                            :run run))
  kind)

(defun kinds () (pine.data:keys *apps*))

(defun %of (kind key)
  (let ((it (pine.data:at *apps* kind)))
    (and it (fset:lookup it key))))

(defun attached (kind client)
  "A frontend of KIND has arrived. Build its session."
  (let ((fn (%of kind :attached)))
    (when fn (funcall fn client))))

(defun received (kind client message)
  "MESSAGE arrived from an attached frontend of KIND."
  (let ((fn (%of kind :received)))
    (when fn (funcall fn client message))))

(defun detached (kind client)
  "The frontend is gone. Tear its session down."
  (let ((fn (%of kind :detached)))
    (when fn (funcall fn client))))

(defun run-frontend (kind)
  "Run KIND's frontend in this image, until it exits."
  (let ((fn (%of kind :run)))
    (if fn
        (funcall fn)
        (format t "pine: no ~(~a~) frontend in this build~%" kind))))

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

(defun on-client-input (client msg)
  (let ((kind (attached-client-kind client)))
    (pine.err:attempt (lambda () (received kind client msg))
                      (format nil "~(~a~) input ~s" kind (first msg)))))

(defun on-client-detach (client)
  (let ((kind (attached-client-kind client)))
    (pine.err:attempt (lambda () (detached kind client))
                      (format nil "~(~a~) detaching" kind))))

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
  (pine.ns:write (pine.path:path (pine.path:parse "/attached")
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
    (handler-case (attached kind client)
      (error (c)
        (format *error-output* "pine: ~a attach handler failed: ~a~%" kind c)
        (finish-output *error-output*)))
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
