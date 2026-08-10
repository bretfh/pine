(defpackage #:pine.net.attach
  (:use #:cl)
  (:local-nicknames (#:endpoint #:pine.run.agent) (#:d #:pine.data) (#:server #:pine.net.server)
                    (#:fault #:pine.run.fault) (#:log #:pine.run.log))
  (:export #:app #:frontend #:kinds #:attached #:received #:detached
           #:run-frontend #:client #:client-id #:client-kind #:client-display
           #:client-uri #:client-session #:clients #:push-to
           #:listen-for-attach #:attach-to #:protocol #:acceptable
           #:reap #:sweep #:alive-p #:*wire* #:*clients* #:attached-p #:accept-attached
           #:attached-at))

(in-package #:pine.net.attach)

(defvar *wire* "ns2")
(defvar *apps* (d:table))
(defvar *clients* (d:box nil))
(defvar *attempts* 60)
(defvar *interval* 2)

(defclass client ()
  ((client-id      :initarg :id      :reader client-id)
   (client-kind    :initarg :kind    :reader client-kind)
   (client-display :initarg :display :reader client-display)
   (client-uri     :initarg :uri     :accessor client-uri :initform nil)
   (client-session :initarg :session :accessor client-session :initform nil)))

(defmethod print-object ((c client) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~(~a~) ~d" (client-kind c) (client-id c))))

(defun protocol () (format nil "~a/~a" *wire* "0.0.1"))

(defun acceptable (theirs) (equal theirs (protocol)))

(defun app (kind declaration)
  (d:keep! *apps* kind (d:with declaration :kind kind))
  kind)

(defun frontend (kind run)
  (d:keep! *apps* kind
           (d:with (or (d:at (d:all *apps*) kind) (d:no-map)) :run run))
  kind)

(defun kinds () (d:keys (d:all *apps*)))

(defun %of (kind key)
  (let ((it (d:at (d:all *apps*) kind)))
    (and it (d:at it key))))

(defun attached (kind client)
  (let ((fn (%of kind :attached)))
    (when fn (funcall fn client))))

(defun received (kind client message)
  (let ((fn (%of kind :received)))
    (when fn (funcall fn client message))))

(defun detached (kind client)
  (let ((fn (%of kind :detached)))
    (when fn (funcall fn client))))

(defun run-frontend (kind)
  (let ((fn (%of kind :run)))
    (if fn
        (funcall fn)
        (format t "pine: no ~(~a~) frontend in this build~%" kind))))

(defun clients (&optional kind)
  (let ((all (d:held *clients*)))
    (if kind (remove kind all :key #'client-kind :test-not #'eq) all)))

(defun attached-p (kind) (and (clients kind) t))

(defgeneric push-to (client &rest message)
  (:method ((c client) &rest message)
    (let ((to (client-display c)))
      (when to (sento.actor:tell to message))
      message)))

(defun attached-at (uri)
  (find uri (d:held *clients*) :key #'client-uri :test #'equal))

(defun %accept (s message display-uri)
  (destructuring-bind (&key kind version uri &allow-other-keys) (rest message)
    (let ((held (attached-at uri)))
      (cond
        ((not (acceptable version))
         (sento.actor:tell
          (sento.remoting:make-remote-ref (server:actor-system s) display-uri)
          (list :refused :reason :version :version (protocol)))
         nil)
        (held
         (sento.actor:tell (client-display held)
                           (list :attached :id (client-id held)
                                 :version (protocol)))
         held)
        (t
         (let* ((display (sento.remoting:make-remote-ref (server:actor-system s)
                                                         display-uri))
                (c (make-instance 'client :id (server:next-client-id s)
                                          :kind kind :display display :uri uri)))
           (d:swap! *clients* (lambda (all) (cons c all)))
           (push c (server:clients s))
           (endpoint:agent (format nil "client-~d" (client-id c))
                           (lambda (m) (received kind c m))
                           :dispatcher :pinned :in (server:actor-system s))
           (sento.actor:tell display (list :attached :id (client-id c)
                                           :version (protocol)))
           (log:note "~(~a~) attached as client ~d" kind (client-id c))
           (%noting kind)
           (fault:attempt (lambda () (attached kind c))
                          (format nil "~(~a~) attaching" kind))
           c))))))

(defun listen-for-attach (s)
  (endpoint:agent "attach"
                  (lambda (message)
                    (case (first message)
                      (:attach (%accept s message (getf (rest message) :display)))
                      (:detach (let ((c (find (getf (rest message) :id)
                                              (d:held *clients*) :key #'client-id)))
                                 (when c (reap c s))))
                      (t nil)))
                  :dispatcher :pinned :in (server:actor-system s)))

(defun %noting (kind)
  (when pine.world.world:*world*
    (setf (pine.fs.node:contents
           (pine.world.world:ensure pine.world.world:*world* "attached"
                                    (string-downcase (string kind))))
          (length (clients kind)))))

(defun sweep (&optional s)
  "Drop the clients that are not there any more. A frontend that was killed
must not still be counted, or the one that replaces it is told not to start."
  (dolist (c (clients) (d:held *clients*))
    (unless (alive-p c) (reap c s))))

(defun reap (c s)
  (fault:attempt (lambda () (detached (client-kind c) c))
                 (format nil "~(~a~) detaching" (client-kind c)))
  (d:swap! *clients* (lambda (all) (remove c all)))
  (when s (setf (server:clients s) (remove c (server:clients s))))
  (%noting (client-kind c))
  c)

(defun alive-p (c)
  (and (client-uri c)
       (multiple-value-bind (host port) (%endpoint (client-uri c))
         (or (null port)
             (let ((socket (ignore-errors
                            (usocket:socket-connect host port :timeout 1))))
               (when socket (usocket:socket-close socket) t))))))

(defun %endpoint (uri)
  (let ((mark (search "//" uri)))
    (when mark
      (let* ((start (+ mark 2))
             (authority (subseq uri start (position #\/ uri :start start)))
             (colon (position #\: authority)))
        (when colon
          (values (subseq authority 0 colon)
                  (parse-integer authority :start (1+ colon) :junk-allowed t)))))))

(defun accept-attached (sys message)
  (destructuring-bind (&key id version &allow-other-keys) (rest message)
    (declare (ignore version))
    (sento.remoting:make-remote-ref
     sys (server:daemon-uri (format nil "client-~d" id)))))

(defun attach-to (sys daemon self &key kind)
  (let ((there (sento.remoting:make-remote-ref sys daemon)))
    (sento.actor:tell there (list :attach :kind kind :version (protocol)
                                          :uri self :display self))
    there))
