(defpackage #:pine.net.attach
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:server #:pine.net.server)
                    (#:fault #:pine.run.fault) (#:log #:pine.run.log))
  (:export #:app #:frontend #:kinds #:attached #:received #:detached
           #:run-frontend #:client #:client-id #:client-kind #:client-display
           #:client-uri #:client-session #:clients #:push-to
           #:listen-for-attach #:attach-to #:protocol #:acceptable
           #:reap #:alive-p #:*wire* #:*clients* #:attached-p))

(in-package #:pine.net.attach)

(defvar *wire* "ns2")
(defvar *apps* (c:cell (fset:empty-map)))
(defvar *clients* nil)
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
  (c:swap *apps* (lambda (all) (fset:with all kind
                                          (fset:with declaration :kind kind))))
  kind)

(defun frontend (kind run)
  (c:swap *apps*
          (lambda (all)
            (fset:with all kind
                       (fset:with (or (fset:lookup all kind) (fset:empty-map))
                                  :run run))))
  kind)

(defun kinds () (fset:convert 'list (fset:domain (c:held *apps*))))

(defun %of (kind key)
  (let ((it (fset:lookup (c:held *apps*) kind)))
    (and it (fset:lookup it key))))

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
  (if kind (remove kind *clients* :key #'client-kind :test-not #'eq) *clients*))

(defun attached-p (kind) (and (clients kind) t))

(defun push-to (client &rest message)
  (let ((to (client-display client)))
    (when to (sento.actor:tell to message))
    message))

(defun %accept (s message display)
  (destructuring-bind (&key kind version uri) (rest message)
    (if (not (acceptable version))
        (progn (sento.actor:tell display (list :refused :reason :version
                                               :version (protocol)))
               nil)
        (let ((c (make-instance 'client :id (server:next-client-id s)
                                        :kind kind :display display :uri uri)))
          (push c *clients*)
          (push c (server:clients s))
          (fault:attempt (lambda () (attached kind c))
                         (format nil "~(~a~) attaching" kind))
          (sento.actor:tell display (list :attached :id (client-id c)
                                          :version (protocol)))
          (log:note "~(~a~) attached as client ~d" kind (client-id c))
          c))))

(defun listen-for-attach (s)
  (sento.actor-context:actor-of (server:actor-system s)
    :name "attach"
    :dispatcher :pinned
    :receive
    (lambda (message)
      (case (first message)
        (:attach (%accept s message (getf (rest message) :display)))
        (:detach (let ((c (find (getf (rest message) :id) *clients*
                                :key #'client-id)))
                   (when c (reap c s))))
        (t nil)))))

(defun reap (c s)
  (fault:attempt (lambda () (detached (client-kind c) c))
                 (format nil "~(~a~) detaching" (client-kind c)))
  (setf *clients* (remove c *clients*)
        (server:clients s) (remove c (server:clients s)))
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

(defun attach-to (sys daemon self &key kind)
  (let ((there (sento.remoting:make-remote-ref sys daemon)))
    (sento.actor:tell there (list :attach :kind kind :version (protocol)
                                          :uri self
                                          :display (sento.remoting:make-remote-ref
                                                    sys self)))
    there))
