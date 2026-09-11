(defpackage #:pine/serve/socket
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs) (#:job #:pine/run/job)
                    (#:peer #:pine/run/peer) (#:fault #:pine/run/fault)
                    (#:log #:pine/fs/log) (#:wire #:pine/serve/wire)
                    (#:actors #:pine/run/actors))
  (:export
   #:where #:listening #:open-socket #:close-socket #:serve-node #:*name*))
(in-package #:pine/serve/socket)

(defvar *name* "pine")
(defvar *listening* nil)
(defparameter +backlog+ 16)

(defun %ours (where)
  (ensure-directories-exist where)
  (let ((it (sb-posix:stat where)))
    (unless (= (sb-posix:stat-uid it) (sb-posix:getuid))
      (error "~a is not yours; pine will not answer in it." where))
    (sb-posix:chmod where #o700))
  where)

(defun where (&optional (name "pine"))
  (or (uiop:getenv "PINE_SOCKET")
      (let ((run (or (uiop:getenv "XDG_RUNTIME_DIR")
                     (format nil "/tmp/pine-~a" (uiop:getenv "USER")))))
        (%ours (format nil "~a/pine/" run))
        (format nil "~a/pine/~a.sock" run name))))

(defclass connection ()
  ((stream-of :initarg :stream :reader stream-of)
   (socket-of :initarg :socket :reader socket-of)
   (watching  :initform (cons :watching nil) :reader watching)
   (saying    :initform (bordeaux-threads:make-lock "pine-connection")
              :reader saying)))

(defun %say (c text)
  (fault:or-nothing "the caller may have gone"
    (bordeaux-threads:with-lock-held ((saying c))
      (write-line text (stream-of c))
      (force-output (stream-of c)))))

(defun %closed (c)
  (peer:forget-watches (watching c))
  (fault:or-nothing "a stream already closed is closed"
    (close (stream-of c)))
  (fault:or-nothing "and so is the socket under it"
    (sb-bsd-sockets:socket-close (socket-of c)))
  c)

(defun %talk (c)
  (unwind-protect
       (peer:telling ((lambda (said) (%say c (wire:encode-event said))) (watching c) t t)
         (wire:serve (stream-of c) #'peer:received
                     (lambda (text) (%say c text))))
    (%closed c)))

(defun %took (socket)
  (let ((c (make-instance 'connection
                          :socket socket
                          :stream (sb-bsd-sockets:socket-make-stream
                                   socket :input t :output t
                                          :element-type 'character
                                          :external-format :utf-8))))
    (pine/run/actors:blocking "a caller"
                              (lambda ()
                                (fault:attempt (lambda () (%talk c))
                                               "answering a caller")))
    c))

(defun %accepting (socket)
  (lambda ()
    (loop :for took := (fault:or-nothing "the socket is closed and we are done"
                         (sb-bsd-sockets:socket-accept socket))
          :while took
          :do (%took took))))

(defun open-socket (&key (name *name*))
  (let ((path (where name))
        (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (fault:or-nothing "nothing may be there to take away"
      (delete-file path))
    (let ((was (sb-posix:umask #o177)))
      (unwind-protect (sb-bsd-sockets:socket-bind socket path)
        (sb-posix:umask was)))
    (sb-bsd-sockets:socket-listen socket +backlog+)
    (sb-posix:chmod path #o600)
    (setf *listening* (list socket path))
    (let ((j (make-instance 'job:thread :name "serve" :on-fault :leave
                            :body (%accepting socket))))
      (job:supervise j)
      (job:start j)
      (log:note "answering on ~a" path)
      j)))

(defun listening () (second *listening*))

(defun close-socket ()
  (let ((held *listening*))
    (when held
      (destructuring-bind (socket path) held
        (fault:or-nothing "one already closed is closed"
          (sb-bsd-sockets:socket-close socket))
        (fault:or-nothing "and the name may be gone from the filesystem"
          (delete-file path)))
      (setf *listening* nil)))
  t)

(defun serve-node ()
  (make-instance 'answering :name "serve"
                 :describes "what this pine is called and where it answers"))

(defclass answering (fs:derived) ())

(defmethod fs:volatile-p ((n answering) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n answering))
  (list :name *name* :socket (listening) :port (actors:remoting)))

(fs:mount #'serve-node "/serve")
