(defpackage #:pine/serve/socket
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:peer #:pine/run/peer) (#:fault #:pine/run/fault)
                    (#:log #:pine/fs/log) (#:wire #:pine/serve/wire))
  (:export
   #:where #:listening #:open-socket #:close-socket #:serve-node))
(in-package #:pine/serve/socket)

(defvar *listening* nil
  "The socket this pine is answering on, or nothing.")
(defparameter +backlog+ 16)

(defun %ours (where)
  "Make WHERE, and make it nobody else's.

The runtime directory is already the user's own and nobody else's. /tmp is not:
the name is one anybody can work out, so a directory left at whatever the umask
said is one somebody else may have made first and may still own. Everything pine
answers rests on who can reach the socket, so the directory it sits in is checked
rather than assumed."
  (ensure-directories-exist where)
  (let ((it (sb-posix:stat where)))
    (unless (= (sb-posix:stat-uid it) (sb-posix:getuid))
      (error "~a is not yours; pine will not answer in it." where))
    (sb-posix:chmod where #o700))
  where)

(defun where (&optional (name "pine"))
  "The path this pine answers on. Under the runtime directory, which is the
user's own and nobody else's: what can be asked here is everything a lisp running
as this user could do anyway, so it must be exactly that reachable and no more.

PINE_SOCKET says otherwise, for a second pine on one machine: a daemon and the
client that talks to it read the same name the same way, so saying it once in the
environment is the whole of pointing them at each other."
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
              :reader saying))
  (:documentation "One caller, and the way back to them.

SAYING because two threads write here: the one answering what was asked, and
whichever one moved a place somebody is watching. A stream is not a queue and a
line written into the middle of another is a line nobody can read."))

(defun %say (c text)
  "One line to this caller, or nothing if they have gone. A watch fires long after
the question that asked for it, and whoever it was for may have closed the socket
in between: that is what happens, not something that broke."
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
  "Answer this caller until they go. Their watches go when they do."
  (unwind-protect
       (peer:telling ((lambda (said) (%say c (wire:evented said))) (watching c) t)
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

(defun open-socket (&key (name "pine"))
  "Answer on a socket, in the words anything can speak.

Not another protocol: every line becomes one of the questions the tree already
takes, and what comes back is what it said. What this adds is that saying it
needs no lisp on the other end."
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
                            :runs (%accepting socket))))
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
  "Where this pine answers, as a place. Somebody who has the tree by another way
can read where to reach it by this one."
  (node:answers "serve"
              :reads #'listening
              :describes "the socket this pine answers on"))

(defun %attach (root)
  (node:attach (serve-node) root))

(pine/fs/tree:builder #'%attach)
