(defpackage #:pine/cli
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:actors #:pine/run/actors))
  (:export
   #:main #:ask #:runningp #:quiet #:*timeout*))
(in-package #:pine/cli)

(defparameter *usage*
  "usage: pine VERB [ARGUMENT...]

  read WHERE            what a node holds
  write WHERE VALUE     write a node
  ls [WHERE]            what is under a node
  watch WHERE           say so whenever it moves, until interrupted
  eval FORM             evaluate a form in the daemon
  run NAME [ARG...]     run one of the daemon's commands
  use NAME              load a system there and start it
  drop NAME             stop one and take it off
  systems               what it has loaded
  jobs                  what is running
  status                what the daemon is
  reload                read the config again
  start | stop | restart   the daemon itself
  daemon                a daemon in this terminal
  shell                 a pine in this terminal, with no daemon")

(defvar *timeout* 5)
(defvar *actor* "tree")

(defun usage () *usage*)

(defun quiet ()
  (pine/run/fault:or-nothing "log4cl may not be loaded"
    (log4cl-impl:log-config :error))
  t)

(defun %system (&key (name "cli"))
  (quiet)
  (let ((sys (sento.actor-system:make-actor-system)))
    (sento.remoting:enable-remoting sys :host actors:*host* :port 0)
    (values sys name)))

(defun %uri (host port actor) (format nil "sento://~a:~d/user/~a" host port actor))

(defun %daemon (sys &key (host actors:*host*) (port actors:*port*))
  (sento.remoting:make-remote-ref sys (%uri host port *actor*)))

(defun ask (message &key (host actors:*host*) (port actors:*port*) system)
  (pine/run/fault:or-nothing "there may be no daemon listening"
    (let* ((sys (or system (%system)))
           (said (sento.actor:ask-s (%daemon sys :host host :port port)
                                    message :time-out *timeout*)))
      (if (and (consp said) (member (first said) '(:ok :no)))
          said
          (list :no (format nil "~a" said))))))

(defun runningp (&key (port actors:*port*))
  (let ((said (ask (list :ping) :port port)))
    (and said (eq :ok (first said)))))

(defun %value (text)
  "What a word on the command line means. A form is read as one, so t is true and
42 is a number; a bare word stays the word it was."
  (if (null text)
      nil
      (let ((said (handler-case (let ((*read-eval* nil))
                                  (values (read-from-string text)))
                    (error () text))))
        (typecase said
          (null nil)
          (keyword said)
          (symbol (if (eq said t) t text))
          (t said)))))

(defun %verb (value)
  "Whether what was typed is a verb a node takes rather than a value to put in it:
(:stop), (:restart). It crosses as its own message, because what a node is told to
do and what it is given are two questions."
  (and (consp value) (keywordp (first value)) value))

(defun %say (said)
  (cond ((null said)
         (format t "pine: no daemon at ~a:~d~%" actors:*host* actors:*port*)
         nil)
        ((eq :no (first said))
         (format t "pine: ~a~%" (second said))
         nil)
        (t (let ((value (second said)))
             (typecase value
               (null nil)
               (string (when (plusp (length value)) (format t "~a~%" value)))
               (cons (dolist (each value) (format t "~a~%" each)))
               (t (format t "~a~%" value))))
           t)))

(defun %evaluate (form)
  (let ((said (ask (list :evaluate form))))
    (cond ((null said)
           (format t "pine: no daemon at ~a:~d~%" actors:*host* actors:*port*)
           nil)
          ((eq :no (first said)) (format t "pine: ~a~%" (second said)) nil)
          (t (let ((answer (rest said)))
               (let ((printed (getf answer :said)))
                 (when (and printed (plusp (length printed)))
                   (write-string printed)))
               (cond ((getf answer :said-broke)
                      (format t "pine: ~a~%" (getf answer :said-broke))
                      (dolist (r (getf answer :offers))
                        (format t "  ~a~%" r))
                      nil)
                     (t (dolist (v (getf answer :answered)) (format t "~s~%" v))
                        t)))))))

(defun %watch (where)
  (multiple-value-bind (sys name) (%system :name "watch")
    (sento.actor-context:actor-of
     sys :name name
     :receive (lambda (message)
                (when (eq :moved (first message))
                  (format t "~a ~a~%" (second message) (third message))
                  (finish-output))))
    (let ((uri (%uri actors:*host* (sento.remoting:remoting-port sys) name)))
      (when (%say (ask (list :watch where uri) :system sys))
        (%park)))))

(defun %park ()
  "Block until this process is killed. A semaphore nobody holds and nobody signals
waits on nothing at all, where a loop round a sleep wakes for ever to find out
that nothing has happened."
  (sb-thread:wait-on-semaphore (sb-thread:make-semaphore)))

(defun %self ()
  (let ((self (first sb-ext:*posix-argv*)))
    (if (and self (not (search "sbcl" (namestring self))))
        (list self)
        (list (namestring sb-ext:*runtime-pathname*)
              "--noinform" "--no-userinit" "--non-interactive"
              "--eval" "(require :asdf)"
              "--eval" "(asdf:load-system :pine)"
              "--eval" "(pine/cli:main (rest sb-ext:*posix-argv*))"
              "--end-toplevel-options"))))

(defun %waiting-on-it (readyp seconds)
  "Wait for another process to answer, or not to. The one place pine looks in a
loop instead of waiting: what it is waiting for is a socket a separate process has
not opened yet, so there is nothing here to be woken by."
  (loop :with due := (+ (get-universal-time) seconds)
        :until (funcall readyp)
        :do (sleep 0.2)
            (when (>= (get-universal-time) due) (return nil))
        :finally (return t)))

(defun %say-line (control &rest arguments)
  (apply #'format t control arguments)
  (finish-output))

(defun %start ()
  (if (runningp)
      (%say-line "pine: already running on ~d~%" actors:*port*)
      (progn
        (uiop:launch-program (append (%self) (list "daemon"))
                             :output nil :error-output nil)
        (%waiting-on-it (lambda () (runningp)) 30)
        (%say-line "pine: ~:[did not come up~;running on ~d~]~%"
                   (runningp) actors:*port*))))

(defun %gonep (&key (seconds 5))
  (%waiting-on-it (lambda () (not (runningp))) seconds))

(defun %stop ()
  (cond ((not (runningp)) (%say-line "pine: not running~%"))
        (t (ask (list :evaluate '(pine:quit)))
           (if (%gonep)
               (%say-line "stopped~%")
               (%say-line "pine: still running on ~d~%" actors:*port*)))))

(defun %run (arguments)
  (%evaluate `(pine/run/command:run ,(first arguments)
                                    (list ,@(rest arguments)))))

(defun main (&optional (arguments (rest sb-ext:*posix-argv*)))
  (quiet)
  (pine/run/libs:attend)
  (let ((verb (first arguments))
        (rest (rest arguments)))
    (cond
      ((null verb) (format t "~a~%" *usage*))
      ((equal verb "read") (%say (ask (list :contents (first rest)))))
      ((equal verb "write")
       (let ((value (%value (second rest))))
         (%say (ask (if (%verb value)
                        (list* :verb (first rest) value)
                        (list :write (first rest) value))))))
      ((equal verb "ls") (%say (ask (list :nodes (or (first rest) "/")))))
      ((equal verb "watch") (%watch (first rest)))
      ((equal verb "eval") (%evaluate (read-from-string (first rest))))
      ((equal verb "run") (%run rest))
      ((equal verb "use") (%run (list "use" (first rest))))
      ((equal verb "drop") (%run (list "drop" (first rest))))
      ((equal verb "systems") (%run (list "systems")))
      ((equal verb "jobs") (%run (list "jobs")))
      ((equal verb "status") (format t "pine: ~:[not running~;running on ~d~]~%"
                                     (runningp) actors:*port*))
      ((equal verb "reload") (%run (list "reload")))
      ((equal verb "start") (%start))
      ((equal verb "stop") (%stop))
      ((equal verb "restart") (%stop) (%start))
      ((equal verb "daemon")
       (setf pine/fs/log:*to* *standard-output*)
       (pine:daemon)
       (%park))
      ((equal verb "shell") (pine:main))
      ((equal verb "help") (format t "~a~%" *usage*))
      (t (format t "pine: no verb ~a~%~a~%" verb *usage*)))
    (finish-output)))

(pine/word:user)
