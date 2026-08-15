(defpackage #:pine.cli
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:server #:pine.net.server) )
  (:export #:main #:usage #:ask #:running-p #:quiet #:*usage*))

(in-package #:pine.cli)

(defparameter *usage*
  "usage: pine VERB [ARGUMENT...]

  read WHERE            what a node holds
  write WHERE VALUE     write a node
  ls [WHERE]            what is under a node
  watch WHERE           say so whenever it moves, until interrupted
  eval FORM             evaluate a form in the daemon
  run NAME [ARG...]     run one of the daemon's commands
  diff WHERE OTHER      what is under one that is not under the other
  status                what the daemon is
  reload                read the config again
  agents                every image attached to this one
  spawn NAME LINE       run a program under the supervisor
  kill NAME             stop one
  start | stop | restart   the daemon itself
  daemon                a daemon in this terminal
  editor | desktop | wm a frontend, attached to the daemon
  shell                 a repl in this terminal, with no daemon")

(defvar *timeout* 5)
(defvar *heard* (d:box nil))

(defun usage () *usage*)

(defun quiet ()
  (ignore-errors (log4cl-impl:log-config :warn))
  t)

(defun %system (&key (name "cli"))
  (quiet)
  (let ((sys (sento.actor-system:make-actor-system)))
    (sento.remoting:enable-remoting sys :host server:*host* :port 0)
    (values sys name)))

(defun %control (sys &key (host server:*host*) (port server:*port*))
  (sento.remoting:make-remote-ref
   sys (server:daemon-uri "control" :host host :port port)))

(defun ask (message &key (host server:*host*) (port server:*port*) (system nil))
  (handler-case
      (let* ((sys (or system (%system)))
             (answer (sento.actor:ask-s (%control sys :host host :port port)
                                        message :time-out *timeout*)))
        (if (and (consp answer) (member (first answer) '(:ok :no)))
            answer
            (list :no (format nil "~a" answer))))
    (error () nil)))

(defun running-p (&key (port server:*port*))
  (let ((answer (ask (list :ping) :port port)))
    (and answer (eq :ok (first answer)))))

(defun %say (answer)
  (cond ((null answer)
         (format t "pine: no daemon at ~a:~d~%" server:*host* server:*port*)
         nil)
        ((eq :no (first answer))
         (format t "pine: ~a~%" (second answer))
         nil)
        (t
         (let ((text (second answer)))
           (when (plusp (length text)) (format t "~a~%" text)))
         t)))

(defun %watch (where)
  (multiple-value-bind (sys name) (%system :name "watch")
    (pine/run/agent:agent name
                          (lambda (message)
                            (when (eq :moved (first message))
                              (format t "~a ~a~%" (second message) (third message))
                              (finish-output)))
                          :dispatcher :pinned :in sys)
    (let ((uri (server:local-uri name (sento.remoting:remoting-port sys))))
      (when (%say (ask (list :watch where uri) :system sys))
        (loop (sleep 60))))))

(defun %self ()
  (let ((self (first sb-ext:*posix-argv*)))
    (if (and self (not (search "sbcl" (namestring self))))
        (list self)
        (list (namestring sb-ext:*runtime-pathname*)
              "--noinform" "--no-userinit" "--non-interactive"
              "--eval" "(require :asdf)"
              "--eval" "(asdf:load-system :pine/wayland)"
              "--eval" "(pine.cli:main (rest sb-ext:*posix-argv*))"
              "--end-toplevel-options"))))

(defun %start ()
  (if (running-p)
      (format t "pine: already running on ~d~%" server:*port*)
      (progn
        (uiop:launch-program (append (%self) (list "daemon"))
                             :output nil :error-output nil)
        (loop :repeat 60
              :until (running-p)
              :do (sleep 0.5))
        (format t "pine: ~:[did not come up~;running on ~d~]~%"
                (running-p) server:*port*))))

(defun %pid ()
  (let ((answer (ask (list :pid))))
    (when (and answer (eq :ok (first answer)))
      (parse-integer (second answer) :junk-allowed t))))

(defun %gone-p (&key (seconds 5))
  (loop :repeat (round (/ seconds 0.2))
        :unless (running-p) :do (return t)
        :do (sleep 0.2)
        :finally (return (not (running-p)))))

(defun %stop ()
  (cond
    ((not (running-p)) (format t "pine: not running~%"))
    (t
     (let ((pid (%pid)))
       (ask (list :quit))
       (unless (%gone-p)
         (when pid
           (format t "pine: ~d did not stop, killing it~%" pid)
           (ignore-errors (sb-posix:kill pid 15))
           (unless (%gone-p :seconds 3)
             (ignore-errors (sb-posix:kill pid 9))
             (%gone-p :seconds 3))))
       (if (running-p)
           (format t "pine: still running on ~d~%" server:*port*)
           (format t "stopped~%"))))))

(defun main (&optional (arguments (rest sb-ext:*posix-argv*)))
  (quiet)
  (pine/run/libs:attend)
  (server:read-environment)
  (let ((verb (first arguments))
        (rest (rest arguments)))
    (cond
      ((null verb) (format t "~a~%" *usage*))
      ((equal verb "read")   (%say (ask (list :read (first rest)))))
      ((equal verb "write")  (%say (ask (list :write (first rest) (second rest)))))
      ((equal verb "ls")     (%say (ask (list :ls (first rest)))))
      ((equal verb "eval")   (%say (ask (list :eval (first rest)))))
      ((equal verb "run")    (%say (ask (list* :run rest))))
      ((equal verb "status") (%say (ask (list :status))))
      ((equal verb "diff")   (%say (ask (list :diff (first rest) (second rest)))))
      ((equal verb "reload") (%say (ask (list :reload))))
      ((equal verb "agents") (%say (ask (list :agents))))
      ((equal verb "spawn")  (%say (ask (list :run "spawn" (first rest)
                                              (second rest)))))
      ((equal verb "kill")   (%say (ask (list :run "kill" (first rest)))))
      ((equal verb "restart") (%stop) (sleep 1) (%start))
      ((equal verb "watch")  (%watch (first rest)))
      ((equal verb "start")  (%start))
      ((equal verb "stop")   (%stop))
      ((equal verb "daemon") (pine:daemon) (loop (sleep 60)))
      ((equal verb "shell")  (pine:main))
      ((member verb '("editor" "desktop" "wm") :test #'equal) (pine:run-app verb))
      ((equal verb "help")   (format t "~a~%" *usage*))
      (t (format t "pine: no verb ~a~%~a~%" verb *usage*)))
    (finish-output)))
