(defpackage #:pine.cli
  (:use #:cl)
  (:local-nicknames (#:server #:pine.core.server))
  (:export #:main #:usage))

(in-package #:pine.cli)

;;;; pine is the three verbs against the running daemon, plus the lifecycle it
;;;; cannot express from inside itself.
;;;;
;;;;   pine read  /audio/volume
;;;;   pine read  '/proc/*/state'
;;;;   pine write /audio/volume 40
;;;;   pine write /buf/scratch/text '[:insert "hello"]'
;;;;   pine watch /media/title
;;;;   pine diff  /was/-1h /
;;;;
;;;;   pine start | stop | restart | status
;;;;   pine eval  '(any lisp form)'
;;;;
;;;; It holds no state and knows nothing about what it is asking for: a path is
;;;; text and a value is readable lisp, so a path added tomorrow is reachable
;;;; from the shell today.

(defparameter *usage*
  "usage: pine {read PATH | write PATH VALUE | watch PATH | diff FROM TO |
             start | stop | restart | daemon | editor | desktop | wm |
             session [DISPLAY] | status | eval FORM | reload | agents |
             spawn NAME | kill NAME}")

(defun usage () *usage*)

(defun %system ()
  "An actor system for this one command. The process exits after, so nothing
here is torn down."
  (let ((sys (sento.actor-system:make-actor-system
              '(:dispatchers (:shared (:workers 1 :strategy :random))))))
    (sento.remoting:enable-remoting sys :host server:*host* :port 0)
    sys))

(defun %control (sys &key (host server:*host*) (port server:*port*))
  (sento.remoting:make-remote-ref
   sys (server:daemon-uri "control" :host host :port port)))

(defun ask (message &key (host server:*host*) (port server:*port*))
  "Ask the daemon MESSAGE, print what it said, and answer it."
  (handler-case
      (let ((answer (pine.core.actor:ask (%control (%system) :host host :port port)
                                         message :timeout 5)))
        (format t "~a~%" answer)
        answer)
    (error () (format t "pine: no daemon at ~a:~d~%" host port) nil)))

;;;; The three verbs. Reading and writing are one ask each; watching stands up
;;;; an actor of its own and stays.

(defun read-path (text) (ask (list :read text)))

(defun write-path (text value) (ask (list :write text value)))

(defun diff-paths (from to) (ask (list :diff from to)))

(defun watch-path (text &key (host server:*host*) (port server:*port*))
  "Print what a path says whenever it moves, until this process is stopped."
  (let* ((sys (%system))
         (name (format nil "watch-~a" (sb-unix:unix-getpid)))
         (uri (server:local-uri name (sento.remoting:remoting-port sys))))
    (sento.actor-context:actor-of
     sys :name name :dispatcher :pinned
     :receive (lambda (message)
                (when (eq :moved (first message))
                  (format t "~a ~a~%" (second message) (third message))
                  (finish-output))
                nil))
    (handler-case
        (pine.core.actor:ask (%control sys :host host :port port)
                             (list :watch text uri) :timeout 5)
      (error ()
        (format t "pine: no daemon at ~a:~d~%" host port)
        (return-from watch-path nil)))
    (loop (sleep 3600))))

;;;; Lifecycle: what the daemon cannot do to itself.

(defun stop-daemon (&key (port server:*port*))
  "Ask the daemon to shut down, taking its frontends with it, then make sure
the port is free even if it was an old or wedged daemon."
  (ignore-errors
    (pine.core.actor:ask (%control (%system) :port port) '(:stop) :timeout 3))
  (sleep 0.4)
  (unless (pine:port-free-p port) (pine:kill-port port))
  (format t "pine: stopped~%"))

(defun restart-daemon (&key (port server:*port*))
  (stop-daemon :port port)
  (loop :repeat 40 :until (pine:port-free-p port) :do (sleep 0.25))
  (pine:run-all))

(defun main (&optional (args (rest sb-ext:*posix-argv*)))
  ;; a CLI prints its answer, nothing else: quiet sento/log4cl's INFO chatter
  ;; (actor-system config, "Remoting enabled on ...") that otherwise buries it.
  (log:config :error)
  (let ((verb (first args)) (more (rest args)))
    (cond
      ((null verb) (format t "~a~%" *usage*))
      ;; the three verbs
      ((string= verb "read")  (read-path (first more)))
      ((string= verb "write") (write-path (first more)
                                          (format nil "~{~a~^ ~}" (rest more))))
      ((string= verb "watch") (watch-path (first more)))
      ((string= verb "diff")  (diff-paths (first more) (second more)))
      ;; the lifecycle
      ((string= verb "start")   (pine:run-all))
      ((string= verb "stop")    (stop-daemon))
      ((string= verb "restart") (restart-daemon))
      ((string= verb "daemon")  (pine:run-daemon))
      ((member verb '("editor" "desktop" "wm") :test #'string=)
       (pine:run-app verb))
      ((string= verb "status") (ask '(:status)))
      ((string= verb "eval")   (ask (list :eval (format nil "~{~a~^ ~}" more))))
      ((string= verb "session")
       (ask (list :session (or (first more) (uiop:getenv "WAYLAND_DISPLAY")))))
      ((string= verb "reload") (ask '(:reload)))
      ((string= verb "agents") (ask '(:agents)))
      ((string= verb "spawn")  (ask (list :spawn (first more))))
      ((string= verb "kill")   (ask (list :kill (first more))))
      (t (format t "pine: unknown verb ~a~%~a~%" verb *usage*)))))
