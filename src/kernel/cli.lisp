(defpackage #:pine/cli
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:actors #:pine/run/actors)
                    (#:socket #:pine/serve/socket) (#:wire #:pine/serve/wire))
  (:export
   #:main))
(in-package #:pine/cli)

(defparameter *usage*
  "usage: pine VERB [ARGUMENT...]

  read WHERE            what a node holds
  write WHERE VALUE     write a node
  ls [WHERE]            what is under a node
  watch WHERE           say so whenever it moves, until interrupted
  toggle WHERE          flip what a node holds
  include WHERE VALUE   put one into the set a node holds
  exclude WHERE VALUE   take one out of it
  blend WHERE MAP       merge into the map a node holds
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
  serve                 speak the wire on stdin and stdout, a line each way
  shell                 a pine in this terminal, with no daemon")

(defvar *held* nil)
(defvar *asked* 0)
(defparameter +last+ -1)

(defun usage () *usage*)

(defun quiet ()
  (pine/run/fault:or-nothing "log4cl may not be loaded"
    (log4cl-impl:log-config :error))
  t)

(defun %connect (&optional (path (socket:where)))
  (pine/run/fault:or-nothing "nothing may be answering there"
    (let ((it (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (sb-bsd-sockets:socket-connect it path)
      (sb-bsd-sockets:socket-make-stream it :input t :output t
                                            :element-type 'character
                                            :external-format :utf-8))))

(defun held ()
  (or *held* (setf *held* (%connect))))

(defun let-go ()
  (when *held*
    (pine/run/fault:or-nothing "one already closed is closed" (close *held*))
    (setf *held* nil))
  t)

(defun listeningp (&optional (path (socket:where)))
  (let ((it (%connect path)))
    (when it (close it) t)))

(defun ask (message &key stream)
  (let ((to (or stream (held))))
    (when to
      (pine/run/fault:or-nothing "the daemon may go while we are asking it"
        (let ((mine (incf *asked*)))
          (write-line (wire:encode-request mine message) to)
          (force-output to)
          (loop :for line := (read-line to nil nil)
                :while line
                :do (multiple-value-bind (said id eventp) (wire:decode-reply line)
                      (unless (or eventp (and id (not (eql id mine))))
                        (return said)))))))))

(defun runningp () (and (ask (list :ping)) t))

(defun %read-whole (text &key syntax)
  (handler-case
      (multiple-value-bind (said at)
          (let ((*read-eval* nil)
                (*readtable* (if syntax
                                 (named-readtables:find-readtable
                                  'pine/fs/reader:syntax)
                                 *readtable*)))
            (read-from-string text))
        (if (< at (length (string-right-trim '(#\Space #\Tab #\Newline) text)))
            (values nil nil)
            (values said t)))
    (error () (values nil nil))))

(defun %value (text)
  (if (null text)
      nil
      (multiple-value-bind (said wholep) (%read-whole text)
        (if (not wholep)
            text
            (typecase said
              (null nil)
              (keyword said)
              (symbol (if (eq said t) t text))
              (t said))))))

(defun %verb (value)
  (and (consp value) (keywordp (first value)) value))

(defun %nobody ()
  (format t "pine: nothing answering at ~a~%" (socket:where))
  nil)

(defun %say (said)
  (cond ((null said)
         (%nobody)
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
    (cond ((null said) (%nobody) nil)
          ((eq :no (first said)) (format t "pine: ~a~%" (second said)) nil)
          (t (let ((answer (second said)))
               (let ((printed (getf answer :said)))
                 (when (and (stringp printed) (plusp (length printed)))
                   (write-string printed)))
               (cond ((getf answer :said-broke)
                      (format t "pine: ~a~%" (getf answer :said-broke))
                      (dolist (r (getf answer :offers))
                        (format t "  ~a~%" r))
                      nil)
                     (t (dolist (v (getf answer :answered)) (format t "~a~%" v))
                        t)))))))

(defun %watch (where)
  (let ((to (held)))
    (when (%say (ask (list :watch where)))
      (loop :for line := (read-line to nil nil)
            :while line
            :do (multiple-value-bind (said id eventp) (wire:decode-reply line)
                  (declare (ignore id))
                  (when eventp
                    (format t "~a~%" (second said))
                    (finish-output)))))))

(defun %serve ()
  (let ((out *standard-output*)
        (*standard-output* *error-output*)
        (to (held)))
    (fresh-line out)
    (cond
      ((null to) (%nobody))
      (t
       (let ((reader (pine/run/actors:blocking
                      "what the daemon says"
                      (lambda ()
                        (loop :for line := (read-line to nil nil)
                              :while line
                              :for id := (nth-value 1 (wire:decode-reply line))
                              :until (eql id +last+)
                              :do (write-line line out)
                                  (finish-output out))))))
         (loop :for line := (read-line *standard-input* nil nil)
               :while line
               :do (write-line line to) (force-output to))
         (write-line (wire:encode-request +last+ (list :ping)) to)
         (force-output to)
         (pine/run/actors:joined reader)
         (let-go))))))

(defun %park ()
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
      (%say-line "pine: already answering on ~a~%" (socket:where))
      (progn
        (uiop:launch-program (append (%self) (list "daemon"))
                             :output nil :error-output nil)
        (%waiting-on-it (lambda () (runningp)) 30)
        (%say-line "pine: ~:[did not come up; run pine daemon to see why~;answering on ~a~]~%"
                   (runningp) (socket:where)))))

(defun %gonep (&key (seconds 5))
  (%waiting-on-it (lambda () (not (runningp))) seconds))

(defun %stop ()
  (cond ((not (runningp)) (%say-line "pine: not running~%"))
        (t (ask (list :evaluate '(pine:quit)))
           (if (%gonep)
               (%say-line "stopped~%")
               (%say-line "pine: still answering on ~a~%" (socket:where))))))

(defun %run (arguments)
  (%evaluate `(pine/run/command:run ,(first arguments)
                                    (list ,@(rest arguments)))))

(defun %wants (verb rest n what)
  (cond ((>= (length rest) n) t)
        (t (format t "pine: ~a wants ~a~%usage: pine ~a ~a~%" verb what verb what)
           nil)))

(defun main (&optional (arguments (rest sb-ext:*posix-argv*)))
  (quiet)
  (pine/run/libs:attend)
  (let ((verb (first arguments))
        (rest (rest arguments)))
    (cond
      ((null verb) (format t "~a~%" *usage*))
      ((equal verb "read")
       (when (%wants verb rest 1 "WHERE")
         (%say (ask (list :contents (first rest))))))
      ((equal verb "write")
       (when (%wants verb rest 2 "WHERE VALUE")
         (let ((value (%value (second rest))))
           (%say (ask (if (%verb value)
                          (list* :verb (first rest) value)
                          (list :write (first rest) value)))))))
      ((equal verb "ls") (%say (ask (list :entries (or (first rest) "/")))))
      ((equal verb "toggle")
       (when (%wants verb rest 1 "WHERE")
         (%say (ask (list :verb (first rest) :toggle)))))
      ((member verb '("include" "exclude" "blend") :test #'equal)
       (when (%wants verb rest 2 "WHERE VALUE")
         (%say (ask (list :verb (first rest)
                          (cond ((equal verb "include") :conj)
                                ((equal verb "exclude") :disj)
                                (t :merge))
                          (%value (second rest)))))))
      ((equal verb "watch")
       (when (%wants verb rest 1 "WHERE") (%watch (first rest))))
      ((equal verb "eval")
       (when (%wants verb rest 1 "FORM")
         (multiple-value-bind (form wholep) (%read-whole (first rest) :syntax t)
           (if wholep
               (%evaluate form)
               (format t "pine: ~s is not one form to evaluate~%" (first rest))))))
      ((equal verb "run")
       (when (%wants verb rest 1 "NAME [ARG...]") (%run rest)))
      ((equal verb "use")
       (when (%wants verb rest 1 "NAME") (%run (list "use" (first rest)))))
      ((equal verb "drop")
       (when (%wants verb rest 1 "NAME") (%run (list "drop" (first rest)))))
      ((equal verb "systems") (%run (list "systems")))
      ((equal verb "jobs") (%run (list "jobs")))
      ((equal verb "status") (format t "pine: ~:[not running~;running on ~d~]~%"
                                     (runningp) (socket:where)))
      ((equal verb "reload") (%run (list "reload")))
      ((equal verb "start") (%start))
      ((equal verb "stop") (%stop))
      ((equal verb "restart") (%stop) (%start))
      ((equal verb "daemon")
       (setf pine/fs/log:*to* *standard-output*)
       (pine:daemon)
       (%park))
      ((equal verb "serve") (%serve))
      ((equal verb "shell") (pine:main))
      ((equal verb "help") (format t "~a~%" *usage*))
      (t (format t "pine: no verb ~a~%~a~%" verb *usage*)))
    (finish-output)))

