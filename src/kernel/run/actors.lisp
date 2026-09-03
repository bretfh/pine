(defpackage #:pine/run/actors
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:fault #:pine/run/fault))
  (:export
   #:boot #:leave #:actors #:runningp #:remoting
   #:dispatcher-for #:*host* #:*port* #:schedule #:unschedule #:after
   #:later #:blocking #:joined #:*reading-workers* #:*watching-workers*))
(in-package #:pine/run/actors)

(defvar *actors* nil)
(defvar *wheel* nil)
(defvar *host* "127.0.0.1")
(defvar *port* 17000)
(defparameter *soonest* 0.05)
(defvar *workers* nil
  "How many workers the shared pool has, or nothing to ask the machine at boot.

Asked then and not while this file loads, because a saved image is built on one
machine and run on another: read at load, the number the binary carries is the
number of cores the machine that built it had.")
(defvar *reading-workers* 8
  "Workers on the pool a slow working-out is handed to.")
(defvar *watching-workers* 4
  "Workers on the pool a watcher is told on: enough that one that shells out does
not hold up the rest, few enough that a hundred of them cannot take the machine.")

(defun dispatcher-for (name)
  "The dispatcher an actor asks for, if this image has it. Round robin for work,
because sixty pieces handed out at random leave some workers holding two."
  (if (member name '(:shared :pinned :working :watch)) name :shared))

(defun workers ()
  "How many workers to run the shared pool with, asked of this machine."
  (or *workers*
      (setf *workers*
            (max 2 (1- (or (fault:or-nothing "a machine that will not say how many cores"
                             (parse-integer
                              (uiop:run-program '("nproc")
                                                :output '(:string :stripped t))))
                           4))))))

(defun %config ()
  (list :dispatchers
        (list :shared (list :workers (workers) :strategy :random)
              :working (list :workers *reading-workers* :strategy :round-robin)
              :watch (list :workers *watching-workers* :strategy :round-robin))
        :scheduler (list :enabled :true :max-size 1000
                         :resolution (round (* 1000 *soonest*)))))

(defun actors () *actors*)

(defun runningp () (and *actors* t))

(defun boot (&key remoting)
  "One actor system for this image, made whether or not remoting is on. Everything
that runs is on it: the wheel, the pools, every actor. There is no second clock and
no thread that sleeps in a loop."
  (let ((sys (sento.actor-system:make-actor-system (%config))))
    (when remoting
      (fault:or-nothing "that port is taken"
        (sento.remoting:enable-remoting sys :host *host* :port remoting))
      (unless (sento.remoting:remoting-enabled-p sys)
        (fault:or-nothing "no port to answer peers on"
          (sento.remoting:enable-remoting sys :host *host* :port 0))))
    (setf *actors* sys
          *wheel* (sento.actor-system:scheduler sys))
    sys))

(defun later (name thunk)
  "Hand THUNK to the dispatcher called NAME and do not wait for it.

One message to a worker, not a task: a task is an actor made and stopped again,
which is the right shape for something a person asked for once and the wrong shape
for what every write hands over."
  (let ((sys *actors*))
    (if sys
        (let ((to (or (getf (sento.actor-system:dispatchers sys) name)
                      (getf (sento.actor-system:dispatchers sys) :shared))))
          (sento.dispatcher:dispatch-async
           to (list (lambda () (fault:attempt thunk (format nil "~a work" name)))))
          t)
        (progn (funcall thunk) t))))

(defun %hand-off (n thunk)
  (declare (ignore n))
  (later :working thunk))

(defun remoting ()
  "The port other pines reach this one on, or nothing. One question, one name."
  (and *actors*
       (sento.remoting:remoting-enabled-p *actors*)
       (sento.remoting:remoting-port *actors*)))

(defun leave ()
  (let ((sys *actors*))
    (when sys
      (when (sento.remoting:remoting-enabled-p sys)
        (fault:or-nothing "remoting may already be off"
          (sento.remoting:disable-remoting sys)))
      (fault:or-nothing "a system already down cannot be put down twice"
        (sento.actor-context:shutdown sys :wait t))))
  (setf *actors* nil *wheel* nil)
  t)

(defun %off-wheel (thunk what)
  "Off the wheel thread. The wheel is one thread for the whole image and these
thunks shell out, read files and paint."
  (lambda ()
    (let ((sys *actors*))
      (flet ((run () (fault:attempt thunk what)))
        (if sys
            (sento.tasks:with-context (sys) (sento.tasks:task-start #'run))
            (run))))))

(defun schedule (seconds thunk what)
  "Run THUNK every SECONDS on the wheel, and answer what to unschedule it by."
  (when *wheel*
    (let ((signature (gensym "PINE-REPEAT-"))
          (seconds (max seconds *soonest*)))
      (sento.wheel-timer:schedule-recurring *wheel* seconds seconds
                                            (%off-wheel thunk what) signature)
      signature)))

(defun unschedule (signature)
  (when (and signature *wheel*)
    (fault:or-nothing "a tick that has already fired is not there to cancel"
      (sento.wheel-timer:cancel *wheel* signature)))
  signature)

(defun after (seconds thunk &key (what "a tick"))
  (when *wheel*
    (sento.wheel-timer:schedule-once *wheel* (max seconds *soonest*)
                                     (%off-wheel thunk what))))

(defun blocking (name thunk)
  "A thread, for something that blocks: a pty read, a child's stdout, a frontend's
own loop. Everything else is an actor or a tick."
  (bordeaux-threads:make-thread thunk :name (format nil "pine ~a" name)))

(defun joined (thread)
  "Wait for one of those to finish. What is read on another thread has to be read
to the end before whoever started it goes, or the last of it is lost."
  (when thread (bordeaux-threads:join-thread thread)))

(setf fs:*working* #'%hand-off)
