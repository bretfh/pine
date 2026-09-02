(defpackage #:pine/run/actors
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:fault #:pine/run/fault))
  (:export
   #:boot #:leave #:actors #:runningp #:remoting
   #:dispatcher-for #:*host* #:*port* #:repeat #:cancel #:pool #:pools
   #:later #:ticks #:blocking #:joined))
(in-package #:pine/run/actors)

(defvar *actors* nil)
(defvar *wheel* nil)
(defvar *ticks* (d:table))
(defvar *host* "127.0.0.1")
(defvar *port* 17000)
(defparameter *soonest* 0.05)
(defvar *workers* nil
  "How many workers the shared pool has, or nothing to ask the machine at boot.

Asked then and not while this file loads, because a saved image is built on one
machine and run on another: read at load, the number the binary carries is the
number of cores the machine that built it had.")
(defvar *reading-workers* 8)
(defvar *pools* (d:table)
  "The dispatchers this image will have, by name. A system that wants work of its
own kept off everybody else's asks for one before boot; sento fixes them when the
system is made, so this is read once and not added to after.")

(defun pool (name workers &key (strategy :round-robin))
  "Ask for a dispatcher of NAME with WORKERS workers. Nothing is reserved: the
parse has one because text asked for it, not because this file knows about text.

Round robin, and not the random the shared one takes. Random is right for
messages, where an actor's mailbox is the thing that keeps order and one worker
being briefly unlucky costs nothing. It is wrong for work: sixty pieces handed out
at random leave some workers holding two while others hold none, and the whole is
only done when the unluckiest is. Measured on thirty-one workers, that is 7.9x
against 11.9x."
  (d:keep! *pools* name (list workers strategy))
  name)

(defun pools () (d:all *pools*))

(defun dispatcher-for (name)
  "The dispatcher an actor asks for, if this image has it. Sento fixes its pools
when the system is made, so a system loaded after boot that asked for one of its
own runs on the shared pool until the next start rather than refusing to run."
  (if (or (member name '(:shared :pinned)) (d:lookup (pools) name))
      name
      :shared))

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
        (list* :shared (list :workers (workers) :strategy :random)
               (loop :for (name . asked) :in (d:pairs (pools))
                     :append (list name (list :workers (first asked)
                                              :strategy (second asked)))))
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
  (dolist (name (ticks)) (cancel name))
  (let ((sys *actors*))
    (when sys
      (when (sento.remoting:remoting-enabled-p sys)
        (fault:or-nothing "remoting may already be off"
          (sento.remoting:disable-remoting sys)))
      (fault:or-nothing "a system already down cannot be put down twice"
        (sento.actor-context:shutdown sys :wait t))))
  (setf *actors* nil *wheel* nil)
  t)

(defun ticks () (d:keys (d:all *ticks*)))

(defun %off-wheel (thunk what)
  "Off the wheel thread. The wheel is one thread for the whole image and these
thunks shell out, read files and paint."
  (lambda ()
    (let ((sys *actors*))
      (flet ((run () (fault:attempt thunk what)))
        (if sys
            (sento.tasks:with-context (sys) (sento.tasks:task-start #'run))
            (run))))))

(defun cancel (name)
  (let ((had (d:lookup (d:all *ticks*) name)))
    (when (and had *wheel*)
      (fault:or-nothing "a tick that has already fired is not there to cancel"
        (sento.wheel-timer:cancel *wheel* had))
      (d:drop! *ticks* name))
    name))

(defun repeat (seconds thunk &key (as (gensym "REPEAT-")) (what "a tick"))
  "Run THUNK every SECONDS, under the name AS. Asking again under one name replaces
what was there."
  (when *wheel*
    (cancel as)
    (let ((signature (gensym "PINE-REPEAT-"))
          (seconds (max seconds *soonest*)))
      (sento.wheel-timer:schedule-recurring *wheel* seconds seconds
                                            (%off-wheel thunk what) signature)
      (d:keep! *ticks* as signature)
      as)))

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

(defun %named (name)
  (find name (ticks) :key #'princ-to-string :test #'equal))

(defun %tick (name)
  (when (%named name)
    (make-instance 'node:place :name name
                :reads (lambda () (and (%named name) t))
                :writes (lambda (value)
                          (let ((had (%named name)))
                            (when (and had (null value)) (cancel had)))))))

(defun %attach (root)
  (node:attach
   (make-instance 'node:place :name "tick"
               :names #'ticks
               :each #'%tick
               :reads (lambda () (mapcar #'princ-to-string (ticks)))
               :describes "what repeats on the image's clock")
   root))

(pine/fs/tree:builder #'%attach)

(pool :working *reading-workers*)

(setf node:*working* #'%hand-off)
