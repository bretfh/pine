(defpackage #:pine/run/job
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault))
  (:import-from #:pine/fs/node #:name)
  (:export
   #:job #:thread #:actor #:program #:start
   #:stop #:alivep #:tell #:ask #:jobs
   #:named #:supervise #:sweep #:attend #:emit
   #:stoppingp #:stoppedp #:heldp #:forget #:name #:state #:tries #:kind #:kinds
   #:took #:thunk #:stopping #:argv #:ref))
(in-package #:pine/run/job)

(defvar *out-kept* 200)
(defvar *backoff-cap* 60)
(defvar *settled* 30
  "Seconds a job has to survive before its tries are forgotten. Without this a job
that failed six times is held at the longest backoff for the life of the image,
however well it runs afterwards.")
(defvar *asking* 5)
(defvar *every* 1)
(defvar *stopping* 2
  "Seconds to wait for a thread asked to stop. It is asked and then joined: a
thread blocked on a stream reads to the end of what it has and then looks, so the
wait is for that look and not for a clock.")
(defvar *jobs* (d:table))
(defvar *kinds* (d:table))
(defvar *supervised* nil)
(defvar *under* nil)
(defvar *tries* 8
  "How many times a job is started again before it is held.

A job that dies as fast as it starts is not one more try away from working. The
backoff already spaces the tries out; this is what says to stop, so a crash loop
is something you can read at /proc rather than something the image does for the
rest of its life. Surviving *SETTLED* seconds forgets the tries, so this counts
a run of failures and not a long life with bad days in it.")

(define-condition blocking-ask (error)
  ((of :initarg :of :reader of))
  (:report (lambda (c stream)
             (format stream "Asked ~a from inside a receive.
A receive owes its mailbox an answer, so it may not wait for one: read what it was
handed, or TELL and take the reply as a message." (of c)))))

(defclass job (node:live)
  ((state     :initform :stopped :accessor state)
   (tries     :initform 0        :accessor tries)
   (on-fault :initarg :on-fault :accessor on-fault :initform :restart)
   (took      :initform nil      :accessor took)
   (exit-of   :initform nil      :accessor exit-of)
   (since     :initform nil      :accessor since)
   (fault     :initform nil      :accessor fault)
   (said      :initform nil :reader said))
  (:documentation "Something that runs. Its state, its tries and what it last said
are nodes under it, so what is running is read the way everything else is."))

(defclass thread (job)
  ((thunk    :initarg :thunk   :accessor thunk)
   (seconds  :initarg :seconds :accessor seconds :initform nil)
   (stopping :initform nil :accessor stopping))
  (:documentation "Blocks on something, or repeats on the wheel. With SECONDS it is
a tick and takes no thread at all."))

(defclass actor (job)
  ((receive    :initarg :receive    :accessor receive)
   (dispatcher :initarg :dispatcher :accessor dispatcher :initform :shared))
  (:documentation "Takes messages one at a time, in the order they were sent."))

(defclass program (job)
  ((argv :initarg :argv :accessor argv)
   (env  :initarg :env  :accessor env :initform nil))
  (:documentation "An os child that is not lisp."))

(defmethod print-object ((j job) stream)
  (print-unreadable-object (j stream :type t)
    (format stream "~a ~a" (name j) (state j))))

(defmethod initialize-instance :after ((j job) &key)
  (node:slots j j "state" 'state "tries" 'tries)
  (node:attach (node:answers "said" :reads (lambda () (said j))
                                  :describes "the last lines it said")
               j)
  (node:attach (node:answers "tell"
                           :writes (lambda (value) (tell j value))
                           :describes "write here to give it something")
               j)
  (d:keep! *jobs* (name j) j))

(defun jobs () (d:vals (d:all *jobs*)))

(defun named (name) (d:lookup (d:all *jobs*) (princ-to-string name)))

(defmethod node:contents ((j job)) (state j))

(defmethod node:verb ((j job) name arguments)
  "Starting one that was given up on forgets what it tried before: a person
asking for it by name is saying to try again."
  (declare (ignore arguments))
  (flet ((afresh () (setf (tries j) 0)))
    (case name
      (:start   (afresh) (start j) (state j))
      (:stop    (stop j) (state j))
      (:restart (stop j) (afresh) (start j) (state j))
      (t (error "~a takes :start, :stop or :restart." (node:full-name j))))))

(defun emit (j line)
  (d:swap (slot-value j 'said) #'d:capped line *out-kept*)
  line)

(defun backoff (j)
  (min *backoff-cap* (expt 2 (min 16 (tries j)))))

(defun settle (j)
  "Forget a job's tries once it has run long enough to have earned it."
  (let ((at (since j)))
    (when (and at (plusp (tries j))
               (>= (- (get-universal-time) at) *settled*))
      (setf (tries j) 0)))
  j)

(defun heldp (j)
  "Whether this job has been given up on. Not stopped: nobody asked it to go, and
nothing will start it again until somebody says so."
  (eq :held (state j)))

(defun %hold (j)
  "Stop trying, and say why where the job stands.

Written down rather than logged, because the question a person asks is about
this job and /proc/<name> is where they ask it."
  (setf (state j) :held
        (fault j) (make-condition
                   'simple-error
                   :format-control "gave up after ~d tr~:@p, none lasting ~d second~:p"
                   :format-arguments (list (tries j) *settled*)))
  j)

(defgeneric alivep (job)
  (:documentation "Whether it is running now, asked of the thing itself.")
  (:method ((j job)) (eq :running (state j))))

(defgeneric start (job)
  (:documentation "Run it.")
  (:method :before ((j job))
    (incf (tries j))
    (setf (state j) :starting (fault j) nil (since j) (get-universal-time)))
  (:method :after ((j job))
    (when (eq :starting (state j)) (setf (state j) :running)))
  (:method ((j job))
    (error "~a says nothing about how it starts." (node:full-name j))))

(defgeneric stoppedp (job)
  (:documentation "Whether asking it to stop worked.

Asked of the kind, because only some kinds can say. An actor is gone when the
context has let it go, and what it was is no longer worth reading; a thread is a
thread, and one that never looks between reads is still running whatever pine has
written down about it.")
  (:method ((j job)) t)
  (:method ((j thread))
    (let ((it (took j)))
      (not (and (typep it 'bordeaux-threads:thread)
                (bordeaux-threads:thread-alive-p it))))))

(defgeneric stop (job)
  (:documentation "Let it go.")
  (:method :before ((j job)) (setf (state j) :stopping))
  (:method :after ((j job))
    "Stopped where it really stopped. One that was asked and did not go is still
running, and calling it stopped leaves it holding what it holds with nothing left
in the tree that names it."
    (if (stoppedp j)
        (setf (state j) :stopped (took j) nil)
        (setf (state j) :stopping)))
  (:method ((j job))
    (error "~a says nothing about how it stops." (node:full-name j))))

(defun stoppingp (j)
  "Whether this thread has been asked to stop. A loop that blocks on a stream cannot
be interrupted, so what can look between reads has to."
  (and (typep j 'thread) (stopping j)))

(defmethod alivep ((j thread))
  (let ((it (took j)))
    (cond ((seconds j) (and (member (name j) (actors:ticks) :test #'equal) t))
          ((typep it 'bordeaux-threads:thread) (bordeaux-threads:thread-alive-p it))
          (t nil))))

(defmethod start ((j thread))
  (setf (stopping j) nil)
  (setf (took j)
        (if (seconds j)
            (actors:repeat (seconds j) (thunk j) :as (name j) :what (name j))
            (actors:blocking
             (name j)
             (lambda ()
               (unwind-protect (fault:attempt (thunk j) (name j))
                 (setf (state j)
                       (if (stopping j) :stopped :failed)))))))
  j)

(defmethod stop ((j thread))
  (let ((it (took j)))
    (cond ((seconds j) (when it (actors:cancel it)))
          (t (setf (stopping j) t)
             (when (typep it 'bordeaux-threads:thread)
               (sb-thread:join-thread it :timeout *stopping* :default nil)))))
  j)

(defmethod alivep ((j actor)) (and (took j) t))

(defmethod start ((j actor))
  (setf (took j)
        (sento.actor-context:actor-of
         (actors:actors)
         :name (name j)
         :dispatcher (actors:dispatcher-for (dispatcher j))
         :receive (lambda (message)
                    (fault:attempt (lambda () (funcall (receive j) message))
                                   (name j)))))
  j)

(defmethod stop ((j actor))
  (let ((it (took j)))
    (when it
      (fault:or-nothing "an actor that has already stopped is gone"
        (sento.actor-context:stop (actors:actors) it :wait t))))
  j)

(defun ref (j) (took j))

(defun %in-receive-p ()
  "True on a thread inside a receive. Read in value position: act:*self* is a symbol
macro, so BOUNDP answers about the macro name and is false anywhere."
  (and sento.actor:*self* t))

(defgeneric tell (to message)
  (:documentation "Give something to a job. What that means is the kind's: an
actor takes a message, and a program is written to.")
  (:method ((j actor) message)
    (when (took j) (sento.actor:tell (took j) message))
    message)
  (:method ((j program) message)
    "A line on its standard input. A program you cannot write to is half a job:
its output is already a place, and this is the other side of it."
    (let ((it (took j)))
      (when it
        (let ((in (uiop:process-info-input it)))
          (when in
            (fault:attempt (lambda ()
                             (write-line (princ-to-string message) in)
                             (force-output in))
                           (name j))))))
    message)
  (:method ((it null) message) (declare (ignore message)) nil)
  (:method ((name string) message) (tell (named name) message)))

(defgeneric ask (of message &key timeout)
  (:method ((j actor) message &key (timeout *asking*))
    (when (%in-receive-p) (error 'blocking-ask :of (name j)))
    (sento.actor:ask-s (took j) message :time-out timeout))
  (:method ((it null) message &key timeout)
    (declare (ignore message timeout))
    nil)
  (:method ((name string) message &key timeout)
    (ask (named name) message :timeout timeout)))

(defmethod alivep ((j program))
  (let ((it (took j)))
    (and it (uiop:process-alive-p it))))

(defmethod start ((j program))
  (let ((it (uiop:launch-program (argv j)
                                 :input :stream
                                 :output :stream :error-output :output
                                 :environment (env j))))
    (setf (took j) it)
    (let ((stream (uiop:process-info-output it)))
      (actors:blocking
       (format nil "~a out" (name j))
       (lambda ()
         (loop :for line := (handler-case (read-line stream nil nil)
                              (stream-error () nil))
               :while (and line (not (stoppingp j)))
               :do (emit j line)))))
    j))

(defmethod stop ((j program))
  (let ((it (took j)))
    (when it
      (when (uiop:process-alive-p it)
        (uiop:terminate-process it :urgent t))
      (setf (exit-of j) (uiop:wait-process it))))
  j)

(defun supervised () *supervised*)

(defun supervise (j)
  "Keep J running. A job is a node already; this is where it hangs, so pine read
/proc/editor answers its state and pine write /proc/editor '(:restart)' starts it
again."
  (d:swap *supervised*
           (lambda (all)
             (append (remove (name j) all :key #'name :test #'equal) (list j))))
  (when *under* (setf (node:parent j) *under*))
  j)

(defun forget (name)
  (let ((j (find name (supervised) :key #'name :test #'equal)))
    (when j
      (fault:or-nothing "forgetting a job it could not stop still forgets it"
        (stop j))
      (d:swap *supervised* (lambda (all) (remove j all)))
      (d:drop! *jobs* name))
    j))

(defun attend (&key (every *every*))
  "Look over what is supervised, on the wheel. Without this the backoff, the tries
and the restart are all written down and none of them ever happens."
  (actors:repeat every #'sweep :as :proc :what "starting again what died"))

(defun kind (name maker)
  "Say that NAME is a kind of job somebody can ask for, and how one is made from
what they said. Registered where the class is, so this file names no kind it does
not define and a kind loaded later is askable without this one being edited."
  (d:keep! *kinds* (intern (string-upcase (princ-to-string name)) :keyword) maker)
  name)

(defun kinds () (sort (mapcar #'princ-to-string (d:keys (d:all *kinds*))) #'string<))

(defun %started (said)
  "Start what SAID asks for, and answer where it stands.

A kind that can be asked for is one that can be described: a program is its argv,
an image is the systems it loads. A thread and an actor are a lisp function, and
no value carries one, so they are not registered and asking for one says so.

The name is given rather than minted, because whoever asked has to find it again
and two of them asking at once must not race for it. What is started is watched,
so it is under /proc where it was asked for; whether it is started again when it
dies is RESTARTS, which is a different question and off unless it is asked for."
  (let* ((name (and (getf said :name) (princ-to-string (getf said :name))))
         (want (getf said :kind))
         (want (and want (intern (string-upcase (princ-to-string want)) :keyword)))
         (maker (d:lookup (d:all *kinds*) want)))
    (unless name (error "a job is started under a name; none was given."))
    (when (named name) (error "~a is already running." name))
    (unless maker
      (error "~(~a~) is not a kind that can be asked for. There is ~{~a~^, ~}: a ~
              thread and an actor are a function, and a value cannot carry one."
             want (kinds)))
    (let ((j (funcall maker name said)))
      (supervise j)
      (start j)
      (node:full-name j))))

(kind :program
      (lambda (name said)
        (make-instance 'program :name name
                                :on-fault (getf said :on-fault)
                                :env (getf said :env)
                                :argv (mapcar #'princ-to-string
                                              (getf said :argv)))))

(defun %attach (root)
  (setf *under* (node:attach (node:lists "proc" :nodes #'supervised
                                         :writes #'%started
                                         :describes "what this pine is running")
                             root))
  (dolist (j (supervised) *under*) (setf (node:parent j) *under*)))

(defun due (j now)
  "Whether enough has passed since the last try to make another. Without this a
program that dies as fast as it starts is started once a second for as long as pine
runs, and the backoff is a number nobody reads."
  (let ((last (since j)))
    (or (null last) (>= now (+ last (backoff j))))))

(defun sweep ()
  "One pass: start again what died and is owed a try, forget the tries of what has
run long enough to have earned it, and give up on what will not run at all."
  (let ((now (get-universal-time)))
    (dolist (j (supervised) t)
      (cond ((alivep j) (settle j))
            ((heldp j))
            ((and (eq :restart (on-fault j))
                  (member (state j) '(:running :failed))
                  (due j now))
             (if (>= (tries j) *tries*)
                 (%hold j)
                 (progn
                   (setf (state j) :failed (since j) now)
                   (handler-case (start j)
                     (error (e) (setf (fault j) e (state j) :failed))))))))))


(pine/fs/tree:builder #'%attach)
