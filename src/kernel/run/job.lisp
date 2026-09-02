(defpackage #:pine/run/job
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault))
  (:import-from #:pine/fs/node #:name)
  (:export
   #:job #:thread #:tick #:actor #:program #:start
   #:stop #:alivep #:tell #:ask #:jobs
   #:named #:supervise #:sweep #:attend #:emit #:asked-for
   #:stoppingp #:stoppedp #:heldp #:forget #:name #:state #:tries #:kind #:kinds
   #:took #:runs #:stopping #:argv #:ref))
(in-package #:pine/run/job)

(defvar *out-kept* 200)
(defvar *asking* 5)
(defvar *stopping* 2
  "Seconds to wait for a thread asked to stop. It is asked and then joined: a
thread blocked on a stream reads to the end of what it has and then looks, so the
wait is for that look and not for a clock.")
(defvar *jobs* (d:table))
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
  (:documentation "Something that runs; its state and tries are nodes under it."))

(defclass thread (job)
  ((runs     :initarg :runs   :accessor runs)
   (stopping :initform nil :accessor stopping))
  (:documentation "Blocks on something, in a thread of its own."))

(defclass tick (job)
  ((runs    :initarg :runs  :accessor runs)
   (seconds :initarg :every :accessor seconds))
  (:documentation "Repeats on the wheel, taking no thread."))

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
  (node:attach (make-instance 'node:place :name "said" :reads (lambda () (said j))
                                  :describes "the last lines it said")
               j)
  (node:attach (make-instance 'node:place :name "tell"
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
  (:method :around ((j job))
    "One that would not start is one that failed, and says so where it stands.

Left where the :BEFORE put it, a job whose START threw stayed :STARTING for ever:
SWEEP starts what is :RUNNING or :FAILED and gives up on what has tried too often,
and :STARTING is neither. A window manager on a compositor pine does not know how
to talk to sat there saying it was starting for the life of the image."
    (handler-bind ((error (lambda (c)
                            (setf (fault j) c (state j) :failed))))
      (call-next-method)))
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
    (and (typep it 'bordeaux-threads:thread) (bordeaux-threads:thread-alive-p it))))

(defmethod alivep ((j tick))
  (and (member (name j) (actors:ticks) :test #'equal) t))

(defmethod start ((j thread))
  (setf (stopping j) nil)
  (setf (took j)
        (actors:blocking
         (name j)
         (lambda ()
           (unwind-protect (fault:attempt (runs j) (name j))
             (setf (state j) (if (stopping j) :stopped :failed))))))
  j)

(defmethod start ((j tick))
  (setf (took j) (actors:repeat (seconds j) (runs j) :as (name j) :what (name j)))
  j)

(defmethod stop ((j thread))
  (setf (stopping j) t)
  (let ((it (took j)))
    (when (typep it 'bordeaux-threads:thread)
      (sb-thread:join-thread it :timeout *stopping* :default nil)))
  j)

(defmethod stop ((j tick))
  (let ((it (took j))) (when it (actors:cancel it))) j)

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

(defun asked-for (said key default)
  "What SAID says about KEY, or DEFAULT where it says nothing.

GETF and not: a key nobody gave answers NIL, and NIL handed to MAKE-INSTANCE is a
slot set to NIL rather than a slot left at what the class says. A program started
by a write to /proc took :ON-FAULT NIL that way, and nothing ever started it
again."
  (let ((said (getf said key :none)))
    (if (eq said :none) default said)))
