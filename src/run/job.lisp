(defpackage #:pine/run/job
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault))
  (:import-from #:pine/fs/node #:name)
  (:export #:job #:thread #:actor #:program
           #:start #:stop #:alivep #:tell #:ask
           #:jobs #:named #:supervise #:supervised #:sweep #:attend #:due #:backoff
           #:settle #:emit #:stoppingp #:forget
           #:name #:state #:tries #:restartsp #:took #:exit-of #:since #:fault
           #:said #:thunk #:seconds #:stopping #:argv #:env #:receive
           #:dispatcher #:ref
           #:blocking-ask #:*out-kept* #:*settled* #:*asking* #:*every*
           #:*stopping*))
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
(defvar *supervised* nil)
(defvar *under* nil)

(define-condition blocking-ask (error)
  ((of :initarg :of :reader of))
  (:report (lambda (c stream)
             (format stream "Asked ~a from inside a receive.
A receive owes its mailbox an answer, so it may not wait for one: read what it was
handed, or TELL and take the reply as a message." (of c)))))

(defclass job (node:node)
  ((livep :initform t)
   (state     :initform :stopped :accessor state)
   (tries     :initform 0        :accessor tries)
   (restartsp :initarg :restarts :accessor restartsp :initform t)
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
  (node:attach (node:place "said" :reads (lambda () (said j))
                                  :describes "the last lines it said")
               j)
  (d:keep! *jobs* (name j) j))

(defun jobs () (d:vals (d:all *jobs*)))

(defun named (name) (d:at (d:all *jobs*) (princ-to-string name)))

(defmethod node:contents ((j job)) (state j))

(defmethod node:verb ((j job) name arguments)
  (declare (ignore arguments))
  (case name
    (:start   (start j) (state j))
    (:stop    (stop j) (state j))
    (:restart (stop j) (start j) (state j))
    (t (error "~a takes :start, :stop or :restart." (node:full-name j)))))

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

(defgeneric stop (job)
  (:documentation "Let it go.")
  (:method :before ((j job)) (setf (state j) :stopping))
  (:method :after ((j job)) (setf (state j) :stopped (took j) nil))
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
  (:method ((j actor) message)
    (when (took j) (sento.actor:tell (took j) message))
    message)
  (:method ((it null) message) (declare (ignore message)) nil)
  (:method ((name string) message) (tell (named name) message)))

(defgeneric ask (of message &key timeout)
  (:method ((j actor) message &key (timeout *asking*))
    (when (%in-receive-p) (error 'blocking-ask :of (name j)))
    (sento.actor:ask-s (took j) message :time-out timeout))
  (:method ((name string) message &key timeout)
    (ask (named name) message :timeout timeout)))

(defmethod alivep ((j program))
  (let ((it (took j)))
    (and it (uiop:process-alive-p it))))

(defmethod start ((j program))
  (let ((it (uiop:launch-program (argv j)
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
  (when *under* (setf (node:over j) *under*))
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

(defun %attach (root)
  (setf *under* (node:attach (node:place "proc" :nodes #'supervised
                                         :describes "what this pine is running")
                             root))
  (dolist (j (supervised) *under*) (setf (node:over j) *under*)))

(defun due (j now)
  "Whether enough has passed since the last try to make another. Without this a
program that dies as fast as it starts is started once a second for as long as pine
runs, and the backoff is a number nobody reads."
  (let ((last (since j)))
    (or (null last) (>= now (+ last (backoff j))))))

(defun sweep ()
  "One pass: start again what died and is owed a try, and forget the tries of what
has run long enough to have earned it."
  (let ((now (get-universal-time)))
    (dolist (j (supervised) t)
      (cond ((alivep j) (settle j))
            ((and (restartsp j)
                  (member (state j) '(:running :failed))
                  (due j now))
             (setf (state j) :failed (since j) now)
             (handler-case (start j)
               (error (e) (setf (fault j) e (state j) :failed))))))))

(pine/word:lends "job" "thread" "actor" "program" "start" "stop" "alivep"
                "tell" "ask")

(pine/fs/tree:builder #'%attach)
