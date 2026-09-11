(defpackage #:pine/run/job
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault))
  (:import-from #:pine/fs #:name)
  (:export
   #:job #:thread #:tick #:actor #:program #:start
   #:stop #:alivep #:tell #:ask #:jobs
   #:named #:supervise #:supervised #:sweep #:attend #:emit
   #:stoppingp #:stoppedp #:giving-up-p #:forget #:name #:state #:tries #:make-job #:kinds
   #:handle #:body #:stopping #:argv #:ref #:started #:again
   #:repeat #:cancel #:ticks))
(in-package #:pine/run/job)

(defvar *out-kept* 200)
(defvar *ask-seconds* 5)
(defvar *stop-seconds* 2)
(define-condition blocking-ask (error)
  ((of :initarg :of :reader of))
  (:report (lambda (c stream)
             (format stream "Asked ~a from inside a receive.
A receive owes its mailbox an answer, so it may not wait for one: read what it was
handed, or TELL and take the reply as a message." (of c)))))

(defclass job (fs:mount)
  ((state     :initform :stopped :accessor state)
   (tries     :initform 0        :accessor tries)
   (supervised :initform nil     :accessor supervisedp)
   (on-fault :initarg :on-fault :accessor on-fault :initform :restart)
   (handle      :initform nil      :accessor handle)
   (exit-of   :initform nil      :accessor exit-of)
   (since     :initform nil      :accessor since)
   (fault     :initform nil      :accessor fault)
   (stopping  :initform nil      :accessor stopping)
   (said      :initform nil :reader said)))

(defclass thread (job)
  ((body     :initarg :body   :accessor body)))

(defclass tick (job)
  ((body    :initarg :body  :accessor body)
   (seconds :initarg :every :accessor seconds)))

(defclass actor (job)
  ((receive    :initarg :receive    :accessor receive)
   (dispatcher :initarg :dispatcher :accessor dispatcher :initform :shared)))

(defclass program (job)
  ((argv :initarg :argv :accessor argv)
   (env  :initarg :env  :accessor env :initform nil)))

(defmethod print-object ((j job) stream)
  (print-unreadable-object (j stream :type t)
    (format stream "~a ~a" (name j) (state j))))

(defmethod initialize-instance :after ((j job) &key)
  (fs:mount j (%proc)))

(defmethod fs:names ((j job))
  '((:state . "which of stopped, starting, running, stopping, failed or given up it is")
    (:tries . "how many times it has been started")
    (:said  . "the last lines it said")
    (:tell  . "give it something")))

(defmethod fs:read ((j job) (name (eql :state)))
  (state j))

(defmethod fs:read ((j job) (name (eql :tries)))
  (tries j))

(defmethod fs:read ((j job) (name (eql :said)))
  (said j))

(defmethod fs:write ((j job) (name (eql :tell)) value)
  (tell j value))

(defun %proc () (fs:at "/proc"))

(defun jobs ()
  (remove-if-not (lambda (each) (typep each 'job)) (fs:children (%proc))))

(defun named (name)
  (let ((it (fs:child (%proc) (princ-to-string name))))
    (and (typep it 'job) it)))

(defun again (j)
  (setf (tries j) 0)
  (start j)
  j)

(defun emit (j line)
  (sb-ext:atomic-update (slot-value j 'said) (lambda (old) (d:capped old line *out-kept*)))
  line)

(defgeneric alivep (job)
  (:method ((j job)) (eq :running (state j))))

(defgeneric start (job)
  (:method :before ((j job))
    (incf (tries j))
    (setf (state j) :starting (fault j) nil (since j) (get-universal-time)))
  (:method :after ((j job))
    (when (eq :starting (state j)) (setf (state j) :running)))
  (:method :around ((j job))
    (handler-bind ((error (lambda (c)
                            (setf (fault j) c (state j) :failed))))
      (call-next-method)))
  (:method ((j job))
    (error "~a says nothing about how it starts." (fs:full-name j))))

(defgeneric stoppedp (job)
  (:method ((j job)) t)
  (:method ((j thread))
    (let ((it (handle j)))
      (not (and (typep it 'bordeaux-threads:thread)
                (bordeaux-threads:thread-alive-p it))))))

(defgeneric stop (job)
  (:method :before ((j job)) (setf (state j) :stopping))
  (:method :after ((j job))
    (if (stoppedp j)
        (setf (state j) :stopped (handle j) nil)
        (setf (state j) :stopping)))
  (:method ((j job))
    (error "~a says nothing about how it stops." (fs:full-name j))))

(defun stoppingp (j)
  (and (stopping j) t))

(defmethod alivep ((j thread))
  (let ((it (handle j)))
    (and (typep it 'bordeaux-threads:thread) (bordeaux-threads:thread-alive-p it))))

(defmethod alivep ((j tick)) (and (handle j) t))

(defmethod start ((j thread))
  (setf (stopping j) nil)
  (setf (handle j)
        (actors:blocking
         (name j)
         (lambda ()
           (unwind-protect (fault:attempt (body j) (name j))
             (setf (state j) (if (stopping j) :stopped :failed))))))
  j)

(defmethod start ((j tick))
  (setf (handle j) (actors:schedule (seconds j) (body j) (name j)))
  j)

(defmethod stop ((j thread))
  (setf (stopping j) t)
  (let ((it (handle j)))
    (when (typep it 'bordeaux-threads:thread)
      (sb-thread:join-thread it :timeout *stop-seconds* :default nil)))
  j)

(defmethod stop ((j tick))
  (let ((it (handle j))) (when it (actors:unschedule it) (setf (handle j) nil))) j)

(defun ticks () (remove-if-not (lambda (j) (typep j 'tick)) (jobs)))

(defun repeat (seconds thunk &key (as (gensym "REPEAT-")) (what "a tick"))
  (let ((name (substitute #\. #\/ (princ-to-string as))))
    (let ((had (named name)))
      (when (typep had 'tick) (stop had) (forget name)))
    (let ((j (make-instance 'tick :name name :every seconds :body thunk
                                  :on-fault :leave :describes what)))
      (start j)
      j)))

(defun cancel (tick)
  (let ((j (if (stringp tick) (named tick) tick)))
    (when (typep j 'tick)
      (stop j)
      (forget (name j)))
    j))

(defmethod alivep ((j actor)) (and (handle j) t))

(defmethod start ((j actor))
  (setf (handle j)
        (sento.actor-context:actor-of
         (actors:actors)
         :name (name j)
         :dispatcher (actors:dispatcher-for (dispatcher j))
         :receive (lambda (message)
                    (fault:attempt (lambda () (funcall (receive j) message))
                                   (name j)))))
  j)

(defmethod stop ((j actor))
  (let ((it (handle j)))
    (when it
      (fault:or-nothing "an actor that has already stopped is gone"
        (sento.actor-context:stop (actors:actors) it :wait t))))
  j)

(defun ref (j) (handle j))

(defun %in-receive-p ()
  (and sento.actor:*self* t))

(defgeneric tell (to message)
  (:method ((j actor) message)
    (when (handle j) (sento.actor:tell (handle j) message))
    message)
  (:method ((j program) message)
    (let ((it (handle j)))
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
  (:method ((j actor) message &key (timeout *ask-seconds*))
    (when (%in-receive-p) (error 'blocking-ask :of (name j)))
    (sento.actor:ask-s (handle j) message :time-out timeout))
  (:method ((it null) message &key timeout)
    (declare (ignore message timeout))
    nil)
  (:method ((name string) message &key timeout)
    (ask (named name) message :timeout timeout)))

(defmethod alivep ((j program))
  (let ((it (handle j)))
    (and it (uiop:process-alive-p it))))

(defmethod start ((j program))
  (setf (stopping j) nil)
  (let ((it (uiop:launch-program (argv j)
                                 :input :stream
                                 :output :stream :error-output :output
                                 :environment (env j))))
    (setf (handle j) it)
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
  (setf (stopping j) t)
  (let ((it (handle j)))
    (when it
      (when (uiop:process-alive-p it)
        (uiop:terminate-process it :urgent t))
      (setf (exit-of j) (uiop:wait-process it))))
  j)

