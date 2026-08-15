(defpackage #:pine/proc/process
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:task #:pine/run/task) (#:timer #:pine/run/timer))
  (:import-from #:pine/fs/node #:name)
  (:export #:process #:program #:thread-process #:name #:state #:attempts
           #:restarts-p #:backoff #:start #:stop #:alivep #:said #:emit
           #:took #:exit-of #:since #:fault #:*out-kept* #:argv #:env #:thunk
           #:every-seconds))
(in-package #:pine/proc/process)

(defvar *out-kept* 200)
(defvar *backoff-cap* 60)

(defclass process (node:node)
  ((state       :initform :stopped :accessor state)
   (attempts    :initform 0        :accessor attempts)
   (restarts-p  :initarg :restarts :accessor restarts-p :initform t)
   (took        :initform nil      :accessor took)
   (exit-of     :initform nil      :accessor exit-of)
   (since       :initform nil      :accessor since)
   (fault       :initform nil      :accessor fault)
   (said        :initform (d:box nil) :reader said)))

(defclass program (process)
  ((argv :initarg :argv :accessor argv)
   (env  :initarg :env  :accessor env :initform nil)))

(defclass thread-process (process)
  ((thunk         :initarg :thunk   :accessor thunk)
   (every-seconds :initarg :every   :accessor every-seconds :initform nil)))

(defmethod print-object ((p process) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~a" (name p) (state p))))

(defclass said-node (node:node) ())

(defmethod node:contents ((n said-node)) (d:held (said (node:parent n))))
(defmethod node:leafp ((n said-node)) t)
(defmethod node:livep ((n said-node)) t)
(defmethod node:persistp ((n said-node)) nil)

(defmethod initialize-instance :after ((p process) &key)
  (node:slots p p "state" 'state "attempts" 'attempts)
  (node:attach (make-instance 'said-node :name "said"
                                         :describes "the last lines it said")
               p))

(defmethod node:contents ((p process)) (state p))
(defmethod node:livep ((p process)) t)
(defmethod node:persistp ((p process)) nil)

(defmethod node:verb ((p process) name arguments)
  (declare (ignore arguments))
  (case name
    (:start   (start p) (state p))
    (:stop    (stop p) (state p))
    (:restart (stop p) (start p) (state p))
    (t (error "~a takes :start, :stop or :restart." (node:full-name p)))))

(defun emit (p line)
  (d:swap! (said p)
          (lambda (all)
            (let ((next (cons line all)))
              (if (> (length next) *out-kept*)
                  (subseq next 0 *out-kept*)
                  next))))
  line)

(defun backoff (p)
  (min *backoff-cap* (expt 2 (min 16 (attempts p)))))

(defgeneric alivep (process)
  (:method ((p process)) (eq :running (state p))))

(defgeneric start (process)
  (:method :before ((p process))
    (incf (attempts p))
    (setf (state p) :starting (fault p) nil))
  (:method :after ((p process))
    (when (eq :starting (state p)) (setf (state p) :running))))

(defgeneric stop (process)
  (:method :before ((p process)) (setf (state p) :stopping))
  (:method :after ((p process)) (setf (state p) :stopped (took p) nil)))

(defmethod alivep ((p program))
  (let ((it (took p)))
    (and it (uiop:process-alive-p it))))

(defmethod start ((p program))
  (flet ((read-out (stream)
           (task:spawn (format nil "~a out" (name p))
                       (lambda ()
                         (loop :for line := (handler-case (read-line stream nil nil)
                                              (stream-error () nil))
                               :while (and line (not (task:stoppingp)))
                               :do (emit p line))))))
    (let ((it (uiop:launch-program (argv p)
                                   :output :stream :error-output :output
                                   :environment (env p))))
      (setf (took p) it)
      (read-out (uiop:process-info-output it))
      p)))

(defmethod stop ((p program))
  (let ((it (took p)))
    (when it
      (when (uiop:process-alive-p it)
        (uiop:terminate-process it :urgent t))
      (setf (exit-of p) (uiop:wait-process it))))
  p)

(defmethod alivep ((p thread-process))
  (let ((it (took p)))
    (cond ((every-seconds p) (and (member it (timer:names) :test #'equal) t))
          (it (task:alivep it))
          (t nil))))

(defmethod start ((p thread-process))
  (setf (took p)
        (if (every-seconds p)
            (timer:every-seconds (every-seconds p) (thunk p)
                                 :as (name p) :what (name p))
            (task:spawn (name p) (thunk p))))
  p)

(defmethod stop ((p thread-process))
  (let ((it (took p)))
    (when it
      (cond ((every-seconds p) (timer:cancel it))
            (t (task:stop it)
               (task:join it :timeout 2)
               (setf (fault p) (task:fault it))))))
  p)
