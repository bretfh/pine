(defpackage #:pine.run.task
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:box #:pine.run.mailbox))
  (:export #:task #:spawn #:tasks #:task-named #:name #:alivep #:stop #:join
           #:inbox #:fault #:answered #:*tasks* #:actor #:ask #:tell #:reply
           #:stopping #:each))

(in-package #:pine.run.task)

(defvar *tasks* (c:cell nil))
(defvar *task* nil)
(defvar *ask-timeout* 5)

(defclass task ()
  ((name     :initarg :name   :reader name)
   (thread   :initform nil    :accessor thread)
   (inbox    :initform nil    :accessor inbox)
   (answered :initform nil    :accessor answered)
   (fault    :initform nil    :accessor fault)
   (stopping :initform (c:cell nil) :reader stopping)))

(defmethod print-object ((tk task) stream)
  (print-unreadable-object (tk stream :type t)
    (format stream "~a ~:[stopped~;running~]" (name tk) (alivep tk))))

(defun tasks () (c:held *tasks*))

(defun task-named (name)
  (find name (tasks) :key #'name :test #'equal))

(defun %remember (tk)
  (c:swap *tasks* (lambda (all) (cons tk (remove (name tk) all
                                                 :key #'name :test #'equal))))
  tk)

(defun %forget (tk)
  (c:swap *tasks* (lambda (all) (remove tk all)))
  tk)

(defgeneric alivep (task)
  (:method ((tk task))
    (and (thread tk) (bordeaux-threads:thread-alive-p (thread tk)))))

(defgeneric stop (task)
  (:method ((tk task))
    (c:put (stopping tk) t)
    (when (inbox tk) (box:close-mailbox (inbox tk)))
    tk))

(defgeneric join (task &key timeout)
  (:method ((tk task) &key (timeout 5))
    (loop :repeat (round (/ timeout 0.02))
          :while (alivep tk)
          :do (sleep 0.02))
    (not (alivep tk))))

(defun stoppingp (&optional (tk *task*))
  (and tk (c:held (stopping tk))))

(defun spawn (name thunk)
  (let ((tk (make-instance 'task :name name)))
    (%remember tk)
    (setf (thread tk)
          (bordeaux-threads:make-thread
           (lambda ()
             (let ((*task* tk))
               (handler-case (setf (answered tk) (funcall thunk))
                 (error (e) (setf (fault tk) e)))))
           :name (format nil "pine ~a" name)))
    tk))

(defun each (name seconds thunk)
  (spawn name
         (lambda ()
           (loop :until (stoppingp)
                 :do (handler-case (funcall thunk)
                       (error (e) (setf (fault *task*) e)))
                    (sleep seconds)))))

(defun actor (name receive)
  (let ((tk (make-instance 'task :name name)))
    (setf (inbox tk) (box:mailbox))
    (%remember tk)
    (setf (thread tk)
          (bordeaux-threads:make-thread
           (lambda ()
             (let ((*task* tk))
               (loop :until (stoppingp)
                     :for message := (box:take (inbox tk) :timeout 0.1)
                     :when message
                       :do (handler-case (funcall receive message)
                             (error (e) (setf (fault tk) e))))))
           :name (format nil "pine ~a" name)))
    tk))

(defgeneric tell (task message)
  (:method ((name string) message) (tell (task-named name) message))
  (:method ((tk task) message) (box:send (inbox tk) message)))

(defgeneric ask (task message &key timeout)
  (:method ((name string) message &key timeout)
    (ask (task-named name) message :timeout timeout))
  (:method ((tk task) message &key (timeout *ask-timeout*))
    (let ((answer (box:mailbox)))
      (tell tk (list :ask answer message))
      (box:take answer :timeout timeout))))

(defun reply (to value)
  (box:send to value)
  value)
