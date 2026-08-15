(defpackage #:pine/run/task
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export #:task #:spawn #:tasks #:task-named #:name #:alivep #:stop #:join
           #:fault #:answered #:*tasks* #:once #:stopping #:stoppingp))
(in-package #:pine/run/task)

(defvar *tasks* (d:box nil))
(defvar *task* nil)

(defclass task ()
  ((name     :initarg :name   :reader name)
   (thread   :initform nil    :accessor thread)
   (answered :initform nil    :accessor answered)
   (fault    :initform nil    :accessor fault)
   (stopping :initform (d:box nil) :reader stopping)))

(defmethod print-object ((tk task) stream)
  (print-unreadable-object (tk stream :type t)
    (format stream "~a ~:[stopped~;running~]" (name tk) (alivep tk))))

(defun tasks () (d:held *tasks*))

(defun task-named (name)
  (find name (tasks) :key #'name :test #'equal))

(defgeneric alivep (task)
  (:method ((tk task))
    (and (thread tk) (bordeaux-threads:thread-alive-p (thread tk)))))

(defgeneric stop (task)
  (:method ((tk task))
    (d:put! (stopping tk) t)
    tk))

(defgeneric join (task &key timeout)
  (:method ((tk task) &key (timeout 5))
    (loop :repeat (round (/ timeout 0.02))
          :while (alivep tk)
          :do (sleep 0.02))
    (not (alivep tk))))

(defun stoppingp (&optional (tk *task*))
  "Whether this task has been asked to stop. A loop that blocks on a stream
cannot be interrupted, so what can check between reads has to."
  (and tk (d:held (stopping tk))))

(defun spawn (name thunk)
  (let ((tk (make-instance 'task :name name)))
    (d:swap! *tasks* (lambda (all)
                       (cons tk (remove name all :key #'name :test #'equal))))
    (setf (thread tk)
          (bordeaux-threads:make-thread
           (lambda ()
             (let ((*task* tk))
               (handler-case (setf (answered tk) (funcall thunk))
                 (error (e) (setf (fault tk) e)))))
           :name (format nil "pine ~a" name)))
    tk))

(defun once (name thunk)
  "Run THUNK on a thread of its own and let its task go when it is done, so
work that happens a click at a time does not pile up in the list."
  (flet ((forget ()
           (let ((tk *task*))
             (d:swap! *tasks* (lambda (all) (remove tk all))))))
    (spawn name (lambda () (unwind-protect (funcall thunk) (forget))))))

