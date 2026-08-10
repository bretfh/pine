(defpackage #:pine.run.fault
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell))
  (:export #:fault #:faults #:report #:attempt #:with-debugger #:forget-faults
           #:condition-of #:label #:backtrace-of #:at-time #:offers #:standing
           #:resume #:attend #:attended #:taken #:forget #:parked #:elsewhere #:await
           #:*kept* #:*on-fault*
           #:*waiting* #:*debugging*))

(in-package #:pine.run.fault)

(defvar *kept* 50)
(defvar *faults* (c:cell nil))
(defvar *on-fault* nil)
(defvar *debugging* nil)
(defvar *waiting* 120)

(defclass fault ()
  ((condition-of :initarg :condition :reader condition-of)
   (label        :initarg :label     :reader label     :initform nil)
   (backtrace-of :initarg :backtrace :reader backtrace-of :initform "")
   (offers       :initarg :offers    :reader offers    :initform nil)
   (parked       :initarg :parked    :reader parked    :initform nil)
   (attended     :initform nil       :accessor attended)
   (taken        :initform nil       :accessor taken)
   (at-time      :initform (get-universal-time) :reader at-time)))

(defclass parked ()
  ((restarts :initarg :restarts :reader restarts)
   (lock     :initform (bordeaux-threads:make-lock) :reader lock)
   (said     :initform (bordeaux-threads:make-condition-variable) :reader said)
   (choice   :initform nil :accessor choice)))

(defmethod print-object ((f fault) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~@[~a: ~]~a~:[~; standing~]" (label f) (condition-of f)
            (parked f))))

(defun faults () (c:held *faults*))

(defun standing ()
  "The faults whose thread is still there, waiting to be told what to do."
  (remove-if-not (lambda (f) (and (parked f) (null (taken f)))) (faults)))

(defun forget (f)
  (c:swap *faults* (lambda (all) (remove f all)))
  f)

(defun forget-faults () (c:put *faults* nil))

(defun %backtrace ()
  (handler-case
      (with-output-to-string (s) (sb-debug:print-backtrace :stream s :count 25))
    (error () "")))

(defun %offers (condition)
  (mapcar (lambda (r) (princ-to-string (restart-name r)))
          (remove nil (compute-restarts condition) :key #'restart-name)))

(defun %noted (f)
  (c:swap *faults*
          (lambda (all)
            (let ((next (cons f all)))
              (if (> (length next) *kept*) (subseq next 0 *kept*) next))))
  (when *on-fault* (ignore-errors (funcall *on-fault* f)))
  f)

(defun report (condition &optional label)
  "Keep a fault, with the restarts and the backtrace of wherever it was
signalled -- which is why this runs under handler-bind and not handler-case."
  (%noted (make-instance 'fault :condition condition :label label
                                :backtrace (%backtrace)
                                :offers (%offers condition))))

(defun elsewhere (condition offers &optional label)
  "A fault another image is standing in. It stands here too, offering what it
offers there, so taking one of them is one act in both."
  (%noted (make-instance 'fault :condition condition :label label
                                :offers offers
                                :parked (make-instance 'parked :restarts offers))))

(defun await (f &optional (seconds *waiting*))
  "Wait for someone to take one of the restarts F is standing in, and say which."
  (let ((p (parked f)))
    (when p (%park f p seconds))))

(defun attend (f)
  (setf (attended f) t)
  f)

(defun resume (f name)
  "Hand a restart to the thread standing in F, and let it go."
  (let ((p (parked f)))
    (when (and p (member name (restarts p) :test #'equal))
      (bordeaux-threads:with-lock-held ((lock p))
        (setf (choice p) name
              (taken f) name)
        (bordeaux-threads:condition-notify (said p)))
      name)))

(defun %park (f p seconds)
  "Wait, holding the frames the fault was signalled in, until someone chooses a
restart. Unattended, it gives up after a while rather than standing for ever."
  (bordeaux-threads:with-lock-held ((lock p))
    (loop :with due := (+ (get-universal-time) seconds)
          :until (choice p)
          :do (bordeaux-threads:condition-wait (said p) (lock p) :timeout 1)
              (when (attended f) (setf due (+ (get-universal-time) seconds)))
              (when (> (get-universal-time) due) (return))))
  (choice p))

(defun attempt (thunk &optional label)
  "Run THUNK; a fault is kept, with its restarts, and the thunk unwinds.

With the debugger on, the thread stops where it faulted and waits: what is
offered is what is still there, because nothing has unwound yet."
  (if *debugging*
      (%attempt-standing thunk label)
      (handler-case (funcall thunk)
        (error (c) (report c label) nil))))

(defun %attempt-standing (thunk label)
  (block attempting
    (handler-bind
        ((error
           (lambda (c)
             (let* ((p (make-instance 'parked :restarts (%offers c)))
                    (f (%noted (make-instance 'fault :condition c :label label
                                                     :backtrace (%backtrace)
                                                     :offers (restarts p)
                                                     :parked p))))
               (let ((name (%park f p *waiting*)))
                 (when name
                   (let ((r (find name (compute-restarts c)
                                  :key (lambda (each)
                                         (princ-to-string (restart-name each)))
                                  :test #'equal)))
                     (when r (invoke-restart r)))))
               (return-from attempting nil)))))
      (funcall thunk))))

(defmacro with-debugger (&body body)
  "Inside, a fault stops its thread where it happened instead of unwinding."
  `(let ((*debugging* t)) ,@body))
