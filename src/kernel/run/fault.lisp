(defpackage #:pine/run/fault
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:log #:pine/fs/log))
  (:export
   #:fault #:borrowed #:take #:resume #:faulted
   #:faults #:suspended #:attempt #:or-nothing #:report #:id #:defer
   #:expected #:expecteds #:forget-expected
   #:borrow #:await #:wake #:wait-until #:forget-faults
   #:with-debugger #:condition-of #:label #:backtrace-of #:offers
   #:taken #:where #:token #:suspendedp #:*unattended-seconds*
   #:*debugging* #:*keeping*))
(in-package #:pine/run/fault)

(defvar *faults-kept* 50)
(defvar *faults* nil)
(defvar *expected* nil)
(defvar *counter* 0)
(defparameter +leaving+ '("EXIT"))
(defvar *debugging* nil)
(defvar *keeping* nil)
(defvar *noticing* (bordeaux-threads:make-lock "pine-faults"))
(defvar *noticed* (bordeaux-threads:make-condition-variable))
(defvar *unattended-seconds* 120)

(defun expected (why condition)
  (sb-ext:atomic-update *expected* (lambda (old) (d:capped old (list why condition (get-universal-time)) *faults-kept*)))
  nil)

(defun expecteds () *expected*)

(defun forget-expected () (setf *expected* nil))

(defmacro or-nothing (why &body body)
  (let ((c (gensym "BROKE")))
    `(handler-case (progn ,@body)
       (error (,c) (expected ,why ,c)))))

(defclass fault ()
  ((id        :initform (sb-ext:atomic-update *counter* (lambda (old) (1+ old))) :reader id)
   (condition-of :initarg :condition :reader condition-of)
   (label     :initarg :label     :reader label     :initform nil)
   (backtrace-of :initarg :backtrace :reader backtrace-of :initform "")
   (offers    :initarg :offers    :reader offers    :initform nil)
   (taken     :initform nil       :accessor taken)
   (at-time   :initform (get-universal-time) :reader at-time)
   (deferred  :initform nil       :accessor deferred)
   (lock      :initarg :lock      :reader lock      :initform nil)
   (make-job      :initarg :told      :reader make-job      :initform nil)))

(defclass borrowed (fault)
  ((where :initarg :where :reader where)
   (token :initarg :token :reader token :initform nil)))

(defmethod where ((f fault)) (declare (ignore f)) nil)

(defmethod print-object ((f fault) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~@[~a: ~]~a~:[~; suspended~]" (label f) (condition-of f) (suspendedp f))))

(defun suspendedp (f) (and (lock f) (null (taken f))))

(defun faults () *faults*)

(defun suspended ()
  (remove-if-not #'suspendedp (faults)))

(defun forget-faults () (setf *faults* nil))

(defun %backtrace ()
  (or-nothing "a thread too deep or too far gone to walk"
    (with-output-to-string (s) (sb-debug:print-backtrace :stream s :count 25))))

(defun %offers (condition)
  (mapcar (lambda (r) (princ-to-string (restart-name r)))
          (remove-if (lambda (r)
                       (let ((it (restart-name r)))
                         (or (null it)
                             (member (princ-to-string it) +leaving+ :test #'equal))))
                     (compute-restarts condition))))

(defgeneric faulted (fault)
  (:method (fault) (log:note "~a" (condition-of fault))))

(defun wake ()
  (bordeaux-threads:with-lock-held (*noticing*)
    (sb-thread:condition-broadcast *noticed*))
  t)

(defun wait-until (readyp &optional (seconds *unattended-seconds*))
  (bordeaux-threads:with-lock-held (*noticing*)
    (loop :with due := (+ (get-universal-time) seconds)
          :for said := (funcall readyp)
          :when said :do (return said)
          :do (bordeaux-threads:condition-wait *noticed* *noticing* :timeout 1)
              (when (> (get-universal-time) due) (return (funcall readyp))))))

(defun %noted (f)
  (sb-ext:atomic-update *faults* (lambda (old) (d:capped old f *faults-kept*)))
  (when *keeping* (setf (car *keeping*) f))
  (wake)
  (handler-case (faulted f)
    (error (broke)
      (log:note "~a, and saying so broke too: ~a" (condition-of f) broke)))
  f)

(defun report (condition &optional label)
  (%noted (make-instance 'fault :condition condition :label label
                                :backtrace (%backtrace)
                                :offers (%offers condition))))

(defun borrow (image condition offers &key token label)
  (%noted (make-instance 'borrowed :condition condition :label label
                                   :offers offers :where image :token token
                                   :lock (bordeaux-threads:make-lock)
                                   :told (bordeaux-threads:make-condition-variable))))

(defun defer (f)
  (setf (deferred f) t)
  f)

(defgeneric resume (image fault restart))

(defgeneric take (fault restart)
  (:method ((f fault) restart)
    (when (and (lock f) (member restart (offers f) :test #'equal))
      (bordeaux-threads:with-lock-held ((lock f))
        (setf (taken f) restart)
        (bordeaux-threads:condition-notify (make-job f)))
      restart))
  (:method ((f borrowed) restart)
    (when (member restart (offers f) :test #'equal)
      (resume (where f) f restart)
      (setf (taken f) restart)
      (when (lock f)
        (bordeaux-threads:with-lock-held ((lock f))
          (bordeaux-threads:condition-notify (make-job f))))
      restart)))

(defun %stand (f seconds)
  (bordeaux-threads:with-lock-held ((lock f))
    (loop :with due := (+ (get-universal-time) seconds)
          :until (taken f)
          :do (bordeaux-threads:condition-wait (make-job f) (lock f) :timeout 1)
              (when (deferred f) (setf due (+ (get-universal-time) seconds)))
              (when (> (get-universal-time) due) (return))))
  (taken f))

(defun await (f &optional (seconds *unattended-seconds*))
  (when (lock f) (%stand f seconds)))

(defun attempt (thunk &optional label)
  (if *debugging*
      (%suspend thunk label)
      (block attempting
        (handler-bind ((error (lambda (c)
                                (report c label)
                                (return-from attempting nil))))
          (funcall thunk)))))

(defun %suspend (thunk label)
  (block attempting
    (handler-bind
        ((error
           (lambda (c)
             (let ((f (%noted (make-instance
                               'fault :condition c :label label
                                      :backtrace (%backtrace)
                                      :offers (%offers c)
                                      :lock (bordeaux-threads:make-lock)
                                      :told (bordeaux-threads:make-condition-variable)))))
               (let ((name (%stand f *unattended-seconds*)))
                 (when name
                   (let ((r (find name (compute-restarts c)
                                  :key (lambda (each)
                                         (princ-to-string (restart-name each)))
                                  :test #'equal)))
                     (when r (invoke-restart r)))))
               (return-from attempting nil)))))
      (funcall thunk))))

(defmacro with-debugger (&body body)
  `(let ((*debugging* t)) ,@body))

(defun %at (name)
  (let ((i (parse-integer (princ-to-string name) :junk-allowed t)))
    (when i (find i (faults) :key #'id))))

(defclass fault-mount (fs:mount) ())

(defmethod fs:volatile-p ((n fault-mount) &optional name) (declare (ignore name)) t)

(defun %it (n) (%at (fs:name n)))

(defmethod fs:names ((n fault-mount))
  '((:said   . "what broke, as it said it")
    (:offers . "the restarts it is suspended in")
    (:taken  . "which restart was taken; writing one takes it")))

(defmethod fs:read ((n fault-mount) (name (eql :said)))
  (let ((f (%it n))) (and f (princ-to-string (condition-of f)))))

(defmethod fs:read ((n fault-mount) (name (eql :offers)))
  (let ((f (%it n))) (when f (defer f) (offers f))))

(defmethod fs:read ((n fault-mount) (name (eql :taken)))
  (let ((f (%it n))) (and f (taken f))))

(defmethod fs:write ((n fault-mount) (name (eql :taken)) value)
  (let ((f (%it n))) (when f (take f (princ-to-string value)))))

(defclass expected (fs:derived) ())

(defmethod fs:volatile-p ((n expected) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n expected))
  (loop :for (why broke at) :in (expecteds)
        :collect (list :why why :at at :said (princ-to-string broke))))

(defmethod fs:takes ((n expected) value)
  (unless value (forget-expected)))

(defclass broken (fs:mount) ())

(defmethod fs:volatile-p ((d broken) &optional name) (declare (ignore name)) t)

(defmethod fs:child ((d broken) name)
  (let ((name (princ-to-string name)))
    (cond ((equal name "expected")
           (fs:ensure-child d name (lambda () (make-instance 'expected :name name :parent d))))
          ((%at name)
           (fs:ensure-child d name (lambda () (make-instance 'fault-mount :name name :parent d)))))))

(defmethod fs:children ((d broken))
  (cons (fs:child d "expected")
        (remove nil (mapcar (lambda (f) (fs:child d (id f))) (faults)))))

(fs:mount (lambda () (make-instance 'broken :describes "what has broken, and what it stands in"))
          "/fault")

(setf pine/fs:*broke*
      (lambda (c where)
        (report c (if where
                      (format nil "working out ~a" where)
                      "telling what is listening that a place moved"))))
