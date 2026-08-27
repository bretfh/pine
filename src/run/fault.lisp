(defpackage #:pine/run/fault
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:log #:pine/run/log))
  (:export #:fault #:borrowed #:take #:resume #:faulted
           #:faults #:standing #:attempt #:report #:borrow #:await #:defer
           #:forget #:forget-faults #:with-debugger #:attach
           #:condition-of #:label #:backtrace-of #:offers #:taken #:where #:token #:at-time
           #:standingp #:*kept* #:*waiting* #:*debugging*))
(in-package #:pine/run/fault)

(defvar *kept* 50)
(defvar *faults* nil)
(defvar *debugging* nil)
(defvar *waiting* 120
  "Seconds a fault stands unattended before it gives up. One number: a caller
waiting on a fault in another image waits at least this long too, or the fault is
still standing when the call that would answer it has already timed out.")

(defclass fault ()
  ((condition-of :initarg :condition :reader condition-of)
   (label     :initarg :label     :reader label     :initform nil)
   (backtrace-of :initarg :backtrace :reader backtrace-of :initform "")
   (offers    :initarg :offers    :reader offers    :initform nil)
   (taken     :initform nil       :accessor taken)
   (at-time   :initform (get-universal-time) :reader at-time)
   (deferred  :initform nil       :accessor deferred)
   (lock      :initarg :lock      :reader lock      :initform nil)
   (told      :initarg :told      :reader told      :initform nil)
   (choice    :initform nil       :accessor choice))
  (:documentation "Something that broke, the restarts it is standing in, and where
the thread holding them is. A fault with a LOCK is one a thread is still standing
in; taking one of its offers lets that thread go."))

(defclass borrowed (fault)
  ((where :initarg :where :reader where)
   (token :initarg :token :reader token :initform nil))
  (:documentation "A fault standing in another image. WHERE is that image and TOKEN
is its own name for it, so taking a restart here is one act in both."))

(defmethod where ((f fault)) (declare (ignore f)) nil)

(defmethod print-object ((f fault) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~@[~a: ~]~a~:[~; standing~]" (label f) (condition-of f) (standingp f))))

(defun standingp (f) (and (lock f) (null (taken f))))

(defun faults () *faults*)

(defun standing ()
  "The faults whose thread is still there, waiting to be told what to do."
  (remove-if-not #'standingp (faults)))

(defun forget (f)
  (d:swap *faults* (lambda (all) (remove f all)))
  f)

(defun forget-faults () (setf *faults* nil))

(defun %backtrace ()
  (handler-case
      (with-output-to-string (s) (sb-debug:print-backtrace :stream s :count 25))
    (error () "")))

(defun %offers (condition)
  (mapcar (lambda (r) (princ-to-string (restart-name r)))
          (remove nil (compute-restarts condition) :key #'restart-name)))

(defgeneric faulted (fault)
  (:documentation "Say a fault happened, to whoever can do something about it.

Answered above: an editor with somebody looking at it puts up the debugger. With
nobody there it is said, because a fault nobody was told about is one nobody can
act on.")
  (:method (fault) (log:note "~a" (condition-of fault))))

(defun %noted (f)
  (d:swap *faults* #'d:capped f *kept*)
  (handler-case (faulted f)
    (error (broke)
      (log:note "~a, and saying so broke too: ~a" (condition-of f) broke)))
  f)

(defun report (condition &optional label)
  "Keep a fault, with the restarts and the backtrace of wherever it was signalled --
which is why this runs under handler-bind and not handler-case."
  (%noted (make-instance 'fault :condition condition :label label
                                :backtrace (%backtrace)
                                :offers (%offers condition))))

(defun borrow (image condition offers &key token label)
  "A fault another image is standing in. It stands here too, offering what it offers
there."
  (%noted (make-instance 'borrowed :condition condition :label label
                                   :offers offers :where image :token token
                                   :lock (bordeaux-threads:make-lock)
                                   :told (bordeaux-threads:make-condition-variable))))

(defun defer (f)
  "Say somebody is reading this fault, so it is not given up on while they decide."
  (setf (deferred f) t)
  f)

(defgeneric resume (image fault restart)
  (:documentation "Tell IMAGE to take one of the restarts it is standing in."))

(defgeneric take (fault restart)
  (:documentation "Take one of the restarts a fault is standing in, and let the
thread holding them go. Here or in another image, one act.")
  (:method ((f fault) restart)
    (when (and (lock f) (member restart (offers f) :test #'equal))
      (bordeaux-threads:with-lock-held ((lock f))
        (setf (choice f) restart
              (taken f) restart)
        (bordeaux-threads:condition-notify (told f)))
      restart))
  (:method ((f borrowed) restart)
    (when (member restart (offers f) :test #'equal)
      (setf (taken f) restart)
      (resume (where f) f restart)
      restart)))

(defun %stand (f seconds)
  "Wait, holding the frames the fault was signalled in, until someone chooses a
restart. Unattended, it gives up after a while rather than standing for ever."
  (bordeaux-threads:with-lock-held ((lock f))
    (loop :with due := (+ (get-universal-time) seconds)
          :until (choice f)
          :do (bordeaux-threads:condition-wait (told f) (lock f) :timeout 1)
              (when (deferred f) (setf due (+ (get-universal-time) seconds)))
              (when (> (get-universal-time) due) (return))))
  (choice f))

(defun await (f &optional (seconds *waiting*))
  "Wait for someone to take one of the restarts F is standing in, and say which."
  (when (lock f) (%stand f seconds)))

(defun attempt (thunk &optional label)
  "Run THUNK; a fault is kept, with its restarts, and the thunk unwinds.

With the debugger on, the thread stops where it faulted and waits: what is offered
is what is still there, because nothing has unwound yet."
  (if *debugging*
      (%standing thunk label)
      (handler-case (funcall thunk)
        (error (c) (report c label) nil))))

(defun %standing (thunk label)
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
               (let ((name (%stand f *waiting*)))
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

(defun %at (name)
  (let ((i (parse-integer (princ-to-string name) :junk-allowed t)))
    (when (and i (not (minusp i))) (nth i (faults)))))

(defun %fault (name)
  "One fault, at a place. Writing a restart's name takes it: the thread is still
standing there, here or in another image, so this is the same act as taking one in
the debugger."
  (when (%at name)
    (node:place name
                :names (constantly '("offers"))
                :each (lambda (field)
                        (when (equal field "offers")
                          (node:place field
                                      :reads (lambda ()
                                               (let ((f (%at name)))
                                                 (and f (offers f)))))))
                :reads (lambda ()
                         (let ((f (%at name)))
                           (and f (princ-to-string (condition-of f)))))
                :writes (lambda (value)
                          (let ((f (%at name)))
                            (when f (take f (princ-to-string value))))))))

(defun attach (root)
  (node:attach
   (node:place "fault"
               :names (lambda ()
                        (loop :for i :from 0 :below (length (faults)) :collect i))
               :each #'%fault
               :reads (lambda () (length (faults)))
               :describes "what has broken, and what it stands in")
   root))

(pine/word:lends "attempt")
