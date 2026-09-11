(in-package #:pine/edit)

(defvar *shown* nil)
(defparameter *name* "*debugger*")

(defclass offered ()
  ((of       :initarg :of       :reader of)
   (restarts :initarg :restarts :reader restarts :initform nil)
   (fault    :initarg :fault    :reader fault-of :initform nil)))

(defmethod print-object ((s offered) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a, ~d restart~:p" (of s) (length (restarts s)))))

(defun shown () *shown*)

(defun %text (s)
  (with-output-to-string (out)
    (let ((all (fault:suspended)))
      (when (rest all)
        (format out "fault ~d of ~d (Tab: next)~%~%"
                (1+ (or (position (fault-of s) all) 0)) (length all))))
    (format out "~a~%~%" (of s))
    (loop :for r :in (restarts s)
          :for i :from 0
          :do (format out "~d  ~a~%" i r))
    (format out "~%a abort   q quit   0-9 a restart~%")
    (let ((f (or (fault-of s) (first (fault:faults)))))
      (when (and f (fault:backtrace-of f))
        (format out "~%~a~%" (fault:backtrace-of f))))))

(defun %follow (f)
  (let ((where (and f (fault:where f))))
    (when where
      (unless (target-was)
        (setf (target-was) (or (target) :here)))
      (setf (target) (job:name where)))))

(defun %back ()
  (let ((was (target-was)))
    (when was
      (setf (target) (unless (eq was :here) was))
      (setf (target-was) nil))))

(defun %front (buffer)
  (setf (text:current) buffer)
  (show (focused) buffer)
  buffer)

(defun %takes-the-front ()
  (and (null *shown*) (not (askingp))))

(defun put-up (condition &key restarts fault (front (%takes-the-front)))
  (%follow fault)
  (let* ((s (make-instance 'offered
                           :of condition
                           :restarts (or restarts
                                         (and fault (fault:offers fault))
                                         (mapcar #'princ-to-string
                                                 (compute-restarts condition)))
                           :fault fault))
         (buffer (or (fs:at "/text" *name*)
                       (text:make-buffer *name*
                                          :mode (make-instance 'debugger)))))
    (setf *shown* s)
    (unless (typep (text:mode-of buffer) 'debugger)
      (setf (text:mode-of buffer) (make-instance 'debugger)))
    (setf (text:text buffer) (%text s))
    (text:goto buffer 0 0)
    (when front (%front buffer))
    buffer))

(defmethod fault:faulted ((f fault:fault))
  (if (and (fault:suspendedp f) (focused))
      (put-up (fault:condition-of f) :fault f)
      (call-next-method)))

(defun choose (n)
  (let ((s (shown)))
    (when (and s (nth n (restarts s)))
      (let ((name (nth n (restarts s)))
            (f (fault-of s)))
        (setf *shown* nil)
        (if f
            (progn (fault:take f name) (log:note "took ~a" name))
            (log:note "~a" name))
        (unless (fault:suspended) (%back))
        name))))

(defun next ()
  (let* ((all (fault:suspended))
         (s (shown))
         (at (position (and s (fault-of s)) all))
         (f (nth (mod (1+ (or at -1)) (max 1 (length all))) all)))
    (when f (put-up (fault:condition-of f) :fault f :front t))))

(defun away ()
  (setf *shown* nil)
  (%back)
  (when (fs:at "/text" *name*) (command:run "kill-buffer" (list *name*)))
  t)

(command:defcommand "debugger-abort" ()
    (:describes "leave the fault alone" :on '(debugger "a"))
  (away))

(command:defcommand "debugger-quit" ()
    (:describes "put the debugger away" :on '(debugger "q"))
  (away))

(command:defcommand "debugger-restart" (n) (:describes "take one of the restarts")
  (choose (if (integerp n)
              n
              (or (parse-integer (princ-to-string n) :junk-allowed t) 0))))

(command:defcommand "debugger" ()
    (:describes "the last fault, as a buffer" :on '(text "C-x e"))
  (let ((f (or (first (fault:suspended)) (first (fault:faults)))))
    (if f
        (fs:name (put-up (fault:condition-of f) :fault f :front t))
        (log:note "nothing has faulted"))))

(command:defcommand "debugger-next" ()
    (:describes "the fault after this one" :on '(debugger "TAB"))
  (and (next) t))

(command:defcommand "toggle-debug-on-error" ()
    (:describes "whether a fault stands its thread or unwinds")
  (setf fault:*debugging* (not fault:*debugging*))
  (log:note "the debugger is ~:[off~;on~]" fault:*debugging*)
  fault:*debugging*)

(macrolet ((restarts ()
             `(progn
                ,@(loop :for n :from 0 :to 9
                        :collect
                        `(command:defcommand ,(format nil "debugger-restart-~d" n)
                             ()
                             (:describes "take this restart"
                              :on '(debugger ,(princ-to-string n)))
                           (choose ,n))))))
  (restarts))
