(defpackage #:pine/edit/debugger
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text)
                    (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:job #:pine/run/job) (#:emode #:pine/edit/mode)
                    (#:window #:pine/edit/window) (#:log #:pine/fs/log)
                    (#:evaluate #:pine/edit/eval))
  (:export #:standing #:choose #:next #:fault-of #:away #:*name*
           #:*standing*))
(in-package #:pine/edit/debugger)

(defvar *standing* nil)
(defparameter *name* "*debugger*")

(defclass standing ()
  ((of       :initarg :of       :reader of)
   (restarts :initarg :restarts :reader restarts :initform nil)
   (fault    :initarg :fault    :reader fault-of :initform nil))
  (:documentation "A fault put up as something to act on: what broke, and the
restarts still being offered where it broke."))

(defmethod print-object ((s standing) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a, ~d restart~:p" (of s) (length (restarts s)))))

(defun standing () *standing*)

(defun %text (s)
  (with-output-to-string (out)
    (let ((all (fault:standing)))
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
  "Work goes where the fault is, so a fix after a break lands in the image that
broke. What the target was is kept, and putting the debugger away puts it back."
  (let ((where (and f (fault:where f))))
    (when where
      (unless (evaluate:target-was)
        (setf (evaluate:target-was) (or (evaluate:target) :here)))
      (setf (evaluate:target) (job:name where)))))

(defun %back ()
  (let ((was (evaluate:target-was)))
    (when was
      (setf (evaluate:target) (unless (eq was :here) was))
      (setf (evaluate:target-was) nil))))

(defun show (condition &key restarts fault)
  "Put a fault up as a document you can act on rather than a line you cannot."
  (%follow fault)
  (let* ((s (make-instance 'standing
                           :of condition
                           :restarts (or restarts
                                         (and fault (fault:offers fault))
                                         (mapcar #'princ-to-string
                                                 (compute-restarts condition)))
                           :fault fault))
         (document (or (text:named *name*)
                       (text:make-document *name*
                                          :mode (make-instance 'emode:debugger)))))
    (setf *standing* s)
    (unless (typep (text:mode-of document) 'emode:debugger)
      (setf (text:mode-of document) (make-instance 'emode:debugger)))
    (setf (node:contents document) (%text s))
    (text:goto document 0 0)
    (setf (text:current) document)
    (window:show (window:focused) document)
    document))

(defmethod fault:faulted ((f fault:fault))
  "A fault somebody can still do something about goes up in front of them. A
window is what says somebody is there to look; with none, it is said instead,
which is what the layer below already does."
  (if (and (fault:standingp f) (window:focused))
      (show (fault:condition-of f) :fault f)
      (call-next-method)))

(defun choose (n)
  "Take the nth restart: the thread standing in the fault is handed it and goes."
  (let ((s (standing)))
    (when (and s (nth n (restarts s)))
      (let ((name (nth n (restarts s)))
            (f (fault-of s)))
        (setf *standing* nil)
        (if f
            (progn (fault:take f name) (log:note "took ~a" name))
            (log:note "~a" name))
        (unless (fault:standing) (%back))
        name))))

(defun next ()
  "The fault after this one, of the ones still standing."
  (let* ((all (fault:standing))
         (s (standing))
         (at (position (and s (fault-of s)) all))
         (f (nth (mod (1+ (or at -1)) (max 1 (length all))) all)))
    (when f (show (fault:condition-of f) :fault f))))

(defun away ()
  (setf *standing* nil)
  (%back)
  (when (text:named *name*) (command:run "kill-document" (list *name*)))
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
    (:describes "the last fault, as a document" :on '(text "C-x e"))
  (let ((f (or (first (fault:standing)) (first (fault:faults)))))
    (if f
        (node:name (show (fault:condition-of f) :fault f))
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
