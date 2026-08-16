(defpackage #:pine/edit/debugger
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:doc #:pine/text/document) (#:emode #:pine/edit/mode)
                    (#:window #:pine/edit/window) (#:log #:pine/run/log))
  (:export #:show #:standing #:choose #:next #:fault-of #:commands #:away #:*name* #:*standing*))
(in-package #:pine/edit/debugger)

(defvar *standing* (d:box nil))
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

(defun standing () (d:held *standing*))

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

(defun show (condition &key restarts fault)
  "Put a fault up as a document you can act on rather than a line you cannot."
  (let* ((s (make-instance 'standing
                           :of condition
                           :restarts (or restarts
                                         (and fault (fault:offers fault))
                                         (mapcar #'princ-to-string
                                                 (compute-restarts condition)))
                           :fault fault))
         (document (or (doc:named *name*)
                       (doc:make-document *name*
                                          :mode (make-instance 'emode:debugger)))))
    (d:put! *standing* s)
    (unless (typep (doc:mode-of document) 'emode:debugger)
      (setf (doc:mode-of document) (make-instance 'emode:debugger)))
    (setf (node:contents document) (%text s))
    (doc:goto document 0 0)
    (setf (doc:current) document)
    (window:show (window:focused) document)
    document))

(defun choose (n)
  "Take the nth restart: the thread standing in the fault is handed it and goes."
  (let ((s (standing)))
    (when (and s (nth n (restarts s)))
      (let ((name (nth n (restarts s)))
            (f (fault-of s)))
        (d:put! *standing* nil)
        (if f
            (progn (fault:take f name) (log:note "took ~a" name))
            (log:note "~a" name))
        name))))

(defun next ()
  "The fault after this one, of the ones still standing."
  (let* ((all (fault:standing))
         (s (standing))
         (at (position (and s (fault-of s)) all))
         (f (nth (mod (1+ (or at -1)) (max 1 (length all))) all)))
    (when f (show (fault:condition-of f) :fault f))))

(defun away ()
  (d:put! *standing* nil)
  (when (doc:named *name*) (command:run "kill-document" (list *name*)))
  t)

(defun commands ()
  (command:defcommand "debugger-abort" () (:describes "leave the fault alone")
    (away))
  (command:defcommand "debugger-quit" () (:describes "put the debugger away")
    (away))
  (command:defcommand "debugger-restart" (n) (:describes "take one of the restarts")
    (choose (if (integerp n)
                n
                (or (parse-integer (princ-to-string n) :junk-allowed t) 0))))
  (command:defcommand "debugger" () (:describes "the last fault, as a document")
    (let ((f (or (first (fault:standing)) (first (fault:faults)))))
      (if f
          (node:name (show (fault:condition-of f) :fault f))
          (log:note "nothing has faulted"))))
  (command:defcommand "debugger-next" () (:describes "the fault after this one")
    (and (next) t))
  (command:defcommand "toggle-debug-on-error" ()
      (:describes "whether a fault stands its thread or unwinds")
    (setf fault:*debugging* (not fault:*debugging*))
    (log:note "the debugger is ~:[off~;on~]" fault:*debugging*)
    fault:*debugging*)
  (dolist (n '(0 1 2 3 4 5 6 7 8 9))
    (let ((n n))
      (command:command (format nil "debugger-restart-~d" n)
                       (lambda () (choose n))
                       :describes "take this restart"))))
