(defpackage #:pine.edit.debugger
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:cmd #:pine.repl.command)
                    (#:mode #:pine.repl.mode) (#:node #:pine.fs.node)
                    (#:fault #:pine.run.fault) (#:buffer #:pine.edit.buffer)
                    (#:window #:pine.edit.window) (#:log #:pine.run.log))
  (:export #:install #:show #:standing #:choose #:*name* #:*standing*))

(in-package #:pine.edit.debugger)

(defvar *standing* (c:cell nil))
(defparameter *name* "*debugger*")

(defclass standing ()
  ((of       :initarg :of       :reader of)
   (restarts :initarg :restarts :reader restarts :initform nil)
   (taken    :initarg :taken    :reader taken    :initform nil)))

(defmethod print-object ((s standing) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a, ~d restart~:p" (of s) (length (restarts s)))))

(defun standing () (c:held *standing*))

(defun %text (s)
  (with-output-to-string (out)
    (format out "~a~%~%" (of s))
    (loop :for r :in (restarts s)
          :for i :from 0
          :do (format out "~d  ~a~%" i r))
    (format out "~%a abort   q quit   0-9 a restart~%")
    (let ((f (first (fault:faults))))
      (when (and f (fault:backtrace-of f))
        (format out "~%~a~%" (fault:backtrace-of f))))))

(defun show (condition &key restarts taken)
  "Put a fault up as a buffer you can act on rather than a line you cannot."
  (let* ((s (make-instance 'standing :of condition
                                     :restarts (or restarts
                                                   (mapcar #'princ-to-string
                                                           (compute-restarts condition)))
                                     :taken taken))
         (b (or (buffer:buffer-named *name*)
                (buffer:make-buffer *name* :mode "debugger"))))
    (c:put *standing* s)
    (setf (buffer:mode-of b) "debugger")
    (setf (node:contents b) (%text s))
    (buffer:goto! b 0 0)
    (setf (buffer:current) b)
    (window:show! (window:focused) b)
    b))

(defun choose (n)
  (let ((s (standing)))
    (when (and s (nth n (restarts s)))
      (let ((taken (taken s)))
        (c:put *standing* nil)
        (if taken
            (funcall taken n)
            (log:note "~a" (nth n (restarts s))))
        (nth n (restarts s))))))

(defun %away ()
  (c:put *standing* nil)
  (let ((b (buffer:buffer-named *name*)))
    (when b (pine.repl.command:run "kill-buffer" (list *name*))))
  t)

(defun install ()
  (mode:mode "debugger" :parent "text" :settings '(:indicator "Debug"))
  (cmd:defcommand "debugger-abort" () (:describes "leave the fault alone")
    (%away))
  (cmd:defcommand "debugger-quit" () (:describes "put the debugger away")
    (%away))
  (cmd:defcommand "debugger-restart" (n) (:describes "take one of the restarts")
    (choose (if (integerp n) n (or (parse-integer (princ-to-string n) :junk-allowed t) 0))))
  (cmd:defcommand "debugger" () (:describes "the last fault, as a buffer")
    (let ((f (first (fault:faults))))
      (if f
          (node:name (show (fault:condition-of f)))
          (log:note "nothing has faulted"))))
  (mode:bind "debugger" "a" "debugger-abort")
  (mode:bind "debugger" "q" "debugger-quit")
  (dolist (n '(0 1 2 3 4 5 6 7 8 9))
    (let ((n n))
      (cmd:command (format nil "debugger-restart-~d" n)
                   (lambda () (choose n))
                   :describes "take this restart")
      (mode:bind "debugger" (princ-to-string n)
                 (format nil "debugger-restart-~d" n))))
  (setf fault:*on-fault*
        (lambda (f)
          (when (buffer:current)
            (log:note "~a" (fault:condition-of f)))))
  t)
