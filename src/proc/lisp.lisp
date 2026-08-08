(defpackage #:pine.proc.lisp
  (:use #:cl)
  (:local-nicknames (#:process #:pine.proc.process) (#:c #:pine.run.cell))
  (:export #:lisp-process #:evaluate #:answered-by #:ready-p #:wait-ready
           #:*sbcl* #:*load-form*))

(in-package #:pine.proc.lisp)

(defvar *sbcl* (namestring sb-ext:*runtime-pathname*))
(defvar *load-form* "(require :asdf)")
(defvar *ready* "pine-process-ready")

(defclass lisp-process (process:program)
  ((systems :initarg :systems :accessor systems :initform '(:pine))
   (readyp  :initform (c:cell nil) :reader readyp)))

(defun %argv (p)
  (list *sbcl* "--noinform" "--no-userinit" "--disable-debugger"
        "--eval" *load-form*
        "--eval" (format nil "(progn ~{(asdf:load-system ~s)~})" (systems p))
        "--eval" (format nil "(progn (princ ~s) (terpri) (force-output))" *ready*)
        "--eval" "(loop (let ((form (read *standard-input* nil :eof)))
                          (when (eq form :eof) (sb-ext:exit))
                          (handler-case (format t \"~s~%\" (eval form))
                            (error (e) (format t \"(:fault ~s)~%\" (princ-to-string e))))
                          (force-output)))"))

(defmethod initialize-instance :after ((p lisp-process) &key)
  (setf (process:argv p) (%argv p)))

(defmethod process:start ((p lisp-process))
  (let ((it (uiop:launch-program (process:argv p)
                                 :input :stream :output :stream
                                 :error-output :output)))
    (setf (process:took p) it)
    (wait-ready p)
    p))

(defun %in (p) (uiop:process-info-input (process:took p)))
(defun %out (p) (uiop:process-info-output (process:took p)))

(defun ready-p (p) (c:held (readyp p)))

(defun wait-ready (p &key (timeout 60))
  (loop :repeat (round (/ timeout 0.05))
        :for line := (read-line (%out p) nil nil)
        :do (cond ((null line) (sleep 0.05))
                  ((search *ready* line)
                   (c:put (readyp p) t)
                   (return t))
                  (t (process:emit p line)))
        :finally (return nil)))

(defgeneric evaluate (process form &key timeout)
  (:method ((p lisp-process) form &key (timeout 30))
    (let ((in (%in p)))
      (write-string (prin1-to-string form) in)
      (terpri in)
      (force-output in))
    (loop :repeat (round (/ timeout 0.02))
          :for line := (read-line (%out p) nil nil)
          :when line
            :do (return (answered-by line))
          :do (sleep 0.02))))

(defun answered-by (line)
  (let* ((*read-eval* nil)
         (value (handler-case (read-from-string line) (error () line))))
    (if (and (consp value) (eq :fault (first value)))
        (values nil (second value))
        (values value nil))))
