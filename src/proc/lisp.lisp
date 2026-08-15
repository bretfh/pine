(defpackage #:pine/proc/lisp
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:process #:pine/proc/process) (#:fault #:pine/run/fault) (#:task #:pine/run/task))
  (:export #:lisp-process #:evaluate #:answered-by #:saidp #:ready-p #:wait-ready
           #:take-there #:there #:*elsewhere*
           #:*sbcl* #:*load-form*))
(in-package #:pine/proc/lisp)

(defvar *sbcl* (namestring sb-ext:*runtime-pathname*))
(defvar *load-form* "(require :asdf)")
(defvar *ready* "pine-process-ready")
(defvar *said* "pine-process-said ")
(defvar *elsewhere* (d:box nil))

(defparameter +loop+
  "(let ((*print-pretty* nil) (*print-circle* nil) (*print-readably* nil))
   (loop
     (let ((form (read *standard-input* nil :eof)))
       (when (eq form :eof) (sb-ext:exit))
       (format t \"~a~~s~~%\"
               (with-simple-restart (abort \"give up on this form\")
                 (handler-bind
                     ((error
                        (lambda (e)
                          (format t \"~a(:fault ~~s ~~s)~~%\"
                                  (princ-to-string e)
                                  (mapcar (lambda (r)
                                            (princ-to-string (restart-name r)))
                                          (remove nil (compute-restarts e)
                                                  :key (function restart-name))))
                          (force-output)
                          (let ((said (read *standard-input* nil :eof)))
                            (when (and (consp said) (eq :take (first said)))
                              (let ((r (find (second said) (compute-restarts e)
                                             :key (lambda (each)
                                                    (princ-to-string
                                                     (restart-name each)))
                                             :test (function equal))))
                                (when r (invoke-restart r))))))))
                   (eval form))))
       (force-output))))"
  "The child's read and eval loop. Every line it means is marked, because the
systems it loads say things on the same stream.")


(defclass lisp-process (process:program)
  ((systems :initarg :systems :accessor systems :initform '(:pine))
   (readyp  :initform (d:box nil) :reader readyp)))

(defun %argv (p)
  (list *sbcl* "--noinform" "--no-userinit" "--disable-debugger"
        "--eval" *load-form*
        "--eval" (format nil "(progn ~{(asdf:load-system ~s)~})" (systems p))
        "--eval" (format nil "(progn (princ ~s) (terpri) (force-output))" *ready*)
        "--eval" (format nil +loop+ *said* *said*)))

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

(defun ready-p (p) (d:held (readyp p)))

(defun wait-ready (p &key (timeout 60))
  (loop :repeat (round (/ timeout 0.05))
        :for line := (read-line (%out p) nil nil)
        :do (cond ((null line) (sleep 0.05))
                  ((search *ready* line)
                   (d:put! (readyp p) t)
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
          :do (cond ((saidp line)
                     (return (multiple-value-bind (value fault offers)
                                 (answered-by line)
                               (when fault (%coming-home p fault offers))
                               (values value fault offers))))
                    (line (process:emit p line))
                    (t (sleep 0.02))))))

(defun %coming-home (p said offers)
  "A fault in the other image joins this one's, standing in what it offers
there. Whoever takes one of those -- the debugger buffer, or nobody, in which
case it gives up -- is what the thread over there is told."
  (let ((f (fault:elsewhere (make-condition 'simple-error
                                            :format-control "~a: ~a"
                                            :format-arguments
                                            (list (process:name p) said))
                            offers
                            (format nil "in ~a" (process:name p)))))
    (d:swap! *elsewhere* (lambda (all) (cons (cons f p) (remove f all :key #'car))))
    (task:spawn (format nil "~a fault" (process:name p))
                (lambda ()
                  (unwind-protect
                       (take-there p (or (fault:await f) "ABORT"))
                    (d:swap! *elsewhere*
                            (lambda (all) (remove f all :key #'car))))))
    f))

(defun there (f)
  "The image a fault came home from."
  (cdr (assoc f (d:held *elsewhere*))))

(defun saidp (line)
  (and line (>= (length line) (length *said*))
       (string= *said* line :end2 (length *said*))))

(defun answered-by (line)
  "What a marked line from the child says: a value, or a fault with the
restarts that image is still offering."
  (let* ((*read-eval* nil)
         (text (if (saidp line) (subseq line (length *said*)) line))
         (value (handler-case (read-from-string text) (error () text))))
    (if (and (consp value) (eq :fault (first value)))
        (values nil (second value) (third value))
        (values value nil nil))))

(defun take-there (p name)
  "Tell the child to take one of the restarts it offered. Its thread is still
standing in the fault, so this is the same act as taking one here."
  (let ((in (%in p)))
    (write-string (prin1-to-string (list :take name)) in)
    (terpri in)
    (force-output in))
  (loop :repeat 1500
        :for line := (read-line (%out p) nil nil)
        :do (cond ((saidp line) (return (answered-by line)))
                  (line (process:emit p line))
                  (t (sleep 0.02)))))
