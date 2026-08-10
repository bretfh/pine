(defpackage #:pine.proc.supervisor
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:task #:pine.run.task)
                    (#:process #:pine.proc.process))
  (:export #:supervisor #:supervise #:forget #:processes #:process-named
           #:attend #:attends #:watch #:unwatch #:start-all #:stop-all #:due))

(in-package #:pine.proc.supervisor)

(defvar *interval* 1)

(defclass supervisor ()
  ((held    :initform (d:box nil) :reader held)
   (attends :initform nil          :accessor attends)))

(defmethod print-object ((s supervisor) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~d" (length (processes s)))))

(defun supervisor () (make-instance 'supervisor))

(defun processes (s) (d:held (held s)))

(defun process-named (s name)
  (find name (processes s) :key #'process:name :test #'equal))

(defun supervise (s p)
  (d:swap! (held s)
          (lambda (all)
            (append (remove (process:name p) all
                            :key #'process:name :test #'equal)
                    (list p))))
  p)

(defun forget (s name)
  (let ((p (process-named s name)))
    (when p
      (ignore-errors (process:stop p))
      (d:swap! (held s) (lambda (all) (remove p all))))
    p))

(defun due (p now)
  (or (zerop (process:attempts p))
      (>= now (+ (process:backoff p) (or (process:exit-of p) 0)))))

(defun attend (s)
  (dolist (p (processes s) s)
    (when (and (process:restarts-p p)
               (member (process:state p) '(:running :failed))
               (not (process:alivep p)))
      (setf (process:state p) :failed)
      (handler-case (process:start p)
        (error (e) (setf (process:fault p) e
                         (process:state p) :failed))))))

(defun watch (s &key (every *interval*))
  (setf (attends s) (task:each "supervisor" every (lambda () (attend s))))
  s)

(defun unwatch (s)
  (when (attends s)
    (task:stop (attends s))
    (setf (attends s) nil))
  s)

(defun start-all (s)
  (dolist (p (processes s) s)
    (handler-case (process:start p)
      (error (e) (setf (process:fault p) e (process:state p) :failed)))))

(defun stop-all (s)
  (dolist (p (reverse (processes s)) s)
    (ignore-errors (process:stop p))))
