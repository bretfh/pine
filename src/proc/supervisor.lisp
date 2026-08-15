(defpackage #:pine/proc/supervisor
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:timer #:pine/run/timer) (#:process #:pine/proc/process))
  (:export #:supervisor #:supervise #:forget #:processes #:process-named
           #:attend #:attends #:watch #:unwatch #:start-all #:stop-all #:due
           #:processes-node #:install))
(in-package #:pine/proc/supervisor)

(defvar *interval* 1)
(defvar *under* nil)

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
  (when *under* (setf (node:parent p) *under*))
  p)

(defclass processes-node (node:node)
  ((of :initarg :of :reader of)))

(defmethod node:nodes ((n processes-node)) (processes (of n)))

(defmethod node:resolve ((n processes-node) name)
  (process-named (of n) name))

(defmethod node:contents ((n processes-node))
  (mapcar #'process:name (processes (of n))))

(defmethod node:livep ((n processes-node)) t)
(defmethod node:persistp ((n processes-node)) nil)

(defun install (s root)
  "What this pine supervises, where anything can read it. A process is a node
already; this is where they hang, so pine read /proc/editor answers its state
and pine write /proc/editor '(:restart)' starts it again."
  (setf *under* (node:attach (make-instance 'processes-node :name "proc" :of s
                                            :describes "what this pine is running")
                             root))
  (dolist (p (processes s) *under*) (setf (node:parent p) *under*)))

(defun forget (s name)
  (let ((p (process-named s name)))
    (when p
      (ignore-errors (process:stop p))
      (d:swap! (held s) (lambda (all) (remove p all))))
    p))

(defun due (p now)
  "Whether enough has passed since the last try to make another. Without this a
program that dies as fast as it starts is started once a second for as long as
pine runs, and the backoff is a number nobody reads."
  (let ((last (process:since p)))
    (or (null last) (>= now (+ last (process:backoff p))))))

(defun attend (s)
  (let ((now (get-universal-time)))
    (dolist (p (processes s) s)
      (when (and (process:restarts-p p)
                 (member (process:state p) '(:running :failed))
                 (not (process:alivep p))
                 (due p now))
        (setf (process:state p) :failed
              (process:since p) now)
        (handler-case (process:start p)
          (error (e) (setf (process:fault p) e
                           (process:state p) :failed)))))))

(defun watch (s &key (every *interval*))
  (setf (attends s)
        (timer:every-seconds every (lambda () (attend s))
                             :as :supervisor :what "attending the processes"))
  s)

(defun unwatch (s)
  (when (attends s)
    (timer:cancel (attends s))
    (setf (attends s) nil))
  s)

(defun start-all (s)
  (dolist (p (processes s) s)
    (handler-case (process:start p)
      (error (e) (setf (process:fault p) e (process:state p) :failed)))))

(defun stop-all (s)
  (dolist (p (reverse (processes s)) s)
    (ignore-errors (process:stop p))))
