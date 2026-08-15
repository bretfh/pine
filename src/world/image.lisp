(defpackage #:pine/world/image
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:task #:pine/run/task) (#:timer #:pine/run/timer)
                    (#:fault #:pine/run/fault) (#:log #:pine/run/log))
  (:export #:install #:tasks-node #:ticks-node #:faults-node #:log-node))
(in-package #:pine/world/image)

(defclass tasks-node (node:node) ())
(defclass task-node (node:node) ())
(defclass ticks-node (node:node) ())
(defclass tick-node (node:node) ())
(defclass faults-node (node:node) ())
(defclass fault-node (node:node) ())
(defclass offers-node (node:node) ())
(defclass log-node (node:node) ())

(defun %kid (n name class)
  (node:child n name (lambda () (make-instance class :name name :parent n))))

(defmethod node:nodes ((n tasks-node))
  (loop :for tk :in (task:tasks) :collect (%kid n (task:name tk) 'task-node)))

(defmethod node:resolve ((n tasks-node) name)
  (when (task:task-named name) (%kid n (princ-to-string name) 'task-node)))

(defmethod node:contents ((n tasks-node)) (mapcar #'task:name (task:tasks)))

(defmethod node:contents ((n task-node))
  (let ((tk (task:task-named (node:name n))))
    (and tk (task:alivep tk))))

(defmethod (setf node:contents) (value (n task-node))
  "Writing nothing to a task asks it to stop. What blocks on a stream cannot be
interrupted, so it goes when its next read comes back."
  (let ((tk (task:task-named (node:name n))))
    (when (and tk (null value)) (task:stop tk))
    value))

(defmethod node:nodes ((n ticks-node))
  (loop :for name :in (timer:names)
        :collect (%kid n (princ-to-string name) 'tick-node)))

(defmethod node:contents ((n ticks-node))
  (mapcar #'princ-to-string (timer:names)))

(defmethod node:contents ((n tick-node))
  (and (member (node:name n) (timer:names)
               :key #'princ-to-string :test #'equal)
       t))

(defmethod (setf node:contents) (value (n tick-node))
  (let ((had (find (node:name n) (timer:names)
                   :key #'princ-to-string :test #'equal)))
    (when (and had (null value)) (timer:cancel had))
    value))

(defun %faults () (fault:faults))

(defun %fault-at (n)
  (nth (or (parse-integer (node:name n) :junk-allowed t) -1) (%faults)))

(defmethod node:nodes ((n faults-node))
  (loop :for i :from 0 :below (length (%faults))
        :collect (%kid n (princ-to-string i) 'fault-node)))

(defmethod node:resolve ((n faults-node) name)
  (let ((i (parse-integer (princ-to-string name) :junk-allowed t)))
    (when (and i (< -1 i (length (%faults))))
      (%kid n (princ-to-string name) 'fault-node))))

(defmethod node:contents ((n faults-node)) (length (%faults)))

(defmethod node:contents ((n fault-node))
  (let ((f (%fault-at n)))
    (and f (princ-to-string (fault:condition-of f)))))

(defmethod (setf node:contents) (value (n fault-node))
  "Writing a restart's name to a fault takes it. The thread is still standing
there, here or in another image, so this is the same act as taking one in the
debugger."
  (let ((f (%fault-at n)))
    (when f (fault:resume f (princ-to-string value)))
    value))

(defmethod node:nodes ((n fault-node))
  (list (%kid n "offers" 'offers-node)))

(defmethod node:contents ((n offers-node))
  (let ((f (%fault-at (node:parent n))))
    (and f (fault:offers f))))

(defmethod node:contents ((n log-node)) (log:said))

(defmethod (setf node:contents) (value (n log-node))
  "Writing nothing to the log forgets what it holds."
  (when (null value) (log:forget))
  value)

(defmethod node:leafp ((n task-node)) t)
(defmethod node:leafp ((n tick-node)) t)
(defmethod node:leafp ((n offers-node)) t)
(defmethod node:leafp ((n log-node)) t)

(defmethod node:livep ((n tasks-node)) t)
(defmethod node:livep ((n task-node)) t)
(defmethod node:livep ((n ticks-node)) t)
(defmethod node:livep ((n tick-node)) t)
(defmethod node:livep ((n faults-node)) t)
(defmethod node:livep ((n fault-node)) t)
(defmethod node:livep ((n offers-node)) t)
(defmethod node:livep ((n log-node)) t)

(defmethod node:persistp ((n tasks-node)) nil)
(defmethod node:persistp ((n task-node)) nil)
(defmethod node:persistp ((n ticks-node)) nil)
(defmethod node:persistp ((n tick-node)) nil)
(defmethod node:persistp ((n faults-node)) nil)
(defmethod node:persistp ((n fault-node)) nil)
(defmethod node:persistp ((n offers-node)) nil)
(defmethod node:persistp ((n log-node)) nil)

(defun install (root)
  "What this image is doing, where anything can read it: pine ls /task, pine
watch /fault, pine write /fault/0 ABORT."
  (node:attach (make-instance 'tasks-node :name "task"
                                          :describes "the threads this image made")
               root)
  (node:attach (make-instance 'ticks-node :name "tick"
                                          :describes "what repeats on the image's clock")
               root)
  (node:attach (make-instance 'faults-node :name "fault"
                                           :describes "what has broken, and what it is standing in")
               root)
  (node:attach (make-instance 'log-node :name "log"
                                        :describes "what pine said")
               root))
