(defpackage #:pine.provider.clock
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:computed #:pine.fs.computed)
                    (#:c #:pine.run.cell) (#:process #:pine.proc.process))
  (:export #:clock-node #:install #:now #:tick #:*every*))

(in-package #:pine.provider.clock)

(defvar *every* 1)
(defvar *now* (c:cell 0))
(defvar *clock* nil)

(defparameter +parts+
  '("second" "minute" "min" "hour" "day" "month" "year" "weekday"))

(defclass clock-node (node:node) ())

(defclass part-node (node:node) ())

(defun now () (c:held *now*))

(defun %part (name at)
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time at)
    (cond ((equal name "second") second)
          ((member name '("minute" "min") :test #'equal)
           (format nil "~2,'0d" minute))
          ((equal name "hour") (format nil "~2,'0d" hour))
          ((equal name "day") day)
          ((equal name "month") month)
          ((equal name "year") year)
          ((equal name "weekday") weekday))))

(defun %part-node (n name)
  (node:child n name
              (lambda () (make-instance 'part-node :name name :parent n))))

(defun tick ()
  (let ((was (c:held *now*)))
    (c:put *now* (get-universal-time))
    (when (and *clock* was)
      (dolist (name +parts+)
        (unless (equal (%part name was) (%part name (now)))
          (node:invalidate (%part-node *clock* name))))))
  (now))

(defmethod node:nodes ((n clock-node))
  (loop :for name :in +parts+ :collect (%part-node n name)))

(defmethod node:resolve ((n clock-node) name)
  (when (member name +parts+ :test #'equal) (%part-node n name)))

(defmethod node:contents ((n clock-node)) (now))
(defmethod node:contents ((n part-node)) (%part (node:name n) (now)))
(defmethod node:leafp ((n part-node)) t)
(defmethod node:persistp ((n clock-node)) nil)
(defmethod node:persistp ((n part-node)) nil)

(defmethod node:livep ((n clock-node)) t)
(defmethod node:livep ((n part-node)) t)

(defun install (root &key supervisor)
  (tick)
  (setf *clock* (node:attach (make-instance 'clock-node :name "clock"
                                                        :describes "the time, as paths")
                             root))
  (when supervisor
    (let ((p (make-instance 'process:thread-process
                            :name "clock" :every *every* :thunk #'tick)))
      (pine.proc.supervisor:supervise supervisor p)
      (process:start p)
      p)))
