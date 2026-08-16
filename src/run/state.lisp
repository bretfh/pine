(defpackage #:pine/run/state
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault)
                    (#:log #:pine/run/log) (#:meter #:pine/run/meter))
  (:export #:attach #:ticks-node #:faults-node #:log-node #:metric-node))
(in-package #:pine/run/state)

(defparameter +fields+ '("count" "per-second" "mean" "p50" "p95" "worst" "last"
                         "total" "seconds")
  "What an instrument answers for, as paths. Milliseconds where it is a duration,
because that is what a person reads a frame in.")

(defclass live (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Answers from the image itself, so no snapshot walks into it."))

(defclass ticks-node (live) ())
(defclass tick-node (live) ())
(defclass faults-node (live) ())
(defclass fault-node (live) ())
(defclass offers-node (live) ())
(defclass log-node (live) ())
(defclass metric-node (live) ())
(defclass instrument-node (live) ())
(defclass field-node (live) ())

(defun %kid (n name class)
  (node:child n name (lambda () (make-instance class :name name :over n))))

(defmethod node:nodes ((n ticks-node))
  (loop :for name :in (actors:ticks)
        :collect (%kid n (princ-to-string name) 'tick-node)))

(defmethod node:contents ((n ticks-node))
  (mapcar #'princ-to-string (actors:ticks)))

(defmethod node:contents ((n tick-node))
  (and (member (node:name n) (actors:ticks) :key #'princ-to-string :test #'equal) t))

(defmethod (setf node:contents) (value (n tick-node))
  (let ((had (find (node:name n) (actors:ticks)
                   :key #'princ-to-string :test #'equal)))
    (when (and had (null value)) (actors:cancel had))
    value))

(defun %faults () (fault:faults))

(defun %fault-at (n)
  (let ((i (parse-integer (node:name n) :junk-allowed t)))
    (when (and i (not (minusp i))) (nth i (%faults)))))

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
    (when f (fault:take f (princ-to-string value)))
    value))

(defmethod node:nodes ((n fault-node)) (list (%kid n "offers" 'offers-node)))

(defmethod node:contents ((n offers-node))
  (let ((f (%fault-at (node:over n))))
    (and f (fault:offers f))))

(defmethod node:contents ((n log-node)) (log:said))

(defmethod (setf node:contents) (value (n log-node))
  (when (null value) (log:forget))
  value)

(defun %instrument (n)
  (find (node:name n) (meter:instruments)
        :key (lambda (each) (string-downcase (princ-to-string each)))
        :test #'equal))

(defun %ms (nanoseconds) (and nanoseconds (/ (round nanoseconds 1000) 1000.0)))

(defmethod node:nodes ((n metric-node))
  (loop :for name :in (meter:instruments)
        :collect (%kid n (string-downcase (princ-to-string name))
                       'instrument-node)))

(defmethod node:resolve ((n metric-node) name)
  (when (find name (meter:instruments)
              :key (lambda (each) (string-downcase (princ-to-string each)))
              :test #'equal)
    (%kid n name 'instrument-node)))

(defmethod node:contents ((n metric-node))
  (mapcar (lambda (row) (getf row :name)) (meter:said)))

(defmethod node:nodes ((n instrument-node))
  (loop :for field :in +fields+ :collect (%kid n field 'field-node)))

(defmethod node:resolve ((n instrument-node) name)
  (when (member name +fields+ :test #'equal) (%kid n name 'field-node)))

(defmethod node:contents ((n instrument-node))
  (let ((name (%instrument n)))
    (when name (meter:reading name))))

(defmethod node:contents ((n field-node))
  (let* ((name (%instrument (node:over n)))
         (said (and name (meter:reading name)))
         (field (node:name n)))
    (when said
      (cond ((equal field "count") (getf said :count))
            ((equal field "per-second") (float (getf said :per-second)))
            ((equal field "seconds") (float (getf said :seconds)))
            ((eq :count (getf said :kind)) nil)
            ((equal field "mean") (%ms (getf said :mean)))
            ((equal field "p50") (%ms (getf said :p50)))
            ((equal field "p95") (%ms (getf said :p95)))
            ((equal field "worst") (%ms (getf said :most)))
            ((equal field "last") (%ms (getf said :last)))
            ((equal field "total") (%ms (getf said :total)))))))

(defun attach (root)
  "What this image is doing, where anything can read it: pine ls /tick, pine watch
/fault, pine write /fault/0 ABORT, pine read /metric/frame/p95."
  (node:attach (make-instance 'ticks-node :name "tick"
                              :describes "what repeats on the image's clock")
               root)
  (node:attach (make-instance 'faults-node :name "fault"
                              :describes "what has broken, and what it stands in")
               root)
  (let ((n (node:attach (make-instance 'log-node :name "log"
                                        :describes "what pine said")
                        root)))
    (setf log:*on-note* (lambda (line) (declare (ignore line)) (node:stir n))))
  (node:attach (make-instance 'metric-node :name "metric"
                              :describes "how long what pine does is taking")
               root))
