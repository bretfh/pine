(defpackage #:pine/world/metric
  (:use #:cl)
  (:local-nicknames (#:meter #:pine/run/meter) (#:node #:pine/fs/node))
  (:export #:metric-node #:install #:readings))
(in-package #:pine/world/metric)

(defparameter +fields+ '("count" "per-second" "mean" "p50" "p95" "worst" "last"
                         "total" "seconds")
  "What an instrument answers for, as paths. Milliseconds where it is a
duration, because that is what a person reads a frame in.")

(defclass metric-node (node:node) ())
(defclass instrument-node (node:node) ())
(defclass field-node (node:node) ())

(defun readings () (meter:said))

(defun %named (n name class)
  (node:child n name (lambda () (make-instance class :name name :parent n))))

(defmethod node:nodes ((n metric-node))
  (loop :for name :in (meter:instruments)
        :collect (%named n (string-downcase (princ-to-string name))
                         'instrument-node)))

(defmethod node:resolve ((n metric-node) name)
  (when (find name (meter:instruments)
              :key (lambda (each) (string-downcase (princ-to-string each)))
              :test #'equal)
    (%named n name 'instrument-node)))

(defmethod node:nodes ((n instrument-node))
  (loop :for field :in +fields+ :collect (%named n field 'field-node)))

(defmethod node:resolve ((n instrument-node) name)
  (when (member name +fields+ :test #'equal) (%named n name 'field-node)))

(defun %instrument (n)
  (find (node:name n) (meter:instruments)
        :key (lambda (each) (string-downcase (princ-to-string each)))
        :test #'equal))

(defun %ms (nanoseconds) (and nanoseconds (/ (round nanoseconds 1000) 1000.0)))

(defmethod node:contents ((n metric-node))
  (mapcar (lambda (row) (getf row :name)) (readings)))

(defmethod node:contents ((n instrument-node))
  (let ((name (%instrument n)))
    (when name (meter:reading name))))

(defmethod node:contents ((n field-node))
  (let* ((name (%instrument (node:parent n)))
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

(defmethod node:leafp ((n field-node)) t)
(defmethod node:livep ((n metric-node)) t)
(defmethod node:livep ((n instrument-node)) t)
(defmethod node:livep ((n field-node)) t)
(defmethod node:persistp ((n metric-node)) nil)
(defmethod node:persistp ((n instrument-node)) nil)
(defmethod node:persistp ((n field-node)) nil)

(defun install (root)
  "What the daemon is doing, where anything can read it: pine read
/metric/frame/p95, pine watch /metric/key/count, or a widget in a bar."
  (node:attach (make-instance 'metric-node
                              :name "metric"
                              :describes "how long what pine does is taking")
               root))
