(defpackage #:pine/run/meter
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:export
   #:timing #:counted #:readings #:reset #:report
   #:now))
(in-package #:pine/run/meter)

(defvar *on* t
  "Whether a sample is taken. On from boot: a number about the daemon you are
using cannot be had by turning something on afterwards and doing it again.")

(defparameter *kept* 256
  "How many samples an instrument keeps. Enough for a p95 that means something,
small enough that a hundred instruments cost nothing to hold.")

(defparameter +fields+ '("count" "per-second" "mean" "p50" "p95" "worst" "last"
                         "total" "seconds")
  "What an instrument answers for, as paths. Milliseconds where it is a duration,
because that is what a person reads a frame in.")

(defclass instrument (fs:dir)
  ((kind  :initarg :kind :reader kind-of)
   (count :initform 0   :reader count-of)
   (total :initform 0   :reader total-of)
   (least :initform nil :reader least-of)
   (most  :initform 0   :reader most-of)
   (last  :initform 0   :accessor last-of)
   (ring  :initform nil :reader ring-of)
   (at    :initform 0   :accessor at-of))
  (:documentation "One thing measured, at /metric/<name>: its samples, and under it
what they add up to."))

(defun now ()
  "Nanoseconds on a clock that only goes forward. GET-INTERNAL-REAL-TIME here
steps in four millisecond jumps, which cannot see a frame, let alone a swap."
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defmethod initialize-instance :after ((it instrument) &key)
  (setf (at-of it) (now))
  (dolist (field +fields+)
    (fs:attach (make-instance 'fs:derived :name field :live t
                              :reads (lambda () (%field (reading it) field)))
               it)))

(defun %metric () (fs:ensure (fs:root) "metric"))

(defun %of (name kind)
  (let* ((d (%metric))
         (name (string-downcase (princ-to-string name)))
         (it (fs:child d name
                       (lambda ()
                         (make-instance 'instrument :name name :kind kind :parent d)))))
    (unless (eq (fs:entry d name) it) (fs:attach it d))
    it))

(defun %record (name kind measure)
  "One sample, into the instrument's own slots. Each is replaced where it stands
rather than the whole thing being copied, which is what a sample costing under a
microsecond is for."
  (let ((it (%of name kind)))
    (d:swap (slot-value it 'count) #'1+)
    (d:swap (slot-value it 'total) #'+ measure)
    (d:swap (slot-value it 'most) #'max measure)
    (d:swap (slot-value it 'least)
            (lambda (had) (if had (min had measure) measure)))
    (d:swap (slot-value it 'ring) #'d:capped measure *kept*)
    (setf (last-of it) measure))
  measure)

(defmacro timing ((name) &body body)
  "Run BODY and record how long it took under NAME, in nanoseconds. What is
recorded is the same work the daemon does for anybody else, which is the whole
point of it being here rather than in a benchmark."
  (let ((start (gensym "START")) (answer (gensym "ANSWER")))
    `(if *on*
         (let* ((,start (now))
                (,answer (multiple-value-list (progn ,@body))))
           (%record ,name :time (- (now) ,start))
           (values-list ,answer))
         (progn ,@body))))

(defun counted (name &optional (by 1))
  "Say that NAME happened, or that it happened BY much: a fork, a frame that was
the same as the last one, the bytes a push carried. A count, not a duration,
and the table says so."
  (when *on* (%record name :count by))
  by)

(defun instruments ()
  (sort (remove-if-not (lambda (each) (typep each 'instrument))
                       (fs:entries (%metric)))
        #'string< :key #'fs:name))

(defun %percentile (ring share)
  (when ring
    (let ((sorted (sort (copy-list ring) #'<)))
      (nth (min (1- (length sorted))
                (floor (* share (length sorted))))
           sorted))))

(defun reading (it)
  "What one instrument has to say, in microseconds, as a plist."
  (let* ((ring (ring-of it))
         (had (if (eq :count (kind-of it)) (total-of it) (count-of it)))
         (seconds (max 0.001 (/ (- (now) (at-of it)) 1000000000.0))))
    (list :name (fs:name it)
          :kind (kind-of it)
          :count had
          :per-second (/ had seconds)
          :mean (if (plusp (count-of it)) (round (total-of it) (count-of it)) 0)
          :p50 (%percentile ring 0.50)
          :p95 (%percentile ring 0.95)
          :least (least-of it)
          :most (most-of it)
          :last (last-of it)
          :total (total-of it)
          :seconds seconds)))

(defun readings ()
  "Every instrument, in one shape. The synthetic runs and the live daemon both
answer this, which is what lets one be laid beside the other."
  (mapcar #'reading (instruments)))

(defun reset (&optional name)
  (let ((d (%metric)))
    (if name
        (fs:erase-entry d (string-downcase (princ-to-string name)))
        (dolist (each (instruments)) (fs:erase-entry d (fs:name each)))))
  t)

(defun %ms (nanoseconds) (and nanoseconds (/ (round nanoseconds 1000) 1000.0)))

(defun %field (said field)
  (cond ((null said) nil)
        ((equal field "count") (getf said :count))
        ((equal field "per-second") (float (getf said :per-second)))
        ((equal field "seconds") (float (getf said :seconds)))
        ((eq :count (getf said :kind)) nil)
        ((equal field "mean") (%ms (getf said :mean)))
        ((equal field "p50") (%ms (getf said :p50)))
        ((equal field "p95") (%ms (getf said :p95)))
        ((equal field "worst") (%ms (getf said :most)))
        ((equal field "last") (%ms (getf said :last)))
        ((equal field "total") (%ms (getf said :total)))))

(defun %attach (root)
  (setf (fs:describes (fs:ensure root "metric")) "how long what pine does is taking"))

(defun report (rows &key (to *standard-output*) about)
  "The table, said once. ABOUT is what produced these numbers: a workload and
its parameters, or that they came off a running daemon. A number without that
is a number about nothing."
  (when about (format to "~&~a~%" about))
  (format to "~&~26a ~8a ~12a ~11a ~11a ~11a~%"
          "instrument" "count" "per second" "mean ms" "p95 ms" "worst ms")
  (dolist (row rows rows)
    (if (eq :count (getf row :kind))
        (format to "~&~26a ~8:d ~12,1f ~11a ~11a ~11a~%"
                (string-downcase (princ-to-string (getf row :name)))
                (getf row :count) (float (getf row :per-second)) "-" "-" "-")
        (format to "~&~26a ~8:d ~12,1f ~11,3f ~11,3f ~11,3f~%"
                (string-downcase (princ-to-string (getf row :name)))
                (getf row :count) (float (getf row :per-second))
                (or (%ms (getf row :mean)) 0) (or (%ms (getf row :p95)) 0)
                (or (%ms (getf row :most)) 0)))))

(pine/fs:builder #'%attach)
