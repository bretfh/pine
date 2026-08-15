(defpackage #:pine/run/meter
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export #:timing #:counted #:said #:reading #:reset #:instruments #:report
           #:now #:*on* #:*kept*))
(in-package #:pine/run/meter)

(defvar *on* t
  "Whether a sample is taken. On from boot: a number about the daemon you are
using cannot be had by turning something on afterwards and doing it again.")

(defparameter *kept* 256
  "How many samples an instrument keeps. Enough for a p95 that means something,
small enough that a hundred instruments cost nothing to hold.")

(defvar *instruments* (d:table))

(defstruct (instrument (:constructor %instrument (name kind)))
  name
  kind
  (count 0)
  (total 0)
  (least nil)
  (most 0)
  (last 0)
  (ring nil)
  (at 0))

(defun now ()
  "Nanoseconds on a clock that only goes forward. GET-INTERNAL-REAL-TIME here
steps in four millisecond jumps, which cannot see a frame, let alone a swap."
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defun %of (name kind)
  (or (d:at (d:all *instruments*) name)
      (d:claim *instruments* name
               (d:box (let ((it (%instrument name kind)))
                        (setf (instrument-at it) (now))
                        it)))))

(defun %record (name kind measure)
  (d:swap! (%of name kind)
           (lambda (had)
             (let ((next (copy-instrument had)))
               (incf (instrument-count next))
               (incf (instrument-total next) measure)
               (setf (instrument-last next) measure
                     (instrument-most next) (max (instrument-most next) measure)
                     (instrument-least next) (if (instrument-least next)
                                                 (min (instrument-least next) measure)
                                                 measure)
                     (instrument-ring next)
                     (let ((ring (cons measure (instrument-ring next))))
                       (if (> (length ring) *kept*) (subseq ring 0 *kept*) ring)))
               next)))
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
  (sort (d:keys (d:all *instruments*)) #'string< :key #'princ-to-string))

(defun %percentile (ring share)
  (when ring
    (let ((sorted (sort (copy-list ring) #'<)))
      (nth (min (1- (length sorted))
                (floor (* share (length sorted))))
           sorted))))

(defun reading (name)
  "What one instrument has to say, in microseconds, as a plist. NIL where it
has never been touched, which reads as `nothing walked this path' rather than
as a zero."
  (let ((box (d:at (d:all *instruments*) name)))
    (when box
      (let* ((it (d:held box))
             (ring (instrument-ring it))
             (seconds (max 0.001 (/ (- (now) (instrument-at it)) 1000000000.0))))
        (list :name name
              :kind (instrument-kind it)
              :count (if (eq :count (instrument-kind it))
                         (instrument-total it)
                         (instrument-count it))
              :per-second (/ (if (eq :count (instrument-kind it))
                                 (instrument-total it)
                                 (instrument-count it))
                             seconds)
              :mean (if (plusp (instrument-count it))
                        (round (instrument-total it) (instrument-count it))
                        0)
              :p50 (%percentile ring 0.50)
              :p95 (%percentile ring 0.95)
              :least (instrument-least it)
              :most (instrument-most it)
              :last (instrument-last it)
              :total (instrument-total it)
              :seconds seconds)))))

(defun said ()
  "Every instrument, in one shape. The synthetic runs and the live daemon both
answer this, which is what lets one be laid beside the other."
  (remove nil (mapcar #'reading (instruments))))

(defun reset (&optional name)
  (if name
      (d:drop! *instruments* name)
      (d:clear! *instruments*))
  t)

(defun %ms (nanoseconds) (if nanoseconds (/ nanoseconds 1000000.0) 0))

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
                (%ms (getf row :mean)) (%ms (getf row :p95))
                (%ms (getf row :most))))))
