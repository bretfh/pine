(defpackage #:pine/run/meter
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node))
  (:export #:timing #:counted #:said #:reading #:reset #:instruments #:report
           #:attach #:now #:*on* #:*kept*))
(in-package #:pine/run/meter)

(defvar *on* t
  "Whether a sample is taken. On from boot: a number about the daemon you are
using cannot be had by turning something on afterwards and doing it again.")

(defparameter *kept* 256
  "How many samples an instrument keeps. Enough for a p95 that means something,
small enough that a hundred instruments cost nothing to hold.")

(defvar *instruments* (d:table))

(defparameter +fields+ '("count" "per-second" "mean" "p50" "p95" "worst" "last"
                         "total" "seconds")
  "What an instrument answers for, as paths. Milliseconds where it is a duration,
because that is what a person reads a frame in.")

(defstruct (instrument (:constructor %made (name kind)))
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
               (let ((it (%made name kind)))
                 (setf (instrument-at it) (now))
                 it))))

(defun %record (name kind measure)
  "One sample, into the instrument's own slots. Each is replaced where it stands
rather than the whole thing being copied, which is what a sample costing under a
microsecond is for."
  (let ((it (%of name kind)))
    (d:swap (instrument-count it) #'1+)
    (d:swap (instrument-total it) #'+ measure)
    (d:swap (instrument-most it) #'max measure)
    (d:swap (instrument-least it)
            (lambda (had) (if had (min had measure) measure)))
    (d:swap (instrument-ring it) #'d:capped measure *kept*)
    (setf (instrument-last it) measure))
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
      (let* ((it box)
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

(defun %ms (nanoseconds) (and nanoseconds (/ (round nanoseconds 1000) 1000.0)))

(defun %named (name)
  (find (princ-to-string name) (instruments)
        :key (lambda (each) (string-downcase (princ-to-string each)))
        :test #'equal))

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

(defun %instrument (name)
  (when (%named name)
    (node:place name
                :names (constantly +fields+)
                :each (lambda (field)
                        (when (member field +fields+ :test #'equal)
                          (node:place field
                                      :reads (lambda ()
                                               (let ((key (%named name)))
                                                 (when key
                                                   (%field (reading key) field)))))))
                :reads (lambda ()
                         (let ((key (%named name))) (when key (reading key)))))))

(defun attach (root)
  (node:attach
   (node:place "metric"
               :names (lambda ()
                        (mapcar (lambda (each)
                                  (string-downcase (princ-to-string each)))
                                (instruments)))
               :each #'%instrument
               :reads (lambda () (mapcar (lambda (row) (getf row :name)) (said)))
               :describes "how long what pine does is taking")
   root))

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
