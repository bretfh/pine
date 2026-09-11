(defpackage #:pine/run/meter
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:export
   #:timing #:counted #:readings #:reset #:report
   #:now))
(in-package #:pine/run/meter)

(defvar *on* t)

(defparameter *samples-kept* 256)

(defclass instrument (fs:mount)
  ((kind  :initarg :kind :reader kind-of)
   (count :initform 0   :reader count-of)
   (total :initform 0   :reader total-of)
   (least :initform nil :reader least-of)
   (most  :initform 0   :reader most-of)
   (last  :initform 0   :accessor last-of)
   (ring  :initform nil :reader ring-of)
   (at    :initform 0   :accessor at-of)))

(defun now ()
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defmethod initialize-instance :after ((it instrument) &key)
  (setf (at-of it) (now)))

(defmethod fs:names ((it instrument))
  '((:count      . "how many samples, or what they counted")
    (:per-second . "how often")
    (:mean       . "milliseconds, where it is a duration")
    (:p50        . "the median")
    (:p95        . "the tail")
    (:worst      . "the largest sample")
    (:last       . "the most recent")
    (:total      . "every sample added up")
    (:seconds    . "how long it has been counting")))

(macrolet ((field (name)
             `(defmethod fs:read ((it instrument) (name (eql ,name)))
                (%field (reading it) ,(string-downcase (symbol-name name))))))
  (field :count) (field :per-second) (field :mean) (field :p50) (field :p95)
  (field :worst) (field :last) (field :total) (field :seconds))

(defun %metric () (fs:at "/metric"))

(defun %of (name kind)
  (let* ((d (%metric))
         (name (string-downcase (princ-to-string name)))
         (it (fs:ensure-child d name
                       (lambda ()
                         (make-instance 'instrument :name name :kind kind :parent d)))))
    (unless (eq (fs:child d name) it) (fs:mount it d))
    it))

(defun %record (name kind measure)
  (let ((it (%of name kind)))
    (sb-ext:atomic-update (slot-value it 'count) (lambda (old) (1+ old)))
    (sb-ext:atomic-update (slot-value it 'total) (lambda (old) (+ old measure)))
    (sb-ext:atomic-update (slot-value it 'most) (lambda (old) (max old measure)))
    (sb-ext:atomic-update (slot-value it 'least)
            (lambda (had) (if had (min had measure) measure)))
    (sb-ext:atomic-update (slot-value it 'ring) (lambda (old) (d:capped old measure *samples-kept*)))
    (setf (last-of it) measure))
  measure)

(defmacro timing ((name) &body body)
  (let ((start (gensym "START")) (answer (gensym "ANSWER")))
    `(if *on*
         (let* ((,start (now))
                (,answer (multiple-value-list (progn ,@body))))
           (%record ,name :time (- (now) ,start))
           (values-list ,answer))
         (progn ,@body))))

(defun counted (name &optional (by 1))
  (when *on* (%record name :count by))
  by)

(defun instruments ()
  (sort (remove-if-not (lambda (each) (typep each 'instrument))
                       (fs:children (%metric)))
        #'string< :key #'fs:name))

(defun %percentile (ring share)
  (when ring
    (let ((sorted (sort (copy-list ring) #'<)))
      (nth (min (1- (length sorted))
                (floor (* share (length sorted))))
           sorted))))

(defun reading (it)
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
  (mapcar #'reading (instruments)))

(defun reset (&optional name)
  (let ((d (%metric)))
    (if name
        (fs:unlink d (string-downcase (princ-to-string name)))
        (dolist (each (instruments)) (fs:unlink d (fs:name each)))))
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

(fs:mount (lambda () (make-instance 'fs:mount :describes "how long what pine does is taking"))
          "/metric")

(defun report (rows &key (to *standard-output*) about)
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
