(defpackage #:pine/host/shell
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs) (#:meter #:pine/run/meter)
                    (#:actors #:pine/run/actors) (#:job #:pine/run/job)
                    (#:fault #:pine/run/fault))
  (:export
   #:sh #:did #:argv #:feed #:lines #:words #:number-in
   #:first-line #:has #:run-line #:launch #:streaming #:last-said
   #:sh-node #:forget-all #:*breath-seconds*))
(in-package #:pine/host/shell)

(defvar *sh* nil)
(defparameter *breath-seconds* 1/4)
(defparameter *ran-kept* 100)
(defparameter *lines-kept* 20)
(defparameter *asked-kept* 256)

(defparameter *out*
  '("GUIX_ENVIRONMENT" "CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"
    "LD_LIBRARY_PATH"))

(defparameter +tethered+
  "~a & pine_child=$!; trap 'kill $pine_child 2>/dev/null' EXIT; ~
   cat >/dev/null; kill $pine_child 2>/dev/null")

(defclass shell (fs:mount)
  ((ran     :initform nil :accessor ran-of)
   (said    :initform nil :accessor said-of)
   (asked   :initform (d:no-map) :accessor asked-of)
   (streams :initform nil :accessor streams)))

(defclass stream-node (fs:derived)
  ((line :initarg :line :reader line)
   (took :initform nil :accessor took)
   (said :initform nil :reader said)))

(defun ran () (ran-of *sh*))

(defun %noted (line)
  (sb-ext:atomic-update (slot-value *sh* 'ran) (lambda (old) (d:capped old line *ran-kept*)))
  (fs:touch *sh*)
  line)

(defun %kept (line out)
  (sb-ext:atomic-update (slot-value *sh* 'said)
          (lambda (all)
            (d:capped (cl:remove line all :key #'car :test #'equal)
                      (cons line out) *ran-kept*)))
  (let ((n (d:lookup (fs::dentries *sh*) line)))
    (when n (fs:touch n)))
  out)

(defun last-said (line) (cdr (assoc line (said-of *sh*) :test #'equal)))

(defun %output (line)
  (multiple-value-bind (out err code)
      (uiop:run-program (list "sh" "-c" line)
                        :output '(:string :stripped t)
                        :error-output nil :ignore-error-status t)
    (declare (ignore err code))
    (%kept line out)))

(defun %breathed () (* *breath-seconds* internal-time-units-per-second))

(defun %forget-stale (now)
  (let ((old (%breathed)))
    (d:do-map (line had (asked-of *sh*))
      (when (> (- now (cdr had)) old)
        (sb-ext:atomic-update (slot-value *sh* 'asked) (lambda (old) (d:without old line)))))))

(defun asked (line)
  (let* ((now (get-internal-real-time))
         (had (d:lookup (asked-of *sh*) line)))
    (cond ((and had (< (- now (cdr had)) (%breathed)))
           (car had))
          (t (when (> (d:size (asked-of *sh*)) *asked-kept*)
               (%forget-stale now))
             (meter:counted :sh-fork)
             (let ((said (%output line)))
               (sb-ext:atomic-update (slot-value *sh* 'asked) (lambda (old) (d:with old line (cons said now))))
               said)))))

(defun sh (format &rest arguments)
  (meter:timing (:sh) (asked (apply #'format nil format arguments))))

(defun did (format &rest arguments)
  (meter:counted :sh-fork)
  (meter:timing (:sh) (%output (apply #'format nil format arguments))))

(defun argv (&rest words)
  (meter:counted :sh-fork)
  (meter:timing (:sh)
    (multiple-value-bind (out err code)
        (uiop:run-program (mapcar #'princ-to-string (remove nil words))
                          :output '(:string :stripped t)
                          :error-output nil :ignore-error-status t)
      (declare (ignore err code))
      out)))

(defun feed (line text)
  (meter:counted :sh-fork)
  (with-input-from-string (in (princ-to-string text))
    (uiop:run-program (list "sh" "-c" line) :input in :output nil
                                            :error-output nil
                                            :ignore-error-status t))
  t)

(defun lines (text)
  (remove "" (uiop:split-string (or text "") :separator '(#\Newline))
          :test #'string=))

(defun words (text &optional (on #\Space))
  (remove "" (uiop:split-string (or text "") :separator (list on))
          :test #'string=))

(defun number-in (text)
  (let* ((text (or text ""))
         (start (position-if (lambda (c) (or (digit-char-p c) (char= c #\-))) text)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                      text :start (1+ start))
                     (length text))))
        (fault:or-nothing "what a program printed may not be a form"
          (read-from-string (subseq text start end)))))))

(defun first-line (text) (first (lines text)))

(defun has (command) (plusp (length (sh "command -v ~a 2>/dev/null" command))))

(defun %environment ()
  (remove-if (lambda (entry)
               (some (lambda (name)
                       (let ((prefix (concatenate 'string name "=")))
                         (and (>= (length entry) (length prefix))
                              (string= prefix entry :end2 (length prefix)))))
                     *out*))
             (sb-ext:posix-environ)))

(defun launch (argv)
  (uiop:launch-program argv :environment (%environment)
                            :directory (user-homedir-pathname)
                            :output nil :error-output nil))

(defun run-line (line)
  (%noted line)
  (launch (list "sh" "-l" "-c" (concatenate 'string "exec " line)))
  t)

(defun hearing (n) (and (took n) t))

(defun hear (n)
  (unless (hearing n)
    (let ((it (uiop:launch-program
               (list "sh" "-c" (format nil +tethered+ (line n)))
               :input :stream :output :stream :error-output nil)))
      (setf (took n) it)
      (actors:blocking
       (format nil "sh ~a" (line n))
       (lambda ()
         (loop :with out := (uiop:process-info-output it)
               :for said := (handler-case (read-line out nil nil)
                              (stream-error () nil))
               :while said
               :do (sb-ext:atomic-update (slot-value n 'said) (lambda (old) (d:capped old said *lines-kept*)))
                   (fs:touch n))))))
  n)

(defun quiet (n)
  (let ((it (took n)))
    (when it
      (fault:or-nothing "a stream to a program that has gone is closed already"
        (close (uiop:process-info-input it)))
      (fault:or-nothing "a program that ended cannot be ended again"
        (uiop:terminate-process it :urgent t))
      (fault:or-nothing "one already reaped has no status left to take"
        (uiop:wait-process it))
      (setf (took n) nil)))
  n)

(defun streaming (line)
  (let ((n (fs:ensure-child *sh* (format nil "stream:~a" line)
                     (lambda ()
                       (make-instance 'stream-node :name line :parent *sh*
                                                   :line line)))))
    (pushnew n (streams *sh*))
    (hear n)))

(defmethod fs:volatile-p ((n stream-node) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n stream-node)) (first (said n)))

(defmethod fs:takes ((n stream-node) value)
  (if value (hear n) (quiet n))
  value)

(defclass spoken (fs:derived) ())

(defmethod fs:works ((n spoken)) (last-said (fs:name n)))

(defmethod fs:takes ((n spoken) value)
  (declare (ignore value))
  (run-line (fs:name n)))

(defmethod fs:volatile-p ((s shell) &optional name) (declare (ignore name)) t)

(defmethod fs:child ((s shell) name)
  (let ((name (princ-to-string name)))
    (fs:ensure-child s name (lambda () (make-instance 'spoken :name name :parent s)))))

(defmethod fs:children ((s shell))
  (mapcar (lambda (line) (fs:child s line)) (ran-of s)))

(defun %shell ()
  (make-instance 'shell :name "sh" :describes "running something, and what it said"))

(defun sh-node ()
  *sh*)

(defun forget-all ()
  (dolist (n (streams *sh*) t) (quiet n))
  (setf (streams *sh*) nil))

(setf *sh* (%shell))
