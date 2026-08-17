(defpackage #:pine/host/shell
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:meter #:pine/run/meter)
                    (#:actors #:pine/run/actors) (#:job #:pine/run/job))
  (:export #:sh #:feed #:lines #:words #:number-in #:firstp #:has #:asked #:ran
           #:run-line #:launch #:streaming #:hear #:quiet #:hearing
           #:stream-node #:attach #:forget-all #:*breath* #:*out*))
(in-package #:pine/host/shell)

(defvar *ran* (d:box (d:no-seq)))
(defvar *asked* (d:table))
(defvar *streams* (d:table))
(defvar *sh* nil)
(defparameter *breath* 1/4
  "Seconds an answer stands for. What a bar reads is read again next frame, not
three times in this one.")
(defparameter *kept* 100)
(defparameter *lines-kept* 20)

(defparameter *out*
  '("GUIX_ENVIRONMENT" "CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"
    "LD_LIBRARY_PATH")
  "What pine's own build put in the environment and a program it launches should
not inherit.")

(defparameter +tethered+
  "~a & pine_child=$!; trap 'kill $pine_child 2>/dev/null' EXIT; ~
   cat >/dev/null; kill $pine_child 2>/dev/null"
  "A stream, tied to the image that asked for it. The shell holding it reads a pipe
pine keeps the other end of, so pine going -- stopped, crashed or killed outright --
closes that end and the stream goes with it.")

(defclass stream-node (node:node)
  ((line :initarg :line :reader line)
   (took :initform (d:box nil) :reader took)
   (said :initform (d:box nil) :reader said)
   (livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defclass shell-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defun ran () (d:as :list (d:held *ran*)))

(defun %noted (line)
  (d:swap! *ran* (lambda (all)
                   (let ((next (d:insert-at all 0 line)))
                     (if (> (d:size next) *kept*) (d:subseq next 0 *kept*) next))))
  line)

(defun %output (line)
  (multiple-value-bind (out err code)
      (uiop:run-program (list "sh" "-c" line)
                        :output '(:string :stripped t)
                        :error-output nil :ignore-error-status t)
    (declare (ignore err code))
    out))

(defun asked (line)
  "What a line says, remembered for a breath, so a panel reading three things out of
one command runs it once and a bar built twice in a frame does not fork twice."
  (let ((now (get-internal-real-time))
        (had (d:at (d:all *asked*) line)))
    (cond ((and had (< (- now (cdr had))
                       (* *breath* internal-time-units-per-second)))
           (car had))
          (t (meter:counted :sh-fork)
             (let ((said (%output line)))
               (d:keep! *asked* line (cons said now))
               said)))))

(defun sh (format &rest arguments)
  "Ask the machine something and answer what it said."
  (meter:timing (:sh) (asked (apply #'format nil format arguments))))

(defun feed (line text)
  "Give a program TEXT on its standard input. Not remembered: this is telling the
machine something, and telling it twice is twice."
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
        (ignore-errors (read-from-string (subseq text start end)))))))

(defun firstp (text) (first (lines text)))

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
  "Run somebody's own program. This is what /sh remembers; asking a question is not."
  (%noted line)
  (launch (list "sh" "-l" "-c" (concatenate 'string "exec " line)))
  t)

(defun hearing (n) (and (d:held (took n)) t))

(defun hear (n)
  (unless (hearing n)
    (let ((it (uiop:launch-program
               (list "sh" "-c" (format nil +tethered+ (line n)))
               :input :stream :output :stream :error-output nil)))
      (d:put! (took n) it)
      (actors:blocking
       (format nil "sh ~a" (line n))
       (lambda ()
         (loop :with out := (uiop:process-info-output it)
               :for said := (handler-case (read-line out nil nil)
                              (stream-error () nil))
               :while said
               :do (d:swap! (said n)
                            (lambda (all)
                              (let ((next (cons said all)))
                                (if (> (length next) *lines-kept*)
                                    (subseq next 0 *lines-kept*)
                                    next))))
                   (node:stir n))))))
  n)

(defun quiet (n)
  (let ((it (d:held (took n))))
    (when it
      (ignore-errors (close (uiop:process-info-input it)))
      (ignore-errors (uiop:terminate-process it :urgent t))
      (ignore-errors (uiop:wait-process it))
      (d:put! (took n) nil)))
  n)

(defun streaming (line)
  "A command whose output says the world moved, listened to for as long as pine
runs."
  (when *sh*
    (let ((n (node:child *sh* (format nil "stream:~a" line)
                         (lambda ()
                           (make-instance 'stream-node :name line :over *sh*
                                                       :line line)))))
      (d:keep! *streams* line n)
      (hear n))))

(defmethod node:contents ((n stream-node)) (first (d:held (said n))))

(defmethod (setf node:contents) (value (n stream-node))
  (if value (hear n) (quiet n))
  value)

(defmethod node:contents ((n shell-node)) (ran))

(defmethod node:nodes ((n shell-node))
  (loop :for line :in (ran)
        :collect (node:child n line
                             (lambda ()
                               (node:derive line
                                            (lambda () (asked line))
                                            :over n
                                            :writes (lambda (v)
                                                      (declare (ignore v))
                                                      (run-line line)))))))

(defmethod node:resolve ((n shell-node) name)
  "Reading a line runs it and answers what it said; writing one launches it. Every
line is a place, whether or not anything has asked for it before."
  (let ((line (princ-to-string name)))
    (node:child n line
                (lambda ()
                  (node:derive line
                               (lambda () (asked line))
                               :over n
                               :writes (lambda (v)
                                         (declare (ignore v))
                                         (run-line line)))))))

(defun attach (root)
  (setf *sh* (node:attach (make-instance 'shell-node :name "sh"
                                         :describes "running something, and what it said")
                          root)))

(defun forget-all ()
  (dolist (n (d:vals (d:all *streams*)) t) (quiet n))
  (d:clear! *streams*))
