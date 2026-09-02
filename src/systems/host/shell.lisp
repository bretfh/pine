(defpackage #:pine/host/shell
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:meter #:pine/run/meter)
                    (#:actors #:pine/run/actors) (#:job #:pine/run/job)
                    (#:fault #:pine/run/fault))
  (:export
   #:sh #:did #:argv #:feed #:lines #:words #:number-in
   #:firstp #:has #:run-line #:launch #:streaming #:last-said
   #:sh-node #:forget-all #:*breath*))
(in-package #:pine/host/shell)

(defvar *ran* nil)
(defvar *said* nil
  "What each of the last few lines said when it was last run, newest first. A read
of /sh/<line> answers out of this, because reading is not running.")
(defvar *asked* (d:table))
(defvar *streams* (d:table))
(defvar *sh* nil)
(defparameter *breath* 1/4
  "Seconds an answer stands for. What a bar reads is read again next frame, not
three times in this one.")
(defparameter *kept* 100)
(defparameter *lines-kept* 20)
(defparameter *asked-kept* 256
  "How many answers stand at once. One is good for a breath and after that is only
taking up room, and the table is keyed by the line: without a cap a bar that asks
about a window holds an answer for every window there has ever been.")

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

(defclass stream-node (node:live)
  ((line :initarg :line :reader line)
   (took :initform nil :accessor took)
   (said :initform nil :reader said)))

(defun ran () *ran*)

(defun %noted (line)
  (d:swap *ran* #'d:capped line *kept*)
  (when *sh* (node:moved *sh*))
  line)

(defun %kept (line out)
  "Keep what a line said, so /sh can answer for it without running it again, and
say the place moved. What a line last said is not a node, so nothing else can see
it change."
  (d:swap *said*
          (lambda (all)
            (d:capped (cl:remove line all :key #'car :test #'equal)
                      (cons line out) *kept*)))
  (when *sh*
    (let ((n (d:lookup (d:all (node:memo *sh*)) line)))
      (when n (node:moved n))))
  out)

(defun last-said (line) (cdr (assoc line *said* :test #'equal)))

(defun %output (line)
  (multiple-value-bind (out err code)
      (uiop:run-program (list "sh" "-c" line)
                        :output '(:string :stripped t)
                        :error-output nil :ignore-error-status t)
    (declare (ignore err code))
    (%kept line out)))

(defun %breathed () (* *breath* internal-time-units-per-second))

(defun %forget-stale (now)
  "Let go of the answers whose breath has passed. Done when the table has grown
rather than on every ask, so a line that is asked about every frame costs a lookup
and nothing else."
  (let ((old (%breathed)))
    (d:do-map (line had (d:all *asked*))
      (when (> (- now (cdr had)) old) (d:drop! *asked* line)))))

(defun asked (line)
  "What a line says, remembered for a breath, so a panel reading three things out of
one command runs it once and a bar built twice in a frame does not fork twice."
  (let ((now (get-internal-real-time))
        (had (d:lookup (d:all *asked*) line)))
    (cond ((and had (< (- now (cdr had)) (%breathed)))
           (car had))
          (t (when (> (d:size (d:all *asked*)) *asked-kept*)
               (%forget-stale now))
             (meter:counted :sh-fork)
             (let ((said (%output line)))
               (d:keep! *asked* line (cons said now))
               said)))))

(defun sh (format &rest arguments)
  "Ask the machine something and answer what it said.

A question, and remembered as one: two things reading /sys/cpu a moment apart are
asking about the same moment. Telling the machine to do something is DID or ARGV,
which are not."
  (meter:timing (:sh) (asked (apply #'format nil format arguments))))

(defun did (format &rest arguments)
  "Tell the machine to do something, through a shell, and answer what it said.

Not remembered. An answer stands for a breath because a question asked twice in
one frame has one answer; a thing done twice is done twice, and routing a write
through the memo made muting twice inside a quarter of a second mute once."
  (meter:counted :sh-fork)
  (meter:timing (:sh) (%output (apply #'format nil format arguments))))

(defun argv (&rest words)
  "Run a program with these arguments and answer what it said. No shell.

What a word says is what the program is given, so a value that came out of a
config or off a socket is an argument and can never be a line of shell. Written
into one it could say anything: a sink named `x; rm -rf ~' is a name a device
takes, and quoting it is not an answer -- a double-quoted shell word still spells
$(...).

Not remembered either, for DID's reason. WITH a pipe or a redirect in it, a line
is a shell line and SH is the one that runs it."
  (meter:counted :sh-fork)
  (meter:timing (:sh)
    (multiple-value-bind (out err code)
        (uiop:run-program (mapcar #'princ-to-string (remove nil words))
                          :output '(:string :stripped t)
                          :error-output nil :ignore-error-status t)
      (declare (ignore err code))
      out)))

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
        (fault:or-nothing "what a program printed may not be a form"
          (read-from-string (subseq text start end)))))))

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
               :do (d:swap (slot-value n 'said) #'d:capped said *lines-kept*)
                   (node:moved n))))))
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
  "A command whose output says the world moved, listened to for as long as pine
runs."
  (when *sh*
    (let ((n (node:child *sh* (format nil "stream:~a" line)
                         (lambda ()
                           (make-instance 'stream-node :name line :parent *sh*
                                                       :line line)))))
      (d:keep! *streams* line n)
      (hear n))))

(defmethod node:contents ((n stream-node)) (first (said n)))

(defmethod (setf node:contents) (value (n stream-node))
  (if value (hear n) (quiet n))
  value)

(defun %line (line)
  "What this line last said, and a place to tell it to run.

Reading is not running. Every line is a place whether or not one has ever been
run, and a read used to run it -- so /sh was a shell anything that could reach the
namespace could type into, by asking it a question. A read is the one thing every
way in may always do; running something is a write, and this is where it is said.

One that has not run answers nothing. That is what ABSENT is for."
  (make-instance 'node:derived :name line
               :reads (lambda () (last-said line))
               :writes (lambda (v) (declare (ignore v)) (run-line line))))

(defun sh-node ()
  "Every shell line that has been run, and what it said."
  (setf *sh* (make-instance 'node:place :name "sh"
                         :names #'ran :each #'%line :reads #'ran
                         :describes "running something, and what it said")))

(defun forget-all ()
  (dolist (n (d:vals (d:all *streams*)) t) (quiet n))
  (d:clear! *streams*))
