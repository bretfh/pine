(in-package #:pine.source)

;;;; Data sources feed reactive refs; widgets read those refs and re-render on
;;;; change. A source is either a STREAM (a subprocess line stream that triggers
;;;; a refresh, event-driven) or a POLL (a refresh on an interval). Either way
;;;; the actual work (shell reads + ref writes) runs on a source actor owned by
;;;; the actor system, off both the reader thread and the timer thread. Sources
;;;; are declared with defsource / defpoll and started together by start-sources.
;;;;
;;;; This file is the mechanism and none of the policy. pine ships no sources:
;;;; which compositor reports workspaces, which mixer reports volume, which
;;;; player reports what is playing are the config's to declare, in whatever
;;;; commands the machine actually runs. What is here is the supervised
;;;; subprocess, the interval, the fault cap, and the macros to declare one --
;;;; plus the few string and file helpers such a declaration needs.

(defun ref-of (name)
  (or (pine.state.ref:find-ref name) (pine.state.ref:make-ref :name name)))

;;; shell helpers

(defun sh (&rest args)
  (or (ignore-errors
        (string-trim '(#\newline #\space)
                     (uiop:run-program args :output :string :ignore-error-status t)))
      ""))

(defun first-number (s)
  (let ((start (position-if (lambda (c) (or (digit-char-p c) (char= c #\.))) s)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                       s :start start)
                     (length s))))
        (ignore-errors (read-from-string (subseq s start end)))))))

(defun read-int-file (path)
  (ignore-errors (parse-integer (sh "cat" (namestring path)))))

;;; source primitives

(defstruct source name actor process super (stopped nil) (faults 0) (last-fault nil)
                (wake (sb-thread:make-semaphore)))

(defparameter *source-fault-cap* 6
  "Consecutive faults before a source is disabled instead of retried, so a broken
source backs off and stops rather than spinning or swallowing errors silently.")

(defun %source-ok (src)
  "A refresh succeeded: clear the consecutive-fault counter."
  (setf (source-faults src) 0))

(defun %source-fault (src condition)
  "Record a refresh fault on SRC. Disable the source after *source-fault-cap*
consecutive faults. Returns the seconds to back off before the next attempt
(exponential, capped)."
  (incf (source-faults src))
  (setf (source-last-fault src) (princ-to-string condition))
  (format *error-output* "~&pine source ~a fault ~d: ~a~%"
          (or (source-name src) "?") (source-faults src) condition)
  (when (>= (source-faults src) *source-fault-cap*)
    (setf (source-stopped src) t)
    (format *error-output* "~&pine source ~a disabled after ~d faults~%"
            (or (source-name src) "?") (source-faults src)))
  (min 60 (* 2 (expt 2 (min 5 (source-faults src))))))

(defvar *running* nil "Started sources, or nil.")
(defun source-status ()
  "Every started source as (name faults last-fault stopped) -- so a broken feed
is visible, not silently dead."
  (loop for s in *running*
        collect (list (source-name s) (source-faults s)
                      (source-last-fault s) (source-stopped s))))

(defvar *actor-counter* 0
  "Serial for unique source-actor names; sento rejects a duplicate name, which
would silently drop every source after the first.")

(defun %actor (system)
  "A lightweight identity actor for a source, so it is addressable in the service
registry. The source's blocking IO does NOT run here -- it runs on the source's
own dedicated thread, never on this actor's shared pool worker."
  (sento.actor-context:actor-of system
    :name (format nil "pine-source-~d" (incf *actor-counter*))
    :receive (lambda (msg) (declare (ignore msg)) nil)))

(defparameter *stream-backoff* 2
  "Seconds a supervised stream waits before relaunching a died subprocess.")

(defun start-stream (system command refresh-fn &optional (trigger (constantly t)))
  "Spawn COMMAND under supervision on the source's OWN dedicated thread; when a
stdout line satisfies TRIGGER, run REFRESH-FN (which sets cells) on that same
thread. The blocking subprocess IO never touches the shared pool, so a slow or
flooding source cannot starve the daemon. The supervisor relaunches the
subprocess if it dies (EOF/error) after a backoff. stop-sources sets the stopped
flag and terminates the process so the read hits EOF and the thread exits
cleanly. No thread is ever killed."
  (let ((src (make-source :actor (%actor system))))
    (setf (source-super src)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop until (source-stopped src) do
               (let ((wait *stream-backoff*))
                 ;; a launch failure or a refresh fault is recorded and backed
                 ;; off (and disables the source after the cap); a clean EOF is
                 ;; not a fault, just a relaunch. no silent (error () nil).
                 (handler-case
                     (let ((proc (uiop:launch-program (list "sh" "-c" command) :output :stream)))
                       (setf (source-process src) proc)
                       (funcall refresh-fn) (%source-ok src)   ; initial read
                       (loop with out = (uiop:process-info-output proc)
                             for line = (read-line out nil nil)
                             while line
                             when (funcall trigger line)
                               do (funcall refresh-fn) (%source-ok src)))
                   (error (e) (setf wait (%source-fault src e))))
                 (unless (source-stopped src) (sleep wait)))))
           :name "pine-source-stream"))
    src))

(defun start-poll (system interval refresh-fn)
  "Run REFRESH-FN every INTERVAL seconds on the source's OWN dedicated thread, so
its blocking IO never touches the shared pool. Waits out the interval on its
wake semaphore, which `stop-sources' signals, so a stop takes effect at once
without the thread waking to ask whether it should."
  (let ((src (make-source :actor (%actor system))))
    (setf (source-super src)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop :until (source-stopped src)
                   ;; wait the interval on success, the backoff (growing, then
                   ;; disable) on a fault; the fault is recorded, never swallowed.
                   :do (let ((wait (handler-case
                                       (progn (funcall refresh-fn)
                                              (%source-ok src)
                                              (max 1 interval))
                                     (error (e) (%source-fault src e)))))
                         (sb-thread:wait-on-semaphore (source-wake src)
                                                      :timeout wait))))
           :name "pine-source-poll"))
    src))

;;; source registry + declarative definition

(defvar *source-defs* (make-hash-table :test 'eq)
  "name -> starter function of the actor system.")

(defmacro defsource (name (system) &body body)
  "Define a source NAME. BODY, with SYSTEM bound to the actor system, starts it
and returns a source (usually via start-stream / start-poll)."
  `(setf (gethash ',name *source-defs*) (lambda (,system) ,@body)))

(defmacro defpoll (name interval &body body)
  "Reactive ref NAME refreshed to (progn BODY) every INTERVAL seconds."
  `(defsource ,name (system)
     (let ((c (ref-of ',name)))
       (start-poll system ,interval
                   (lambda () (pine.state.ref:set-ref c (progn ,@body)))))))

(defmacro deflisten (name command (line) &body body)
  "Reactive ref NAME fed from COMMAND's stdout: each LINE sets it to (progn
BODY) when that is non-nil."
  (let ((val (gensym)))
    `(defsource ,name (system)
       (let ((c (ref-of ',name)))
         (start-stream system ,command
           (constantly nil)
           (lambda (,line)
             (let ((,val (progn ,@body)))
               (when ,val (pine.state.ref:set-ref c ,val)) nil)))))))

(defun start-sources (server)
  "Start every declared source once and register it as an adapter in the service
registry (pine.core.actor), named source:<name>, so it is addressable and listable
alongside the other agents. A source is the desktop's data adapter."
  (unless *running*
    (let ((system (pine.core.server:actor-system server)))
      (setf *running*
            (loop for name being the hash-keys of *source-defs* using (hash-value starter)
                  for s = (pine.core.eval:attempt (lambda () (funcall starter system))
                                       (format nil "source ~a" name))
                  when s
                    do (setf (source-name s) name)
                       (pine.core.eval:attempt
                        (lambda ()
                          (pine.core.actor:register-agent
                           server (format nil "source:~(~a~)" name)
                           :source (source-actor s) :meta (list :ref name)))
                        (format nil "registering source ~a" name))
                    and collect s)))))

(defun stop-sources ()
  "Stop each source: flag its supervisor to exit, then terminate its subprocess
so the read hits EOF and the supervisor loop ends. Actors and timers die with
the actor system. No thread is killed."
  (dolist (s *running*)
    (setf (source-stopped s) t)
    (sb-thread:signal-semaphore (source-wake s))
    (when (source-process s)
      (ignore-errors (uiop:terminate-process (source-process s) :urgent t))))
  (setf *running* nil))

;;; string helpers, for the sources a config declares

(defun split (s ch &optional limit)
  "Split S on CH into at most LIMIT parts (the last keeps any remaining CH)."
  (let ((parts nil) (start 0) (n 0))
    (dotimes (i (length s))
      (when (and (char= (char s i) ch) (or (null limit) (< (1+ n) limit)))
        (push (subseq s start i) parts) (setf start (1+ i)) (incf n)))
    (push (subseq s start) parts)
    (nreverse parts)))

(defun lines (s)
  "S split into non-empty lines."
  (remove "" (split s #\newline) :test #'string=))

(defun starts-with (s prefix)
  (and (>= (length s) (length prefix)) (string= s prefix :end1 (length prefix))))

(defun json (s)
  "Parse S as JSON into a hash table, or nil when it is not JSON."
  (ignore-errors (com.inuoe.jzon:parse s)))
