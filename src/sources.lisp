(defpackage #:pine.source
  (:use #:cl)
  (:export #:start-sources #:stop-sources #:workspaces
           #:defsource #:defpoll #:set! #:cell-of))

(in-package #:pine.source)

;;;; Data sources feed reactive cells; widgets read those cells and re-render on
;;;; change. A source is either a STREAM (a subprocess line stream that triggers
;;;; a refresh, event-driven) or a POLL (a refresh on an interval). Either way
;;;; the actual work (shell reads + set-cell) runs on a source actor owned by
;;;; the actor system, off both the reader thread and the timer thread. Sources
;;;; are declared with defsource / defpoll and started together by start-sources.
;;;; This is the runtime behind the widgets' cell reads -- pine's eww broker.

(defun cell-of (name)
  (or (pine.cell:find-cell name) (pine.cell:make-cell :name name)))

(defun set! (name value)
  "Set reactive cell NAME to VALUE (deduped by the cell)."
  (pine.cell:set-cell (cell-of name) value))

;;; shell helpers

(defun sh (&rest args)
  (or (ignore-errors
        (string-trim '(#\newline #\space)
                     (uiop:run-program args :output :string :ignore-error-status t)))
      ""))

(defun %first-number (s)
  (let ((start (position-if (lambda (c) (or (digit-char-p c) (char= c #\.))) s)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                       s :start start)
                     (length s))))
        (ignore-errors (read-from-string (subseq s start end)))))))

(defun read-int-file (path)
  (ignore-errors (parse-integer (sh "cat" (namestring path)))))

;;; source primitives

(defstruct source actor process)

(defun %actor (system refresh-fn)
  (sento.actor-context:actor-of system
    :name "pine-source"
    :receive (lambda (msg)
               (case (first msg)
                 ((:refresh :poll) (ignore-errors (funcall refresh-fn)))
                 (t nil)))))

(defun start-stream (system command refresh-fn &optional (trigger (constantly t)))
  "Spawn COMMAND; when a stdout line satisfies TRIGGER, run REFRESH-FN on the
source actor (REFRESH-FN sets cells). The reader thread only forwards events;
stopping terminates the process so it hits EOF and exits. No thread is killed."
  (let* ((actor (%actor system refresh-fn))
         (proc (uiop:launch-program (list "sh" "-c" command) :output :stream)))
    (sento.actor:tell actor '(:refresh))
    (bordeaux-threads:make-thread
     (lambda ()
       (handler-case
           (loop with out = (uiop:process-info-output proc)
                 for line = (read-line out nil nil)
                 while line
                 when (funcall trigger line) do (sento.actor:tell actor '(:refresh)))
         (error () nil)))
     :name "pine-source-reader")
    (make-source :actor actor :process proc)))

(defun start-poll (system interval refresh-fn)
  "Run REFRESH-FN every INTERVAL seconds on the source actor, driven by the
actor system's wheel-timer (the timer thread only tells the actor)."
  (let ((actor (%actor system refresh-fn)))
    (sento.actor:tell actor '(:poll))
    (sento.wheel-timer:schedule-recurring
     (sento.actor-system::scheduler system) interval interval
     (lambda () (sento.actor:tell actor '(:poll))))
    (make-source :actor actor)))

;;; source registry + declarative definition

(defvar *source-defs* (make-hash-table :test 'eq)
  "name -> starter function of the actor system.")
(defvar *running* nil)

(defmacro defsource (name (system) &body body)
  "Define a source NAME. BODY, with SYSTEM bound to the actor system, starts it
and returns a source (usually via start-stream / start-poll)."
  `(setf (gethash ',name *source-defs*) (lambda (,system) ,@body)))

(defmacro defpoll (name interval &body body)
  "Reactive cell NAME refreshed to (progn BODY) every INTERVAL seconds."
  `(defsource ,name (system)
     (let ((c (cell-of ',name)))
       (start-poll system ,interval
                   (lambda () (pine.cell:set-cell c (progn ,@body)))))))

(defmacro deflisten (name command (line) &body body)
  "Reactive cell NAME fed from COMMAND's stdout: each LINE sets it to (progn
BODY) when that is non-nil."
  (let ((val (gensym)))
    `(defsource ,name (system)
       (let ((c (cell-of ',name)))
         (start-stream system ,command
           (constantly nil)
           (lambda (,line)
             (let ((,val (progn ,@body)))
               (when ,val (pine.cell:set-cell c ,val)) nil)))))))

(defun start-sources (system)
  "Start every declared source once."
  (unless *running*
    (setf *running*
          (loop for starter being the hash-values of *source-defs*
                for s = (ignore-errors (funcall starter system))
                when s collect s))))

(defun stop-sources ()
  "Terminate each source's subprocess so its reader exits; actors and timers die
with the actor system."
  (dolist (s *running*)
    (when (source-process s)
      (ignore-errors (uiop:terminate-process (source-process s) :urgent t))))
  (setf *running* nil))

;;;; The built-in sources, declared with the sugar above.

(defun workspaces ()
  "niri workspaces as (:idx N :focused BOOL :urgent BOOL), sorted by idx."
  (sort (map 'list
             (lambda (w) (list :idx (gethash "idx" w)
                               :focused (gethash "is_focused" w)
                               :urgent (gethash "is_urgent" w)))
             (com.inuoe.jzon:parse (sh "niri" "msg" "--json" "workspaces")))
        #'< :key (lambda (p) (getf p :idx))))

(defsource :workspaces (system)
  (start-stream system "niri msg --json event-stream"
    (lambda () (set! :workspaces (workspaces)))
    (lambda (line) (search "Workspace" line))))

(defun audio-volume ()
  (let ((n (%first-number (sh "wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@"))))
    (if (numberp n) (round (* 100 n)) 0)))

(defun audio-muted ()
  (and (search "MUTED" (sh "wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@")) t))

(defsource :audio (system)
  (start-stream system "pactl subscribe"
    (lambda () (set! :vol (audio-volume)) (set! :muted (audio-muted)))
    (lambda (line) (search "sink" line))))

(defun brightness ()
  (let ((dir (first (ignore-errors (directory "/sys/class/backlight/*/")))))
    (if dir
        (let ((cur (read-int-file (merge-pathnames "brightness" dir)))
              (mx  (read-int-file (merge-pathnames "max_brightness" dir))))
          (if (and cur mx (plusp mx)) (round (* 100 cur) mx) 50))
        50)))

(defpoll :bri 15 (brightness))
