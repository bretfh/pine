(defpackage #:pine.proc
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:mount #:unmount #:tick #:*interval* #:*backoff-cap* #:*out-kept*))

(in-package #:pine.proc)
(named-readtables:in-readtable pine.path:syntax)

;;;; Everything running, whatever its blast radius, in one table.
;;;;
;;;; A declaration says what to run and where. Writing it is what keeps it
;;;; alive: pine starts what it was told to run, backs off when it keeps dying,
;;;; and stops it when the path is written nil. There is no supervisor to
;;;; declare, because a declared process is one that is kept running.
;;;;
;;;; The interval belongs to sento's wheel timer. A thread that sleeps in a
;;;; loop is a supervisor nobody asked for.

(defparameter *interval* 1
  "Seconds between passes over the table.")

(defparameter *backoff-cap* 60
  "The longest pine waits before trying something that keeps dying again.")

(defparameter *out-kept* 200
  "Lines of a process's output the table remembers.")

(defstruct (entry (:copier nil))
  name
  declaration
  (state :stopped)                  ; :running :stopped :failed
  process
  thread
  reader
  (attempts 0)
  (due 0)                           ; not before this universal time
  (wanted nil))                     ; whether it is meant to be up

(defvar *table* (make-hash-table :test 'equal))
(defvar *lock* (bordeaux-threads:make-recursive-lock "pine-proc"))
(defvar *timer* nil)
(defvar *timer-key* :pine-proc)
(defvar *image-argv* nil
  "Called with a name, answering the argv that starts an image of pine that
joins this daemon. Nothing here knows what that command looks like.")

(defun %entries ()
  (bordeaux-threads:with-recursive-lock-held (*lock*)
    (let (acc)
      (maphash (lambda (k v) (declare (ignore k)) (push v acc)) *table*)
      (sort acc #'string< :key #'entry-name))))

(defun %entry (name)
  (bordeaux-threads:with-recursive-lock-held (*lock*)
    (gethash name *table*)))

;;;; Is it up

(defun %alive-p (entry)
  (let ((process (entry-process entry))
        (thread (entry-thread entry)))
    (cond (process (uiop:process-alive-p process))
          (thread (bordeaux-threads:thread-alive-p thread))
          (t nil))))

(defun %pid (entry)
  (let ((process (entry-process entry)))
    (and process (uiop:process-info-pid process))))

;;;; Starting and stopping

(defun %needs-met-p (declaration)
  (let ((needs (fset:lookup declaration :needs)))
    (or (null needs)
        (every (lambda (path) (ns:read path))
               (if (fset:seq? needs) (fset:convert 'list needs) (list needs))))))

(defun %argv (value)
  (cond ((stringp value) (list "sh" "-c" value))
        ((fset:seq? value) (fset:convert 'list value))
        ((listp value) value)
        (t (error "~s is not a command." value))))

(defun %reader (entry stream)
  "Push STREAM's lines into the entry's output ring, on a thread of its own,
because a process that says nothing must not block one that does."
  (let ((where (p:path /proc (entry-name entry) "out")))
    (bordeaux-threads:make-thread
     (lambda ()
       ;; a closed stream is how this thread is told to stop, not a fault
       (loop :for line = (handler-case (read-line stream nil nil)
                           (stream-error () nil))
             :while line
             :do (ns:write where line :max *out-kept*)))
     :name (format nil "pine-proc-out-~a" (entry-name entry)))))

(defun %launch (entry argv)
  (let ((process (uiop:launch-program argv :output :stream
                                           :error-output :output)))
    (setf (entry-process entry) process
          (entry-reader entry) (%reader entry (uiop:process-info-output process)))
    process))

(defun %start (entry)
  "Start what ENTRY declares. Answers whether it came up."
  (let* ((declaration (entry-declaration entry))
         (image (fset:lookup declaration :image))
         (run (fset:lookup declaration :run))
         (thread (fset:lookup declaration :thread)))
    (setf (entry-state entry) :stopped
          (entry-process entry) nil
          (entry-thread entry) nil)
    (cond
      ((not (%needs-met-p declaration)) nil)
      (image
       (unless *image-argv*
         (error "~a declares an image, and nothing here knows how to start one."
                (entry-name entry)))
       (%launch entry (funcall *image-argv* image))
       (setf (entry-state entry) :running)
       t)
      (run (%launch entry (%argv run))
           (setf (entry-state entry) :running)
           t)
      (thread
       (setf (entry-thread entry)
             (bordeaux-threads:make-thread
              (lambda () (pine.err:attempt thread (format nil "~a" (entry-name entry))))
              :name (format nil "pine-proc-~a" (entry-name entry)))
             (entry-state entry) :running)
       t)
      (t (error "~a declares nothing to run." (entry-name entry))))))

(defun %stop (entry)
  (let ((process (entry-process entry))
        (thread (entry-thread entry)))
    (when process
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process :urgent t))
      ;; reaped rather than left: the pipe then reaches its end on its own,
      ;; which is how the reader is told to stop. Closing the stream under a
      ;; thread that is blocked reading it is a race, not a signal.
      (uiop:wait-process process))
    (when (and thread (bordeaux-threads:thread-alive-p thread))
      (bordeaux-threads:destroy-thread thread))
    (setf (entry-process entry) nil
          (entry-thread entry) nil
          (entry-reader entry) nil
          (entry-state entry) :stopped)))

(defun %backoff (entry)
  (setf (entry-due entry)
        (+ (get-universal-time)
           (min *backoff-cap* (expt 2 (min 6 (entry-attempts entry)))))))

;;;; The pass

(defun %intervalp (entry)
  (fset:lookup (entry-declaration entry) :every))

(defun %attend (entry)
  "Bring ENTRY to what its declaration asks for."
  (cond
    ((not (entry-wanted entry))
     (when (%alive-p entry) (%stop entry)))
    ((%intervalp entry)
     (let ((every (fset:lookup (entry-declaration entry) :every)))
       (when (and (not (%alive-p entry))
                  (>= (get-universal-time) (entry-due entry)))
         (setf (entry-due entry) (+ (get-universal-time) every))
         (pine.err:attempt (lambda () (%start entry))
                           (format nil "starting ~a" (entry-name entry))))))
    ((%alive-p entry) (setf (entry-state entry) :running
                            (entry-attempts entry) 0))
    ((>= (get-universal-time) (entry-due entry))
     (incf (entry-attempts entry))
     (%backoff entry)
     (multiple-value-bind (started condition)
         (pine.err:attempt (lambda () (%start entry))
                           (format nil "starting ~a" (entry-name entry)))
       (unless (and started (null condition))
         (setf (entry-state entry) :failed))))
    (t (setf (entry-state entry) :failed))))

(defun tick ()
  "One pass over the table. Answers the names it touched."
  (let ((touched nil))
    (dolist (entry (%entries) (nreverse touched))
      (let ((before (entry-state entry)))
        (%attend entry)
        (unless (eq before (entry-state entry))
          (push (entry-name entry) touched))))))

;;;; Declaring

(defun %declare (name declaration)
  "Take DECLARATION for NAME. Writing nil stops it and drops it."
  (bordeaux-threads:with-recursive-lock-held (*lock*)
    (let ((entry (gethash name *table*)))
      (cond
        ((null declaration)
         (when entry (setf (entry-wanted entry) nil) (%stop entry))
         (remhash name *table*))
        (t
         (unless entry
           (setf entry (make-entry :name name)
                 (gethash name *table*) entry))
         (setf (entry-declaration entry) declaration
               (entry-wanted entry) t
               (entry-attempts entry) 0
               (entry-due entry) 0)
         ;; idempotent: writing the same declaration again leaves it running
         (unless (%alive-p entry) (%attend entry)))))
    nil))

(defun %verb (name verb)
  (let ((entry (%entry name)))
    (when entry
      (ecase verb
        (:start (setf (entry-wanted entry) t
                      (entry-attempts entry) 0
                      (entry-due entry) 0)
                (unless (%alive-p entry) (%attend entry)))
        (:stop (setf (entry-wanted entry) nil)
               (%stop entry))
        (:restart (%stop entry)
                  (setf (entry-wanted entry) t
                        (entry-attempts entry) 0
                        (entry-due entry) 0)
                  (%attend entry))))))

;;;; The paths

(defun provider ()
  (ns:provider
   ;; the leaves come first: a clause matches in the order it is written
   (/proc/?name/state
    {:read (pine.data:fn [] (let ((e (%entry name))) (and e (entry-state e))))
     :doc "running, stopped or failed"})
   (/proc/?name/pid
    {:read (pine.data:fn [] (let ((e (%entry name))) (and e (%pid e))))
     :doc "the pid, where it has one"})
   (/proc/?name
    {:read (pine.data:fn [] (let ((e (%entry name))) (and e (entry-declaration e))))
     :write (pine.data:fn [v] (%declare name v))
     :verbs {:start (pine.data:fn [] (%verb name :start))
             :stop (pine.data:fn [] (%verb name :stop))
             :restart (pine.data:fn [] (%verb name :restart))}
     :doc "what to run and where: :image, :run or :thread"})
   (/proc
    {:ls (pine.data:fn [] (mapcar #'entry-name (%entries)))
     :doc "everything running, whatever its blast radius"})))

(defun mount (&key system image-argv)
  "Serve /proc, and attend it on SYSTEM's wheel timer.

IMAGE-ARGV answers the argv that starts an image of pine which joins this
daemon; without one, a declaration that asks for an image says so."
  (unmount)
  (setf *image-argv* image-argv)
  (ns:write /proc (provider))
  (when system
    (let ((timer (sento.actor-system:scheduler system)))
      (unless timer
        (error "This actor system has no scheduler, so nothing can attend /proc."))
      (setf *timer* timer)
      (sento.wheel-timer:schedule-recurring
       timer *interval* *interval*
       (lambda () (pine.err:attempt #'tick "attending /proc"))
       *timer-key*)))
  nil)

(defun unmount ()
  "Stop attending, stop everything declared, and forget the table."
  (when *timer*
    (sento.wheel-timer:cancel *timer* *timer-key*)
    (setf *timer* nil))
  (bordeaux-threads:with-recursive-lock-held (*lock*)
    (maphash (lambda (name entry)
               (declare (ignore name))
               (setf (entry-wanted entry) nil)
               (%stop entry))
             *table*)
    (clrhash *table*))
  nil)
