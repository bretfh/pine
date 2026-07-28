(defpackage #:pine.proc
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:proc #:mount #:unmount #:tick
           #:*interval* #:*backoff-cap* #:*out-kept*))

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
;;;;
;;;; The table is one agent's state and an entry is a map, so a read is a slot
;;;; read of a whole immutable value and never a message. That is what lets a
;;;; declaration name a path under /proc in its :needs: the read cannot come
;;;; back around and ask the thread already running it.

(defparameter *interval* 1
  "Seconds between passes over the table.")

(defparameter *backoff-cap* 60
  "The longest pine waits before trying something that keeps dying again.")

(defparameter *out-kept* 200
  "Lines of a process's output the table remembers.")

;;;; The state one agent holds: {:argv HOW-TO-START-AN-IMAGE :entries {NAME ENTRY}}
;;;;
;;;; An entry is {:name :declaration :state :process :thread :attempts :due
;;;; :exit :error :wanted}. STATE is :running, :stopped or :failed; DUE is the
;;;; universal time before which nothing is tried again; WANTED is whether it is
;;;; meant to be up.

(defstruct (proc (:constructor %proc (owner timer)) (:copier nil) (:predicate nil))
  owner
  timer)

(defun %fresh (name)
  {:name name :state :stopped :attempts 0 :due 0 :wanted nil})

(defun %set (map changes)
  "MAP with every key in CHANGES replacing what was there."
  (fset:map-union map changes))

(defun %entries (state) (or (fset:lookup state :entries) (fset:empty-map)))

(defun %now (proc)
  "The state as it stands: one slot read, no message, no wait."
  (let ((state (sento.agent:agent-get-quick (proc-owner proc) #'identity)))
    (if (fset:map? state) state (fset:empty-map))))

(defun %entry (proc name) (fset:lookup (%entries (%now proc)) name))

(defun %field (proc name key)
  "What NAME's entry says under KEY, or NIL when nothing is declared there."
  (let ((entry (%entry proc name)))
    (and entry (fset:lookup entry key))))

(defun %alter (proc fn)
  "Run FN over the state on the owner's thread and take its answer as the state.

Nothing FN does may ask anything, which is why every read above is a snapshot."
  (let ((answer (sento.agent:agent-update-and-get (proc-owner proc) fn)))
    (if (and (consp answer) (eq :handler-error (car answer)))
        (error (cdr answer))
        answer)))

;;;; Is it up

(defun %alive-p (entry)
  (let ((process (fset:lookup entry :process))
        (thread (fset:lookup entry :thread)))
    (cond (process (uiop:process-alive-p process))
          (thread (bordeaux-threads:thread-alive-p thread))
          (t nil))))

(defun %pid (entry)
  (let ((process (fset:lookup entry :process)))
    (and process (uiop:process-info-pid process))))

;;;; Starting and stopping

(defun %paths (value)
  (cond ((null value) nil)
        ((fset:seq? value) (fset:convert 'list value))
        ((p:pathp value) (list value))
        (t value)))

(defun %needs-met-p (declaration)
  "Whether what the declaration waits for is there, and what it waits to be
gone is gone."
  (and (every (lambda (path) (ns:read path))
              (%paths (fset:lookup declaration :needs)))
       (notany (lambda (path) (ns:read path))
               (%paths (fset:lookup declaration :unless)))))

(defun %argv (value)
  (cond ((stringp value) (list "sh" "-c" value))
        ((fset:seq? value) (fset:convert 'list value))
        ((listp value) value)
        (t (error "~s is not a command." value))))

(defun %reader (name stream)
  "Push STREAM's lines into the entry's output ring, on a thread of its own,
because a process that says nothing must not block one that does.

The ring is what a process is saying rather than something anyone wrote, so it
is not kept in the file."
  (let ((where (p:path /proc name "out")))
    (bordeaux-threads:make-thread
     (lambda ()
       ;; a closed stream is how this thread is told to stop, not a fault
       (loop :for line = (handler-case (read-line stream nil nil)
                           (stream-error () nil))
             :while line
             :do (ns:write where line :max *out-kept* :keep nil)))
     :name (format nil "pine-proc-out-~a" name))))

(defun %launch (entry argv)
  "ENTRY with the process ARGV names running under it."
  (let* ((env (fset:lookup (fset:lookup entry :declaration) :env))
         (process (if env
                      (uiop:launch-program argv :output :stream
                                                :error-output :output
                                                :environment (%argv env))
                      (uiop:launch-program argv :output :stream
                                                :error-output :output))))
    (%reader (fset:lookup entry :name) (uiop:process-info-output process))
    (%set entry {:process process :state :running})))

(defun %start (entry image-argv)
  "ENTRY with what it declares started, or as it stands when nothing could be."
  (let* ((declaration (fset:lookup entry :declaration))
         (image (fset:lookup declaration :image))
         (run (fset:lookup declaration :run))
         (thread (fset:lookup declaration :thread))
         (name (fset:lookup entry :name))
         (down (%set entry {:state :stopped :process nil :thread nil})))
    (cond
      ((not (%needs-met-p declaration)) down)
      (image
       (unless image-argv
         (error "~a declares an image, and nothing here knows how to start one."
                name))
       (%launch down (funcall image-argv image)))
      (run (%launch down (%argv run)))
      (thread
       (%set down {:thread (bordeaux-threads:make-thread
                            (lambda ()
                              (pine.err:attempt thread (format nil "~a" name)))
                            :name (format nil "pine-proc-~a" name))
                   :state :running}))
      (t (error "~a declares nothing to run." name)))))

(defun %stop (entry)
  "ENTRY with whatever it was running gone."
  (let ((process (fset:lookup entry :process))
        (thread (fset:lookup entry :thread)))
    (when process
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process :urgent t))
      ;; reaped rather than left: the pipe then reaches its end on its own,
      ;; which is how the reader is told to stop. Closing the stream under a
      ;; thread that is blocked reading it is a race, not a signal.
      (uiop:wait-process process))
    (when (and thread (bordeaux-threads:thread-alive-p thread))
      (bordeaux-threads:destroy-thread thread))
    (%set entry {:process nil :thread nil :state :stopped})))

(defun %backoff (entry)
  (%set entry {:due (+ (get-universal-time)
                       (min *backoff-cap*
                            (expt 2 (min 6 (fset:lookup entry :attempts)))))}))

;;;; The pass

(defun %note-exit (entry)
  "ENTRY carrying what a process that has stopped said on its way out, so a
policy above can act on it rather than pine guessing."
  (let ((process (fset:lookup entry :process)))
    (if (and process (not (uiop:process-alive-p process)))
        (%set entry {:exit (uiop:wait-process process)})
        entry)))

(defun %try (entry image-argv)
  "ENTRY started, or carrying why it could not be.

A start that does not take is something about the process -- a command that is
not there, a permission -- so it is recorded where the process is read rather
than raised as a fault in pine."
  (handler-case (%start (%set entry {:error nil}) image-argv)
    (error (c) (%set entry {:error (princ-to-string c) :state :failed}))))

(defun %attend (entry image-argv)
  "ENTRY brought to what its declaration asks for."
  (let* ((entry (if (%alive-p entry) entry (%note-exit entry)))
         (every (fset:lookup (fset:lookup entry :declaration) :every))
         (now (get-universal-time)))
    (cond
      ((not (fset:lookup entry :wanted))
       (if (%alive-p entry) (%stop entry) entry))
      (every
       (if (and (not (%alive-p entry)) (>= now (fset:lookup entry :due)))
           (%try (%set entry {:due (+ now every)}) image-argv)
           entry))
      ((%alive-p entry) (%set entry {:state :running :attempts 0 :error nil}))
      ((>= now (fset:lookup entry :due))
       (let* ((again (%set entry {:attempts (1+ (fset:lookup entry :attempts))}))
              (started (%try (%backoff again) image-argv)))
         (if (%alive-p started) started (%set started {:state :failed}))))
      (t (%set entry {:state :failed})))))

(defun %pass (state)
  "STATE with every entry attended."
  (let ((argv (fset:lookup state :argv))
        (entries (%entries state)))
    (fset:do-map (name entry entries)
      (setf entries (fset:with entries name (%attend entry argv))))
    (fset:with state :entries entries)))

;;;; Declaring

(defun %declare (state name declaration)
  "STATE with DECLARATION taken for NAME. Writing nil stops it and drops it."
  (let* ((entries (%entries state))
         (argv (fset:lookup state :argv))
         (entry (or (fset:lookup entries name) (%fresh name))))
    (fset:with state :entries
               (cond
                 ((null declaration)
                  (when (fset:lookup entries name)
                    (%stop (%set entry {:wanted nil})))
                  (fset:less entries name))
                 (t
                  (let ((taken (%set entry {:declaration declaration :wanted t
                                            :attempts 0 :due 0})))
                    ;; idempotent: writing the same declaration again leaves it
                    ;; running
                    (fset:with entries name
                               (if (%alive-p taken)
                                   taken
                                   (%attend taken argv)))))))))

(defun %verb (state name verb)
  (let* ((entries (%entries state))
         (argv (fset:lookup state :argv))
         (entry (fset:lookup entries name)))
    (if (null entry)
        state
        (fset:with state :entries
                   (fset:with
                    entries name
                    (ecase verb
                      (:start (let ((up (%set entry {:wanted t :attempts 0 :due 0})))
                                (if (%alive-p up) up (%attend up argv))))
                      (:stop (%stop (%set entry {:wanted nil})))
                      (:restart (%attend (%set (%stop entry)
                                               {:wanted t :attempts 0 :due 0})
                                         argv))))))))

;;;; The paths

(defun provider (proc)
  (ns:provider
   ;; the leaves come first: a clause matches in the order it is written
   (/proc/?name/state
    {:read (pine.data:fn [] (%field proc name :state))
     :doc "running, stopped or failed"})
   (/proc/?name/pid
    {:read (pine.data:fn [] (let ((e (%entry proc name))) (and e (%pid e))))
     :doc "the pid, where it has one"})
   (/proc/?name/exit
    {:read (pine.data:fn [] (%field proc name :exit))
     :doc "what it said on its way out, last time it stopped"})
   (/proc/?name/error
    {:read (pine.data:fn [] (%field proc name :error))
     :doc "why the last start did not take"})
   (/proc/?name
    {:read (pine.data:fn [] (%field proc name :declaration))
     :write (pine.data:fn [v]
              (%alter proc (lambda (state) (%declare state name v)))
              nil)
     :verbs {:start (pine.data:fn []
                      (%alter proc (lambda (s) (%verb s name :start))) nil)
             :stop (pine.data:fn []
                     (%alter proc (lambda (s) (%verb s name :stop))) nil)
             :restart (pine.data:fn []
                        (%alter proc (lambda (s) (%verb s name :restart))) nil)}
     :doc "what to run and where: :image, :run or :thread"})
   (/proc
    {:ls (pine.data:fn []
           (sort (pine.data:keys (%entries (%now proc))) #'string<))
     :doc "everything running, whatever its blast radius"})))

;;;; Mounting. What /proc owns is made here and let go in UNMOUNT, which is the
;;;; whole of its lifecycle.

(defun tick (proc)
  "One pass over the table."
  (%alter proc #'%pass)
  nil)

(defun mount (&key system image-argv)
  "Serve /proc in the current space, and attend it on SYSTEM's wheel timer.
Answers the table, which is what unmounts it.

IMAGE-ARGV answers the argv that starts an image of pine which joins this
daemon; without one, a declaration that asks for an image says so."
  (let* ((timer (when system
                  (or (sento.actor-system:scheduler system)
                      (error "This actor system has no scheduler, so nothing ~
                              can attend /proc."))))
         (proc (%proc (sento.agent:make-agent
                       (lambda () {:argv image-argv :entries (fset:empty-map)}))
                      timer)))
    (ns:write /proc (provider proc))
    (when timer
      (sento.wheel-timer:schedule-recurring
       timer *interval* *interval*
       (lambda () (pine.err:attempt (lambda () (tick proc)) "attending /proc"))
       :pine-proc))
    proc))

(defun unmount (proc)
  "Stop attending, stop everything declared, and let the table's thread go."
  (when (proc-timer proc)
    (sento.wheel-timer:cancel (proc-timer proc) :pine-proc))
  (%alter proc (lambda (state)
                 (fset:do-map (name entry (%entries state))
                   (declare (ignore name))
                   (%stop (%set entry {:wanted nil})))
                 {:argv (fset:lookup state :argv) :entries (fset:empty-map)}))
  (sento.agent:agent-stop (proc-owner proc))
  (ns:write /proc nil)
  nil)
