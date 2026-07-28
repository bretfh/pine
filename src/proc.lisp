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
;;;; The table is a value in an atomic reference, like the namespace itself, so
;;;; a read is a slot read and nothing here waits on anything. Launching and
;;;; terminating are not pure, so they happen on the caller's thread outside the
;;;; swap; two callers racing to start the same entry are settled by claiming
;;;; :starting with a compare-and-swap, and only the winner launches.

(defparameter *interval* 1
  "Seconds between passes over the table.")

(defparameter *backoff-cap* 60
  "The longest pine waits before trying something that keeps dying again.")

(defparameter *out-kept* 200
  "Lines of a process's output the table remembers.")

;;;; An entry is {:name :declaration :state :process :thread :attempts :due
;;;; :exit :error :wanted}. STATE is :running, :starting, :stopped or :failed;
;;;; DUE is the universal time before which nothing is tried again; WANTED is
;;;; whether it is meant to be up.

(defstruct (proc (:constructor %proc (cell timer argv)) (:copier nil)
                 (:predicate nil))
  cell
  timer
  argv)

(defun %fresh (name)
  {:name name :state :stopped :attempts 0 :due 0 :wanted nil})

(defun %set (map changes)
  "MAP with every key in CHANGES replacing what was there."
  (fset:map-union map changes))

(defun %now (proc)
  "The table as it stands: one slot read, no message, no wait."
  (sento.atomic:atomic-get (proc-cell proc)))

(defun %entry (proc name) (fset:lookup (%now proc) name))

(defun %field (proc name key)
  "What NAME's entry says under KEY, or NIL when nothing is declared there."
  (let ((entry (%entry proc name)))
    (and entry (fset:lookup entry key))))

(defun %claim (proc name fn)
  "Put FN's answer at NAME and say this caller did it, retrying until it lands.

FN sees the entry as it stands and answers NIL to decline, which is how two
threads racing over the same process end with one of them doing the work: the
loser's FN sees what the winner wrote and declines."
  (loop
    (let* ((table (%now proc))
           (answer (funcall fn (fset:lookup table name))))
      (when (null answer) (return nil))
      (when (sento.atomic:atomic-cas (proc-cell proc) table
                                     (fset:with table name answer))
        (return answer)))))

(defun %put (proc name entry)
  "Put ENTRY at NAME. For the caller that has already claimed it."
  (sento.atomic:atomic-swap (proc-cell proc)
                            (lambda (table) (fset:with table name entry)))
  entry)

(defun %drop (proc name)
  (sento.atomic:atomic-swap (proc-cell proc)
                            (lambda (table) (fset:less table name)))
  nil)

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

;;;; Starting and stopping. Neither is pure, so neither runs inside a swap.

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

(defun %halt (entry)
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

(defun %try (entry image-argv)
  "ENTRY started, or carrying why it could not be.

A start that does not take is something about the process -- a command that is
not there, a permission -- so it is recorded where the process is read rather
than raised as a fault in pine."
  (handler-case (%start (%set entry {:error nil}) image-argv)
    (error (c) (%set entry {:error (princ-to-string c) :state :failed}))))

;;;; The pass. Each of these claims before it acts, so a wheel-timer pass and a
;;;; write arriving at once cannot both start the same process.

(defun %claim-start (proc name)
  "Claim NAME for starting. Answers the claimed entry, or NIL to leave it be."
  (%claim proc name
          (lambda (entry)
            (and entry
                 (fset:lookup entry :wanted)
                 (not (eq :starting (fset:lookup entry :state)))
                 (not (%alive-p entry))
                 (%set entry {:state :starting})))))

(defun %claim-halt (proc name)
  (%claim proc name
          (lambda (entry)
            (and entry
                 (not (eq :stopping (fset:lookup entry :state)))
                 (or (%alive-p entry) (fset:lookup entry :process))
                 (%set entry {:state :stopping})))))

(defun %note-exit (proc name)
  "Record what a process that has stopped said on its way out, so a policy
above can act on it rather than pine guessing."
  (%claim proc name
          (lambda (entry)
            (let ((process (and entry (fset:lookup entry :process))))
              (and process
                   (not (uiop:process-alive-p process))
                   (%set entry {:exit (uiop:wait-process process)}))))))

(defun %halt-now (proc name)
  "Stop what NAME is running, if this caller is the one that claims it."
  (let ((claimed (%claim-halt proc name)))
    (when claimed (%put proc name (%halt claimed)))))

(defun %start-now (proc name)
  "Start what NAME declares, if this caller is the one that claims it."
  (let ((claimed (%claim-start proc name)))
    (when claimed (%put proc name (%try claimed (proc-argv proc))))))

(defun %attend (proc name)
  "Bring NAME to what its declaration asks for.

Every step here claims rather than writing a value it read a moment ago, so a
pass and a write arriving at once cannot undo each other: the loser's claim
sees what the winner left."
  (let ((first (%entry proc name)))
    (when (and first (not (%alive-p first))) (%note-exit proc name)))
  (let ((entry (%entry proc name)))
    (when entry
      (let ((every (fset:lookup (fset:lookup entry :declaration) :every))
            (now (get-universal-time)))
        (cond
          ((not (fset:lookup entry :wanted)) (%halt-now proc name))
          ((eq :starting (fset:lookup entry :state)) nil)
          (every
           (when (and (not (%alive-p entry)) (>= now (fset:lookup entry :due)))
             (%claim proc name
                     (lambda (e) (and e (%set e {:due (+ now every)}))))
             (%start-now proc name)))
          ((%alive-p entry)
           (%claim proc name
                   (lambda (e)
                     (and e (not (eq :running (fset:lookup e :state)))
                          (%set e {:state :running :attempts 0 :error nil})))))
          ((>= now (fset:lookup entry :due))
           (%claim proc name
                   (lambda (e)
                     (and e (%backoff (%set e {:attempts (1+ (fset:lookup
                                                              e :attempts))})))))
           (%start-now proc name)
           (%claim proc name
                   (lambda (e) (and e (not (%alive-p e))
                                    (%set e {:state :failed})))))
          (t (%claim proc name
                     (lambda (e) (and e (%set e {:state :failed}))))))))))

(defun tick (proc)
  "One pass over the table."
  (fset:do-map (name entry (%now proc))
    (declare (ignore entry))
    (%attend proc name))
  nil)

;;;; Declaring

(defun %declare (proc name declaration)
  "Take DECLARATION for NAME. Writing nil stops it and drops it."
  (cond
    ((null declaration)
     (when (%entry proc name)
       (%put proc name (%set (%entry proc name) {:wanted nil}))
       (%halt-now proc name)
       (%drop proc name)))
    (t
     (%claim proc name
             (lambda (entry)
               (%set (or entry (%fresh name))
                     {:declaration declaration :wanted t :attempts 0 :due 0})))
     ;; idempotent: writing the same declaration again leaves it running
     (%attend proc name)))
  nil)

(defun %verb (proc name verb)
  (when (%entry proc name)
    (ecase verb
      (:start (%claim proc name
                      (lambda (e) (and e (%set e {:wanted t :attempts 0 :due 0}))))
              (%attend proc name))
      (:stop (%claim proc name (lambda (e) (and e (%set e {:wanted nil}))))
             (%halt-now proc name))
      (:restart (%halt-now proc name)
                (%claim proc name
                        (lambda (e) (and e (%set e {:wanted t :attempts 0 :due 0}))))
                (%attend proc name))))
  nil)

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
     :write (pine.data:fn [v] (%declare proc name v))
     :verbs {:start (pine.data:fn [] (%verb proc name :start))
             :stop (pine.data:fn [] (%verb proc name :stop))
             :restart (pine.data:fn [] (%verb proc name :restart))}
     :doc "what to run and where: :image, :run or :thread"})
   (/proc
    {:ls (pine.data:fn [] (sort (pine.data:keys (%now proc)) #'string<))
     :doc "everything running, whatever its blast radius"})))

;;;; Mounting. What /proc holds is made here and let go in UNMOUNT.

(defun mount (&key system image-argv)
  "Serve /proc in the current space, and attend it on SYSTEM's wheel timer.
Answers the table, which is what unmounts it.

IMAGE-ARGV answers the argv that starts an image of pine which joins this
daemon; without one, a declaration that asks for an image says so."
  (let* ((timer (when system
                  (or (sento.actor-system:scheduler system)
                      (error "This actor system has no scheduler, so nothing ~
                              can attend /proc."))))
         (proc (%proc (sento.atomic:make-atomic-reference :value (fset:empty-map))
                      timer image-argv)))
    (ns:write /proc (provider proc))
    (when timer
      (sento.wheel-timer:schedule-recurring
       timer *interval* *interval*
       (lambda () (pine.err:attempt (lambda () (tick proc)) "attending /proc"))
       :pine-proc))
    proc))

(defun unmount (proc)
  "Stop attending, and stop everything declared."
  (when (proc-timer proc)
    (sento.wheel-timer:cancel (proc-timer proc) :pine-proc))
  (fset:do-map (name entry (%now proc))
    (declare (ignore entry))
    (%put proc name (%set (%entry proc name) {:wanted nil}))
    (%halt-now proc name))
  (sento.atomic:atomic-swap (proc-cell proc) (lambda (old)
                                               (declare (ignore old))
                                               (fset:empty-map)))
  (ns:write /proc nil)
  nil)
