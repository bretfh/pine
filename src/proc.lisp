(defpackage #:pine.proc
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:proc #:server #:tick #:emit
           #:runnable #:kinds #:kind-of #:start #:halt #:alive-p #:took
           #:interval #:*backoff-cap* #:*out-kept*))

(in-package #:pine.proc)
(named-readtables:in-readtable pine.path:syntax)

;;;; Everything running, whatever its blast radius, in one table at /proc. An
;;;; entry is {:name :declaration :state :took :attempts :due :exit :error
;;;; :wanted}; :took is what the kind is holding and :state is one of :running
;;;; :starting :stopping :stopped :failed.

(defstruct (proc (:constructor %proc (cell timer argv)) (:copier nil)
                 (:predicate nil))
  cell timer argv)

(defstruct (spawned (:constructor %spawned (process reader)) (:copier nil))
  process reader)

(defparameter *backoff-cap* 60)

(defparameter *out-kept* 200)

(defvar *kinds* (sento.atomic:make-atomic-reference :value (fset:empty-seq))
  "The declaration keys /proc can run, in the order one is asked about them.")

(defvar *saying* nil "The entry whose thread is running, for EMIT.")

(defgeneric start (kind entry &key image-argv &allow-other-keys)
  (:documentation "ENTRY with what it declares running, under :TOOK."))

(defgeneric halt (kind entry)
  (:documentation "ENTRY with what it took given back."))

(defgeneric alive-p (kind entry)
  (:documentation "Whether what ENTRY took is still going."))

(defun runnable (key)
  "Say a declaration key /proc can run. START, HALT and ALIVE-P on it are the
rest of a kind."
  (sento.atomic:atomic-swap
   *kinds*
   (lambda (seq)
     (if (find key (fset:convert 'list seq)) seq (fset:with-last seq key))))
  key)

(defun kinds () (fset:convert 'list (sento.atomic:atomic-get *kinds*)))

(defun kind-of (declaration)
  (loop :for key :in (kinds) :thereis (and (fset:lookup declaration key) key)))

(defun took (entry) (fset:lookup entry :took))

(defun interval ()
  (or (ns:read /proc-interval) 1))

(defun %fresh (name)
  {:name name :state :stopped :attempts 0 :due 0 :wanted nil})

(defun %set (map changes) (fset:map-union map changes))

(defun %now (proc) (sento.atomic:atomic-get (proc-cell proc)))

(defun %entry (proc name) (fset:lookup (%now proc) name))

(defun %field (proc name key)
  (let ((entry (%entry proc name)))
    (and entry (fset:lookup entry key))))

(defun %claim (proc name fn)
  "Put FN's answer at NAME and say this caller did it, retrying until it lands.
FN answers NIL to decline, which is how two threads racing end with one."
  (loop
    (let* ((table (%now proc))
           (answer (funcall fn (fset:lookup table name))))
      (when (null answer) (return nil))
      (when (sento.atomic:atomic-cas (proc-cell proc) table
                                     (fset:with table name answer))
        (return answer)))))

(defun %put (proc name entry)
  (sento.atomic:atomic-swap (proc-cell proc)
                            (lambda (table) (fset:with table name entry)))
  entry)

(defun %drop (proc name)
  (sento.atomic:atomic-swap (proc-cell proc)
                            (lambda (table) (fset:less table name)))
  nil)

(defun %alive-p (entry)
  (and entry
       (alive-p (kind-of (fset:lookup entry :declaration)) entry)))

(defun say (name line)
  (ns:write (p:path /proc name "out") line :max *out-kept* :keep nil))

(defun emit (&rest arguments)
  "Say a line at this process's /proc/?name/out. A :thread has no stdout."
  (let ((name *saying*))
    (when name
      (let ((line (if (and arguments (stringp (first arguments)) (rest arguments))
                      (apply #'format nil arguments)
                      (princ-to-string (or (first arguments) "")))))
        (say name line)
        line))))

(defun %reader (name stream)
  (bordeaux-threads:make-thread
   (lambda ()
     (loop :for line = (handler-case (read-line stream nil nil)
                         (stream-error () nil))
           :while line
           :do (say name line)))
   :name (format nil "pine-proc-out-~a" name)))

(defun %paths (value)
  (cond ((null value) nil)
        ((fset:seq? value) (fset:convert 'list value))
        ((p:pathp value) (list value))
        (t value)))

(defun %needs-met-p (declaration)
  (and (every (lambda (path) (ns:read path))
              (%paths (fset:lookup declaration :needs)))
       (notany (lambda (path) (ns:read path))
               (%paths (fset:lookup declaration :unless)))))

(defun %argv (value)
  (cond ((stringp value) (list "sh" "-c" value))
        ((fset:seq? value) (fset:convert 'list value))
        ((listp value) value)
        (t (error "~s is not a command." value))))

(defun %launch (entry argv)
  (let* ((env (fset:lookup (fset:lookup entry :declaration) :env))
         (process (if env
                      (uiop:launch-program argv :output :stream
                                                :error-output :output
                                                :environment (%argv env))
                      (uiop:launch-program argv :output :stream
                                                :error-output :output)))
         (name (fset:lookup entry :name)))
    (%set entry {:took (%spawned process
                                 (%reader name (uiop:process-info-output process)))
                 :state :running})))

(defun %reap (entry)
  (let ((it (took entry)))
    (when (spawned-p it)
      (let ((process (spawned-process it)))
        (when (uiop:process-alive-p process)
          (uiop:terminate-process process :urgent t))
        ;; the pipe ends when the process is reaped, which is what tells the
        ;; reader to stop. Closing it under a blocked read is a race.
        (uiop:wait-process process))))
  (%set entry {:took nil :state :stopped}))

(defun %running-p (entry)
  (let ((it (took entry)))
    (and (spawned-p it) (uiop:process-alive-p (spawned-process it)))))

(defun %pid (entry)
  (let ((it (took entry)))
    (and (spawned-p it) (uiop:process-info-pid (spawned-process it)))))

(defun %exit-of (entry)
  (let ((it (took entry)))
    (and (spawned-p it)
         (not (uiop:process-alive-p (spawned-process it)))
         (uiop:wait-process (spawned-process it)))))

(defmethod start ((kind (eql :run)) entry &key &allow-other-keys)
  (%launch entry (%argv (fset:lookup (fset:lookup entry :declaration) :run))))

(defmethod halt ((kind (eql :run)) entry) (%reap entry))

(defmethod alive-p ((kind (eql :run)) entry) (%running-p entry))

(defmethod start ((kind (eql :image)) entry &key image-argv &allow-other-keys)
  (let ((image (fset:lookup (fset:lookup entry :declaration) :image)))
    (unless image-argv
      (error "~a declares an image, and nothing here knows how to start one."
             (fset:lookup entry :name)))
    (%launch entry (funcall image-argv image))))

(defmethod halt ((kind (eql :image)) entry) (%reap entry))

(defmethod alive-p ((kind (eql :image)) entry) (%running-p entry))

(defmethod start ((kind (eql :thread)) entry &key &allow-other-keys)
  (let ((name (fset:lookup entry :name))
        (thunk (fset:lookup (fset:lookup entry :declaration) :thread)))
    (%set entry
          {:took (bordeaux-threads:make-thread
                  (lambda ()
                    (let ((*saying* name))
                      (pine.err:attempt thunk (format nil "~a" name))))
                  :name (format nil "pine-proc-~a" name))
           :state :running})))

(defmethod halt ((kind (eql :thread)) entry)
  (let ((it (took entry)))
    (when (and it (bordeaux-threads:thread-alive-p it))
      (bordeaux-threads:destroy-thread it)))
  (%set entry {:took nil :state :stopped}))

(defmethod alive-p ((kind (eql :thread)) entry)
  (let ((it (took entry)))
    (and it (bordeaux-threads:thread-alive-p it))))

(defmethod start ((kind null) entry &key &allow-other-keys)
  (error "~a declares nothing to run." (fset:lookup entry :name)))

(defmethod halt ((kind null) entry) (%set entry {:took nil :state :stopped}))

(defmethod alive-p ((kind null) entry) (declare (ignore entry)) nil)

(runnable :image)
(runnable :run)
(runnable :thread)

(defun %start (entry image-argv)
  (let ((declaration (fset:lookup entry :declaration))
        (down (%set entry {:state :stopped :took nil})))
    (if (%needs-met-p declaration)
        (start (kind-of declaration) down :image-argv image-argv)
        down)))

(defun %halt (entry)
  (halt (kind-of (fset:lookup entry :declaration)) entry))

(defun %backoff (entry)
  (%set entry {:due (+ (get-universal-time)
                       (min *backoff-cap*
                            (expt 2 (min 6 (fset:lookup entry :attempts)))))}))

(defun %try (entry image-argv)
  (handler-case (%start (%set entry {:error nil}) image-argv)
    (error (c) (%set entry {:error (princ-to-string c) :state :failed}))))

(defun %claim-start (proc name)
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
                 (or (%alive-p entry) (took entry))
                 (%set entry {:state :stopping})))))

(defun %note-exit (proc name)
  (%claim proc name
          (lambda (entry)
            (let ((exit (and entry (%exit-of entry))))
              (and exit (%set entry {:exit exit}))))))

(defun %halt-now (proc name)
  (let ((claimed (%claim-halt proc name)))
    (when claimed (%put proc name (%halt claimed)))))

(defun %start-now (proc name)
  (let ((claimed (%claim-start proc name)))
    (when claimed (%put proc name (%try claimed (proc-argv proc))))))

(defun %attend (proc name)
  "Bring NAME to what its declaration asks for. Every step claims rather than
writing a value it read a moment ago."
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
  (fset:do-map (name entry (%now proc))
    (declare (ignore entry))
    (%attend proc name))
  nil)

(defun %declare (proc name declaration)
  "Take DECLARATION for NAME. Writing nil stops it and drops it; the same
declaration again leaves it running."
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

(defun provider (proc)
  (ns:provider
   ;; the leaves come first: a clause matches in the order it is written
   (/proc/?name/state
    {:read (pine.data:fn [] (%field proc name :state))
     :doc "running, stopped or failed"})
   (/proc/?name/pid
    {:read (pine.data:fn [] (let ((e (%entry proc name))) (and e (%pid e))))
     :doc "the pid, where it has one"})
   (/proc/?name/out
    {:doc "a ring of what it has said"})
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

(defun %raise (&key system image-argv)
  (let* ((timer (when system
                  (or (sento.actor-system:scheduler system)
                      (error "This actor system has no scheduler, so nothing ~
                              can attend /proc."))))
         (proc (%proc (sento.atomic:make-atomic-reference :value (fset:empty-map))
                      timer image-argv)))
    (ns:write /proc (provider proc))
    (when timer
      (sento.wheel-timer:schedule-recurring
       timer (interval) (interval)
       (lambda () (pine.err:attempt (lambda () (tick proc)) "attending /proc"))
       :pine-proc))
    proc))

(defun %lower (proc)
  (when (proc-timer proc)
    (sento.wheel-timer:cancel (proc-timer proc) :pine-proc))
  (fset:do-map (name entry (%now proc))
    (declare (ignore entry))
    (%put proc name (%set (%entry proc name) {:wanted nil}))
    (%halt-now proc name))
  (sento.atomic:atomic-swap (proc-cell proc)
                            (lambda (old) (declare (ignore old)) (fset:empty-map)))
  (ns:write /proc nil)
  nil)

(defclass server (ns:server) ()
  (:default-initargs :name :proc :serves (list /proc))
  (:documentation "Everything running, in one table. The table holds processes
and threads, which cannot be values, so the space keeps it."))

(defmethod ns:raise ((s server) &key system image-argv &allow-other-keys)
  (declare (ignore s))
  (%raise :system system :image-argv image-argv))

(defmethod ns:lower ((s server))
  (let ((proc (ns:kept (ns:name s))))
    (when proc (%lower proc)))
  (call-next-method))

(ns:register (make-instance 'server))
