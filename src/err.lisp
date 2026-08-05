(defpackage #:pine.err
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:d #:pine.data))
  (:export #:fault #:parked #:fault-id #:fault-label #:fault-condition
           #:fault-type #:fault-backtrace #:fault-offered #:fault-at
           #:fault-root #:fault-attended #:fault-taken #:attended-p
           #:offers #:resume #:described #:note #:forget #:faults
           #:report-failure #:attempt
           #:evaluation #:evaluation-id #:evaluation-form #:evaluation-status
           #:evaluation-values #:evaluation-output #:evaluation-fault
           #:evaluate #:evaluate-string #:evaluate-thunk
           #:list-evaluations #:find-evaluation #:abort-evaluation
           #:with-debugger #:call-with-debugger
           #:park-seconds #:faults-kept #:evaluations-kept #:server))

(in-package #:pine.err)
(named-readtables:in-readtable pine.path:syntax)

;;;; What breaks and what can decide are rarely the same thread, and often not
;;;; the same image. A condition becomes a record at /err/?id and the decision
;;;; is a write to that path, so the debugger is one surface over (read /err/*).
;;;;
;;;; Whether the faulting thread is still standing there is the fault's own
;;;; business, which is why RESUME is a method:
;;;;
;;;;   FAULT   the stack is gone. Something on shared machinery unwound at once,
;;;;           because a thread parked there is a dispatcher worker that never
;;;;           comes back.
;;;;   PARKED  the stack is live. An evaluation is a thread whose whole job is
;;;;           that one form, so every restart CL established can still be taken.
;;;;
;;;; The registry is {:n NEXT-ID :ran (EVALUATION ...) :faults {ID FAULT}}, an
;;;; atomic cell the space keeps: nothing on the path a fault takes waits.

(defvar *noting* nil
  "True while a fault is being announced, so a fault in whatever is looking at
/err records itself without announcing again.")

(defun fresh ()
  (sento.atomic:make-atomic-reference
   :value {:n 0 :ran nil :faults (fset:empty-map)}))

(defun park-seconds ()
  "How long a standing fault waits with nobody attending it before taking its
abort. The wait restarts while it is attended; nil waits as long as it is."
  (ns:read /park-seconds))

(defun faults-kept ()
  "How many faults /err holds. One a thread is standing in is never dropped."
  (or (ns:read /faults-kept) 50))

(defun evaluations-kept ()
  "How many finished evaluations are kept. One still running is never dropped."
  (or (ns:read /evaluations-kept) 50))

(defun %cell ()
  "Where this space keeps what has faulted, made the first time it is asked for.

Boot is when a fault is most likely and least expected, and /err is served part
way through it. Without a cell before then every fault built a registry of its
own, took the id 1 and vanished. It belongs to the space rather than the image,
so two spaces do not share what has faulted; two threads faulting in the same
instant before the first one exists is the one race, and it costs the loser's
fault."
  (or (ns:kept :err)
      (setf (ns:kept :err) (fresh))))

(defun %registry ()
  (sento.atomic:atomic-get (%cell)))

(defun %change (fn)
  "Replace the registry with FN's answer, retrying until it lands. FN is pure."
  (sento.atomic:atomic-swap (%cell) fn))

(defun %next-id ()
  "The next id. Two threads asking at once get two."
  (fset:lookup (%change (lambda (r) (fset:with r :n (1+ (fset:lookup r :n))))) :n))

;;;; The fault

(defclass fault ()
  ((id :initarg :id :initform (%next-id) :reader fault-id)
   (label :initarg :label :initform nil :reader fault-label)
   (condition :initarg :condition :initform "" :reader fault-condition)
   (type :initarg :type :initform "" :reader fault-type)
   (offered :initarg :offered :initform nil :reader fault-offered)
   (backtrace :initarg :backtrace :initform "" :reader fault-backtrace)
   (at :initarg :at :initform (get-universal-time) :reader fault-at)
   (root :initarg :root :initform nil :reader fault-root)
   (attended :initform nil :accessor fault-attended)
   (taken :initform nil :accessor fault-taken))
  (:documentation "A condition that happened, as a value at /err/?id. Its
restarts are recorded as names rather than held as objects, so what crosses to
another image is data."))

(defgeneric offers (fault)
  (:documentation "What FAULT can still be resumed by, as (NAME REPORT). A
fault whose stack unwound can only be dismissed.")
  (:method ((f fault))
    (list (list "ABORT" "dismiss this fault"))))

(defgeneric resume (fault name)
  (:documentation "Take the restart NAME on FAULT, and answer whether it took.
Where the decision lands is the kind's business.")
  (:method ((f fault) name)
    (setf (fault-taken f) (string name))
    (forget f)
    t))

(defgeneric described (fault)
  (:documentation "FAULT as the map /err/?id answers. Everything in it is data,
so it reads the same over remoting.")
  (:method ((f fault))
    (fset:map (:id (fault-id f))
              (:label (princ-to-string (or (fault-label f) "")))
              (:condition (fault-condition f))
              (:type (fault-type f))
              (:restarts (fset:convert 'fset:seq (mapcar #'first (offers f))))
              (:backtrace (fault-backtrace f))
              (:at (fault-at f))
              (:root (fault-root f))
              (:attended (and (fault-attended f) t)))))

(defclass parked (fault)
  ((lock :initform (bordeaux-threads:make-lock) :reader fault-lock)
   (cvar :initform (bordeaux-threads:make-condition-variable) :reader fault-cvar))
  (:documentation "A fault whose thread is still standing in it. Only something
that owns its thread outright parks."))

(defmethod offers ((f parked))
  (fault-offered f))

(defmethod resume ((f parked) name)
  "Hand NAME to the thread standing in F, then take it off /err here rather
than leaving that to the thread: a fault is decided the moment someone decides
it."
  (bordeaux-threads:with-lock-held ((fault-lock f))
    (setf (fault-taken f) (string name))
    (bordeaux-threads:condition-notify (fault-cvar f)))
  (call-next-method))

;;;; What is at /err

(defun faults (&optional id)
  "Every fault at /err, oldest first, or the one with ID."
  (let ((all (fset:lookup (%registry) :faults)))
    (if id
        (fset:lookup all id)
        (let (acc)
          (fset:do-map (k f all) (declare (ignore k)) (push f acc))
          (sort acc #'< :key #'fault-id)))))

(defun %root ()
  "The root the namespace stands at, or NIL where nothing is keeping one."
  (let ((log (ns:read /history)))
    (when (and (fset:seq? log) (plusp (fset:size log)))
      (fset:lookup (fset:lookup log 0) :n))))

(defun %noted (r fault kept)
  "R with FAULT at /err, and the oldest records dropped once there are more of
them than KEPT. Pure: it runs inside the swap."
  (let* ((all (fset:with (fset:lookup r :faults) (fault-id fault) fault))
         (extra (- (fset:size all) kept)))
    (when (plusp extra)
      (let ((records (sort (let (acc)
                             (fset:do-map (k f all)
                               (declare (ignore k))
                               (unless (typep f 'parked) (push f acc)))
                             acc)
                           #'< :key #'fault-id)))
        (dolist (f (subseq records 0 (min extra (length records))))
          (setf all (fset:less all (fault-id f))))))
    (fset:with r :faults all)))

(defun note (fault)
  "Put FAULT at /err/?id, say that /err moved, and answer it. Whoever is
watching runs on the thread that faulted, which is doing nothing else."
  (%change (let ((kept (faults-kept))) (lambda (r) (%noted r fault kept))))
  (cond (*noting*
         (pine.log:note "~a: ~a" (or (fault-label fault) "a fault")
                        (fault-condition fault)))
        ((ns:watched /err)
         (let ((*noting* t))
           (handler-case (ns:stir /err)
             (error (c)
               (format *error-output* "pine: what is watching /err failed on ~a: ~a~%"
                       (fault-label fault) c)
               (finish-output *error-output*)))))
        (t (pine.log:note "~a: ~a" (or (fault-label fault) "a fault")
                          (fault-condition fault))))
  fault)

(defun forget (fault)
  "Take FAULT off /err. Forgetting one already gone says nothing, so the thread
standing in it and the surface that decided it can both call this."
  (when (faults (fault-id fault))
    (%change (lambda (r)
               (fset:with r :faults
                          (fset:less (fset:lookup r :faults) (fault-id fault)))))
    (unless *noting* (ignore-errors (ns:stir /err))))
  nil)

(defun attended-p (fault)
  "Whether someone has FAULT open and is still in a position to decide, so a
thread parked in it has a reason to keep waiting."
  (and (fault-attended fault) (ns:watched /err) t))

(defun (setf attended-p) (value fault)
  (setf (fault-attended fault) (and value t)))

(defun %at (segment)
  "The fault SEGMENT names, or NIL."
  (let ((id (parse-integer segment :junk-allowed t)))
    (and id (faults id))))

(defclass server (ns:server) ()
  (:default-initargs :name :err :serves (list /err))
  (:documentation "What has faulted, and what it is waiting to be told."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  "Serve /err from what has faulted, and answer the cell this space keeps them
in. Live, not held: the file has no business holding a fault.

The cell is whichever one this space already has, so a fault from before /err
was served is still there afterwards."
  (declare (ignore s))
  (ns:write /err
    (ns:provider
     (/err {:ls (d:fn [] (mapcar (lambda (f) (princ-to-string (fault-id f))) (faults)))
            :doc "every fault waiting on a decision"})
     (/err/?id
      {:read (d:fn [] (let ((f (%at id))) (and f (described f))))
       :verbs {:restart (d:fn [name]
                          (let ((f (%at id))) (when f (resume f name))))}
       :doc "a fault; write [:restart NAME] to decide it"})
     (/err/?id/attended
      {:read (d:fn [] (let ((f (%at id))) (and f (fault-attended f))))
       :write (d:fn [v]
                (let ((f (%at id))) (when f (setf (fault-attended f) (and v t)))))
       :doc "true while someone has this fault open"})))
  (%cell))

(ns:register (make-instance 'server))

;;;; Making one out of a condition

(defun %capture-backtrace ()
  (handler-case
      (with-output-to-string (s) (sb-debug:print-backtrace :stream s :count 25))
    (error () "")))

(defun %restart-descriptions (condition)
  (loop :for r :in (compute-restarts condition)
        :collect (list (and (restart-name r) (string (restart-name r)))
                       (handler-case (princ-to-string r) (error () "")))))

(defun %fault (condition &key label id (as 'fault))
  "CONDITION as a fault of class AS. The restarts and the backtrace are the
ones live at the signal, so this is called from a handler."
  (apply #'make-instance as
         :label label
         :condition (handler-case (princ-to-string condition)
                      (error () "<unprintable condition>"))
         :type (string (type-of condition))
         :offered (%restart-descriptions condition)
         :backtrace (%capture-backtrace)
         :root (%root)
         (when id (list :id id))))

(defun %take (condition name)
  "Invoke the restart NAME on CONDITION, or abort when there is no such restart
and when nobody named one."
  (let ((r (or (and name
                    (find name (compute-restarts condition)
                          :key (lambda (x) (and (restart-name x) (string (restart-name x))))
                          :test (lambda (a b) (and a b (string-equal a b)))))
               (find-restart 'abort condition))))
    (when r (invoke-restart r))))

(defun report-failure (condition context)
  "Record CONDITION as a fault named by CONTEXT, and answer it. Nothing waits:
the stack has already gone."
  (note (%fault condition :label context)))

(defun attempt (thunk context)
  "Run THUNK. Answers (values result nil), or (values nil fault) when it fails,
having recorded it under CONTEXT. For a callback pine invokes on behalf of
something else, where the caller still has work to finish.

The fault is built in the handler, before the stack unwinds, so the restarts
and the backtrace it carries are the ones the signalling frames established."
  (let ((fault nil))
    (values (block attempted
              (handler-bind ((error (lambda (c)
                                      (setf fault (report-failure c context))
                                      (return-from attempted nil))))
                (funcall thunk)))
            fault)))

(defun call-with-debugger (thunk &key label)
  "Run THUNK, and when it fails record the fault and unwind out of it. For a
thread pine did not make for this one piece of work: parking one of those parks
a dispatcher worker. Answers THUNK's values, or NIL."
  (with-simple-restart (abort "Abort ~a" (or label "this operation"))
    (handler-bind ((error (lambda (c)
                            (note (%fault c :label label))
                            (%take c nil))))
      (funcall thunk))))

(defmacro with-debugger ((&key label) &body body)
  "Run BODY so that a failure in it becomes a fault at /err and unwinds, rather
than stopping the thread it is on (see CALL-WITH-DEBUGGER)."
  `(call-with-debugger (lambda () ,@body) :label ,label))

;;;; Evaluation: a form on a thread of its own, which is what may park.

(defstruct (evaluation (:constructor %make-evaluation))
  (id (%next-id))
  form
  thunk
  package
  ;; the named-readtable this was read under, as its NAME. A readtable object
  ;; would not survive the trip to another image; a name is a symbol, which is
  ;; what the wire already carries.
  readtable
  (status :running)                 ; :running :ok :error :aborted
  (values nil)
  (output "")
  fault
  thread
  on-done)

(defun %kept-of (ran kept)
  "RAN, newest first, with the oldest finished evaluations dropped once there
are more than KEPT of them. One still running is never dropped."
  (loop :with finished := 0
        :for ev :in ran
        :for done := (not (eq :running (evaluation-status ev)))
        :do (when done (incf finished))
        :unless (and done (> finished kept))
          :collect ev))

(defun %ran (ev)
  "Note EV as one this image started."
  (%change (let ((kept (evaluations-kept)))
             (lambda (r)
               (fset:with r :ran
                          (%kept-of (cons ev (fset:lookup r :ran)) kept)))))
  ev)

(defun list-evaluations ()
  (fset:lookup (%registry) :ran))

(defun find-evaluation (id)
  (find id (list-evaluations) :key #'evaluation-id))

(defun %park (f)
  "Wait on this thread for a restart to be chosen for F, and answer its name.
NIL when the fault went unattended for /park-seconds, which is the caller's cue
to abort. The deadline restarts for as long as it is attended."
  (let* ((seconds (park-seconds))
         (cap (and seconds
                   (round (* seconds internal-time-units-per-second))))
         (deadline (and cap (+ (get-internal-real-time) cap))))
    (loop
      (bordeaux-threads:with-lock-held ((fault-lock f))
        (when (fault-taken f) (return (fault-taken f)))
        (bordeaux-threads:condition-wait (fault-cvar f) (fault-lock f) :timeout 0.5))
      (cond ((attended-p f)
             (when cap (setf deadline (+ (get-internal-real-time) cap))))
            ((and deadline (> (get-internal-real-time) deadline))
             (format *error-output*
                     "pine: nothing decided ~a within ~d unattended second~:p; aborting it~%"
                     (fault-label f) seconds)
             (finish-output *error-output*)
             (return nil))))))

(defun %faulted (ev condition)
  "Record CONDITION as EV's fault and stand in it until someone decides. The
fault carries EV's id, so /err/7 and job 7 name the same evaluation. With
nothing watching /err it takes its abort at once."
  (let ((f (%fault condition :label (or (evaluation-form ev) "an evaluation")
                             :id (evaluation-id ev) :as 'parked)))
    (setf (evaluation-status ev) :error
          (evaluation-fault ev) f)
    (note f)
    (let ((name (and (ns:watched /err) (%park f))))
      (forget f)
      (%take condition name))))

(defmacro with-debugger-hook ((ev) &body body)
  "Bind SBCL's debugger hook so an unhandled error in BODY becomes EV's fault.
This thread is the evaluation, so it is the thread that parks in it."
  `(let ((sb-ext:*invoke-debugger-hook*
           (lambda (c hook) (declare (ignore hook)) (%faulted ,ev c))))
     ,@body))

(defun run-evaluation (ev bindings)
  (let ((out (make-string-output-stream)))
    (unwind-protect
         (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
           (let ((*standard-output* out)
                 (*error-output* out)
                 (*trace-output* out)
                 (*package* (or (evaluation-package ev) (find-package :cl-user)))
                 ;; around the EVAL as well as the read: a macroexpansion or an
                 ;; inner READ in a thunk is still in this buffer's language
                 (*readtable* (%readtable (evaluation-readtable ev))))
             (with-debugger-hook (ev)
               (multiple-value-bind (ok abortp)
                   (with-simple-restart (abort "Abort this evaluation")
                     (setf (evaluation-values ev)
                           (multiple-value-list
                            (if (evaluation-thunk ev)
                                (funcall (evaluation-thunk ev))
                                (eval (evaluation-form ev))))
                           (evaluation-status ev) :ok)
                     t)
                 (declare (ignore ok))
                 (when abortp (setf (evaluation-status ev) :aborted))))))
      (setf (evaluation-output ev) (get-output-stream-string out))
      (when (evaluation-on-done ev)
        ;; under the same bindings as the eval, so a callback reaching for
        ;; *client* works the moment it finishes
        (attempt (lambda ()
                   (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
                     (funcall (evaluation-on-done ev) ev)))
                 "evaluation completion")))))

(defun %readtable (name)
  "The readtable NAME names, or the one in force. A name nothing answers to
degrades rather than failing: a buffer that asks for a readtable this image has
never heard of still evaluates, in the standard one."
  (or (and name (ignore-errors (named-readtables:find-readtable name)))
      *readtable*))

(defun evaluate (form &key thunk (package *package*) readtable bindings on-done)
  "Evaluate FORM (or call THUNK) on a thread of its own. Answers the EVALUATION
at once. BINDINGS is an alist (special-symbol . value) rebound around the eval,
e.g. *client*. ON-DONE runs on the eval thread. READTABLE is a named-readtable
name, so the eval reads what the buffer reads."
  (let ((ev (%ran (%make-evaluation :form form :thunk thunk :package package
                                    :readtable readtable :on-done on-done))))
    (setf (evaluation-thread ev)
          (bordeaux-threads:make-thread
           (lambda () (run-evaluation ev bindings))
           :name (format nil "pine-eval-~a" (evaluation-id ev))))
    ev))

(defun evaluate-thunk (thunk &key (package *package*) readtable bindings on-done)
  "Run THUNK as an evaluation, so a widget's click handler cannot hang or crash
the thread that clicked it."
  (evaluate nil :thunk thunk :package package :readtable readtable
                :bindings bindings :on-done on-done))

(defun evaluate-string (string &key (package *package*) readtable bindings on-done)
  "Read STRING and evaluate it. READTABLE names the language it is written in:
a buffer holding pine's own paths and maps is not read by the standard reader,
and neither is one holding a readtable a config defined."
  (let ((form (let ((*package* (or package *package*))
                    (*readtable* (%readtable readtable)))
                (read-from-string string))))
    (evaluate form :package package :readtable readtable
                   :bindings bindings :on-done on-done)))

(defun abort-evaluation (ev)
  "Abort EV: take the abort on the fault it is parked in, or interrupt the
thread into its abort restart, which is what kills a runaway loop."
  (let ((f (evaluation-fault ev)))
    (if (and f (eq (evaluation-status ev) :error))
        (resume f "ABORT")
        (ignore-errors
         (bordeaux-threads:interrupt-thread
          (evaluation-thread ev)
          (lambda () (ignore-errors (invoke-restart 'abort))))))))


