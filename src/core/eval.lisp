(defpackage #:pine.eval
  (:use #:cl)
  (:export #:evaluation #:evaluation-id #:evaluation-form #:evaluation-status
           #:evaluation-values #:evaluation-output #:evaluation-condition
           #:evaluation-condition-type #:evaluation-restarts #:evaluation-backtrace
           #:evaluate #:evaluate-string #:evaluate-thunk
           #:list-evaluations #:find-evaluation
           #:pick-restart #:abort-evaluation #:*on-debug*
           #:with-debugger #:call-with-debugger #:make-error-evaluation
           #:attempt #:report-failure))

(in-package #:pine.eval)

(defvar *on-debug* nil
  "Default surface for an evaluation that enters the debugger: a function of the
evaluation, installed by the editor. Any eval path (C-x C-e, repl, widget
onclick, remote) reaches the same debugger through it.")

;;;; Evaluation as a first-class, off-UI-thread operation. Each eval runs on its
;;;; own thread (so it can block or run forever without touching the editor) and
;;;; produces one uniform EVALUATION object -- form, package, status, values,
;;;; captured output, and on error the condition/restarts/backtrace. An error
;;;; does not drop to a console debugger: the worker captures the restarts and
;;;; BLOCKS, and a surface picks one (invoke-restart) or aborts. Same object for
;;;; C-x C-e, the repl, a widget onclick, or a remote agent.

(defvar *evaluations* nil)
(defvar *counter* 0)
(defvar *current* nil)
(defvar *registry-lock* (bordeaux-threads:make-lock "pine-eval-registry")
  "Guards *evaluations* and *counter*, since any thread (command path, repl,
agent) may start an evaluation concurrently.")

(defstruct (evaluation (:constructor %make-evaluation))
  (id (incf *counter*))
  form
  thunk
  package
  (status :running)                 ; :running :ok :error :aborted
  (values nil)
  (output "")
  condition
  condition-type
  restarts                          ; list of (name-string report-string)
  backtrace
  thread
  on-done
  on-error
  (lock (bordeaux-threads:make-lock))
  (cvar (bordeaux-threads:make-condition-variable))
  (chosen nil))

;;;; Failures on the paths that cannot stop to ask.
;;;;
;;;; The debugger below runs an evaluation in its own thread and waits for a
;;;; restart to be chosen. A ref notify, a surface build or a broadcast to
;;;; every attached app cannot wait for that, and must not vanish either. So
;;;; the condition becomes a value the caller decides about, and is recorded
;;;; where the debug surface shows it.

(defun report-failure (condition context)
  "Record CONDITION as a failed evaluation named by CONTEXT, and show it.

Returns the evaluation. Nothing blocks: there are no restarts to choose from
by the time this is called, so the surface only has to display it."
  (let ((ev (%make-evaluation :form context :status :error
                              :condition (handler-case (princ-to-string condition)
                                           (error () "<unprintable condition>"))
                              :condition-type (string (type-of condition))
                              :backtrace (%capture-backtrace))))
    (bordeaux-threads:with-lock-held (*registry-lock*)
      (push ev *evaluations*))
    (let ((surface *on-debug*))
      (if surface
          (handler-case (funcall surface ev)
            (error (c)
              (format *error-output* "pine: the error surface failed on ~a: ~a~%"
                      context c)
              (finish-output *error-output*)))
          (progn
            (format *error-output* "pine: ~a: ~a~%" context condition)
            (finish-output *error-output*))))
    ev))

(defun attempt (thunk context)
  "Run THUNK. Returns (values result nil), or (values nil condition) when it
fails, having reported it under CONTEXT.

For the callbacks pine invokes on behalf of something else -- a configuration's
builder, a subscriber, a source -- where the failure belongs to that thing and
the caller still has work to finish."
  (handler-case (values (funcall thunk) nil)
    (error (c) (values nil (report-failure c context)))))

(defun list-evaluations ()
  (bordeaux-threads:with-lock-held (*registry-lock*) (copy-list *evaluations*)))
(defun find-evaluation (id)
  (bordeaux-threads:with-lock-held (*registry-lock*)
    (find id *evaluations* :key #'evaluation-id)))

(defun %capture-backtrace ()
  (handler-case
      (with-output-to-string (s) (sb-debug:print-backtrace :stream s :count 25))
    (error () "")))

(defun %restart-descriptions (condition)
  (loop for r in (compute-restarts condition)
        collect (list (and (restart-name r) (string (restart-name r)))
                      (handler-case (princ-to-string r) (error () "")))))

(defun make-error-evaluation (condition &key label)
  "Record CONDITION into a fresh EVALUATION (status :error, restarts, backtrace)
WITHOUT blocking -- for surfacing an error on a thread that must not park, e.g.
the session command loop. Called from a handler-bind so the restarts/backtrace
are those live at the signal. Hand the result to *on-debug* to open the menu."
  (let ((ev (%make-evaluation :form label)))
    (setf (evaluation-status ev) :error
          (evaluation-condition ev) (handler-case (princ-to-string condition)
                                      (error () "<unprintable condition>"))
          (evaluation-condition-type ev) (string (type-of condition))
          (evaluation-restarts ev) (%restart-descriptions condition)
          (evaluation-backtrace ev) (%capture-backtrace))
    ev))

(defun eval-debugger-hook (ev condition)
  "Runs on the eval thread when an unhandled error propagates. Records the
condition + restarts + backtrace, notifies the surface, then blocks until a
restart is chosen and invokes it."
  (setf (evaluation-status ev) :error
        (evaluation-condition ev) (handler-case (princ-to-string condition)
                                    (error () "<unprintable condition>"))
        (evaluation-condition-type ev) (string (type-of condition))
        (evaluation-restarts ev) (%restart-descriptions condition)
        (evaluation-backtrace ev) (%capture-backtrace))
  (let ((surface (or (evaluation-on-error ev) *on-debug*)))
    ;; No surface can drive a restart choice: abort immediately rather than block
    ;; this thread forever. A hung worker/actor is exactly the wedge we avoid.
    (unless surface
      (let ((r (find-restart 'abort condition)))
        (when r (invoke-restart r)))
      (return-from eval-debugger-hook))
    (ignore-errors (funcall surface ev)))
  (let ((name (bordeaux-threads:with-lock-held ((evaluation-lock ev))
                (loop until (evaluation-chosen ev)
                      do (bordeaux-threads:condition-wait
                          (evaluation-cvar ev) (evaluation-lock ev)))
                (evaluation-chosen ev))))
    (let ((r (or (find name (compute-restarts condition)
                       :key (lambda (x) (and (restart-name x) (string (restart-name x))))
                       :test (lambda (a b) (and a b (string-equal a b))))
                 (find-restart 'abort condition))))
      (when r (invoke-restart r)))))

(defmacro with-debugger-hook ((ev) &body body)
  "Bind SBCL's debugger hook so an unhandled error in BODY is captured into EV
and surfaced through *on-debug*, blocking this (off-session) thread until a
restart is picked. The single debugger-entry codepath -- eval workers, buffer
actors, anything off the session input thread routes through here."
  `(let ((sb-ext:*invoke-debugger-hook*
           (lambda (c hook) (declare (ignore hook)) (eval-debugger-hook ,ev c))))
     ,@body))

(defun call-with-debugger (thunk &key label package)
  "Run THUNK under the pine debugger with an always-available abort restart: an
unhandled error opens the *debugger* restart menu (via *on-debug*) and blocks
THIS thread until a restart is picked; `abort' unwinds out of THUNK. For threads
OTHER than the session input thread (eval workers, buffer actors) -- blocking
parks that thread while the surface drives the choice. Returns THUNK's values,
NIL on abort.

Uses handler-bind, not *invoke-debugger-hook*: an actor's message pump may trap
errors around receive, so we must catch in the signal's own dynamic context
(innermost handler wins) before anything upstream sees it. eval-debugger-hook is
still the single shared debugger-entry."
  (let ((ev (%make-evaluation :form label :package package)))
    (with-simple-restart (abort "Abort ~a" (or label "this operation"))
      (handler-bind ((error (lambda (c) (eval-debugger-hook ev c))))
        (funcall thunk)))))

(defmacro with-debugger ((&key label package) &body body)
  "Run BODY off the session thread under the pine debugger (see
call-with-debugger)."
  `(call-with-debugger (lambda () ,@body) :label ,label :package ,package))

(defun run-evaluation (ev bindings)
  (let ((*current* ev)
        (out (make-string-output-stream)))
    (unwind-protect
         (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
           (let ((*standard-output* out)
                 (*error-output* out)
                 (*trace-output* out)
                 (*package* (or (evaluation-package ev) (find-package :cl-user))))
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
        ;; under the same bindings as the eval, so a callback that reaches
        ;; for *client* (echo + repaint) works the moment the eval finishes
        (attempt (lambda ()
                   (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
                     (funcall (evaluation-on-done ev) ev)))
                 "evaluation completion")))))

(defun evaluate (form &key thunk (package *package*) bindings on-done on-error)
  "Evaluate FORM (or call THUNK) on a fresh thread. Returns the EVALUATION
immediately. BINDINGS is an alist (special-symbol . value) rebound around the
eval (e.g. *client*). ON-DONE / ON-ERROR run on the eval thread."
  (let ((ev (bordeaux-threads:with-lock-held (*registry-lock*)
              ;; id assignment (incf *counter*) and the push share the lock so
              ;; concurrent starts get distinct ids and no lost entries.
              (let ((ev (%make-evaluation :form form :thunk thunk :package package
                                          :on-done on-done :on-error on-error)))
                (push ev *evaluations*)
                ev))))
    (setf (evaluation-thread ev)
          (bordeaux-threads:make-thread
           (lambda () (run-evaluation ev bindings))
           :name (format nil "pine-eval-~a" (evaluation-id ev))))
    ev))

(defun evaluate-thunk (thunk &key (package *package*) bindings on-done on-error)
  "Run THUNK (a nullary function) as an evaluation -- a widget onclick handler
can't hang or crash the UI thread."
  (evaluate nil :thunk thunk :package package :bindings bindings
                :on-done on-done :on-error on-error))

(defun evaluate-string (string &key (package *package*) bindings on-done on-error)
  (let ((form (let ((*package* (or package *package*)))
                (read-from-string string))))
    (evaluate form :package package :bindings bindings
                   :on-done on-done :on-error on-error)))

(defun pick-restart (ev name)
  "Resume a debugging evaluation by invoking the restart named NAME."
  (bordeaux-threads:with-lock-held ((evaluation-lock ev))
    (setf (evaluation-chosen ev) (string name))
    (bordeaux-threads:condition-notify (evaluation-cvar ev))))

(defun abort-evaluation (ev)
  "Abort EV: if it is in the debugger, invoke its abort restart; otherwise
interrupt the worker into its abort restart (kills a runaway loop)."
  (if (eq (evaluation-status ev) :error)
      (pick-restart ev "ABORT")
      (ignore-errors
       (bordeaux-threads:interrupt-thread
        (evaluation-thread ev)
        (lambda () (ignore-errors (invoke-restart 'abort)))))))
