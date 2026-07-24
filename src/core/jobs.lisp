(in-package #:pine.jobs)

;;;; The jobs surface's data: every live evaluation across the daemon and every
;;;; agent, in one place. Local evals come from pine.eval's registry; agent evals
;;;; are reported here by their :agent-debug / :agent-result messages (the same
;;;; ones that drive the cross-image debugger). The editor renders this list as a
;;;; buffer where an errored job opens its restarts and a running one can be
;;;; aborted -- many agents reporting to one place.

(defvar *agent-jobs* (make-hash-table :test 'equal)
  "(agent-name . eval-id) -> a job plist reported by a process agent.")

(defun record (msg)
  (destructuring-bind (tag &key agent eval-id restarts condition status values
                       &allow-other-keys)
      msg
    (let ((key (cons agent eval-id)))
      (case tag
        (:agent-debug
         (setf (gethash key *agent-jobs*)
               (list :agent agent :id eval-id :status :error
                     :restarts restarts :condition condition)))
        (:agent-result
         (setf (gethash key *agent-jobs*)
               (list :agent agent :id eval-id :status status :values values)))))))

(defun install-jobs ()
  "Route process-agent job reports into the jobs registry, chaining any hook
already installed (the editor's cross-image debugger surfacing) so both fire."
  (let ((prev pine.actor:*agent-debug-hook*))
    (setf pine.actor:*agent-debug-hook*
          (lambda (msg)
            (when prev (ignore-errors (funcall prev msg)))
            (ignore-errors (record msg))))))

(defun list-jobs ()
  "Every live job: the daemon's local evaluations plus every agent's reported
evals, as a flat list of plists (:agent :id :status ...)."
  (append
   (mapcar (lambda (ev)
             (list :agent "local" :id (pine.eval:evaluation-id ev)
                   :status (pine.eval:evaluation-status ev)
                   :form (princ-to-string (pine.eval:evaluation-form ev))))
           (pine.eval:list-evaluations))
   (loop for v being the hash-values of *agent-jobs* collect v)))
