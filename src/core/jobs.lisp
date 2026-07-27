(defpackage #:pine.core.jobs
  (:use #:cl)
  (:export #:list-jobs #:*agent-jobs*))

(in-package #:pine.core.jobs)

;;;; The jobs surface's data: every live evaluation across the daemon and every
;;;; agent, in one place. Local evals come from pine.err's registry; agent evals
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

(let ((prev pine.core.actor:*agent-debug-hook*))
  (setf pine.core.actor:*agent-debug-hook*
        (lambda (msg)
          (when prev
            (pine.err:attempt (lambda () (funcall prev msg)) "agent debug relay"))
          (pine.err:attempt (lambda () (record msg)) "jobs record"))))

(defun list-jobs ()
  "Every live job: the daemon's local evaluations plus every agent's reported
evals, as a flat list of plists (:agent :id :status ...)."
  (append
   (mapcar (lambda (ev)
             (list :agent "local" :id (pine.err:evaluation-id ev)
                   :status (pine.err:evaluation-status ev)
                   :form (princ-to-string (pine.err:evaluation-form ev))))
           (pine.err:list-evaluations))
   (loop for v being the hash-values of *agent-jobs* collect v)))
