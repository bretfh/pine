(defpackage #:pine.agent
  (:use #:cl)
  (:export #:connect #:*agent-system* #:*name*))

(in-package #:pine.agent)

;;;; A process agent's own code. A spawned SBCL image loads :pine and calls
;;;; connect: it enables remoting, runs evals through pine.eval (the same engine
;;;; the daemon uses), and -- crucially -- routes any error's restarts HOME to
;;;; the master by name. The eval blocks in its own thread holding the live
;;;; restarts; the master (the editor, the helm) picks a name and sends :resume;
;;;; the agent invokes that restart locally. Move the decision, not the handler.

(defvar *agent-system* nil)
(defvar *name* nil)
(defvar *evals* (make-hash-table) "eval-id -> evaluation, for :resume by name.")
(defvar *ev-ids* (make-hash-table :test 'eq) "evaluation -> eval-id, for *on-debug*.")
(defvar *counter* 0)
(defvar *master-debug* nil "remote-ref to the master's agent-debug actor.")

(defun connect (&key name master-host master-port self-port)
  (setf *name* name)
  (let ((sys (sento.actor-system:make-actor-system
              '(:dispatchers (:shared (:workers 2 :strategy :random))))))
    (setf *agent-system* sys)
    (sento.remoting:enable-remoting sys :host pine.server:*host* :port self-port)
    (setf *master-debug*
          (sento.remoting:make-remote-ref
           sys (pine.server:daemon-uri "agent-debug" :host master-host :port master-port)))
    ;; an error in this image ships its restart list home, by name
    (setf pine.eval:*on-debug*
          (lambda (ev)
            (ignore-errors
             (sento.actor:tell *master-debug*
               (list :agent-debug :agent name :eval-id (gethash ev *ev-ids*)
                     :condition (princ-to-string (pine.eval:evaluation-condition ev))
                     :restarts (mapcar #'first (pine.eval:evaluation-restarts ev)))))))
    (sento.actor-context:actor-of sys :name "agent"
      :receive
      (lambda (msg)
        (case (first msg)
          (:ping (sento.actor:reply :pong))
          (:eval
           (destructuring-bind (&key form package &allow-other-keys) (rest msg)
             (let* ((id (incf *counter*))
                    (pkg (or (and package (find-package package)) (find-package :cl-user)))
                    (ev (pine.eval:evaluate-string
                         form :package pkg
                         :on-done
                         (lambda (ev)
                           (ignore-errors
                            (sento.actor:tell *master-debug*
                              (list :agent-result :agent name :eval-id id
                                    :status (pine.eval:evaluation-status ev)
                                    :values (mapcar #'prin1-to-string
                                                    (pine.eval:evaluation-values ev)))))))))
               (setf (gethash id *evals*) ev (gethash ev *ev-ids*) id)
               (sento.actor:reply (list :started id)))))
          (:resume
           (destructuring-bind (&key eval-id restart) (rest msg)
             (let ((ev (gethash eval-id *evals*)))
               (when ev (pine.eval:pick-restart ev restart)))
             (sento.actor:reply :resumed)))
          (:crash (sb-ext:exit :abort t))
          (t (sento.actor:reply :unknown)))))
    (sento.actor:tell
     (sento.remoting:make-remote-ref
      sys (pine.server:daemon-uri "agent-registry" :host master-host :port master-port))
     (list :register-remote :name name :host pine.server:*host* :port self-port))
    sys))
