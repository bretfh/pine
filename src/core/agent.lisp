(defpackage #:pine.core.agent
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:err #:pine.err) (#:d #:pine.data))
  (:export #:connect #:serve #:*agent-system* #:*name*))

(in-package #:pine.core.agent)
(named-readtables:in-readtable pine.path:syntax)

;;;; A process agent's own code. The image loads :pine and calls CONNECT: it
;;;; enables remoting, runs evals through pine.err and reports what its /err
;;;; holds to the master by name. The eval stands in its own thread holding the
;;;; live restarts; the master picks a name and sends :resume. A fault and the
;;;; job it stopped carry one id, so :eval, :agent-debug and :agent-result all
;;;; name the same thing.

(defvar *agent-system* nil)

(defvar *name* nil)

(defvar *master-debug* nil "remote-ref to the master's agent-debug actor.")

(defun %report-home (name)
  "Tell the master every fault, then the whole set. Nothing here remembers what
was sent, so two threads faulting at once cannot lose a report between them, and
a fault decided over there stops being one here by being missing from the next
set."
  (let ((here (err:faults)))
    (handler-case
        (progn
          (dolist (f here)
            (sento.actor:tell *master-debug*
              (list :agent-debug :agent name :eval-id (err:fault-id f)
                    :condition (err:fault-condition f)
                    :restarts (mapcar #'first (err:offers f)))))
          (sento.actor:tell *master-debug*
            (list :agent-faults :agent name
                  :ids (mapcar #'err:fault-id here))))
      (error (c)
        (format *error-output*
                "pine agent ~a: could not report a fault home: ~a~%" name c)
        (finish-output *error-output*)))))

(defun serve (sys &key name master-host master-port self-port)
  "Make a remoting-enabled SYS addressable as agent NAME: the :eval/:resume
actor, the watch that reports faults home, and a registration with the master.
Any image with a system of its own can take this: a spawned agent, or a frontend
serving itself up for eval and debugging."
  (setf *name* name)
  (setf *master-debug*
        (sento.remoting:make-remote-ref
         sys (pine.core.server:daemon-uri "agent-debug" :host master-host :port master-port)))
  ;; watching /err is also what tells pine.err that something in this image is
  ;; in a position to decide, so an eval here parks holding its restarts
  ;; instead of taking its abort for want of anyone to ask
  (ns:watch /err (lambda (value) (declare (ignore value)) (%report-home name))
            :as :fault-home)
  (sento.actor-context:actor-of sys :name "agent"
    :dispatcher :pinned
    :receive
    (lambda (msg)
      (case (first msg)
        (:ping (sento.actor:reply :pong))
        (:eval
         (destructuring-bind (&key form package readtable bindings
                              &allow-other-keys)
             (rest msg)
           (let* ((pkg (or (and package (find-package package)) (find-package :cl-user)))
                  (ev (err:evaluate-string
                       form :package pkg :readtable readtable :bindings bindings
                       :on-done
                       (lambda (ev)
                         (err:attempt
                          (lambda ()
                            (sento.actor:tell *master-debug*
                              (list :agent-result :agent name
                                    :eval-id (err:evaluation-id ev)
                                    :status (err:evaluation-status ev)
                                    :values (mapcar #'prin1-to-string
                                                    (err:evaluation-values ev)))))
                          "agent result home")))))
             (sento.actor:reply (list :started (err:evaluation-id ev))))))
        (:resume
         (destructuring-bind (&key eval-id restart) (rest msg)
           (let ((f (err:faults eval-id)))
             (when f (err:resume f restart)))
           (sento.actor:reply :resumed)))
        (:shutdown
         ;; kill-agent means the process ends, not just a reply
         (sento.actor:reply :ok)
         (bordeaux-threads:make-thread
          (lambda () (sleep 0.2) (sb-ext:exit :code 0 :abort t))
          :name "agent-shutdown"))
        (:crash (sb-ext:exit :abort t))
        (t (sento.actor:reply :unknown)))))
  (sento.actor:tell
   (sento.remoting:make-remote-ref
    sys (pine.core.server:daemon-uri "agent-registry" :host master-host :port master-port))
   (list :register-remote :name name :host pine.core.server:*host* :port self-port))
  sys)

(defun connect (&key name master-host master-port self-port)
  (let ((sys (sento.actor-system:make-actor-system
               pine.core.server:*app-actor-config*)))
    (setf *agent-system* sys)
    (sento.remoting:enable-remoting sys :host pine.core.server:*host* :port self-port)
    (serve sys :name name :master-host master-host :master-port master-port
               :self-port self-port)))
