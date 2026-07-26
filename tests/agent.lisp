(in-package :pine.test)

(def-suite* :pine.agent :in :pine)

(defun agent-file-read (path)
  (with-open-file (s path :if-does-not-exist nil)
    (when s (read s nil nil))))

(test a-process-agent-redefines-a-function-in-its-own-image
  (with-fixture substrate ()
    (ensure-remoting)
    (let ((out "/tmp/pine-agent-probe")
          (server *server*))
      (uiop:delete-file-if-exists out)
      (let ((info (pine.core.actor:spawn-agent server "probe-agent")))
        (is (not (null info)))
        (unwind-protect
             (flet ((evaluate (form)
                      (pine.core.actor:agent-eval server "probe-agent" form)
                      (sleep 0.6)))
               (evaluate "(defun cross-mark () 1)")
               (evaluate (format nil "(with-open-file (s ~s :direction :output ~
:if-exists :supersede) (print (cross-mark) s))" out))
               (is (= 1 (agent-file-read out)))
               (evaluate "(defun cross-mark () 2)")
               (evaluate (format nil "(with-open-file (s ~s :direction :output ~
:if-exists :supersede) (print (cross-mark) s))" out))
               (is (= 2 (agent-file-read out))))
          (pine.core.actor:kill-agent server "probe-agent"))))))

(test an-agent-registers-itself-and-answers-a-ping
  (with-fixture substrate ()
    (ensure-remoting)
    (let ((server *server*))
      (unwind-protect
           (progn
             (pine.core.actor:spawn-agent server "ping-agent")
             (is (not (null (pine.core.actor:find-agent server "ping-agent"))))
             (is (member "ping-agent"
                         (mapcar #'pine.core.actor:agent-info-name
                                 (pine.core.actor:list-agents server))
                         :test #'string=))
             (is-true (pine.core.actor:agent-alive-p server "ping-agent")))
        (pine.core.actor:unsupervise-agent "ping-agent")
        (pine.core.actor:kill-agent server "ping-agent")))))

(test killing-an-agent-unregisters-it
  (with-fixture substrate ()
    (ensure-remoting)
    (let ((server *server*))
      (pine.core.actor:spawn-agent server "gone-agent")
      (pine.core.actor:unsupervise-agent "gone-agent")
      (pine.core.actor:kill-agent server "gone-agent")
      (is (null (pine.core.actor:find-agent server "gone-agent")))
      (is-false (pine.core.actor:agent-alive-p server "gone-agent")))))

(test the-local-agent-is-registered-and-evaluates-in-this-image
  (with-fixture substrate ()
    (let ((done :none))
      (is (not (null (pine.core.actor:find-agent *server* "local"))))
      (pine.core.actor:agent-eval *server* "local" "(+ 20 22)"
                                  :on-done (lambda (ev)
                                             (setf done (first (pine.core.eval:evaluation-values ev)))))
      (sleep 0.4)
      (is (= 42 done)))))

(test the-registry-brokers-an-agent-and-a-buffer-by-name
  (with-fixture substrate ()
    (is (not (null (pine.core.actor:request *server* :agent "local"))))
    (is (not (null (pine.core.actor:request *server* :buffer "scratch"))))
    (is (null (pine.core.actor:request *server* :agent "no-such-agent")))))

(test spawning-without-remoting-is-an-error-not-a-silence
  (let ((server (pine.core.server:start-server)))
    (unwind-protect
         (signals error (pine.core.actor:spawn-agent server "no-remoting"))
      (pine.core.server:stop-server server))))
