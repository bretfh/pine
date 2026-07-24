(in-package :pine.test)

(def-suite :pine.agent :in :pine)
(in-suite :pine.agent)

(test cross-image-redefine
  "A real :process agent (its own SBCL image) runs a defun, writes its result
to a file from its own image, is redefined live, and writes again: the second
write proves the redefinition landed in the agent's heap over the eval path."
  (ensure-editor-env)
  (let ((out "/tmp/pine-xtest-out")
        (srv *server*))
    (unless (pine.server:remoting-port srv)
      (sento.remoting:enable-remoting (pine.server:actor-system srv)
                                      :host pine.server:*host* :port 17055)
      (setf (pine.server:remoting-port srv) 17055)
      (pine.actor:start-agent-debug srv))
    (ignore-errors (delete-file out))
    (let ((info (pine.actor:spawn-agent srv "xtest")))
      (is (not (null info)))
      (unwind-protect
           (flet ((ev (form) (pine.actor:agent-eval srv "xtest" form) (sleep 0.6))
                  (readf () (ignore-errors
                             (with-open-file (s out :if-does-not-exist nil)
                               (when s (read s nil nil))))))
             (ev "(defun cross-mark () 1)")
             (ev (format nil "(with-open-file (s ~s :direction :output :if-exists :supersede) (print (cross-mark) s))" out))
             (is (eql 1 (readf)))
             (ev "(defun cross-mark () 2)")
             (ev (format nil "(with-open-file (s ~s :direction :output :if-exists :supersede) (print (cross-mark) s))" out))
             (is (eql 2 (readf))))
        (pine.actor:kill-agent srv "xtest")))))
