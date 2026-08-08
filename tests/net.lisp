(in-package :pine.test)

(def-suite* :pine.net :in :pine)

(defmacro with-server ((var &key (remoting 0)) &body body)
  `(let ((,var (pine.net.server:start-server :workers 2 :remoting-port ,remoting)))
     (unwind-protect (progn ,@body)
       (pine.net.server:stop-server ,var))))

(test an-image-serves-a-remoting-port
  (with-server (s)
    (is (typep s 'pine.net.server:server))
    (is (integerp (pine.net.server:remoting-port s)))
    (is (plusp (pine.net.server:remoting-port s)))))

(test the-wire-refuses-a-version-it-does-not-know
  (is-true (pine.net.attach:acceptable (pine.net.attach:protocol)))
  (is-false (pine.net.attach:acceptable "ns1/0.0.1"))
  (is (search "ns2" (pine.net.attach:protocol))))

(test a-kind-is-declared-and-a-frontend-says-how-it-runs
  (unwind-protect
       (progn
         (pine.net.attach:app :probe-kind
                              (fset:map (:attached (lambda (c) (list :got c)))))
         (is (member :probe-kind (pine.net.attach:kinds)))
         (is (equal '(:got :a-client) (pine.net.attach:attached :probe-kind :a-client)))
         (is (null (pine.net.attach:received :probe-kind :a-client :m))
             "a kind that declares no receive answers nothing")
         (let ((said (with-output-to-string (*standard-output*)
                       (pine.net.attach:run-frontend :probe-kind))))
           (is (search "no probe-kind frontend" said)))
         (pine.net.attach:frontend :probe-kind (lambda () :ran))
         (is (eq :ran (pine.net.attach:run-frontend :probe-kind))))
    (pine.net.attach:frontend :probe-kind nil)))

(test a-second-image-answers-what-it-was-asked
  "An agent was always a process. This is the process speaking a protocol: pine
spawns an sbcl, it attaches back, and an evaluation crosses as data."
  (with-server (s :remoting 0)
    (let ((sys (pine.net.server:actor-system s)))
      (pine.net.agent:answer-for sys :name "probe-agent")
      (let* ((uri (pine.net.server:local-uri
                   "probe-agent" (pine.net.server:remoting-port s)))
             (there (sento.remoting:make-remote-ref sys uri))
             (a (pine.net.agent:register "probe" there :uri uri)))
        (unwind-protect
             (progn
               (is (equal a (pine.net.agent:agent-named "probe")))
               (let ((answer (pine.net.agent:evaluate-there a '(+ 2 2))))
                 (is (equal '(4) (getf answer :answered)))
                 (is (null (getf answer :fault))))
               (let ((answer (pine.net.agent:evaluate-there a '(princ "over here"))))
                 (is (equal "over here" (getf answer :said))))
               (let ((answer (pine.net.agent:evaluate-there a '(error "over there"))))
                 (is (search "over there" (getf answer :fault))
                     "a fault there comes back as a value here")))
          (pine.net.agent:forget "probe"))))))

(test a-session-on-the-far-side-is-a-session
  "Evaluating in another image is (evaluate session form) with the session
remote. Nothing above it knows the difference."
  (with-server (s :remoting 0)
    (let ((sys (pine.net.server:actor-system s)))
      (pine.net.agent:answer-for sys :name "probe-session")
      (let* ((uri (pine.net.server:local-uri
                   "probe-session" (pine.net.server:remoting-port s)))
             (a (pine.net.agent:register "probe-session"
                                         (sento.remoting:make-remote-ref sys uri)
                                         :uri uri))
             (remote (pine.net.agent:open-remote a)))
        (unwind-protect
             (progn
               (is (typep remote 'pine.repl.session:session))
               (let ((e (pine.repl.session:evaluate remote '(* 6 7))))
                 (is (equal '(42) (pine.repl.session:answered e)))
                 (is (null (pine.repl.session:fault e))))
               (pine.repl.session:evaluate remote '(+ 1 1))
               (is (= 2 (length (pine.repl.session:history remote)))
                   "the history is the session's, wherever it evaluates"))
          (pine.net.agent:forget "probe-session"))))))

(test a-client-that-is-gone-gives-its-kind-back
  (with-server (s)
    (let ((c (make-instance 'pine.net.attach:client :id 1 :kind :probe-kind
                                                    :display nil)))
      (push c pine.net.attach:*clients*)
      (push c (pine.net.server:clients s))
      (is (pine.net.attach:attached-p :probe-kind))
      (pine.net.attach:reap c s)
      (is-false (pine.net.attach:attached-p :probe-kind))
      (is (null (pine.net.server:clients s))))))

(test a-daemon-listens-and-says-what-is-attached
  (unwind-protect
       (let ((image (pine:daemon :remoting 0)))
         (is (typep image 'pine.net.server:server))
         (is (plusp (pine.net.server:remoting-port image)))
         (is (null (pine.repl.command:run "agents")) "nothing has attached yet")
         (is (member "agents" (pine.fs.tree:listing
                               (pine.world.world:at pine.world.world:*world* "cmd"))
                     :test #'equal)
             "the daemon's own commands are nodes like any other"))
    (pine:stop)))

(test the-daemon-reads-an-init-file-and-what-it-declares-is-there
  (unwind-protect
       (let ((config (merge-pathnames "probe-init.lisp"
                                      (asdf:system-relative-pathname :pine "tests/"))))
         (pine:daemon :remoting 0 :config config)
         (is (eq t (pine.fs.node:contents
                    (pine.world.world:at pine.world.world:*world* "config/loaded")))
             "the config ran")
         (is (equal "hello from the config" (pine.repl.command:run "hello"))
             "a command a config defined is a command")
         (is (equal "hello"
                    (pine.repl.command:name
                     (pine.repl.mode:binding
                      (pine.edit.buffer:current) "C-c h")))
             "and a chord a config bound reaches it")
         (is (member "hello" (pine.fs.tree:listing
                              (pine.world.world:at pine.world.world:*world* "cmd"))
                     :test #'equal)
             "it is a node under /cmd like any other"))
    (pine:stop)))

(test a-config-that-will-not-load-is-reported-and-the-daemon-comes-up
  (let ((bad (merge-pathnames "pine-probe-bad-init.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out bad :direction :output :if-exists :supersede)
             (write-string "(in-package :pine) (this-is-not-a-function)" out))
           (pine:daemon :remoting 0 :config bad)
           (is (typep pine:*image* 'pine.net.server:server)
               "a broken config does not stop the daemon coming up")
           (is (find-if (lambda (f) (search "init" (or (pine.run.fault:label f) "")))
                        (pine.run.fault:faults))
               "and what went wrong is a fault, not a backtrace on the terminal"))
      (pine:stop)
      (ignore-errors (delete-file bad)))))

(test a-frontend-is-a-declaration-the-supervisor-keeps-running
  (unwind-protect
       (progn
         (pine:daemon :remoting 0 :config nil)
         (let ((p (pine:frontend "editor")))
           (is (typep p 'pine.proc.process:program))
           (is (equal "editor" (pine.proc.process:name p)))
           (is (find-if (lambda (entry) (search "PINE_PORT=" entry))
                        (pine.proc.process:env p))
               "the frontend is told which pine to attach to")))
    (pine:stop)))
