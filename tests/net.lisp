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
               (is (typep remote 'pine/repl/session:session))
               (let ((e (pine/repl/session:evaluate remote '(* 6 7))))
                 (is (equal '(42) (pine/repl/session:answered e)))
                 (is (null (pine/repl/session:fault e))))
               (pine/repl/session:evaluate remote '(+ 1 1))
               (is (= 2 (length (pine/repl/session:history remote)))
                   "the history is the session's, wherever it evaluates"))
          (pine.net.agent:forget "probe-session"))))))

(test a-client-that-is-gone-gives-its-kind-back
  (with-server (s)
    (let ((c (make-instance 'pine.net.attach:client :id 1 :kind :probe-kind
                                                    :display nil)))
      (pine/data:swap! pine.net.attach:*clients* (lambda (all) (cons c all)))
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
         (is (null (pine/repl/command:run "agents")) "nothing has attached yet")
         (is (member "agents" (pine/fs/tree:listing
                               (pine/world/world:at pine/world/world:*world* "cmd"))
                     :test #'equal)
             "the daemon's own commands are nodes like any other"))
    (pine:stop)))

(test the-daemon-reads-an-init-file-and-what-it-declares-is-there
  (unwind-protect
       (let ((config (merge-pathnames "probe-init.lisp"
                                      (asdf:system-relative-pathname :pine "tests/"))))
         (pine:daemon :remoting 0 :config config)
         (is (eq t (pine/fs/node:contents
                    (pine/world/world:at pine/world/world:*world* "config/loaded")))
             "the config ran")
         (is (equal "hello from the config" (pine/repl/command:run "hello"))
             "a command a config defined is a command")
         (is (equal "hello"
                    (pine/repl/command:name
                     (pine/repl/mode:binding
                      (pine.edit.buffer:current) "C-c h")))
             "and a chord a config bound reaches it")
         (is (member "hello" (pine/fs/tree:listing
                              (pine/world/world:at pine/world/world:*world* "cmd"))
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
           (is (find-if (lambda (f) (search "init" (or (pine/run/fault:label f) "")))
                        (pine/run/fault:faults))
               "and what went wrong is a fault, not a backtrace on the terminal"))
      (pine:stop)
      (ignore-errors (delete-file bad)))))

(test a-frontend-is-a-declaration-the-supervisor-keeps-running
  (unwind-protect
       (progn
         (pine:daemon :remoting 0 :config nil)
         (let ((p (pine:frontend "editor")))
           (is (typep p 'pine/proc/process:program))
           (is (equal "editor" (pine/proc/process:name p)))
           (is (find-if (lambda (entry) (search "PINE_PORT=" entry))
                        (pine/proc/process:env p))
               "the frontend is told which pine to attach to")))
    (pine:stop)))

(test a-client-attaches-over-the-wire-and-the-daemon-holds-it
  "The handshake, end to end: a client with its own actor system tells the
daemon's attach actor, the daemon accepts, and both sides see it. Nothing below
this was tested before, which is why the message shape was wrong."
  (let ((seen nil))
    (unwind-protect
         (progn
           (pine:daemon :remoting 0 :config nil)
           (pine.net.attach:app :probe-front
                                (fset:map (:attached (lambda (c) (push c seen)))))
           (let ((client (pine.net.server:start-server :workers 2 :remoting-port 0))
                 (answered (pine/data:box nil)))
             (unwind-protect
                  (let ((sys (pine.net.server:actor-system client)))
                    (sento.actor-context:actor-of sys
                      :name "display"
                      :dispatcher :pinned
                      :receive (lambda (m) (pine/data:put! answered m)))
                    (pine.net.attach:attach-to
                     sys
                     (pine.net.server:daemon-uri
                      "attach" :port (pine.net.server:remoting-port pine:*image*))
                     (pine.net.server:local-uri
                      "display" (pine.net.server:remoting-port client))
                     :kind :probe-front)
                    (is-true (wait-until (lambda () (pine/data:held answered)))
                             "the daemon never answered the attach")
                    (let ((reply (pine/data:held answered)))
                      (is (eq :attached (first reply))
                          "the daemon refused or said something else: ~s" reply)
                      (is (integerp (getf (rest reply) :id))))
                    (is (= 1 (length (pine.net.attach:clients :probe-front)))
                        "the daemon is not holding the client")
                    (is (= 1 (length seen)) "the kind's :attached never ran"))
               (pine.net.server:stop-server client))))
      (pine:stop))))

(test a-client-built-against-another-wire-is-told-so
  (unwind-protect
       (progn
         (pine:daemon :remoting 0 :config nil)
         (let ((client (pine.net.server:start-server :workers 2 :remoting-port 0))
               (answered (pine/data:box nil)))
           (unwind-protect
                (let ((sys (pine.net.server:actor-system client)))
                  (sento.actor-context:actor-of sys
                    :name "display" :dispatcher :pinned
                    :receive (lambda (m) (pine/data:put! answered m)))
                  (sento.actor:tell
                   (sento.remoting:make-remote-ref
                    sys (pine.net.server:daemon-uri
                         "attach" :port (pine.net.server:remoting-port pine:*image*)))
                   (list :attach :kind :probe-old :version "ns1/0.0.1"
                         :uri "x" :display (pine.net.server:local-uri
                                            "display"
                                            (pine.net.server:remoting-port client))))
                  (is-true (wait-until (lambda () (pine/data:held answered))))
                  (is (eq :refused (first (pine/data:held answered)))
                      "an old frontend should be told plainly, not left painting"))
             (pine.net.server:stop-server client))))
    (pine:stop)))

(test attaching-as-an-editor-gets-a-frame-that-rebuilds-into-widgets
  "What nothing renders means: the daemon must push a frame the frontend can
turn back into a widget tree. This walks the whole path with no display."
  (unwind-protect
       (progn
         (pine:daemon :remoting 0 :config nil)
         (setf (pine/fs/node:contents (pine.edit.buffer:current)) "hello
there")
         (let ((client (pine.net.server:start-server :workers 2 :remoting-port 0))
               (got (pine/data:box nil)))
           (unwind-protect
                (let ((sys (pine.net.server:actor-system client)))
                  (sento.actor-context:actor-of sys
                    :name "display" :dispatcher :pinned
                    :receive (lambda (m)
                               (pine/data:swap! got (lambda (all) (cons m all)))))
                  (pine.net.attach:attach-to
                   sys
                   (pine.net.server:daemon-uri
                    "attach" :port (pine.net.server:remoting-port pine:*image*))
                   (pine.net.server:local-uri
                    "display" (pine.net.server:remoting-port client))
                   :kind :editor)
                  (is-true (wait-until
                            (lambda ()
                              (find :widgets (pine/data:held got) :key #'first)))
                           "no frame was pushed to the editor")
                  (let* ((frame (find :widgets (pine/data:held got) :key #'first))
                         (tree (getf (rest frame) :tree)))
                    (is (equal "editor" (getf (rest frame) :surface)))
                    (is (integerp (getf (rest frame) :generation)))
                    (let ((rebuilt (pine.ui.wire:wire->node tree)))
                      (is (typep rebuilt 'pine.ui.node:node)
                          "the frontend cannot rebuild what it was sent")
                      (let ((rows (pine.ui.cells:render rebuilt 40)))
                        (is (find-if (lambda (row) (search "hello" (car row))) rows)
                            "the buffer's text is not in the frame")
                        (is (find-if (lambda (row) (search "scratch" (car row))) rows)
                            "the modeline is not in the frame")))))
             (pine.net.server:stop-server client))))
    (pine:stop)))

(test a-key-from-the-frontend-edits-the-buffer-and-a-new-frame-follows
  (unwind-protect
       (progn
         (pine:daemon :remoting 0 :config nil)
         (setf (pine/fs/node:contents (pine.edit.buffer:current)) "")
         (let ((client (pine.net.server:start-server :workers 2 :remoting-port 0))
               (got (pine/data:box nil)))
           (unwind-protect
                (let ((sys (pine.net.server:actor-system client)))
                  (sento.actor-context:actor-of sys
                    :name "display" :dispatcher :pinned
                    :receive (lambda (m)
                               (pine/data:swap! got (lambda (all) (cons m all)))))
                  (pine.net.attach:attach-to
                   sys
                   (pine.net.server:daemon-uri
                    "attach" :port (pine.net.server:remoting-port pine:*image*))
                   (pine.net.server:local-uri
                    "display" (pine.net.server:remoting-port client))
                   :kind :editor)
                  (is-true (wait-until
                            (lambda () (pine.net.attach:clients :editor))))
                  (let ((c (first (pine.net.attach:clients :editor))))
                    (dolist (ch '("h" "i"))
                      (pine.edit.session:received
                       c (list :key :key-str ch :ctrl nil :meta nil)))
                    (is-true (wait-until
                              (lambda ()
                                (equal "hi" (pine/fs/node:contents
                                             (pine.edit.buffer:current)))))
                             "a key from the wire did not reach the buffer")))
             (pine.net.server:stop-server client))))
    (pine:stop)))
