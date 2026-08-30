(in-package :pine/test)

(def-suite* :pine/run :in :pine)

(test a-tick-is-a-job-and-a-node
  (booted)
  (with-tree
    (tree:built)
    (let* ((n (cons 0 nil))
           (j (make-instance 'job:tick :name "ticker" :every 0.05
                                         :runs (lambda () (d:swap (car n) #'1+)))))
      (unwind-protect
           (progn
             (job:supervise j)
             (job:start j)
             (is (job:alivep j))
             (is (until (lambda () (> (car n) 2))))
             (is (eq j (tree:at "/proc/ticker")))
             (is (eq :running (node:contents j)))
             (setf (node:contents j) (d:seq :stop))
             (is (eq :stopped (node:contents j))))
        (ignore-errors (job:stop j))
        (job:forget "ticker")))))

(test starting-pine-puts-what-runs-at-proc
  "A job hangs at /proc, and START is what puts /proc there. Without this every
supervised thing pine has is running and unreadable."
  (booted)
  (let ((was (tree:root)))
    (unwind-protect
         (let ((j (make-instance 'job:tick :name "probe" :every 0.05
                                             :runs (lambda () nil))))
           (pine:start)
           (is (not (null (tree:at "/proc"))))
           (unwind-protect
                (progn (job:supervise j)
                       (job:start j)
                       (is (eq j (tree:at "/proc/probe")))
                       (is (eq :running (pine:read "/proc/probe"))))
             (ignore-errors (job:stop j))
             (job:forget "probe")))
      (setf tree:*root* was))))

(test what-died-on-its-own-is-started-again
  "Supervision that never looks is a list of jobs and a promise. A thread that
returned without being asked to is failed, and the next sweep starts it."
  (booted)
  (with-tree
    (tree:built)
    (let* ((runs (cons 0 nil))
           (j (make-instance 'job:thread :name "flaky" :on-fault :restart
                                         :runs (lambda ()
                                                  (d:swap (car runs) #'1+)))))
      (unwind-protect
           (progn
             (job:supervise j)
             (job:start j)
             (is (until (lambda () (eq :failed (job:state j))))
                 "a thread that ended by itself is failed, not stopped")
             (job:sweep)
             (is (until (lambda () (> (car runs) 1)))
                 "and a sweep starts it again"))
        (ignore-errors (job:stop j))
        (job:forget "flaky")))))

(test being-asked-to-stop-is-not-dying
  (booted)
  (with-tree
    (tree:built)
    (let ((j nil))
      (setf j (make-instance 'job:thread
                             :name "quiet" :on-fault :restart
                             :runs (lambda ()
                                      (loop :until (job:stoppingp j)
                                            :do (sleep 0.01)))))
      (unwind-protect
           (progn
             (job:supervise j)
             (job:start j)
             (job:stop j)
             (is (eq :stopped (job:state j)))
             (job:sweep)
             (is (eq :stopped (job:state j))
                 "a sweep does not start what was asked to stop"))
        (job:forget "quiet")))))

(test an-actor-takes-messages-in-order
  (booted)
  (let* ((said (cons nil nil))
         (j (make-instance 'job:actor
                           :name "orderly" :on-fault :leave
                           :receive (lambda (m)
                                      (d:swap (car said) (lambda (all) (cons m all)))))))
    (unwind-protect
         (progn
           (job:start j)
           (dolist (n '(1 2 3)) (job:tell j n))
           (is (until (lambda () (= 3 (length (car said))))))
           (is (equal '(1 2 3) (reverse (car said)))))
      (ignore-errors (job:stop j))
      (job:forget "orderly"))))

(test a-fault-stands-and-taking-a-restart-carries-on
  (booted)
  (fault:forget-faults)
  (let ((took nil))
    (actors:blocking
     "prober"
     (lambda ()
       (fault:with-debugger
         (fault:attempt (lambda ()
                          (with-simple-restart (carry-on "carry on")
                            (error "a probe"))
                          (setf took :past-it))
                        "probing"))))
    (is (until (lambda () (fault:standing))))
    (let ((f (first (fault:standing))))
      (is (not (null f)))
      (is (member "CARRY-ON" (fault:offers f) :test #'equal))
      (is (null (fault:where f)) "it is standing in this image")
      (fault:take f "CARRY-ON")
      (is (until (lambda () (eq took :past-it))))))
  (fault:forget-faults))

(test a-fault-in-another-image-comes-back-with-its-restarts
  (booted)
  (fault:forget-faults)
  (let ((c (make-instance 'image:child :name "kid" :on-fault :leave :systems nil)))
    (unwind-protect
         (progn
           (job:start c)
           (is (equal '(4) (image:evaluate c '(+ 2 2))))
           (fault:forget-faults)
           (multiple-value-bind (v broke offers)
               (image:evaluate c '(error "over there"))
             (is (null v))
             (is (search "over there" broke))
             (is (member "ABORT" offers :test #'equal)))
           (let ((f (first (fault:faults))))
             (is (typep f 'fault:borrowed))
             (is (eq c (fault:where f)))
             (fault:take f "ABORT")
             (is (until (lambda () (fault:taken f))))
             (is (equal '(4) (image:evaluate c '(+ 2 2)))
                 "and the child carried on")))
      (ignore-errors (job:stop c))
      (job:forget "kid")
      (fault:forget-faults))))

(test a-command-is-a-name-and-what-it-does
  (command:defcommand "probe-add" (a b) (:describes "two numbers")
    (+ a b))
  (unwind-protect
       (progn
         (is (= 3 (command:run "probe-add" '(1 2))))
         (is (equal "two numbers" (command:describes (command:named "probe-add"))))
         (signals command:unknown-command (command:run "nothing-of-the-sort")))
    (command:forget "probe-add")))

(test a-command-belongs-where-it-was-written-not-where-it-was-run
  "A system may define commands as it starts, and START runs in whatever package
called it. Taking the package standing at that moment made the command the
caller's, so the system stopping left it behind and an app had to name its own
commands back off by hand."
  (let ((home (string-downcase (package-name *package*))))
    (unwind-protect
         (let ((*package* (find-package :cl-user)))
           (command:defcommand "probe-home" () (:describes "written here") t)
           (is (not (null (command:named "probe-home"))))
           (command:withdraw home)
           (is (null (command:named "probe-home"))
               "withdrawing what this package wrote takes it off"))
      (command:forget "probe-home"))))

(test a-pine-in-this-terminal-has-a-session-to-read-in
  "What PINE SHELL stands in. OPEN-SESSION takes whatever initargs it is handed
straight to MAKE-INSTANCE, so a slot renamed under it takes the console with it and
says nothing until somebody runs the verb."
  (with-tree
    (let ((s (pine:console)))
      (unwind-protect
           (is (eq (tree:root) (session:in s))
               "a relative name typed there is measured from the root")
        (session:close s)))))

(test a-session-evaluates-and-runs-commands
  (command:defcommand "probe-say" () (:describes "a word") :said)
  (unwind-protect
       (let ((s (session:open-session :name "probe"
                                      :package (find-package :pine/test))))
         (is (equal '(4) (session:answered (session:evaluate s '(+ 2 2)))))
         (is (equal '(:said) (session:answered
                              (session:evaluate s '(|probe-say|)))))
         (is (session:fault (session:evaluate s '(error "no"))))
         (session:close s))
    (command:forget "probe-say")))

(test another-pine-is-mounted-and-evaluated-in
  "Remoting is asked for when the actor system is made, so this test takes the
image's one down and puts a listening one in its place."
  (unwind-protect
       (progn
         (actors:leave)
         (actors:boot :remoting 0)
         (with-tree
           (tree:put "/dev/audio/volume" nil 41)
           (peer:serve)
           (let ((p (peer:reach "self" :port (actors:remoting))))
             (unwind-protect
                  (progn
                    (is (job:alivep p))
                    (mount:mount p (tree:root) "host")
                    (is (equal 41 (node:contents
                                   (tree:at "/host/dev/audio/volume"))))
                    (setf (node:contents (tree:at "/host/dev/audio/volume"))
                          77)
                    (is (equal 77 (node:contents
                                   (tree:at "/dev/audio/volume"))))
                    (is (equal '("audio")
                               (tree:listing (tree:at "/host/dev"))))
                    (is (equal '(4) (image:evaluate p '(+ 2 2))))
                    (fault:forget-faults)
                    (multiple-value-bind (answered broke offers)
                        (image:evaluate p '(error "over there"))
                      (declare (ignore answered))
                      (is (search "over there" (princ-to-string broke))
                          "a fault in the other one comes back")
                      (is (member "ABORT" offers :test #'equal)
                          "with the restarts it is still offering: ~s" offers))
                    (let ((f (find-if (lambda (each) (typep each 'fault:borrowed))
                                      (fault:faults))))
                      (is (not (null f)) "and it stands here as a borrowed one")
                      (when f
                        (is (eq p (fault:where f))
                            "knowing which image it is standing in"))))
               (ignore-errors (job:stop p))
               (job:forget "self")))))
    (actors:leave)
    (booted)))

(test nothing-that-can-block-draws-from-a-shared-pool
  "A receive that waits behind another one is a keystroke behind a parse. Every
actor pine starts owns its thread, so the isolation is structural rather than
something each caller has to remember."
  (booted)
  (let ((shared (loop :for j :in (job:jobs)
                      :when (and (typep j 'job:actor) (job:ref j))
                        :unless (typep (sento.actor-cell:msgbox (job:ref j))
                                       'sento.messageb:message-box/bt)
                          :collect (job:name j))))
    (is (null shared) "these queue behind each other: ~{~a~^, ~}" shared)))

(defclass a-far-image () ()
  (:documentation "Somewhere a fault is standing that is not here. Enough of an
image to be told a restart was taken."))

(defmethod fault:resume ((it a-far-image) f restart)
  (declare (ignore f restart))
  t)

(test a-fault-taken-in-another-image-stops-standing-here
  "A borrowed fault is answered by being taken there, and nothing here ever sets
its choice. Waiting only on the choice waits out the whole timeout on a fault
somebody already dealt with -- and then takes ABORT on a child that carried on two
minutes ago."
  (booted)
  (let ((f (fault:borrow (make-instance 'a-far-image)
                         (make-condition 'simple-error
                                         :format-control "something over there")
                         '("ABORT" "CARRY-ON"))))
    (bordeaux-threads:make-thread
     (lambda () (sleep 0.05) (fault:take f "CARRY-ON")))
    (let ((from (get-internal-real-time)))
      (is (equal "CARRY-ON" (fault:await f 30)))
      (is (< (/ (- (get-internal-real-time) from)
                internal-time-units-per-second)
             10)
          "it came back when the fault was taken, not when the wait ran out")))
  (fault:forget-faults))

(test a-fault-keeps-the-restarts-that-were-offered
  "Reported after the unwind, the restarts a fault stood in are gone and what is
left is the toplevel's own. Reported from a handler-bind, they are still there."
  (fault:forget-faults)
  (fault:attempt (lambda ()
                   (restart-case (error "deliberate")
                     (use-the-other-one () :nope)
                     (give-up () :nope)))
                 "probe")
  (let ((offers (fault:offers (first (fault:faults)))))
    (is (member "USE-THE-OTHER-ONE" offers :test #'equal)
        "the ones it was standing in: ~s" offers)
    (is (not (member "EXIT" offers :test #'equal))
        "and not the one that ends the image")))

(test a-fault-keeps-its-own-number
  "/fault/N is written to take a restart. Numbered by where it sits in the ring,
a fault arriving in between moves every other one along and the restart is taken
on whatever slid into the place that was being read."
  (fault:forget-faults)
  (fault:attempt (lambda () (error "first")) "first")
  (let ((id (fault:id (first (fault:faults)))))
    (fault:attempt (lambda () (error "second")) "second")
    (fault:attempt (lambda () (error "third")) "third")
    (is (search "first" (princ-to-string
                         (fault:condition-of (pine/run/fault::%at id))))
        "it still names the one it named")))

(test a-thread-that-will-not-stop-is-not-called-stopped
  "Asked and not gone, it is still running whatever pine has written down, and
calling it stopped leaves it holding what it holds with nothing naming it."
  (booted)
  (let* ((running t)
         (j (make-instance 'job:thread :name "test-stubborn" :on-fault :leave
                           :runs (lambda () (loop :while running
                                                   :do (sleep 0.02))))))
    (unwind-protect
         (progn
           (job:start j)
           (until (lambda () (job:alivep j)))
           (job:stop j)
           (is (eq :stopping (job:state j)) "it says what is true")
           (is (not (null (job:took j))) "and still knows what to look at")
           (setf running nil)
           (until (lambda () (not (job:alivep j))))
           (job:stop j)
           (is (eq :stopped (job:state j)) "and once it goes, it went"))
      (setf running nil)
      (job:forget "test-stubborn"))))

(test asking-a-job-nothing-answers-to-answers-nothing
  "TELL had a method for it and ASK did not, so one was quiet and the other
signalled that no method applied."
  (is (null (job:ask "no-such-job" :hi)))
  (is (null (job:tell "no-such-job" :hi))))

(test a-reader-error-at-a-repl-does-not-end-the-session
  "The reader is outside what EVALUATE catches, so an unbalanced paren unwound out
of INTERACT and, in a shell, took the image with it."
  (let* ((in (make-string-input-stream ")(+ 1 2)"))
         (out (make-string-output-stream))
         (s (session:open-session :name "test-reader" :input in :output out)))
    (unwind-protect
         (progn (finishes (session:interact s))
                (is (search "3" (get-output-stream-string out))
                    "and it went on to read the form after it"))
      (session:close s))))

(test a-fault-at-a-repl-reaches-the-fault-system
  "It was kept on the evaluation and nowhere else, so the debugger buffer could
not show what somebody had just typed."
  (let* ((in (make-string-input-stream "(error \"repl-fault\")"))
         (out (make-string-output-stream))
         (s (session:open-session :name "test-repl-fault" :input in :output out))
         (before (length (fault:faults))))
    (unwind-protect
         (progn (session:interact s)
                (is (= 1 (- (length (fault:faults)) before))
                    "it is one of the faults")
                (is (search "repl-fault"
                            (princ-to-string
                             (fault:condition-of (first (fault:faults)))))))
      (session:close s))))

(test a-watcher-on-a-collection-does-not-fire-on-every-read
  "EQUAL asks whether two maps are the same object. Two structurally equal ones
are not, so a bar reading a map was pushed at every tick."
  (with-tree
    (let* ((fires 0)
           (n (node:attach (node:answers "coll" :reads (lambda () (d:map :a 1)))
                           (tree:root)))
           (w (watch:watch n (lambda (of said) (declare (ignore of said))
                               (incf fires))
                           :poll t)))
      (unwind-protect
           (progn (pine/run/watch::fire w)
                  (pine/run/watch::fire w)
                  (is (zerop fires) "nothing moved, so nothing was said"))
        (watch:unwatch w))))
  (with-tree
    (let* ((fires 0)
           (which (list (d:map :a 1)))
           (n (node:attach (node:answers "coll" :reads (lambda () (first which)))
                           (tree:root)))
           (w (watch:watch n (lambda (of said) (declare (ignore of said))
                               (incf fires))
                           :poll t)))
      (unwind-protect
           (progn (pine/run/watch::fire w)
                  (setf which (list (d:map :a 2)))
                  (pine/run/watch::fire w)
                  (is (= 1 fires) "and when it moves it is said once"))
        (watch:unwatch w)))))

(test the-shell-remembers-a-line-for-a-breath-and-not-for-ever
  "The table is keyed by the line, so a bar that asks about a window held an
answer for every window there had ever been."
  (dotimes (i 400) (sh:sh "echo bounded-~d" i))
  (is (<= (d:size (d:all pine/host/shell::*asked*))
          (+ pine/host/shell::*asked-kept* 2))
      "~d stand, and the cap is ~d"
      (d:size (d:all pine/host/shell::*asked*)) pine/host/shell::*asked-kept*))

(test a-job-that-will-not-run-is-held-rather-than-spun
  "A crash loop is something to read at /proc, not something the image does for
the rest of its life."
  (with-tree
    (let ((j (make-instance 'job:thread :name "never"
                                        :runs (lambda () (error "never runs")))))
      (job:supervise j)
      (unwind-protect
           (let ((pine/run/job::*tries* 3)
                 (pine/run/job::*backoff-cap* 0))
             (setf (job:state j) :failed
                   (job:tries j) 3
                   (pine/run/job::since j) nil)
             (job:sweep)
             (is (job:heldp j) "given up on rather than started again")
             (is (search "gave up" (princ-to-string (pine/run/job::fault j)))
                 "and where it stands says why")
             (job:sweep)
             (is (job:heldp j) "and a later pass leaves it alone")
             (node:verb j :start nil)
             (is (not (job:heldp j))
                 "asking for it by name takes it out of being held")
             (is (< (job:tries j) 3)
                 "and forgets what it tried before"))
        (job:forget "never")))))

(test a-watcher-that-is-slow-does-not-hold-up-the-write
  "A watcher is told on a worker. One that shells out must not hold up the write,
nor everything else the walk has still to reach, nor whoever was waiting on it."
  (with-tree
    (booted)
    (let ((n (tree:ensure "/probe-slow"))
          (let-go (bordeaux-threads:make-semaphore))
          (told nil))
      (setf (node:contents n) "before")
      (let ((w (watch:watch n (lambda (of said)
                                (declare (ignore of said))
                                (bordeaux-threads:wait-on-semaphore let-go
                                                                   :timeout 10)
                                (setf told t)))))
        (unwind-protect
             (let ((at (get-internal-real-time)))
               (setf (node:contents n) "after")
               (is (< (- (get-internal-real-time) at)
                      internal-time-units-per-second)
                   "the write does not wait to be told about")
               (is (null told) "and the telling has not finished")
               (bordeaux-threads:signal-semaphore let-go)
               (is (until (lambda () told)) "but it lands afterwards"))
          (watch:unwatch w))))))

(test a-working-out-can-happen-in-another-image
  "The boundary Genera did not have. A READS is somebody else's code and it talks to
the world; one that will not stop takes the image it runs in with it. Everything
around the working-out protects the other readers of a node -- the claim, the
deadline, the fault that comes home with its restarts. This is what protects the
image, and it is DERIVE :IN."
  (booted)
  (with-tree
    (let ((kid (make-instance 'image:child :name "elsewhere" :on-fault :leave
                                           :systems nil)))
      (job:supervise kid)
      (job:start kid)
      (unwind-protect
           (let ((n (node:derive "sum" '(+ 20 22) :in kid)))
             (node:attach n (tree:root))
             (is (eql 42 (node:contents n))
                 "worked out over there, and what came back is a value")
             (is (null (node:saw n))
                 "and it read nothing here, because what it read is that image's"))
        (job:stop kid)
        (job:forget "elsewhere")))))
