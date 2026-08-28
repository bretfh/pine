(in-package :pine/test)

(def-suite* :pine/run :in :pine)

(test a-tick-is-a-job-and-a-node
  (booted)
  (with-tree
    (tree:built)
    (let* ((n (cons 0 nil))
           (j (make-instance 'job:thread :name "ticker" :seconds 0.05
                                         :thunk (lambda () (d:swap (car n) #'1+)))))
      (unwind-protect
           (progn
             (job:supervise j)
             (job:start j)
             (is (job:alivep j))
             (is (until (lambda () (> (car n) 2))))
             (is (eq j (tree:at nil "proc" "ticker")))
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
         (let ((j (make-instance 'job:thread :name "probe" :seconds 0.05
                                             :thunk (lambda () nil))))
           (pine:start)
           (is (not (null (tree:at nil "proc"))))
           (unwind-protect
                (progn (job:supervise j)
                       (job:start j)
                       (is (eq j (tree:at nil "proc" "probe")))
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
           (j (make-instance 'job:thread :name "flaky" :restarts t
                                         :thunk (lambda ()
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
                             :name "quiet" :restarts t
                             :thunk (lambda ()
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
                           :name "orderly" :restarts nil
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
  (let ((c (make-instance 'image:child :name "kid" :restarts nil :systems nil)))
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
           (tree:put nil '("dev" "audio" "volume") 41)
           (peer:serve)
           (let ((p (peer:reach "self" :port (actors:remoting))))
             (unwind-protect
                  (progn
                    (is (job:alivep p))
                    (mount:mount p (tree:root) "host")
                    (is (equal 41 (node:contents
                                   (tree:at nil "host/dev/audio/volume"))))
                    (setf (node:contents (tree:at nil "host/dev/audio/volume"))
                          77)
                    (is (equal 77 (node:contents
                                   (tree:at nil "dev/audio/volume"))))
                    (is (equal '("audio")
                               (tree:listing (tree:at nil "host/dev"))))
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
