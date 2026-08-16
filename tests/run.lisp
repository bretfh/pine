(in-package :pine/test)

(def-suite* :pine/run :in :pine)

(test a-tick-is-a-job-and-a-node
  (booted)
  (with-tree
    (job:attach (tree:root))
    (let* ((n (d:box 0))
           (j (make-instance 'job:thread :name "ticker" :seconds 0.05
                                         :thunk (lambda () (d:swap! n #'1+)))))
      (unwind-protect
           (progn
             (job:supervise j)
             (job:start j)
             (is (job:alivep j))
             (is (until (lambda () (> (d:held n) 2))))
             (is (eq j (tree:at nil "proc" "ticker")))
             (is (eq :running (node:contents j)))
             (setf (node:contents j) (d:seq :stop))
             (is (eq :stopped (node:contents j))))
        (ignore-errors (job:stop j))
        (job:forget "ticker")))))

(test an-actor-takes-messages-in-order
  (booted)
  (let* ((said (d:box nil))
         (j (make-instance 'job:actor
                           :name "orderly" :restarts nil
                           :receive (lambda (m)
                                      (d:swap! said (lambda (all) (cons m all)))))))
    (unwind-protect
         (progn
           (job:start j)
           (dolist (n '(1 2 3)) (job:tell j n))
           (is (until (lambda () (= 3 (length (d:held said))))))
           (is (equal '(1 2 3) (reverse (d:held said)))))
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
                    (is (equal '(4) (image:evaluate p '(+ 2 2)))))
               (ignore-errors (job:stop p))
               (job:forget "self")))))
    (actors:leave)
    (booted)))
