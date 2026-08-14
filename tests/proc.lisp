(in-package :pine.test)

(def-suite* :pine.proc :in :pine)

(test a-program-runs-and-says-what-it-said
  (let ((p (make-instance 'pine.proc.process:program
                          :name "probe-echo" :restarts nil
                          :argv '("sh" "-c" "echo from the process"))))
    (unwind-protect
         (progn
           (pine.proc.process:start p)
           (is-true (wait-until (lambda () (pine.data:held
                                            (pine.proc.process:said p)))))
           (is (equal '("from the process")
                      (pine.data:held (pine.proc.process:said p)))))
      (pine.proc.process:stop p))))

(test a-thread-is-a-process-like-anything-else
  "One that repeats is an entry on the image's clock, not a thread of its own
sleeping in a loop."
  (unwind-protect
       (progn
         (pine:start)
         (let* ((n (pine.data:box 0))
                (p (make-instance 'pine.proc.process:thread-process
                                  :name "probe-thread" :every 0.1
                                  :thunk (lambda () (pine.data:swap! n #'1+)))))
           (unwind-protect
                (progn
                  (pine.proc.process:start p)
                  (is-true (pine.proc.process:alivep p))
                  (is (member "probe-thread" (pine.run.timer:names) :test #'equal)
                      "it is on the clock")
                  (is (null (pine.run.task:task-named "probe-thread"))
                      "and it took no thread of its own")
                  (is-true (wait-until (lambda () (> (pine.data:held n) 2)))))
             (pine.proc.process:stop p))
           (is (eq :stopped (pine.proc.process:state p)))
           (is-false (pine.proc.process:alivep p))
           (is (null (member "probe-thread" (pine.run.timer:names) :test #'equal)))))
    (pine:stop)))

(test what-dies-is-started-again-and-the-backoff-grows
  (let* ((s (pine.proc.supervisor:supervisor))
         (p (make-instance 'pine.proc.process:program
                           :name "probe-dies" :argv '("sh" "-c" "exit 0"))))
    (unwind-protect
         (progn
           (pine.proc.supervisor:supervise s p)
           (pine.proc.process:start p)
           (is (eql 1 (pine.proc.process:attempts p)))
           (is-true (wait-until (lambda () (not (pine.proc.process:alivep p)))))
           (pine.proc.supervisor:attend s)
           (is (>= (pine.proc.process:attempts p) 2)
               "the supervisor started it again")
           (is (> (pine.proc.process:backoff p) 1)))
      (pine.proc.supervisor:forget s "probe-dies"))))

(test a-process-that-says-it-does-not-restart-is-left-alone
  (let* ((s (pine.proc.supervisor:supervisor))
         (p (make-instance 'pine.proc.process:program
                           :name "probe-once" :restarts nil
                           :argv '("sh" "-c" "exit 0"))))
    (unwind-protect
         (progn
           (pine.proc.supervisor:supervise s p)
           (pine.proc.process:start p)
           (is-true (wait-until (lambda () (not (pine.proc.process:alivep p)))))
           (pine.proc.supervisor:attend s)
           (is (eql 1 (pine.proc.process:attempts p))))
      (pine.proc.supervisor:forget s "probe-once"))))

(test the-supervisor-is-told-by-the-images-clock
  (unwind-protect
       (progn
         (pine:start)
         (let ((s (pine.proc.supervisor:supervisor)))
           (unwind-protect
                (progn
                  (pine.proc.supervisor:watch s :every 0.1)
                  (is (member :supervisor (pine.run.timer:names))
                      "attending is a tick, not a thread"))
             (pine.proc.supervisor:unwatch s))
           (is (null (pine.proc.supervisor:attends s)))
           (is (null (member :supervisor (pine.run.timer:names))))))
    (pine:stop)))

(test a-lisp-process-is-a-second-image-evaluated-in
  "An agent was always a process. This is the one that can be evaluated in."
  (let ((p (make-instance 'pine.proc.lisp:lisp-process
                          :name "probe-lisp" :restarts nil :systems nil)))
    (unwind-protect
         (progn
           (pine.proc.process:start p)
           (is-true (pine.proc.lisp:ready-p p))
           (is (eql 4 (pine.proc.lisp:evaluate p '(+ 2 2))))
           (is (equal "SBCL" (pine.proc.lisp:evaluate p '(lisp-implementation-type))))
           (multiple-value-bind (value fault)
               (pine.proc.lisp:evaluate p '(error "over there"))
             (is (null value))
             (is (search "over there" fault)
                 "a fault in the other image comes back as a value")))
      (pine.proc.process:stop p))
    (is-false (pine.proc.process:alivep p))))

(test a-fault-in-the-other-image-comes-home-with-its-restarts
  "The other image does not unwind: it stands in the fault and says what it is
still offering, so taking one of those resumes the thread over there."
  (let ((p (make-instance 'pine.proc.lisp:lisp-process
                          :name "probe-fault" :restarts nil :systems nil)))
    (pine.run.fault:forget-faults)
    (unwind-protect
         (progn
           (pine.proc.process:start p)
           (multiple-value-bind (value said offers)
               (pine.proc.lisp:evaluate p '(error "over there"))
             (is (null value))
             (is (search "over there" said))
             (is (member "ABORT" offers :test #'equal)
                 "the restarts that image is still standing in"))
           (let ((f (first (pine.run.fault:faults))))
             (is (search "over there" (princ-to-string (pine.run.fault:condition-of f)))
                 "and the fault joined this image's own")
             (is (eq p (pine.proc.lisp:there f)) "knowing where it came from")
             (is (member "ABORT" (pine.run.fault:offers f) :test #'equal))
             (pine.run.fault:resume f "ABORT")
             (loop :repeat 200 :while (pine.proc.lisp:there f) :do (sleep 0.01))
             (is (eql 4 (pine.proc.lisp:evaluate p '(+ 2 2)))
                 "and taking one here let the thread there carry on")))
      (pine.run.fault:forget-faults)
      (pine.proc.process:stop p))))
