(in-package :pine.test)

(def-suite* :pine.proc :in :pine)

(test a-program-runs-and-says-what-it-said
  (let ((p (make-instance 'pine.proc.process:program
                          :name "probe-echo" :restarts nil
                          :argv '("sh" "-c" "echo from the process"))))
    (unwind-protect
         (progn
           (pine.proc.process:start p)
           (is-true (wait-until (lambda () (pine.run.cell:held
                                            (pine.proc.process:said p)))))
           (is (equal '("from the process")
                      (pine.run.cell:held (pine.proc.process:said p)))))
      (pine.proc.process:stop p))))

(test a-thread-is-a-process-like-anything-else
  (let* ((n (pine.run.cell:cell 0))
         (p (make-instance 'pine.proc.process:thread-process
                           :name "probe-thread" :every 0.01
                           :thunk (lambda () (pine.run.cell:swap n #'1+)))))
    (unwind-protect
         (progn
           (pine.proc.process:start p)
           (is-true (pine.proc.process:alivep p))
           (is-true (wait-until (lambda () (> (pine.run.cell:held n) 2)))))
      (pine.proc.process:stop p))
    (is (eq :stopped (pine.proc.process:state p)))
    (is-false (pine.proc.process:alivep p))))

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

(test the-supervisor-is-told-by-a-process-of-its-own
  (let ((s (pine.proc.supervisor:supervisor)))
    (unwind-protect
         (progn
           (pine.proc.supervisor:watch s :every 0.02)
           (is-true (pine.run.task:alivep (pine.proc.supervisor:attends s))))
      (pine.proc.supervisor:unwatch s))
    (is (null (pine.proc.supervisor:attends s)))))

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
