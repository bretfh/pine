(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.proc :in :pine)

(defmacro with-proc ((&key system) &body body)
  "A namespace of its own with /proc served, torn down afterwards."
  `(pine.ns:with-space ()
     (unwind-protect
          (progn (pine.proc:mount :system ,system) ,@body)
       (pine.proc:unmount))))

(defun settle (predicate &key (seconds 5))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :when (funcall predicate) :return t
          :when (> (get-internal-real-time) deadline) :return nil
          :do (pine.proc:tick)
              (sleep 0.05))))

;;;; declaring is starting

(test declaring-a-thread-starts-it
  (with-proc ()
    (let ((ran nil))
      (pine.ns:write /proc/probe {:thread (pine.data:fn []
                                            (setf ran t)
                                            (sleep 30))})
      (is-true (settle (lambda () ran)))
      (is (eq :running (pine.ns:read /proc/probe/state))))))

(test the-table-is-the-only-list-of-what-runs
  (with-proc ()
    (pine.ns:write /proc/one {:thread (pine.data:fn [] (sleep 30))})
    (pine.ns:write /proc/two {:thread (pine.data:fn [] (sleep 30))})
    (let ((names (mapcar #'pine.path:leaf (pine.data:keys (pine.ns:read /proc/*)))))
      (is (equal '("one" "two") (sort names #'string<))))))

(test a-declaration-reads-back
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is (fset:equal? ["sleep" "30"] (fset:lookup (pine.ns:read /proc/probe) :run)))))

(test writing-nil-stops-it-and-drops-it
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is-true (settle (lambda () (eq :running (pine.ns:read /proc/probe/state)))))
    (pine.ns:write /proc/probe nil)
    (is (null (pine.ns:read /proc/probe/state)))
    (is (fset:empty? (pine.ns:read /proc/* {})))))

(test declaring-the-same-thing-again-leaves-it-running
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is-true (settle (lambda () (pine.ns:read /proc/probe/pid))))
    (let ((pid (pine.ns:read /proc/probe/pid)))
      (pine.ns:write /proc/probe {:run ["sleep" "30"]})
      (is (eql pid (pine.ns:read /proc/probe/pid))
          "an idempotent write must not churn what is up"))))

;;;; a subprocess

(test a-subprocess-runs-and-reports-its-pid
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is-true (settle (lambda () (eq :running (pine.ns:read /proc/probe/state)))))
    (is (integerp (pine.ns:read /proc/probe/pid)))))

(test what-a-process-says-lands-in-its-ring
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sh" "-c" "echo hello; echo again; sleep 30"]})
    (is-true (settle (lambda () (equal "again" (pine.ns:read /proc/probe/out)))))
    (is (= 2 (fset:size (pine.ns:read /proc/probe/out/*))))
    (is (string= "hello" (pine.ns:read /proc/probe/out/1)))))

;;;; the verbs

(test stop-and-start
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is-true (settle (lambda () (eq :running (pine.ns:read /proc/probe/state)))))
    (pine.ns:write /proc/probe [:stop])
    (is (eq :stopped (pine.ns:read /proc/probe/state)))
    (pine.proc:tick)
    (is (eq :stopped (pine.ns:read /proc/probe/state))
        "a pass does not bring back what was stopped on purpose")
    (pine.ns:write /proc/probe [:start])
    (is-true (settle (lambda () (eq :running (pine.ns:read /proc/probe/state)))))))

(test restart-gives-it-a-new-process
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is-true (settle (lambda () (pine.ns:read /proc/probe/pid))))
    (let ((pid (pine.ns:read /proc/probe/pid)))
      (pine.ns:write /proc/probe [:restart])
      (is-true (settle (lambda () (and (pine.ns:read /proc/probe/pid)
                                       (not (eql pid (pine.ns:read /proc/probe/pid))))))))))

;;;; keeping it alive

(test what-dies-comes-back
  "Writing the declaration is what keeps it alive; there is no supervisor to
declare separately."
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"]})
    (is-true (settle (lambda () (pine.ns:read /proc/probe/pid))))
    (let ((pid (pine.ns:read /proc/probe/pid)))
      (uiop:run-program (list "kill" "-9" (princ-to-string pid))
                        :ignore-error-status t)
      (is-true (settle (lambda () (let ((now (pine.ns:read /proc/probe/pid)))
                                    (and now (not (eql pid now)))))
                       :seconds 10)))))

(test something-that-keeps-dying-is-backed-off-rather-than-spun
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sh" "-c" "exit 1"]})
    (settle (lambda () nil) :seconds 1)
    (let ((entry-attempts (length (pine.data:keys (pine.ns:read /proc/*)))))
      (is (= 1 entry-attempts))
      (dotimes (i 20) (pine.proc:tick))
      (is (member (pine.ns:read /proc/probe/state) '(:running :failed))
          "it is either up or known to be down, never spinning"))))

;;;; needs

(test a-declaration-waits-for-what-it-needs
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sleep" "30"] :needs [/display]})
    (pine.proc:tick)
    (is (not (eq :running (pine.ns:read /proc/probe/state)))
        "nothing to run on yet")
    (pine.ns:write /display "wayland-1")
    (is-true (settle (lambda () (eq :running (pine.ns:read /proc/probe/state)))))))

(test a-declaration-stands-down-for-what-is-already-there
  "The rule that kept a second editor from starting was a condition inside a
loop. It is a path now."
  (with-proc ()
    (pine.ns:write /attached/probe t)
    (pine.ns:write /proc/probe {:run ["sleep" "30"] :unless [/attached/probe]})
    (pine.proc:tick)
    (is (not (eq :running (pine.ns:read /proc/probe/state))))
    (pine.ns:write /attached/probe nil)
    (is-true (settle (lambda () (eq :running (pine.ns:read /proc/probe/state)))))))

(test what-a-process-said-on-its-way-out-is-readable
  "A frontend says it cannot run here by exiting 70. Pine records what it said
rather than deciding what it meant."
  (with-proc ()
    (pine.ns:write /proc/probe {:run ["sh" "-c" "exit 70"]})
    (is-true (settle (lambda () (eql 70 (pine.ns:read /proc/probe/exit)))))))

(test the-environment-a-declaration-asks-for-reaches-it
  (with-proc ()
    ;; the environment given replaces the one inherited, so it carries
    ;; everything the command needs to run at all
    (pine.ns:write /proc/probe
                   {:run ["sh" "-c" "echo $PINE_PROBE; sleep 30"]
                    :env (fset:convert 'fset:seq
                                       (cons "PINE_PROBE=here"
                                             (sb-ext:posix-environ)))})
    (is-true (settle (lambda () (equal "here" (pine.ns:read /proc/probe/out)))))))

;;;; an interval instead of staying up

(test an-interval-runs-it-again-rather-than-keeping-it-up
  (with-proc ()
    (let ((runs 0))
      (pine.ns:write /proc/probe {:every 1 :thread (pine.data:fn [] (incf runs))})
      (is-true (settle (lambda () (>= runs 1))))
      (is-true (settle (lambda () (>= runs 2)) :seconds 6))
      (is (not (eq :failed (pine.ns:read /proc/probe/state)))
          "finishing is not failing when it runs on an interval"))))

;;;; an image without one to start

(test asking-for-an-image-with-no-way-to-start-one-says-so
  "A start that does not take is something about the process, so it reads
where the process is read rather than being raised as a fault in pine."
  (with-proc ()
    (pine.ns:write /proc/probe {:image "pine editor"})
    (pine.proc:tick)
    (is (eq :failed (pine.ns:read /proc/probe/state)))
    (is (search "image" (pine.ns:read /proc/probe/error)))))

;;;; the pass runs on the actor system's timer

(test the-table-is-attended-on-sentos-wheel-timer
  "The interval belongs to the wheel timer. A thread that sleeps in a loop is
a supervisor nobody asked for."
  (let ((server (pine.core.server:start-server :workers 1)))
    (unwind-protect
         (pine.ns:with-space ()
           (unwind-protect
                (let ((interval pine.proc:*interval*))
                  (setf pine.proc:*interval* 1)
                  (pine.proc:mount :system (pine.core.server:actor-system server))
                  (pine.ns:write /proc/probe {:run ["sleep" "30"]})
                  (let ((deadline (+ (get-internal-real-time)
                                     (* 8 internal-time-units-per-second))))
                    ;; nothing here calls tick: the timer has to
                    (is-true (loop :when (eq :running (pine.ns:read /proc/probe/state))
                                     :return t
                                   :when (> (get-internal-real-time) deadline) :return nil
                                   :do (sleep 0.1))))
                  (setf pine.proc:*interval* interval))
             (pine.proc:unmount)))
      (pine.core.server:stop-server server))))
