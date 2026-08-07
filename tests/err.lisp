(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.err :in :pine)

(defmacro with-err (&body body)
  "A namespace of its own with /err served, and the park deadline short enough
that a test can watch it bite."
  `(pine.ns:with-space ()
     (let ((saved (pine.ns:read (pine.path:parse "/park-seconds"))))
       (pine.ns:up :err)
       (pine.ns:write (pine.path:parse "/park-seconds") 1)
       (unwind-protect (progn ,@body)
         (pine.ns:write (pine.path:parse "/park-seconds") saved)))))

(defmacro watching-err (&body body)
  "Run BODY with something looking at /err, which is what lets a fault stand
there holding its restarts instead of taking its abort for want of anyone to
ask."
  `(progn
     (pine.ns:watch /err (pine.data:fn [v] (declare (ignore v)) {}) :as :probe)
     ,@body))

(defun err-ids ()
  (pine.data:keys (pine.ns:read /err/*)))

(defun fault-path (id)
  (pine.path:path /err (princ-to-string id)))

(defun waiting-at (id)
  (and (pine.err:faults id) t))

(defun wait-until (predicate &key (seconds 5))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :when (funcall predicate) :return t
          :when (> (get-internal-real-time) deadline) :return nil
          :do (sleep 0.02))))

;;;; a fault is a path

(test an-eval-reads-in-the-language-the-buffer-is-written-in
  "A buffer holding pine's own paths and maps is not read by the standard
reader. Binding the package and not the readtable meant C-x C-e on any line of
pine's own source signalled instead of evaluating."
  (let ((done nil))
    (pine.err:evaluate-string "(pine.path:text /audio/volume)"
                              :package (find-package :pine.test)
                              :readtable 'pine.path:syntax
                              :on-done (lambda (ev) (setf done ev)))
    (is-true (wait-until (lambda () done)) "the evaluation never finished")
    (when done
      (is (eq :ok (pine.err:evaluation-status done))
          "reading a path under its own readtable failed: ~a"
          (pine.err:evaluation-output done))
      (is (equal '("/audio/volume") (pine.err:evaluation-values done))))))

(test an-unknown-readtable-name-degrades-rather-than-failing
  (let ((done nil))
    (pine.err:evaluate-string "(+ 1 2)"
                              :readtable :no-such-readtable-here
                              :on-done (lambda (ev) (setf done ev)))
    (is-true (wait-until (lambda () done)))
    (when done (is (equal '(3) (pine.err:evaluation-values done))))))

(test a-fault-stands-at-err-holding-its-restarts
  (with-err
    (watching-err
      (let ((ev (pine.err:evaluate-thunk (lambda () (error "probe boom")))))
        (is-true (wait-until (lambda () (waiting-at (pine.err:evaluation-id ev)))))
        (let ((fault (pine.ns:read (fault-path (pine.err:evaluation-id ev)))))
          (is-true fault "the fault reads back at its own path")
          (is (search "probe boom" (fset:lookup fault :condition)))
          (is (plusp (fset:size (fset:lookup fault :restarts)))
              "it is holding live restarts")
          (is (find "ABORT" (fset:convert 'list (fset:lookup fault :restarts))
                    :test #'string-equal)))
        (pine.ns:write (fault-path (pine.err:evaluation-id ev)) [:restart "ABORT"])
        (is-true (wait-until (lambda () (not (waiting-at (pine.err:evaluation-id ev)))))
                 "writing the restart resumed the thread")))))

(test a-fault-and-the-job-it-stopped-are-one-thing
  "So the *jobs* listing and /err name the same evaluation, and an agent needs
no table to match a fault to the eval it came from."
  (with-err
    (watching-err
      (let ((ev (pine.err:evaluate-thunk (lambda () (error "probe")))))
        (is-true (wait-until (lambda () (pine.err:evaluation-fault ev))))
        (is (eql (pine.err:evaluation-id ev)
                 (pine.err:fault-id (pine.err:evaluation-fault ev))))
        (pine.ns:write (fault-path (pine.err:evaluation-id ev)) [:restart "ABORT"])))))

(test ls-answers-every-fault-waiting
  (with-err
    (watching-err
      (let ((a (pine.err:evaluate-thunk (lambda () (error "one"))))
            (b (pine.err:evaluate-thunk (lambda () (error "two")))))
        (is-true (wait-until (lambda () (and (waiting-at (pine.err:evaluation-id a))
                                             (waiting-at (pine.err:evaluation-id b))))))
        (is (>= (length (err-ids)) 2))
        (pine.ns:write (fault-path (pine.err:evaluation-id a)) [:restart "ABORT"])
        (pine.ns:write (fault-path (pine.err:evaluation-id b)) [:restart "ABORT"])
        (is-true (wait-until
                  (lambda () (not (or (waiting-at (pine.err:evaluation-id a))
                                      (waiting-at (pine.err:evaluation-id b)))))))))))

(test a-fault-is-live-not-held
  "The thread is standing there in this image; the file has no business
holding it."
  (with-err
    (watching-err
      (let ((ev (pine.err:evaluate-thunk (lambda () (error "probe")))))
        (is-true (wait-until (lambda () (waiting-at (pine.err:evaluation-id ev)))))
        (is (eq :live (pine.ns:kind (fault-path (pine.err:evaluation-id ev)))))
        (pine.ns:write (fault-path (pine.err:evaluation-id ev)) [:restart "ABORT"])
        (is-true (wait-until (lambda () (not (waiting-at (pine.err:evaluation-id ev))))))))))

;;;; what may hold a thread, and what may not

(test a-fault-on-a-thread-that-is-not-its-own-unwinds-at-once
  "An actor's receive, a callback, a provider's poll: a thread parked there is
a dispatcher worker that never comes back. It records what happened and goes."
  (with-err
    (watching-err
      (let* ((reached nil)
             (answer (pine.err:with-debugger (:label "a probe")
                       (error "probe boom")
                       (setf reached t))))
        (is (null answer) "it unwound out of the body")
        (is (null reached) "and did not carry on past the error")
        (let ((f (first (pine.err:faults))))
          (is-true f "the fault is at /err even though nothing is waiting")
          (is (search "probe boom" (pine.err:fault-condition f)))
          (is (equal '("ABORT") (mapcar #'first (pine.err:offers f)))
              "the stack is gone, so all that is left is to dismiss it")
          (pine.ns:write (fault-path (pine.err:fault-id f)) [:restart "ABORT"])
          (is (null (pine.err:faults (pine.err:fault-id f)))
              "deciding one takes it off /err"))))))

;;;; attendance, and the bound on a park

(test attendance-is-a-path-and-holds-the-restarts-open
  (with-err
    (watching-err
      (let* ((done nil)
             (ev (pine.err:evaluate-thunk (lambda () (error "probe"))
                                          :on-done (lambda (e) (setf done e))))
             (at (fault-path (pine.err:evaluation-id ev))))
        (is-true (wait-until (lambda () (waiting-at (pine.err:evaluation-id ev)))))
        (pine.ns:write (pine.path:path at "attended") t)
        (is (eq t (pine.ns:read (pine.path:path at "attended"))))
        (sleep 2.5)
        (is (null done) "an attended fault keeps its restarts past the deadline")
        (pine.ns:write at [:restart "ABORT"])
        (is-true (wait-until (lambda () done)))
        (is (eq :aborted (pine.err:evaluation-status done)))))))

(test an-unattended-park-still-aborts-on-the-deadline
  "A thread waiting on a decision nobody is coming to make is a thread lost."
  (with-err
    (watching-err
      (let ((done nil))
        (let ((ev (pine.err:evaluate-thunk (lambda () (error "probe"))
                                           :on-done (lambda (e) (setf done e)))))
          (is-true (wait-until (lambda () done) :seconds 6))
          (is (eq :aborted (pine.err:evaluation-status done)))
          (is (null (pine.err:faults (pine.err:evaluation-id ev)))
              "and it is no longer at /err"))))))

(test with-nobody-in-a-position-to-look-a-fault-aborts-at-once
  (with-err
    (let ((done nil))
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                               :on-done (lambda (ev) (setf done ev)))
      (is-true (wait-until (lambda () done) :seconds 2)
               "nothing watching /err means abort now, not in a minute")
      (is (eq :aborted (pine.err:evaluation-status done))))))

;;;; a kind of fault the file does not know about

(defclass probe-fault (pine.err:fault)
  ((sent :initform nil :accessor probe-sent))
  (:documentation "A fault whose decision goes somewhere this file invented."))

(defmethod pine.err:offers ((f probe-fault))
  (list (list "SOMEWHERE-ELSE" "hand it to whoever owns it")))

(defmethod pine.err:resume ((f probe-fault) name)
  (setf (probe-sent f) name)
  (call-next-method))

(test a-fault-kind-defined-outside-err-reads-and-resumes-like-any-other
  "The contract, not a closed set: a layer that can fault its own way adds a
subclass and two methods, and /err serves it without knowing it exists."
  (with-err
    (watching-err
      (let* ((f (pine.err:note (make-instance 'probe-fault
                                              :label "elsewhere"
                                              :condition "it broke over there")))
             (at (fault-path (pine.err:fault-id f))))
        (let ((m (pine.ns:read at)))
          (is (equal "it broke over there" (fset:lookup m :condition)))
          (is (equal '("SOMEWHERE-ELSE")
                     (fset:convert 'list (fset:lookup m :restarts)))))
        (pine.ns:write at [:restart "SOMEWHERE-ELSE"])
        (is (equal "SOMEWHERE-ELSE" (probe-sent f))
            "the decision reached the kind's own method")
        (is (null (pine.err:faults (pine.err:fault-id f))))))))

;;;; the rest of the eval path is unchanged

(test an-evaluation-answers-its-values
  (with-err
    (let ((done nil))
      (pine.err:evaluate-string "(+ 1 2)" :on-done (lambda (ev) (setf done ev)))
      (is-true (wait-until (lambda () done)))
      (is (eq :ok (pine.err:evaluation-status done)))
      (is (equal '(3) (pine.err:evaluation-values done))))))

(test attempt-answers-the-fault-rather-than-unwinding
  (with-err
    (multiple-value-bind (value fault)
        (pine.err:attempt (lambda () (error "nope")) "a probe")
      (is (null value))
      (is-true fault)
      (is (search "nope" (pine.err:fault-condition fault)))
      (is (equal "a probe" (pine.err:fault-label fault))))))

(test attempt-records-the-restarts-that-were-live-at-the-signal
  "A restart is recovery at the signaller, chosen by outer code, so the fault
has to be built before the stack unwinds. Nothing can be offered a restart the
signalling frames established once those frames are gone."
  (with-err
    (multiple-value-bind (value fault)
        (pine.err:attempt
         (lambda ()
           (with-simple-restart (use-nothing "Carry on with nothing")
             (error "nope")))
         "a probe")
      (is (null value))
      (is-true fault)
      (is (member "USE-NOTHING" (mapcar #'first (pine.err:fault-offered fault))
                  :test #'equal))
      (is (plusp (length (pine.err:fault-backtrace fault)))))))

(test two-spaces-fault-separately
  "A fault is at /err, and /err is the space's. Two pines in one image each
have their own faults and their own ids, and neither can decide the other's."
  (let ((a (pine.ns:fresh))
        (b (pine.ns:fresh)))
    (pine.ns:with-space (a)
      (pine.ns:up :err)
      (pine.err:report-failure (make-condition 'simple-error
                                               :format-control "in a")
                               "a probe"))
    (pine.ns:with-space (b)
      (pine.ns:up :err)
      (is (null (pine.err:faults))
          "a fault in one space showed up in another"))
    (pine.ns:with-space (a)
      (is (= 1 (length (pine.err:faults)))
          "the space that faulted lost it"))))

(test what-has-run-stops-somewhere
  "An evaluation holds its form, its values, its output and its thread. A
daemon that has been up a week had every one it ever ran, because the list they
went on had no bound. Finished ones are dropped past /evaluations-kept; one
still running never is."
  (with-err
    (pine.ns:write (pine.path:parse "/evaluations-kept") 4)
    (dotimes (i 20)
      (let ((ev (pine.err:evaluate (list '+ i 1))))
        (bordeaux-threads:join-thread (pine.err::evaluation-thread ev))))
    ;; the twentieth is the one that pruned, so what it left plus itself
    (pine.err:evaluate '(+ 1 1))
    (is (>= 6 (length (pine.err:list-evaluations)))
        "twenty evaluations left ~d of them behind"
        (length (pine.err:list-evaluations)))
    (is (find :ok (pine.err:list-evaluations) :key #'pine.err:evaluation-status)
        "the ones kept are the newest, and they finished")))

(test one-still-running-is-never-dropped
  (with-err
    (pine.ns:write (pine.path:parse "/evaluations-kept") 1)
    (let* ((go (bordeaux-threads:make-semaphore))
           (held (pine.err:evaluate-thunk
                  (lambda () (sb-thread:wait-on-semaphore go)))))
      (unwind-protect
           (progn
             (dotimes (i 10)
               (let ((ev (pine.err:evaluate (list '+ i 1))))
                 (bordeaux-threads:join-thread (pine.err::evaluation-thread ev))))
             (pine.err:evaluate '(+ 1 1))
             (is (find held (pine.err:list-evaluations))
                 "the evaluation still on its thread was pruned away"))
        (bordeaux-threads:signal-semaphore go)))))
