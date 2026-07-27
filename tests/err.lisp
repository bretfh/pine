(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.err :in :pine)

(defmacro with-err (&body body)
  "A namespace of its own with /err served, and the park deadline short enough
that a test can watch it bite."
  `(pine.ns:with-space ()
     (let ((saved pine.err:*park-seconds*)
           (surface pine.err:*on-debug*))
       (pine.err:mount)
       (setf pine.err:*park-seconds* 1
             pine.err:*on-debug* nil)
       (unwind-protect (progn ,@body)
         (setf pine.err:*park-seconds* saved
               pine.err:*on-debug* surface)))))

(defun err-ids ()
  (pine.data:keys (pine.ns:read /err/*)))

(defun wait-until (predicate &key (seconds 5))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :when (funcall predicate) :return t
          :when (> (get-internal-real-time) deadline) :return nil
          :do (sleep 0.02))))

;;;; a fault is a path

(test a-fault-parks-at-err-holding-its-restarts
  (with-err
    (pine.ns:watch /err (pine.data:fn [v] (declare (ignore v)) {}) :as :probe)
    (pine.err:evaluate-thunk (lambda () (error "probe boom")))
    (is-true (wait-until (lambda () (pine.err:parked))))
    (let* ((ev (first (pine.err:parked)))
           (at (pine.path:child /err (princ-to-string
                                      (pine.err:evaluation-id ev))))
           (fault (pine.ns:read at)))
      (is-true fault "the fault reads back at its own path")
      (is (search "probe boom" (fset:lookup fault :condition)))
      (is (plusp (fset:size (fset:lookup fault :restarts)))
          "it is holding live restarts")
      (is (find "ABORT" (fset:convert 'list (fset:lookup fault :restarts))
                :test #'string-equal))
      (pine.ns:write at [:restart "ABORT"])
      (is-true (wait-until (lambda () (null (pine.err:parked))))
               "writing the restart resumed the thread"))))

(test ls-answers-every-fault-waiting
  (with-err
    (pine.ns:watch /err (pine.data:fn [v] (declare (ignore v)) {}) :as :probe)
    (pine.err:evaluate-thunk (lambda () (error "one")))
    (pine.err:evaluate-thunk (lambda () (error "two")))
    (is-true (wait-until (lambda () (= 2 (length (pine.err:parked))))))
    (is (= 2 (length (err-ids))))
    (dolist (ev (pine.err:parked))
      (pine.ns:write (pine.path:child /err (princ-to-string
                                            (pine.err:evaluation-id ev)))
                     [:restart "ABORT"]))
    (is-true (wait-until (lambda () (null (pine.err:parked)))))))

(test a-fault-is-live-not-held
  "The threads are waiting in this image; the file has no business holding them."
  (with-err
    (pine.ns:watch /err (pine.data:fn [v] (declare (ignore v)) {}) :as :probe)
    (pine.err:evaluate-thunk (lambda () (error "probe")))
    (is-true (wait-until (lambda () (pine.err:parked))))
    (let ((at (pine.path:child /err (princ-to-string
                                     (pine.err:evaluation-id
                                      (first (pine.err:parked)))))))
      (is (eq :live (pine.ns:kind at)))
      (pine.ns:write at [:restart "ABORT"]))))

;;;; attendance, and the bound on a park

(test attendance-is-a-path-and-holds-the-restarts-open
  (with-err
    (let ((done nil))
      (pine.ns:watch /err (pine.data:fn [v] (declare (ignore v)) {}) :as :probe)
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                               :on-done (lambda (ev) (setf done ev)))
      (is-true (wait-until (lambda () (pine.err:parked))))
      (let ((at (pine.path:child /err (princ-to-string
                                       (pine.err:evaluation-id
                                        (first (pine.err:parked)))))))
        (pine.ns:write (pine.path:child at "attended") t)
        (is (eq t (pine.ns:read (pine.path:child at "attended"))))
        (sleep 2.0)
        (is (null done) "an attended fault keeps its restarts past the deadline")
        (pine.ns:write at [:restart "ABORT"])
        (is-true (wait-until (lambda () done)))
        (is (eq :aborted (pine.err:evaluation-status done)))))))

(test an-unattended-park-still-aborts-on-the-deadline
  "A thread waiting on a decision nobody is coming to make is a thread lost."
  (with-err
    (let ((done nil))
      (pine.ns:watch /err (pine.data:fn [v] (declare (ignore v)) {}) :as :probe)
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                               :on-done (lambda (ev) (setf done ev)))
      (is-true (wait-until (lambda () done) :seconds 6))
      (is (eq :aborted (pine.err:evaluation-status done)))
      (is (null (pine.err:parked)) "and it is no longer at /err"))))

(test with-nobody-in-a-position-to-look-a-fault-aborts-at-once
  (with-err
    (let ((done nil))
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                               :on-done (lambda (ev) (setf done ev)))
      (is-true (wait-until (lambda () done) :seconds 2)
               "no surface and no watch on /err means abort now, not in a minute")
      (is (eq :aborted (pine.err:evaluation-status done))))))

;;;; the rest of the eval path is unchanged

(test an-evaluation-answers-its-values
  (with-err
    (let ((done nil))
      (pine.err:evaluate-string "(+ 1 2)" :on-done (lambda (ev) (setf done ev)))
      (is-true (wait-until (lambda () done)))
      (is (eq :ok (pine.err:evaluation-status done)))
      (is (equal '(3) (pine.err:evaluation-values done))))))

(test attempt-answers-the-condition-rather-than-unwinding
  (with-err
    (multiple-value-bind (value condition)
        (pine.err:attempt (lambda () (error "nope")) "a probe")
      (is (null value))
      (is-true condition)
      (is (search "nope" (pine.err:evaluation-condition condition))))))
