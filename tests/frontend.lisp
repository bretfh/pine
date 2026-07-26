(in-package :pine.test)

(def-suite :pine.frontend :in :pine)
(in-suite :pine.frontend)

;;;; The loop, on a backing that has no compositor behind it. What is under
;;;; test is the part with no pixels in it: that queued work runs, that a
;;;; deadline is honoured, and that the loop never settles down to wait while
;;;; it still owes something.

(defclass probe (pine.frontend:backing)
  ((waits :initform 0 :accessor probe-waits
          :documentation "How many times the loop chose to wait.")
   (dispatches :initform 0 :accessor probe-dispatches)
   (timeouts :initform nil :accessor probe-timeouts
             :documentation "The timeout asked for at each wait, newest last."))
  (:documentation "A backing that answers at once and remembers what it was
asked, so the loop can be watched without a display."))

(defmethod pine.frontend:dispatch-pending ((b probe))
  (incf (probe-dispatches b)))

(defmethod pine.frontend:wait-for-work ((b probe) pump timeout)
  (declare (ignore pump))
  (incf (probe-waits b))
  (setf (probe-timeouts b) (append (probe-timeouts b) (list timeout)))
  nil)

(defun run-probe (&key ready pending deadline (passes 3))
  "Run the loop PASSES times over a fresh probe. Returns the probe and pump."
  (let* ((backing (make-instance 'probe))
         (pump (pine.frontend:make-pump))
         (n 0))
    (unwind-protect
         (pine.frontend:run backing pump
                            :done (lambda () (> (incf n) passes))
                            :ready ready :pending pending :deadline deadline)
      (pine.frontend:close-pump pump))
    (values backing pump)))

(test loop-dispatches-every-pass
  (let ((backing (run-probe :passes 3)))
    (is (= 3 (probe-dispatches backing)))))

(test loop-waits-when-idle
  (let ((backing (run-probe :passes 3)))
    (is (= 3 (probe-waits backing)))
    (is (every (lambda (ms) (= ms -1)) (probe-timeouts backing))
        "with no deadline the loop waits indefinitely")))

(test loop-does-not-wait-while-work-is-pending
  (let ((backing (run-probe :pending (constantly t) :passes 3)))
    (is (zerop (probe-waits backing))
        "a frontend that owes a repaint must not block first")
    (is (= 3 (probe-dispatches backing)))))

(test loop-carries-the-deadline
  (let ((backing (run-probe :deadline (constantly 40) :passes 2)))
    (is (equal '(40 40) (probe-timeouts backing)))))

(test loop-runs-queued-work
  (let* ((backing (make-instance 'probe))
         (pump (pine.frontend:make-pump))
         (ran nil)
         (n 0))
    (pine.frontend:enqueue pump (lambda () (setf ran t)))
    (unwind-protect
         (pine.frontend:run backing pump :done (lambda () (> (incf n) 1)))
      (pine.frontend:close-pump pump))
    (is-true ran "a thunk queued from another thread runs on the loop")))

(test loop-does-not-wait-with-a-thunk-queued
  (let* ((backing (make-instance 'probe))
         (pump (pine.frontend:make-pump))
         (n 0))
    ;; queue from inside the first pass, the way a daemon message arrives
    ;; mid-loop; the second pass has nothing left and settles down to wait
    (unwind-protect
         (pine.frontend:run backing pump
                            :done (lambda () (> (incf n) 2))
                            :ready (lambda ()
                                     (when (= n 1)
                                       (pine.frontend:enqueue
                                        pump (lambda () nil)))))
      (pine.frontend:close-pump pump))
    (is (= 1 (probe-waits backing))
        "the pass that queued work goes round again instead of blocking")))

(test a-queued-thunk-that-signals-is-reported-and-the-rest-still-run
  (let* ((pump (pine.frontend:make-pump))
         (ran nil))
    (pine.frontend:enqueue pump (lambda () (error "probe")))
    (pine.frontend:enqueue pump (lambda () (setf ran t)))
    (let ((report (with-output-to-string (*error-output*)
                    (pine.frontend:drain pump))))
      (pine.frontend:close-pump pump)
      (is-true ran "one bad thunk must not strand the others")
      (is (search "probe" report) "and it must say so"))))
