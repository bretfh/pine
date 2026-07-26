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

;;;; Failures on the callback paths. The rule is that a broken callback is
;;;; reported and returned, never swallowed: a surface that fails to build
;;;; must not look like a surface that had nothing to draw.

(test attempt-returns-the-condition-as-a-value
  (multiple-value-bind (result failure)
      (let ((*error-output* (make-broadcast-stream)))
        (pine.eval:attempt (lambda () (error "probe")) "probe context"))
    (is (null result))
    (is (typep failure 'pine.eval:evaluation)
        "the failure comes back as something the caller can inspect")
    (is (eq :error (pine.eval:evaluation-status failure)))
    (is (search "probe" (pine.eval:evaluation-condition failure)))
    (is (equal "probe context" (pine.eval:evaluation-form failure))
        "named by where it happened, not by the condition alone")))

(test attempt-passes-the-value-through-when-it-works
  (multiple-value-bind (result failure)
      (pine.eval:attempt (lambda () :fine) "probe context")
    (is (eq :fine result))
    (is (null failure))))

(test a-failure-reaches-the-debug-surface
  (let ((seen nil))
    (let ((pine.eval:*on-debug* (lambda (ev) (push ev seen))))
      (pine.eval:attempt (lambda () (error "probe")) "probe context"))
    (is (= 1 (length seen)) "the editor's debugger is told, once")
    (is (search "probe" (pine.eval:evaluation-condition (first seen))))))

(test a-broken-ref-subscriber-does-not-stop-the-others
  (let* ((ref (pine.ref:make-ref :name :probe-ref :value 0))
         (ran nil))
    (pine.ref:ref-subscribe ref (lambda () (error "probe")))
    (pine.ref:ref-subscribe ref (lambda () (setf ran t)))
    (let ((*error-output* (make-broadcast-stream)))
      (pine.ref:set-ref ref 1))
    (is-true ran "the second subscriber still sees the change")))

;;;; Rows patching. The invariant: applying a patch to the form the frontend
;;;; holds yields exactly the form the daemon built. A patch that cannot say
;;;; the difference must refuse, so the caller sends the tree whole.

(defun %bench-row (cols text)
  (cons (let ((s (make-string cols :initial-element #\space)))
          (replace s text)
          s)
        (list (list 0 200 200 200 30 30 40 0))))

(defun %editor-form (lines &key (cols 40) (crow 0) (ccol 0))
  (pine.layout:node->wire
   (pine.layout:column
    (pine.layout:window (mapcar (lambda (l) (%bench-row cols l)) lines)
                        :kind :window :crow crow :ccol ccol)
    (pine.layout:window (list (%bench-row cols "")) :kind :echo))))

(test a-patch-applied-equals-the-tree-the-daemon-built
  (let* ((old (%editor-form '("alpha" "beta" "gamma")))
         (new (%editor-form '("alpha" "BETA!" "gamma")))
         (patch (pine.layout:rows-patch old new)))
    (is-true patch "one changed line is patchable")
    (is (equal new (pine.layout:apply-rows-patch old patch))
        "the patched form is the form, exactly")))

(test a-patch-carries-only-the-lines-that-moved
  (let* ((old (%editor-form '("a" "b" "c" "d" "e")))
         (new (%editor-form '("a" "b" "CHANGED" "d" "e")))
         (patch (pine.layout:rows-patch old new))
         (lines (fourth (first patch))))
    (is (= 1 (length lines)) "one line differs, one line ships")
    (is (= 2 (car (first lines))) "and it is the third")))

(test the-cursor-moving-is-patchable-without-any-line
  (let* ((old (%editor-form '("a" "b") :crow 0 :ccol 0))
         (new (%editor-form '("a" "b") :crow 1 :ccol 3))
         (patch (pine.layout:rows-patch old new)))
    (is-true patch)
    (is (null (fourth (first patch))) "no line changed")
    (is (equal new (pine.layout:apply-rows-patch old patch)))))

(test a-changed-tree-refuses-to-patch
  (let ((old (%editor-form '("a" "b")))
        (new (pine.layout:node->wire
              (pine.layout:column
               (pine.layout:window (list (%bench-row 40 "a")) :kind :window)
               (pine.layout:window (list (%bench-row 40 "b")) :kind :window)
               (pine.layout:window (list (%bench-row 40 "")) :kind :echo)))))
    (is (null (pine.layout:rows-patch old new))
        "a split changes the shape, so the tree goes whole")))

(test a-changed-line-count-refuses-to-patch
  (let ((old (%editor-form '("a" "b")))
        (new (%editor-form '("a" "b" "c"))))
    (is (null (pine.layout:rows-patch old new))
        "a resized window goes whole")))

(test no-previous-form-refuses-to-patch
  (is (null (pine.layout:rows-patch nil (%editor-form '("a"))))))

(test scrolling-every-line-is-still-correct-when-patched
  (let* ((old (%editor-form '("1" "2" "3" "4")))
         (new (%editor-form '("5" "6" "7" "8")))
         (patch (pine.layout:rows-patch old new)))
    (is (= 4 (length (fourth (first patch)))) "every line differs")
    (is (equal new (pine.layout:apply-rows-patch old patch))
        "and applying it is still exact")))
