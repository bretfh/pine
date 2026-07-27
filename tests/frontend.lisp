(in-package :pine.test)

(def-suite* :pine.frontend :in :pine)

;;;; The loop, on a backing with no compositor behind it. What is under test is
;;;; the part with no pixels in it: that queued work runs, that a deadline is
;;;; honoured, and that the loop never settles down to wait while it still owes
;;;; something.

(defclass probe (pine.frontend:backing)
  ((waits :initform 0 :accessor probe-waits)
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
  "Run the loop PASSES times over a fresh probe. Returns the probe."
  (let ((backing (make-instance 'probe))
        (pump (pine.frontend:make-pump))
        (n 0))
    (unwind-protect
         (pine.frontend:run backing pump
                            :done (lambda () (> (incf n) passes))
                            :ready ready :pending pending :deadline deadline)
      (pine.frontend:close-pump pump))
    backing))

(test the-loop-dispatches-every-pass
  (is (= 3 (probe-dispatches (run-probe :passes 3)))))

(test the-loop-waits-when-it-is-idle
  (let ((backing (run-probe :passes 3)))
    (is (= 3 (probe-waits backing)))
    (is (every (lambda (ms) (= ms -1)) (probe-timeouts backing))
        "with no deadline the loop waits for as long as it takes")))

(test the-loop-does-not-block-while-it-owes-a-repaint
  (let ((backing (run-probe :pending (constantly t) :passes 3)))
    (is (zerop (probe-waits backing)))
    (is (= 3 (probe-dispatches backing)))))

(test the-loop-carries-the-deadline-to-the-wait
  (is (equal '(40 40) (probe-timeouts (run-probe :deadline (constantly 40)
                                                 :passes 2)))))

(test a-thunk-queued-from-another-thread-runs-on-the-loop
  (let ((backing (make-instance 'probe))
        (pump (pine.frontend:make-pump))
        (ran nil)
        (n 0))
    (pine.frontend:enqueue pump (lambda () (setf ran t)))
    (unwind-protect
         (pine.frontend:run backing pump :done (lambda () (> (incf n) 1)))
      (pine.frontend:close-pump pump))
    (is-true ran)))

(test the-pass-that-queued-work-goes-round-again-instead-of-blocking
  (let ((backing (make-instance 'probe))
        (pump (pine.frontend:make-pump))
        (n 0))
    (unwind-protect
         (pine.frontend:run backing pump
                            :done (lambda () (> (incf n) 2))
                            :ready (lambda ()
                                     (when (= n 1)
                                       (pine.frontend:enqueue
                                        pump (lambda () nil)))))
      (pine.frontend:close-pump pump))
    (is (= 1 (probe-waits backing)))))

(test a-queued-thunk-that-signals-is-reported-and-the-rest-still-run
  (let ((pump (pine.frontend:make-pump))
        (ran nil))
    (pine.frontend:enqueue pump (lambda () (error "probe")))
    (pine.frontend:enqueue pump (lambda () (setf ran t)))
    (let ((report (with-output-to-string (*error-output*)
                    (pine.frontend:drain pump))))
      (pine.frontend:close-pump pump)
      (is-true ran)
      (is (search "probe" report)))))

(test the-pump-reports-whether-anything-is-queued
  (let ((pump (pine.frontend:make-pump)))
    (unwind-protect
         (progn
           (is-false (pine.frontend:pump-queued-p pump))
           (pine.frontend:enqueue pump (lambda () nil))
           (is-true (pine.frontend:pump-queued-p pump))
           (pine.frontend:drain pump)
           (is-false (pine.frontend:pump-queued-p pump)))
      (pine.frontend:close-pump pump))))

;;;; Failures on the callback paths. A broken callback is reported and
;;;; returned, never swallowed: a surface that fails to build must not look
;;;; like a surface that had nothing to draw.

(test attempt-returns-the-condition-as-a-value
  (multiple-value-bind (result failure)
      (let ((*error-output* (make-broadcast-stream)))
        (pine.err:attempt (lambda () (error "probe")) "probe context"))
    (is (null result))
    (is (typep failure 'pine.err:evaluation))
    (is (eq :error (pine.err:evaluation-status failure)))
    (is (search "probe" (pine.err:evaluation-condition failure)))
    (is (equal "probe context" (pine.err:evaluation-form failure)))))

(test attempt-passes-the-value-through-when-it-works
  (multiple-value-bind (result failure)
      (pine.err:attempt (lambda () :fine) "probe context")
    (is (eq :fine result))
    (is (null failure))))

(test a-failure-reaches-the-debug-surface-once
  (let* ((seen nil)
         (pine.err:*on-debug* (lambda (ev) (push ev seen))))
    (pine.err:attempt (lambda () (error "probe")) "probe context")
    (is (= 1 (length seen)))
    (is (search "probe" (pine.err:evaluation-condition (first seen))))))

(test a-failure-with-no-surface-is-loud-on-the-error-stream
  (let ((pine.err:*on-debug* nil))
    (let ((report (with-output-to-string (*error-output*)
                    (pine.err:report-failure
                     (make-condition 'simple-error :format-control "probe text")
                     "probe context"))))
      (is (search "probe context" report))
      (is (search "probe text" report)))))

(test an-evaluation-records-its-values-its-output-and-its-status
  (let ((done nil))
    (pine.err:evaluate-thunk (lambda () (princ "said") 7)
                                   :on-done (lambda (ev) (setf done ev)))
    (sleep 0.3)
    (is (not (null done)))
    (is (eq :ok (pine.err:evaluation-status done)))
    (is (equal '(7) (pine.err:evaluation-values done)))
    (is (string= "said" (pine.err:evaluation-output done)))))

(defmacro with-debug-surface ((surface) &body body)
  "Install SURFACE globally around BODY, for the paths that surface from an
eval's own thread, which a dynamic binding here would not reach. ATTEMPT runs
on the caller's thread and takes an ordinary LET instead."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved pine.err:*on-debug*))
       (setf pine.err:*on-debug* ,surface)
       (unwind-protect (progn ,@body)
         (setf pine.err:*on-debug* ,saved)))))

(test an-evaluation-with-no-surface-aborts-rather-than-parking-its-thread
  (let ((done nil))
    (with-debug-surface (nil)
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                                     :on-done (lambda (ev) (setf done ev)))
      (sleep 0.5))
    (is (not (null done)))
    (is (eq :aborted (pine.err:evaluation-status done))
        "with nobody to choose a restart the worker aborts instead of parking")))

(defmacro with-park-deadline ((seconds) &body body)
  "Run BODY with the unattended park bounded at SECONDS. Set globally for the
same reason the surface is: the thread that waits is the eval's own."
  (let ((s (gensym "SECONDS")))
    `(let ((,s pine.err:*park-seconds*))
       (setf pine.err:*park-seconds* ,seconds)
       (unwind-protect (progn ,@body)
         (setf pine.err:*park-seconds* ,s)))))

(test an-unattended-park-aborts-itself-on-the-deadline
  (let ((done nil))
    (with-park-deadline (1)
      (with-debug-surface ((lambda (ev) (declare (ignore ev)) nil))
        (pine.err:evaluate-thunk (lambda () (error "probe"))
                                       :on-done (lambda (ev) (setf done ev)))
        (sleep 2.5)))
    (is (not (null done))
        "a surface that took the fault and never answered held the thread past its deadline")
    (is (eq :aborted (pine.err:evaluation-status done)))))

(test an-attended-park-waits-past-the-deadline-and-resumes
  (let ((done nil) (surfaced nil))
    (with-park-deadline (1)
      (with-debug-surface ((lambda (ev)
                             (setf surfaced ev
                                   (pine.err:attended-p ev) t)))
        (pine.err:evaluate-thunk (lambda () (error "probe"))
                                       :on-done (lambda (ev) (setf done ev)))
        (sleep 2.0)
        (is (null done) "an attended fault must keep its restarts, deadline or not")
        (is (eq :error (pine.err:evaluation-status surfaced)))
        (pine.err:pick-restart surfaced "ABORT")
        (sleep 0.5)))
    (is (not (null done)) "the restart chosen late must still resume the thread")))

(test a-restart-picked-by-name-resumes-the-blocked-evaluation
  (let ((done nil)
        (surfaced nil))
    (with-debug-surface ((lambda (ev)
                           (setf surfaced ev)
                           (pine.err:pick-restart ev "ABORT")))
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                                     :on-done (lambda (ev) (setf done ev)))
      (sleep 0.5))
    (is (not (null surfaced)))
    (is (member "ABORT" (mapcar #'first (pine.err:evaluation-restarts surfaced))
                :test #'string=))
    (is (not (null done)))))

(test every-evaluation-joins-the-registry-and-can-be-found-by-id
  (let ((ev (pine.err:evaluate-thunk (lambda () 1))))
    (sleep 0.2)
    (is (eq ev (pine.err:find-evaluation (pine.err:evaluation-id ev))))
    (is (member ev (pine.err:list-evaluations)))))


;;;; The attach handshake. The daemon's reply and the frontend that reads it are
;;;; a contract between separately built images: a key added to one side killed
;;;; every display actor on the other, and a display actor that dies never
;;;; builds its client ref, so it sends no input and takes no frame while the
;;;; process looks alive and the surface stays blank.

(defun attach-reply (&rest extra)
  "The daemon's :attached reply, as %ACCEPT-ATTACH sends it."
  (append (list :attached :id 1
                :client-uri "sento://127.0.0.1:17000/user/client-1"
                :version (pine.core.attach:protocol-version))
          extra))

(test the-daemon-s-attached-reply-is-the-one-the-frontends-read
  "Every key %ACCEPT-ATTACH puts in the reply has to survive the read, and the
read has to answer a ref. This is the check the :version key walked past."
  (let ((sys (sento.actor-system:make-actor-system
              '(:dispatchers (:shared (:workers 1 :strategy :random))))))
    (unwind-protect
         (progn
           (sento.remoting:enable-remoting sys :host "127.0.0.1" :port 0)
           (is (not (null (pine.core.attach:accept-attached sys (attach-reply))))
               "the reply the daemon sends must yield a client ref")
           (is (not (null (pine.core.attach:accept-attached
                           sys (attach-reply :future-key :whatever))))
               "a key a newer daemon adds must not break an older frontend")
           (is (null (pine.core.attach:accept-attached sys '(:attached :id 1)))
               "a reply naming no client is no ref, not a fault"))
      (sento.actor-context:shutdown sys :wait t))))

(test the-frontends-read-the-reply-through-that-one-function
  "Three frontends had three copies of the destructuring and one keyword broke
all of them. They call ACCEPT-ATTACHED now; this fails if a copy comes back."
  (dolist (file '("src/wayland/app/editor.lisp"
                  "src/wayland/app/desktop.lisp"
                  "src/wayland/app/wm.lisp"))
    (let ((source (uiop:read-file-string
                   (merge-pathnames file (asdf:system-source-directory :pine)))))
      (is (search "accept-attached" source)
          "~a should read the attach reply through accept-attached" file)
      (is (not (search "client-uri)" source))
          "~a should not destructure the attach reply itself" file))))
