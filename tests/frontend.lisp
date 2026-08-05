(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

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

(test attempt-returns-the-fault-as-a-value
  (multiple-value-bind (result failure)
      (let ((*error-output* (make-broadcast-stream)))
        (pine.err:attempt (lambda () (error "probe")) "probe context"))
    (is (null result))
    (is (typep failure 'pine.err:fault))
    (is (search "probe" (pine.err:fault-condition failure)))
    (is (equal "probe context" (pine.err:fault-label failure)))
    (pine.err:forget failure)))

(test attempt-passes-the-value-through-when-it-works
  (multiple-value-bind (result failure)
      (pine.err:attempt (lambda () :fine) "probe context")
    (is (eq :fine result))
    (is (null failure))))

(defmacro collecting-faults ((var) &body body)
  "Run BODY with something looking at /err that collects every fault into VAR.

Set on the space rather than bound here, because the thread that faults is not
always this one. This is also what tells pine.err that anything is in a
position to decide."
  `(let ((,var nil))
     (pine.ns:watch /err
                    (lambda (value)
                      (declare (ignore value))
                      (dolist (f (pine.err:faults)) (pushnew f ,var))
                      nil)
                    :as :probe)
     (unwind-protect (progn ,@body)
       (pine.ns:watch /err nil :as :probe))))

(test a-failure-reaches-what-is-watching-err-once
  (collecting-faults (seen)
    (pine.err:attempt (lambda () (error "probe")) "probe context")
    (let ((ours (remove-if-not (lambda (f)
                                 (equal "probe context" (pine.err:fault-label f)))
                               seen)))
      (is (= 1 (length ours)))
      (is (search "probe" (pine.err:fault-condition (first ours))))
      (mapc #'pine.err:forget ours))))

(test a-failure-nothing-is-watching-is-loud-on-the-error-stream
  (let ((report (with-output-to-string (*error-output*)
                  (pine.err:forget
                   (pine.err:report-failure
                    (make-condition 'simple-error :format-control "probe text")
                    "probe context")))))
    (is (search "probe context" report))
    (is (search "probe text" report))))

(test an-evaluation-records-its-values-its-output-and-its-status
  (let ((done nil))
    (pine.err:evaluate-thunk (lambda () (princ "said") 7)
                                   :on-done (lambda (ev) (setf done ev)))
    (sleep 0.3)
    (is (not (null done)))
    (is (eq :ok (pine.err:evaluation-status done)))
    (is (equal '(7) (pine.err:evaluation-values done)))
    (is (string= "said" (pine.err:evaluation-output done)))))

(defmacro with-err-watch ((fn) &body body)
  "Run BODY with FN watching /err, for the faults that happen on an eval's own
thread, which a dynamic binding here would not reach."
  `(progn
     (pine.ns:watch /err (lambda (value) (declare (ignore value)) (funcall ,fn) nil)
                    :as :probe)
     (unwind-protect (progn ,@body)
       (pine.ns:watch /err nil :as :probe))))

(test an-evaluation-nothing-is-watching-aborts-rather-than-parking-its-thread
  (let ((done nil))
    (pine.err:evaluate-thunk (lambda () (error "probe"))
                             :on-done (lambda (ev) (setf done ev)))
    (sleep 0.5)
    (is (not (null done)))
    (is (eq :aborted (pine.err:evaluation-status done))
        "with nobody in a position to choose, the thread aborts instead of parking")))

(defmacro with-park-deadline ((seconds) &body body)
  "Run BODY with the unattended park bounded at SECONDS. Set globally for the
same reason the surface is: the thread that waits is the eval's own."
  (let ((s (gensym "SECONDS")))
    `(let ((,s (pine.ns:read (pine.path:parse "/park-seconds"))))
       (pine.ns:write (pine.path:parse "/park-seconds") ,seconds)
       (unwind-protect (progn ,@body)
         (pine.ns:write (pine.path:parse "/park-seconds") ,s)))))

(test an-unattended-park-aborts-itself-on-the-deadline
  (let ((done nil))
    (with-park-deadline (1)
      (with-err-watch ((lambda () nil))
        (pine.err:evaluate-thunk (lambda () (error "probe"))
                                       :on-done (lambda (ev) (setf done ev)))
        (sleep 2.5)))
    (is (not (null done))
        "something looked at the fault and never answered, holding the thread past its deadline")
    (is (eq :aborted (pine.err:evaluation-status done)))))

(test an-attended-park-waits-past-the-deadline-and-resumes
  (let ((done nil) (seen nil))
    (with-park-deadline (1)
      (with-err-watch ((lambda ()
                         (dolist (f (pine.err:faults))
                           (setf seen f (pine.err:attended-p f) t))))
        (pine.err:evaluate-thunk (lambda () (error "probe"))
                                       :on-done (lambda (ev) (setf done ev)))
        (sleep 2.0)
        (is (null done) "an attended fault must keep its restarts, deadline or not")
        (is (typep seen 'pine.err:parked))
        (pine.err:resume seen "ABORT")
        (sleep 0.5)))
    (is (not (null done)) "the restart chosen late must still resume the thread")))

(test a-restart-picked-by-name-resumes-the-standing-evaluation
  (let ((done nil) (seen nil))
    (with-err-watch ((lambda ()
                       (dolist (f (pine.err:faults))
                         (setf seen f)
                         (pine.err:resume f "ABORT"))))
      (pine.err:evaluate-thunk (lambda () (error "probe"))
                                     :on-done (lambda (ev) (setf done ev)))
      (sleep 0.5))
    (is (not (null seen)))
    (is (member "ABORT" (mapcar #'first (pine.err:offers seen)) :test #'string=))
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

;;;; a kind stops being taken when the app holding it goes away

(test an-app-that-is-gone-gives-its-kind-back
  "Remoting says nothing when a peer dies, so a frontend that was killed would
hold /attached/<kind> forever and the :unless rule on its declaration would
never let another start. Reaping is what gives the kind back."
  (pine.ns:with-space ()
    (let ((clients pine.core.attach:*clients*))
      (unwind-protect
           (let ((client (pine.core.attach::make-attached-client
                          :id 9001 :kind :probe-kind
                          ;; a port nothing listens on: the app is gone
                          :uri "sento://127.0.0.1:1/user/display")))
             (setf pine.core.attach:*clients* (list client))
             (pine.ns:write /attached/probe-kind t)
             (is (eq t (pine.ns:read /attached/probe-kind)))
             (let ((dead (pine.core.attach:reap-clients)))
               (is (equal (list client) dead) "the gone app was reaped")
               (is (null (pine.ns:read /attached/probe-kind))
                   "and its kind is free for another to take")
               (is (null pine.core.attach:*clients*)))))
      (setf pine.core.attach:*clients* clients))))
