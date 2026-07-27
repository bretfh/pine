(in-package :pine.test)

(def-suite* :pine.liveness :in :pine)

;;;; What a fault is allowed to cost. A buffer that faults parks its own thread
;;;; while someone decides what to do, so these checks fault one deliberately and
;;;; then ask the rest of the daemon whether it noticed: another buffer, the paint
;;;; path, the input path. Every check is bounded, so a daemon that does get stuck
;;;; fails the suite instead of hanging it.

;; the mode class is built by ENSURE-CLASS at load time, so the specializer below
;; has to exist before this file's methods are compiled
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pine.editor.mode:defmode probe-mode (:parent :text-mode :indicator "PROBE")))

(defmethod pine.editor.mode:dispatch-message ((mode probe-mode) self tag plist)
  (case tag
    (:probe-fault (error "probe fault"))
    (:probe-self-ask (pine.core.actor:ask self '(:get-state) :timeout 1))
    (t (call-next-method))))

(defun probe-buffer (name)
  "A buffer in probe-mode, ready to be faulted on demand."
  (let ((buf (pine.editor.frame::make-buffer name)))
    (pine.editor.frame::set-buffer-mode buf :probe-mode)
    (sleep 0.1)
    buf))

(defmacro with-surface ((var &key (attended nil) (park-seconds nil)) &body body)
  "Run BODY with a debug surface that records each evaluation into VAR and the
unattended park bounded at PARK-SECONDS. With ATTENDED the surface also marks
each fault as being looked at, so it keeps its restarts past the deadline.

Both are set globally, because the thread that faults is the buffer's own and a
dynamic binding here would not reach it."
  (let ((saved-surface (gensym "SURFACE"))
        (saved-park (gensym "PARK")))
    `(let ((,var nil)
           (,saved-surface pine.err:*on-debug*)
           (,saved-park pine.err:*park-seconds*))
       (setf pine.err:*on-debug* (lambda (ev)
                                   (push ev ,var)
                                   (when ,attended
                                     (setf (pine.err:attended-p ev) t)))
             pine.err:*park-seconds* ,park-seconds)
       (unwind-protect (progn ,@body)
         (setf pine.err:*on-debug* ,saved-surface
               pine.err:*park-seconds* ,saved-park)))))

(defun wait-for (predicate &key (seconds 5))
  "Poll PREDICATE until it holds, and answer whether it did."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :when (funcall predicate) :return t
          :when (> (get-internal-real-time) deadline) :return nil
          :do (sleep 0.05))))

(test a-parked-buffer-costs-nothing-but-itself
  "The fault is held open on one buffer's thread while another buffer, the paint
path and the input path are all asked whether they still work."
  (with-fixture substrate ()
    (within-seconds 30
      (let ((painted 0))
        (setf (pine.editor.frame::paint-sink *client*)
              (lambda (&rest args) (declare (ignore args)) (incf painted)))
        (with-surface (faults :attended t)
          (let ((probe (probe-buffer "probe-parked")))
            (sento.actor:tell probe '(:probe-fault))
            (is (wait-for (lambda () faults))
                "the fault never reached the surface")
            (let ((other (probe-buffer "probe-neighbour")))
              (sento.actor:tell other (list :replace-content :content "still here"))
              (sleep 0.2)
              (is (string= "still here" (btext "probe-neighbour"))
                  "a second buffer stopped answering while one was parked"))
            ;; the command path, on this thread, the way the session loop runs it
            (press "x")
            (is (search "x" (btext "scratch"))
                "command dispatch stopped reaching buffers while one was parked")
            (is (plusp painted)
                "the renderer stopped painting while a buffer was parked")
            (pine.err:pick-restart (first faults) "ABORT")
            (is (wait-for (lambda () (stringp (btext "probe-parked"))))
                "the buffer did not resume after its restart was chosen")))))))

(test an-unattended-fault-frees-the-buffer-on-the-deadline
  "Nobody answers the surface, so the park has to end itself."
  (with-fixture substrate ()
    (within-seconds 30
      (with-surface (faults :park-seconds 1)
        (let ((probe (probe-buffer "probe-unattended")))
          (sento.actor:tell probe (list :replace-content :content "before"))
          (sleep 0.2)
          (sento.actor:tell probe '(:probe-fault))
          (is (wait-for (lambda () faults))
              "the fault never reached the surface")
          (is (wait-for (lambda () (equal "before" (btext "probe-unattended")))
                        :seconds 10)
              "the buffer never came back, so the daemon lost the thread")
          (is (eq :error (pine.err:evaluation-status (first faults)))))))))

(test asking-your-own-actor-signals-instead-of-hanging
  "The liveness contract is checked, not documented: a receive that blocks on
itself gets a condition, which the debugger surfaces like any other fault."
  (with-fixture substrate ()
    (within-seconds 30
      (with-surface (faults :park-seconds 1)
        (let ((probe (probe-buffer "probe-self-ask")))
          (sento.actor:tell probe '(:probe-self-ask))
          (is (wait-for (lambda () faults))
              "asking your own actor hung instead of signalling")
          (is (string= "BLOCKING-ASK-IN-RECEIVE"
                       (pine.err:evaluation-condition-type (first faults))))
          (is (wait-for (lambda () (stringp (btext "probe-self-ask"))))
              "the buffer must keep receiving after a refused ask"))))))

(defun mailbox-of (actor)
  "ACTOR's message box. Pinned actors get a message-box/bt, one thread each;
anything on a shared dispatcher gets a message-box/dp and queues behind whatever
else that pool is running."
  (sento.actor-cell:msgbox actor))

(defun shared-pool-actors (system)
  "Every actor on SYSTEM that draws from a shared dispatcher, by name."
  (loop :for actor :in (sento.actor-context:all-actors system)
        :unless (typep (mailbox-of actor) 'sento.messageb:message-box/bt)
          :collect (sento.actor-cell:name actor)))

(test no-actor-the-daemon-creates-draws-from-a-shared-pool
  "The isolation is structural, not conventional. Every actor pine starts owns
its thread, so no receive can be behind another one: not a repaint behind an
edit, not one app's input behind another's, not the registries behind a buffer
that parked in the debugger."
  (with-fixture substrate ()
    (within-seconds 20
      (probe-buffer "probe-structural")
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (let ((shared (shared-pool-actors
                     (pine.core.server:actor-system *server*))))
        (is (null shared)
            "these actors still queue on a shared dispatcher: ~{~a~^, ~}" shared)))))

(test a-blocking-ask-outside-a-receive-is-allowed
  "The rule is about receives, not about asking: the command path reads buffers
this way on every keystroke."
  (with-fixture substrate ()
    (within-seconds 20
      (is-false (pine.core.actor:in-actor-p))
      (is (stringp (btext "scratch"))))))
