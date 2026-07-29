(in-package :pine.test)

(def-suite* :pine.liveness :in :pine)

;;;; What a fault is allowed to cost. A buffer that faults parks its own thread
;;;; while someone decides what to do, so these checks fault one deliberately and
;;;; then ask the rest of the daemon whether it noticed: another buffer, the paint
;;;; path, the input path. Every check is bounded, so a daemon that does get stuck
;;;; fails the suite instead of hanging it.

;;;; A parser that faults on demand: a buffer with a language, whose parser is
;;;; the one actor a buffer still has. Telling it a verb it does not know is a
;;;; fault on its own thread, which is what these check the cost of.

(defun probe-parser (name)
  "NAME's parser, started and ready to be faulted."
  (pine.editor.frame::make-buffer name :content "(defun f (x) x)")
  (pine.editor.frame::set-buffer-mode name :lisp)
  (pine.buf:showing name (fset:seq 0 200))
  (wait-for (lambda () (pine.buf:parser-of name)) :seconds 10)
  (pine.buf:parser-of name))

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

(test a-parked-parser-costs-nothing-but-itself
  "The fault is held open on one parser's thread while another buffer, the
paint path and the input path are all asked whether they still work."
  (with-fixture substrate ()
    (within-seconds 30
      (let ((painted 0))
        (setf (pine.editor.frame::paint-sink *client*)
              (lambda (&rest args) (declare (ignore args)) (incf painted)))
        (with-surface (faults :attended t)
          (let ((probe (probe-parser "probe-parked")))
            (sento.actor:tell probe '(:probe-fault))
            (is (wait-for (lambda () faults))
                "the fault never reached the surface")
            (pine.editor.frame::make-buffer "probe-neighbour")
            (pine.ns:write (pine.buf:at "probe-neighbour" :text) "still here")
            (is (string= "still here" (btext "probe-neighbour"))
                "a second buffer stopped answering while a parser was parked")
            ;; the command path, on this thread, the way the session loop runs it
            (press "x")
            (is (search "x" (btext "scratch"))
                "command dispatch stopped reaching buffers while one was parked")
            (is (plusp painted)
                "the renderer stopped painting while a parser was parked")
            (pine.err:pick-restart (first faults) "ABORT")
            (is (wait-for (lambda () (pine.buf:parser-of "probe-parked")))
                "the parser did not come back after its restart was chosen")))))))

(test an-unattended-fault-frees-the-thread-on-the-deadline
  "Nobody answers the surface, so the park has to end itself."
  (with-fixture substrate ()
    (within-seconds 30
      (with-surface (faults :park-seconds 1)
        (let ((probe (probe-parser "probe-unattended")))
          (sento.actor:tell probe '(:probe-fault))
          (is (wait-for (lambda () faults))
              "the fault never reached the surface")
          ;; the parser takes work again once the park ends itself
          (pine.ns:write (pine.buf:at "probe-unattended" :text) "(defun g () 1)")
          (is (wait-for (lambda () (pine.ns:read (pine.buf:at "probe-unattended" :face)))
                        :seconds 15)
              "the thread never came back, so the daemon lost it")
          (is (eq :error (pine.err:evaluation-status (first faults)))))))))

(test asking-your-own-actor-signals-instead-of-hanging
  "The liveness contract is checked, not documented: a receive that blocks on
itself gets a condition, which the debugger surfaces like any other fault."
  (with-fixture substrate ()
    (within-seconds 30
      (with-surface (faults :park-seconds 1)
        (let* ((sys (pine.core.server:actor-system *server*))
               (probe (sento.actor-context:actor-of
                       sys :name (format nil "self-ask-~a" (gensym))
                       :dispatcher :pinned
                       :receive (lambda (msg)
                                  (pine.err:with-debugger (:label "self ask")
                                    (when (eq :ask-self (first msg))
                                      (pine.core.actor:ask sento.actor:*self*
                                                           '(:ping) :timeout 1)))))))
          (sento.actor:tell probe '(:ask-self))
          (is (wait-for (lambda () faults))
              "asking your own actor hung instead of signalling")
          (is (string= "BLOCKING-ASK-IN-RECEIVE"
                       (pine.err:evaluation-condition-type (first faults)))))))))
(defun mailbox-of (actor)
  "ACTOR's message box. Pinned actors get a message-box/bt, one thread each;
anything on a shared dispatcher gets a message-box/dp and queues behind
whatever else that pool is running."
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
      (probe-parser "probe-structural")
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
