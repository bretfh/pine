(in-package :pine.test)

;;;; No path readtable here: sento.messageb:message-box/bt is a symbol with a
;;;; slash in it, and under pine.path:syntax that reads as a path.

(def-suite* :pine.liveness :in :pine)

;;;; What a fault is allowed to cost. A parser is an actor, so its fault records
;;;; itself at /err and unwinds rather than standing on that thread: these checks
;;;; fault one deliberately and then ask the rest of the daemon whether it
;;;; noticed -- another buffer, the paint path, the input path, and the faulted
;;;; parser itself. Every check is bounded, so a daemon that does get stuck fails
;;;; the suite instead of hanging it.

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
  "Run BODY with something watching /err that records each fault into VAR, and
the unattended park bounded at PARK-SECONDS. With ATTENDED it also marks each
fault as being looked at, so one that is standing on a thread keeps its
restarts past the deadline.

The watch is what tells pine.err that anything here is in a position to decide.
PARK-SECONDS is set globally, because the thread that faults is not this one
and a dynamic binding here would not reach it."
  (let ((saved-park (gensym "PARK")))
    `(let ((,var nil)
           (,saved-park (pine.ns:read (pine.path:parse "/park-seconds"))))
       (pine.ns:write (pine.path:parse "/park-seconds") ,park-seconds)
       (pine.ns:watch (pine.path:parse "/err")
                      (lambda (value)
                        (declare (ignore value))
                        (dolist (f (pine.err:faults))
                          (pushnew f ,var)
                          (when ,attended (setf (pine.err:attended-p f) t)))
                        nil)
                      :as :probe)
       (unwind-protect (progn ,@body)
         (pine.ns:watch (pine.path:parse "/err") nil :as :probe)
         (pine.ns:write (pine.path:parse "/park-seconds") ,saved-park)))))

(defun wait-for (predicate &key (seconds 5))
  "Poll PREDICATE until it holds, and answer whether it did."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :when (funcall predicate) :return t
          :when (> (get-internal-real-time) deadline) :return nil
          :do (sleep 0.05))))

(test a-faulted-parser-costs-nothing-at-all
  "The parser's fault lands at /err and its thread comes straight back, while
another buffer, the paint path and the input path are asked whether they
noticed. Nobody has to choose anything for that to be true."
  (with-fixture substrate ()
    (within-seconds 30
      (let ((painted 0))
        (setf (pine.editor.frame::paint-sink *client*)
              (lambda (&rest args) (declare (ignore args)) (incf painted)))
        (with-surface (faults)
          (let ((probe (probe-parser "probe-faulted")))
            (sento.actor:tell probe '(:probe-fault))
            (is (wait-for (lambda () faults))
                "the fault never reached /err")
            (pine.editor.frame::make-buffer "probe-neighbour")
            (pine.ns:write (pine.buf:at "probe-neighbour" :text) "still here")
            (is (string= "still here" (btext "probe-neighbour"))
                "a second buffer stopped answering after a parser faulted")
            ;; the command path, on this thread, the way the session loop runs it
            (press "x")
            (is (search "x" (btext "scratch"))
                "command dispatch stopped reaching buffers after a parser faulted")
            (is (plusp painted)
                "the renderer stopped painting after a parser faulted")
            (pine.ns:write (pine.buf:at "probe-faulted" :text) "(defun g () 1)")
            (is (wait-for (lambda () (pine.ns:read (pine.buf:at "probe-faulted" :face)))
                          :seconds 15)
                "the parser never took another message, so its thread was held")))))))

(test a-parser-that-faults-again-and-again-holds-nothing
  "Nothing decides any of them. Under a design that stood on the thread, the
second tell would be queued behind the first for as long as the deadline."
  (with-fixture substrate ()
    (within-seconds 30
      (with-surface (faults)
        (let ((probe (probe-parser "probe-repeat")))
          (dotimes (i 5) (sento.actor:tell probe '(:probe-fault)))
          (is (wait-for (lambda () (>= (length faults) 5)) :seconds 10)
              "only ~d of 5 faults came through, so the thread was held by one"
              (length faults))
          (is (every (lambda (f) (equal '("ABORT") (mapcar #'first (pine.err:offers f))))
                     faults)
              "a fault whose stack is gone must not claim it can be resumed"))))))

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
                       (pine.err:fault-type (first faults)))))))))
(defun mailbox-of (actor)
  "ACTOR's message box. Pinned actors get a message-box/bt, one thread each;
anything on a shared dispatcher gets a message-box/dp and queues behind
whatever else that pool is running."
  (sento.actor-cell:msgbox actor))

(defparameter +not-ours+ '("remote-sender")
  "Actors sento starts for itself, by name prefix.

ENABLE-REMOTING puts a sender on the shared pool. How the library carries a
message to another image is the library's business; the claim below is about
the actors pine starts.")

(defun shared-pool-actors (system)
  "Every actor pine started on SYSTEM that draws from a shared dispatcher."
  (loop :for actor :in (sento.actor-context:all-actors system)
        :for name := (sento.actor-cell:name actor)
        :unless (or (typep (mailbox-of actor) 'sento.messageb:message-box/bt)
                    (some (lambda (theirs)
                            (pine.provider.out:starts-with name theirs))
                          +not-ours+))
          :collect name))

(test no-actor-the-daemon-creates-draws-from-a-shared-pool
  "The isolation is structural, not conventional. Every actor pine starts owns
its thread, so no receive can be behind another one: not a repaint behind an
edit, and not one app's input behind another's."
  (with-fixture substrate ()
    (within-seconds 20
      (probe-parser "probe-structural")
      (pine.key::call-command "open-repl")
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
