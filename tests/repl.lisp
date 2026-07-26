(in-package :pine.test)

(def-suite* :pine.repl :in :pine)

;;;; The repl submits from inside the buffer actor's receive, so everything it
;;;; touches has to be reachable from there. Reaching for the client is not: no
;;;; buffer actor binds *client*, and a fault in a receive parks a worker of the
;;;; shared dispatcher. Every test here is bounded, so a receive that wedges
;;;; fails the suite instead of hanging it.

(defmacro within-seconds (seconds &body body)
  "Run BODY, failing rather than hanging if it takes longer than SECONDS."
  `(handler-case (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout ()
       (fail "did not finish within ~d second~:p; a receive is wedged" ,seconds))))

(defun repl-text ()
  "The repl buffer's text. A wedged actor answers an ask with a handler-error
cons rather than a string, and SEARCH over that quietly finds nothing, so the
type is asserted here instead of every caller passing by accident."
  (let ((answer (btext "*repl*")))
    (is (stringp answer) "the repl buffer answered ~s, so its receive is wedged" answer)
    (if (stringp answer) answer "")))

(defun wait-for-repl (predicate &key (seconds 5))
  "Poll the repl buffer until PREDICATE holds on its text, or give up."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :for text = (repl-text)
          :when (funcall predicate text) :return text
          :when (> (get-internal-real-time) deadline) :return nil
          :do (sleep 0.05))))

(test the-repl-opens-with-a-prompt
  (with-fixture substrate ()
    (within-seconds 20
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (is (search "pine> " (repl-text))))))

(test submitting-an-expression-answers-with-its-value
  (with-fixture substrate ()
    (within-seconds 30
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (type-text "1")
      (press "Return")
      ;; the answer is a line of its own: the input line already reads "pine> 1",
      ;; so matching "1" anywhere would pass on the echo alone
      (is (not (null (wait-for-repl
                      (lambda (text)
                        (member "1" (pine.text.buffer:split-lines text)
                                :test #'string=)))))
          "the repl never answered 1 on a line of its own")
      (is (search "pine> " (repl-text))
          "the prompt must come back for the next form"))))

(test submitting-arithmetic-answers-with-the-result
  (with-fixture substrate ()
    (within-seconds 30
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (type-text "(+ 2 3)")
      (press "Return")
      (is (not (null (wait-for-repl (lambda (text) (search "5" text)))))
          "(+ 2 3) never answered 5"))))

(test an-empty-submit-is-not-an-evaluation
  (with-fixture substrate ()
    (within-seconds 20
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (let ((before (repl-text)))
        (press "Return")
        (sleep 0.3)
        (is (string= before (repl-text)))))))

(test a-failing-form-answers-and-leaves-the-repl-usable
  (with-fixture substrate ()
    (within-seconds 30
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (type-text "(error \"probe\")")
      (press "Return")
      (is (not (null (wait-for-repl (lambda (text) (search "probe" text)))))
          "a failing form must report, not wedge the buffer")
      (type-text "(+ 1 1)")
      (press "Return")
      (is (not (null (wait-for-repl (lambda (text) (search "2" text)))))
          "the repl must still evaluate after a failure"))))

(test the-daemon-still-answers-after-a-repl-submit
  "The point of the whole exercise: a repl submit must not park a worker of the
shared dispatcher, because every actor in the daemon draws from it."
  (with-fixture substrate ()
    (within-seconds 30
      (pine.editor.command::call-command "open-repl")
      (sleep 0.3)
      (type-text "1")
      (press "Return")
      (sleep 0.5)
      (is (equal '("a" "b")
                 (let ((probe (pine.editor.frame::make-buffer "post-repl-probe")))
                   (pine.editor.frame::set-buffer-mode probe :text-mode)
                   (sento.actor:tell probe (list :replace-content
                                                 :content (format nil "a~%b")))
                   (sleep 0.2)
                   (pine.text.buffer:split-lines (btext probe))))
          "a buffer actor stopped answering after the repl submitted"))))
