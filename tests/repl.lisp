(in-package :pine.test)

(def-suite* :pine.repl :in :pine)

(defmacro with-session ((var &key (in "") (package :cl-user)) &body body)
  `(let* ((out (make-string-output-stream))
          (,var (pine.repl:open-session :input (make-string-input-stream ,in)
                                        :output out
                                        :package (find-package ,package))))
     (unwind-protect (progn ,@body) (pine.repl:close ,var))))

(defun said-by (session)
  (get-output-stream-string (pine.repl:output session)))

(test a-session-evaluates-lisp-and-answers-what-it-evaluated-to
  (with-session (s)
    (let ((e (pine.repl:evaluate s '(+ 1 2))))
      (is (equal '(3) (pine.repl:answered e)))
      (is (null (pine.repl:fault e))))))

(test the-image-never-sees-an-error-a-session-made
  "Handling errors our way is why the loop is ours. A form that signals lands in
the evaluation, the session stays open, and nothing reaches the image's
debugger."
  (with-session (s)
    (let ((e (pine.repl:evaluate s '(error "a probe"))))
      (is (typep (pine.repl:fault e) 'error))
      (is (null (pine.repl:answered e)))
      (is-true (pine.repl:openp s))
      (is (equal '(4) (pine.repl:answered (pine.repl:evaluate s '(* 2 2))))))))

(test what-a-form-printed-belongs-to-the-evaluation
  (with-session (s)
    (let ((e (pine.repl:evaluate s '(princ "hello"))))
      (is (equal "hello" (pine.repl:said e)))
      (is (equal "" (said-by s)) "it reached the terminal before PRINT was asked"))))

(test history-is-kept-across-evaluations-newest-first
  (with-session (s)
    (pine.repl:evaluate s '(+ 1 1))
    (pine.repl:evaluate s '(+ 2 2))
    (is (= 2 (length (pine.repl:history s))))
    (is (equal '(+ 2 2) (pine.repl:form (first (pine.repl:history s)))))))

(test a-session-reads-in-its-own-package
  (with-session (s :package :pine.repl)
    (is (eq (find-package :pine.repl) (symbol-package (pine.repl:read s "session"))))))

(test a-command-is-not-a-lisp-function
  "The reason for our own loop: a name that is not fbound is still something to
run, and its arguments are not evaluated."
  (pine.repl:defcommand "probe-echo" (&rest words) ()
    (format nil "~{~a~^ ~}" words))
  (unwind-protect
       (with-session (s)
         (is (null (fboundp 'probe-echo)))
         (let ((e (pine.repl:evaluate s '(probe-echo one two))))
           (is (equal '("one two") (pine.repl:answered e))
               "a bare word reaches a command as a word, not as a symbol")
           (is (null (pine.repl:fault e))))
         (is (equal '("a b")
                    (pine.repl:answered
                     (pine.repl:evaluate s '(probe-echo "a" "b")))))
         (is (equal '("ONE")
                    (pine.repl:answered
                     (pine.repl:evaluate s '(probe-echo 'one))))
             "quoting is how a command is handed an actual symbol"))
    (pine.repl:forget "probe-echo")))

(test a-command-defined-now-is-runnable-now
  (with-session (s)
    (is (null (pine.repl:command-named "probe-late")))
    (pine.repl:evaluate s '(pine.repl:defcommand "probe-late" () () :ran))
    (unwind-protect
         (is (equal '(:ran) (pine.repl:answered (pine.repl:evaluate s 'probe-late))))
      (pine.repl:forget "probe-late"))))

(test a-command-that-asks-reads-its-arguments-from-the-session
  (pine.repl:defcommand "probe-ask" (who) (:asks '((:prompt "Who? ")))
    (format nil "hello ~a" who))
  (unwind-protect
       (with-session (s :in "world
")
         (let ((e (pine.repl:evaluate s 'probe-ask)))
           (is (equal '("hello world") (pine.repl:answered e)))
           (is (search "Who? " (said-by s)))))
    (pine.repl:forget "probe-ask")))

(test running-a-command-nobody-defined-says-so
  (signals pine.repl:unknown-command (pine.repl:run "probe-absent")))

(test evaluation-can-be-wrapped-without-touching-the-loop
  "Controlling what happens around an evaluation is a method, which is the whole
reason EVALUATE is a generic function."
  (let ((seen nil))
    (unwind-protect
         (progn
           (defmethod pine.repl:evaluate :before ((s pine.repl:session) form)
             (push form seen))
           (with-session (s)
             (pine.repl:evaluate s '(+ 1 1))
             (is (equal '((+ 1 1)) seen))))
      (remove-method #'pine.repl:evaluate
                     (find-method #'pine.repl:evaluate '(:before)
                                  (list (find-class 'pine.repl:session) t))))))

(test the-loop-runs-until-its-input-ends
  (with-session (s :in "(+ 1 2)
(+ 3 4)
")
    (pine.repl:interact s)
    (let ((text (said-by s)))
      (is (search "3" text))
      (is (search "7" text)))
    (is-false (pine.repl:openp s))
    (is (= 2 (length (pine.repl:history s))))))

(test a-session-is-one-of-many
  (with-session (a)
    (with-session (b)
      (is (member a (pine.repl:sessions)))
      (is (member b (pine.repl:sessions)))
      (setf (pine.repl:package-of a) (find-package :pine.repl))
      (is (eq (find-package :cl-user) (pine.repl:package-of b))))))
