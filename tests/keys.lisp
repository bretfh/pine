(in-package :pine.test)

(def-suite* :pine.keys :in :pine)

(test a-chord-spec-parses-into-modifiers-and-a-symbol
  (let ((k (pine.key:parse-key "C-M-S-s-Return")))
    (is (string= "Return" (pine.key:key-sym k)))
    (is-true (pine.key:key-ctrl k))
    (is-true (pine.key:key-meta k))
    (is-true (pine.key:key-shift k))
    (is-true (pine.key:key-super k)))
  (is (string= "-" (pine.key:key-sym (pine.key:parse-key "M--"))))
  (is (string= "x" (pine.key:key-sym (pine.key:parse-key "x")))))

(test keys-are-interned-so-identity-is-equality
  (is (eq (pine.key:parse-key "C-x") (pine.key:parse-key "C-x")))
  (is-true (pine.key:key= (pine.key:parse-key "C-x")
                                 (pine.key:parse-key "C-x")))
  (is-false (pine.key:key= (pine.key:parse-key "C-x")
                                  (pine.key:parse-key "M-x"))))

(test a-chord-string-round-trips
  (dolist (spec '("C-x" "M-." "C-M-f" "Return" "s-Return" "C-S-a"))
    (is (string= spec (pine.key:key->string
                       (pine.key:parse-key spec))))))

(test parse-chord-yields-one-key-or-a-sequence
  (is (typep (pine.key:parse-chord "C-x") 'pine.key:key))
  (let ((seq (pine.key:parse-chord "C-x C-f")))
    (is (listp seq))
    (is (= 2 (length seq)))
    (is (eq (pine.key:parse-key "C-f") (second seq)))))

;;;; A key sequence is a path, because a prefix map is a directory.

(test a-binding-is-a-path-and-a-prefix-is-a-directory
  (pine.ns:with-space ()
    (pine.ns:raise :key)
    (pine.key:bind :probe "a" "cmd-a")
    (pine.key:bind :probe "C-c p" "deep")
    (is (fset:equal? (pine.cmd:at "cmd-a") (pine.key:lookup :probe "a")))
    (is (null (pine.key:lookup :probe "b")))
    (is-true (pine.key:prefix-p (pine.key:lookup :probe "C-c")))
    (is (fset:equal? (pine.cmd:at "deep")
                     (pine.ns:read (pine.key:at :probe "C-c p"))))))

(test a-chord-normalizes-on-write
  "C-M-x and M-C-x are one path, so there is no aliasing to remember."
  (pine.ns:with-space ()
    (pine.ns:raise :key)
    (pine.ns:write (pine.path:parse "/key/mode/probe/M-C-x") (pine.cmd:at "aliased"))
    (is (fset:equal? (pine.cmd:at "aliased")
                     (pine.ns:read (pine.path:parse "/key/mode/probe/C-M-x"))))))

(test bindings-render-chords-space-joined
  (pine.ns:with-space ()
    (pine.ns:raise :key)
    (pine.key:define-keys :probe-bindings
      "C-x C-f" "find-file"
      "M-x"     "execute-command")
    (let ((bindings (pine.key:bindings :probe-bindings)))
      (is (fset:equal? (pine.cmd:at "find-file")
                       (cdr (assoc "C-x C-f" bindings :test #'string=))))
      (is (fset:equal? (pine.cmd:at "execute-command")
                       (cdr (assoc "M-x" bindings :test #'string=)))))))

(test a-mode-falls-back-through-its-parents-for-a-key
  "The keymap chain is the mode chain, read now."
  (pine.ns:with-space ()
    (pine.ns:raise :mode)
    (pine.ns:raise :key)
    (pine.key:bind :text "a" "from-text")
    (pine.key:bind :lisp "b" "from-lisp")
    (let ((roots (pine.key:roots :lisp nil)))
      (is (member "/key/global" (mapcar #'pine.path:text roots) :test #'string=))
      (is (fset:equal? (pine.cmd:at "from-text")
                       (loop :for root :in roots
                             :for v = (pine.ns:read (pine.path:path root "a"))
                             :when v :do (return v)))))))
(test a-command-answers-to-a-symbol-and-a-string-alike
  "A command is a path, and 'greet and \"greet\" name the same one."
  (is (fset:equal? (pine.cmd:at 'greet) (pine.cmd:at "greet")))
  (is (string= "/cmd/greet" (pine.path:text (pine.cmd:at 'greet))))
  (with-fixture substrate ()
    (pine.ns:write (pine.cmd:at "probe-sym") (lambda () :ran))
    (is (member "probe-sym" (pine.cmd:names) :test #'string=))
    (is (not (null (pine.ns:read (pine.cmd:at 'probe-sym)))))))

(test the-prefix-argument-reads-as-a-number
  (flet ((times (arg) (setf (pine.key:prefix) arg) (pine.key:times)))
    (is (= 1 (times nil)))
    (is (= 3 (times 3)))
    (is (= 4 (times '(4))))
    (is (= 16 (times '(16))))
    (is (= -1 (times '-)))
    (setf (pine.key:prefix) nil)))

(test only-a-bare-printable-key-self-inserts
  (is-true (pine.key:self-insert-key-p (pine.key:parse-key "a")))
  (is-true (pine.key:self-insert-key-p (pine.key:parse-key "S-a")))
  (is-false (pine.key:self-insert-key-p (pine.key:parse-key "C-a")))
  (is-false (pine.key:self-insert-key-p (pine.key:parse-key "M-a")))
  (is-false (pine.key:self-insert-key-p (pine.key:parse-key "Return"))))

(test dispatch-runs-a-binding-and-clears-the-pending-chord
  (with-fixture substrate ()
    (in-user "(defvar *probe-chord* nil)")
    (in-user "(write /cmd/probe-chord-cmd (fn () (setf *probe-chord* :ran) {}))")
    (in-user "(write /key/global/C-c/9 /cmd/probe-chord-cmd)")
    (press "C-c")
    (is (equal "C-c" (car (pine.key:said :pending))))
    (press "9")
    (is (eq :ran (user-value "*PROBE-CHORD*")))
    (is (null (car (pine.key:said :pending))))))

(test a-mode-prefix-does-not-hide-a-global-chord
  (with-fixture substrate ()
    (in-user "(defvar *probe-which* nil)")
    (in-user "(write /cmd/probe-global (fn () (setf *probe-which* :global) {}))")
    (in-user "(write /cmd/probe-mode (fn () (setf *probe-which* :mode) {}))")
    (in-user "(write /mode/probe-chord {:parent :text})")
    (in-user "(write /key/mode/probe-chord/C-c/C-q /cmd/probe-mode)")
    (in-user "(write /key/global/C-c/q /cmd/probe-global)")
    (in-user "(write /buf/scratch/mode :probe-chord)")
    (press* "C-c" "q")
    (is (eq :global (user-value "*PROBE-WHICH*")))
    (press* "C-c" "C-q")
    (is (eq :mode (user-value "*PROBE-WHICH*")))))

(test a-dead-end-chord-echoes-undefined-instead-of-inserting
  (with-fixture substrate ()
    (in-user "(write /buf/scratch/mode :text)")
    (let ((before (btext "scratch")))
      (press* "C-c" "j")
      (is (equal before (btext "scratch")))
      (is (search "undefined" (pine.echo:current-message))))))

(test a-universal-argument-multiplies-and-a-digit-accumulates
  (with-fixture substrate ()
    (press "C-u")
    (is (equal '(4) (pine.key:prefix)))
    (press "C-u")
    (is (equal '(16) (pine.key:prefix)))
    (press "C-g")
    (press* "M-1" "M-2")
    (is (= 12 (pine.key:prefix)))))

(test a-negative-argument-flips-the-sign
  (with-fixture substrate ()
    (press "M--")
    (is (eq '- (pine.key:prefix)))
    (press "M-3")
    (is (= -3 (pine.key:prefix)))))

(test the-prefix-argument-repeats-a-self-insert
  (with-fixture substrate ()
    (pine.buf:make "prefix-probe")
    (pine.ns:write (pine.buf:at "prefix-probe" :mode) :text)
    (pine.ns:write (pine.path:parse "/buf/current") (pine.buf:at "prefix-probe"))
    (press* "M-3" "z")
    (is (string= "zzz" (btext "prefix-probe")))))

(test read-next-key-captures-one-key-and-then-lets-go
  (with-fixture substrate ()
    (let ((seen nil))
      (pine.key:read-next-key (lambda (k) (setf seen k)))
      (press "q")
      (is (eq (pine.key:parse-key "q") seen))
      (is (null (pine.key:said :reader))))))

(test key-binding-reports-a-command-a-prefix-or-nothing
  (with-fixture substrate ()
    (is (fset:equal? (pine.cmd:at "execute-command")
                     (pine.key:key-binding (pine.key:parse-key "M-x"))))
    (is (eq t (pine.key:key-binding (pine.key:parse-key "C-x"))))
    (is (null (pine.key:key-binding (pine.key:parse-key "s-F12"))))))

(test two-spaces-dispatch-separately
  "The chord typed so far, the prefix argument, the key that fired and the
command before it are at /dispatch. They were one table this image shared, so
two frontends attached to one daemon had one C-u between them and a prefix
argument typed in either reached whichever command ran next."
  (let ((a (pine.ns:fresh))
        (b (pine.ns:fresh)))
    (pine.ns:with-space (a)
      (pine.ns:raise :cmd)
      (pine.ns:raise :key)
      (setf (pine.key:prefix) (list 4))
      (setf (pine.key:last) "probe-a"))
    (pine.ns:with-space (b)
      (pine.ns:raise :cmd)
      (pine.ns:raise :key)
      (is (null (pine.key:prefix))
          "a prefix argument typed in one space is waiting in the other")
      (is (null (pine.key:last))
          "the command one space ran is what the other says ran last")
      (setf (pine.key:last) "probe-b"))
    (pine.ns:with-space (a)
      (is (equal (list 4) (pine.key:prefix)) "the space that typed C-u lost it")
      (is (string= "probe-a" (pine.key:last))
          "and the other space decided what this one ran last")
      (is (string= "probe-a" (pine.ns:read (pine.path:parse "/cmd/last")))
          "/cmd/last is the same value read through the path"))))
