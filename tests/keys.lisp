(in-package :pine.test)

(def-suite* :pine.keys :in :pine)

(test a-chord-spec-parses-into-modifiers-and-a-symbol
  (let ((k (pine.editor.key:parse-key "C-M-S-s-Return")))
    (is (string= "Return" (pine.editor.key:key-sym k)))
    (is-true (pine.editor.key:key-ctrl k))
    (is-true (pine.editor.key:key-meta k))
    (is-true (pine.editor.key:key-shift k))
    (is-true (pine.editor.key:key-super k)))
  (is (string= "-" (pine.editor.key:key-sym (pine.editor.key:parse-key "M--"))))
  (is (string= "x" (pine.editor.key:key-sym (pine.editor.key:parse-key "x")))))

(test keys-are-interned-so-identity-is-equality
  (is (eq (pine.editor.key:parse-key "C-x") (pine.editor.key:parse-key "C-x")))
  (is-true (pine.editor.key:key= (pine.editor.key:parse-key "C-x")
                                 (pine.editor.key:parse-key "C-x")))
  (is-false (pine.editor.key:key= (pine.editor.key:parse-key "C-x")
                                  (pine.editor.key:parse-key "M-x"))))

(test a-chord-string-round-trips
  (dolist (spec '("C-x" "M-." "C-M-f" "Return" "s-Return" "C-S-a"))
    (is (string= spec (pine.editor.key:key->string
                       (pine.editor.key:parse-key spec))))))

(test parse-chord-yields-one-key-or-a-sequence
  (is (typep (pine.editor.key:parse-chord "C-x") 'pine.editor.key:key))
  (let ((seq (pine.editor.key:parse-chord "C-x C-f")))
    (is (listp seq))
    (is (= 2 (length seq)))
    (is (eq (pine.editor.key:parse-key "C-f") (second seq)))))

;;;; A key sequence is a path, because a prefix map is a directory.

(test a-binding-is-a-path-and-a-prefix-is-a-directory
  (pine.ns:with-space ()
    (pine.editor.keymap:mount)
    (pine.editor.keymap:bind :probe "a" "cmd-a")
    (pine.editor.keymap:bind :probe "C-c p" "deep")
    (is (fset:equal? (pine.cmd:at "cmd-a") (pine.editor.keymap:lookup :probe "a")))
    (is (null (pine.editor.keymap:lookup :probe "b")))
    (is-true (pine.editor.keymap:prefix-p (pine.editor.keymap:lookup :probe "C-c")))
    (is (fset:equal? (pine.cmd:at "deep")
                     (pine.ns:read (pine.editor.keymap:at :probe "C-c p"))))))

(test a-chord-normalizes-on-write
  "C-M-x and M-C-x are one path, so there is no aliasing to remember."
  (pine.ns:with-space ()
    (pine.editor.keymap:mount)
    (pine.ns:write (pine.path:parse "/key/mode/probe/M-C-x") (pine.cmd:at "aliased"))
    (is (fset:equal? (pine.cmd:at "aliased")
                     (pine.ns:read (pine.path:parse "/key/mode/probe/C-M-x"))))))

(test bindings-render-chords-space-joined
  (pine.ns:with-space ()
    (pine.editor.keymap:mount)
    (pine.editor.keymap:define-keys :probe-bindings
      "C-x C-f" "find-file"
      "M-x"     "execute-command")
    (let ((bindings (pine.editor.keymap:bindings :probe-bindings)))
      (is (fset:equal? (pine.cmd:at "find-file")
                       (cdr (assoc "C-x C-f" bindings :test #'string=))))
      (is (fset:equal? (pine.cmd:at "execute-command")
                       (cdr (assoc "M-x" bindings :test #'string=)))))))

(test a-mode-falls-back-through-its-parents-for-a-key
  "The keymap chain is the mode chain, read now."
  (pine.ns:with-space ()
    (pine.mode:mount)
    (pine.editor.keymap:mount)
    (pine.editor.keymap:bind :text "a" "from-text")
    (pine.editor.keymap:bind :lisp "b" "from-lisp")
    (let ((roots (pine.editor.keymap:roots :lisp nil)))
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
  (flet ((times (arg) (setf (pine.cmd:prefix) arg) (pine.cmd:times)))
    (is (= 1 (times nil)))
    (is (= 3 (times 3)))
    (is (= 4 (times '(4))))
    (is (= 16 (times '(16))))
    (is (= -1 (times '-)))
    (setf (pine.cmd:prefix) nil)))

(test only-a-bare-printable-key-self-inserts
  (is-true (pine.editor.command:self-insert-key-p (pine.editor.key:parse-key "a")))
  (is-true (pine.editor.command:self-insert-key-p (pine.editor.key:parse-key "S-a")))
  (is-false (pine.editor.command:self-insert-key-p (pine.editor.key:parse-key "C-a")))
  (is-false (pine.editor.command:self-insert-key-p (pine.editor.key:parse-key "M-a")))
  (is-false (pine.editor.command:self-insert-key-p (pine.editor.key:parse-key "Return"))))

(test dispatch-runs-a-binding-and-clears-the-pending-chord
  (with-fixture substrate ()
    (in-user "(defvar *probe-chord* nil)")
    (in-user "(defcommand probe-chord-cmd () (setf *probe-chord* :ran))")
    (in-user "(global-set-key (kbd \"C-c 9\") 'probe-chord-cmd)")
    (press "C-c")
    (is (equal "C-c" (pine.editor.ask:ask :client :pending-keys)))
    (press "9")
    (is (eq :ran (user-value "*PROBE-CHORD*")))
    (is (null (pine.editor.ask:ask :client :pending-keys)))))

(test a-mode-prefix-does-not-hide-a-global-chord
  (with-fixture substrate ()
    (in-user "(defvar *probe-which* nil)")
    (in-user "(defcommand probe-global () (setf *probe-which* :global))")
    (in-user "(defcommand probe-mode () (setf *probe-which* :mode))")
    (in-user "(write /mode/probe-chord {:parent :text})")
    (in-user "(define-key (keymap :probe-chord) (kbd \"C-c C-q\") 'probe-mode)")
    (in-user "(global-set-key (kbd \"C-c q\") 'probe-global)")
    (in-user "(set-buffer-mode (buffer \"scratch\") :probe-chord)")
    (press* "C-c" "q")
    (is (eq :global (user-value "*PROBE-WHICH*")))
    (press* "C-c" "C-q")
    (is (eq :mode (user-value "*PROBE-WHICH*")))))

(test a-dead-end-chord-echoes-undefined-instead-of-inserting
  (with-fixture substrate ()
    (in-user "(set-buffer-mode (buffer \"scratch\") :text)")
    (let ((before (btext "scratch")))
      (press* "C-c" "j")
      (is (equal before (btext "scratch")))
      (is (search "undefined" (pine.editor.echo:current-message))))))

(test a-universal-argument-multiplies-and-a-digit-accumulates
  (with-fixture substrate ()
    (press "C-u")
    (is (equal '(4) (pine.cmd:prefix)))
    (press "C-u")
    (is (equal '(16) (pine.cmd:prefix)))
    (press "C-g")
    (press* "M-1" "M-2")
    (is (= 12 (pine.cmd:prefix)))))

(test a-negative-argument-flips-the-sign
  (with-fixture substrate ()
    (press "M--")
    (is (eq '- (pine.cmd:prefix)))
    (press "M-3")
    (is (= -3 (pine.cmd:prefix)))))

(test the-prefix-argument-repeats-a-self-insert
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "prefix-probe")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (press* "M-3" "z")
      (is (string= "zzz" (btext buf))))))

(test read-next-key-captures-one-key-and-then-lets-go
  (with-fixture substrate ()
    (let ((seen nil))
      (pine.editor.command:read-next-key *client* (lambda (k) (setf seen k)))
      (press "q")
      (is (eq (pine.editor.key:parse-key "q") seen))
      (is (null (pine.cmd:said :reader))))))

(test key-binding-reports-a-command-a-prefix-or-nothing
  (with-fixture substrate ()
    (is (fset:equal? (pine.cmd:at "execute-command")
                     (pine.editor.command:key-binding
                      *client* (pine.editor.key:parse-key "M-x"))))
    (is (eq t (pine.editor.command:key-binding
               *client* (pine.editor.key:parse-key "C-x"))))
    (is (null (pine.editor.command:key-binding
               *client* (pine.editor.key:parse-key "s-F12"))))))
