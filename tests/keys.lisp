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

(test a-keymap-stores-a-name-and-answers-it
  (let ((map (pine.editor.keymap:make-keymap :name :probe)))
    (pine.editor.keymap:define-key map (pine.editor.key:parse-key "a") "cmd-a")
    (is (string= "cmd-a" (pine.editor.keymap:keymap-lookup
                          map (pine.editor.key:parse-key "a"))))
    (is (null (pine.editor.keymap:keymap-lookup
               map (pine.editor.key:parse-key "b"))))))

(test a-chord-makes-a-prefix-table
  (let ((map (pine.editor.keymap:make-keymap :name :probe-chord)))
    (pine.editor.keymap:define-key map (pine.editor.key:parse-chord "C-c p") "deep")
    (let ((entry (pine.editor.keymap:keymap-lookup
                  map (pine.editor.key:parse-key "C-c"))))
      (is-true (pine.editor.keymap:prefix-p entry))
      (is (string= "deep" (gethash (pine.editor.key:parse-key "p") entry))))))

(test a-local-binding-shadows-the-parent
  (let* ((parent (pine.editor.keymap:make-keymap :name :probe-parent))
         (child (pine.editor.keymap:make-keymap :name :probe-child :parent parent)))
    (pine.editor.keymap:define-key parent (pine.editor.key:parse-key "a") "from-parent")
    (pine.editor.keymap:define-key parent (pine.editor.key:parse-key "b") "only-parent")
    (pine.editor.keymap:define-key child (pine.editor.key:parse-key "a") "from-child")
    (is (string= "from-child" (pine.editor.keymap:keymap-lookup
                               child (pine.editor.key:parse-key "a"))))
    (is (string= "only-parent" (pine.editor.keymap:keymap-lookup
                                child (pine.editor.key:parse-key "b"))))
    (is (= 2 (length (pine.editor.keymap:keymap-tables child))))))

(test keymap-bindings-render-chords-space-joined
  (let ((map (pine.editor.keymap:make-keymap :name :probe-bindings)))
    (pine.editor.keymap:define-keys map
      "C-x C-f" "find-file"
      "M-x"     "execute-command")
    (let ((bindings (pine.editor.keymap:keymap-bindings map)))
      (is (equal "find-file" (cdr (assoc "C-x C-f" bindings :test #'string=))))
      (is (equal "execute-command" (cdr (assoc "M-x" bindings :test #'string=)))))))

(test keymap-bindings-append-unshadowed-parent-entries
  (let* ((parent (pine.editor.keymap:make-keymap :name :probe-bp))
         (child (pine.editor.keymap:make-keymap :name :probe-bc :parent parent)))
    (pine.editor.keymap:define-keys parent "a" "parent-a" "b" "parent-b")
    (pine.editor.keymap:define-keys child "a" "child-a")
    (let ((bindings (pine.editor.keymap:keymap-bindings child t)))
      (is (equal "child-a" (cdr (assoc "a" bindings :test #'string=))))
      (is (equal "parent-b" (cdr (assoc "b" bindings :test #'string=)))))))

(test a-command-answers-to-a-symbol-and-a-string-alike
  (is (string= "greet" (pine.editor.command:command-key 'greet)))
  (is (string= "greet" (pine.editor.command:command-key "greet")))
  (pine.editor.command:define-command probe-sym () :ran)
  (is (not (null (pine.editor.command:find-command "probe-sym"))))
  (is (not (null (pine.editor.command:find-command 'probe-sym)))))

(test the-prefix-argument-reads-as-a-number
  (is (= 1 (pine.editor.command:prefix-numeric-value nil)))
  (is (= 7 (pine.editor.command:prefix-numeric-value nil 7)))
  (is (= 3 (pine.editor.command:prefix-numeric-value 3)))
  (is (= 4 (pine.editor.command:prefix-numeric-value '(4))))
  (is (= 16 (pine.editor.command:prefix-numeric-value '(16))))
  (is (= -1 (pine.editor.command:prefix-numeric-value '-))))

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
    (in-user "(defmode probe-chord-mode (:parent :text-mode))")
    (in-user "(define-key (keymap :probe-chord-mode) (kbd \"C-c C-q\") 'probe-mode)")
    (in-user "(global-set-key (kbd \"C-c q\") 'probe-global)")
    (in-user "(set-buffer-mode (buffer \"scratch\") :probe-chord-mode)")
    (press* "C-c" "q")
    (is (eq :global (user-value "*PROBE-WHICH*")))
    (press* "C-c" "C-q")
    (is (eq :mode (user-value "*PROBE-WHICH*")))))

(test a-dead-end-chord-echoes-undefined-instead-of-inserting
  (with-fixture substrate ()
    (in-user "(set-buffer-mode (buffer \"scratch\") :text-mode)")
    (let ((before (btext "scratch")))
      (press* "C-c" "j")
      (is (equal before (btext "scratch")))
      (is (search "undefined" (pine.editor.echo:current-message))))))

(test a-universal-argument-multiplies-and-a-digit-accumulates
  (with-fixture substrate ()
    (press "C-u")
    (is (equal '(4) (pine.editor.frame:prefix-arg *client*)))
    (press "C-u")
    (is (equal '(16) (pine.editor.frame:prefix-arg *client*)))
    (press "C-g")
    (press* "M-1" "M-2")
    (is (= 12 (pine.editor.frame:prefix-arg *client*)))))

(test a-negative-argument-flips-the-sign
  (with-fixture substrate ()
    (press "M--")
    (is (eq '- (pine.editor.frame:prefix-arg *client*)))
    (press "M-3")
    (is (= -3 (pine.editor.frame:prefix-arg *client*)))))

(test the-prefix-argument-repeats-a-self-insert
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "prefix-probe")))
      (pine.editor.frame::set-buffer-mode buf :text-mode)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (press* "M-3" "z")
      (is (string= "zzz" (btext buf))))))

(test read-next-key-captures-one-key-and-then-lets-go
  (with-fixture substrate ()
    (let ((seen nil))
      (pine.editor.command:read-next-key *client* (lambda (k) (setf seen k)))
      (press "q")
      (is (eq (pine.editor.key:parse-key "q") seen))
      (is (null (pine.editor.frame:pending-key-reader *client*))))))

(test key-binding-reports-a-command-a-prefix-or-nothing
  (with-fixture substrate ()
    (is (equal "execute-command"
               (pine.editor.command:key-binding
                *client* (pine.editor.key:parse-key "M-x"))))
    (is (listp (pine.editor.command:key-binding
                *client* (pine.editor.key:parse-key "C-x"))))
    (is (null (pine.editor.command:key-binding
               *client* (pine.editor.key:parse-key "s-F12"))))))
