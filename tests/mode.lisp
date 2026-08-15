(in-package :pine.test)

(def-suite* :pine.mode :in :pine)

(defmacro with-modes (&body body)
  "Only the modes this test declares, so what pine itself ships cannot answer
for it."
  `(let ((had (pine/repl/mode:modes)))
     (dolist (m had) (pine/repl/mode:unmode (pine/repl/mode:name m)))
     (unwind-protect (progn ,@body)
       (dolist (m (pine/repl/mode:modes))
         (pine/repl/mode:unmode (pine/repl/mode:name m)))
       (dolist (m had) (pine/repl/mode:remode m)))))

(test a-mode-falls-back-to-its-parent
  (with-modes
    (pine/repl/mode:mode "probe-text" :settings '(:tab-width 8 :indicator "Text"))
    (pine/repl/mode:mode "probe-prog" :parent "probe-text" :settings '(:comment ";"))
    (pine/repl/mode:mode "probe-lisp" :parent "probe-prog" :settings '(:indicator "Lisp"))
    (is (equal '("probe-lisp" "probe-prog" "probe-text")
               (mapcar #'pine/repl/mode:name (pine/repl/mode:chain "probe-lisp"))))
    (is (equal ";" (pine/repl/mode:setting "probe-lisp" :comment))
        "a setting the mode does not carry comes from up the chain")
    (is (eql 8 (pine/repl/mode:setting "probe-lisp" :tab-width)))
    (is (equal "Lisp" (pine/repl/mode:setting "probe-lisp" :indicator))
        "the most specific mode that says wins")))

(test a-chain-someone-wrote-in-a-circle-ends
  (with-modes
    (pine/repl/mode:mode "probe-a" :parent "probe-b")
    (pine/repl/mode:mode "probe-b" :parent "probe-a")
    (is (= 2 (length (pine/repl/mode:chain "probe-a"))))))

(test a-minor-mode-answers-before-the-mode-and-in-precedence-order
  (with-modes
    (pine/repl/mode:mode "probe-text")
    (pine/repl/mode:minor "probe-list" :precedence 15)
    (pine/repl/mode:minor "probe-select" :precedence 20)
    (pine/repl/mode:handle "probe-text" :activate (lambda () :from-mode))
    (pine/repl/mode:handle "probe-list" :activate (lambda () :from-list))
    (pine/repl/mode:handle "probe-select" :activate (lambda () :from-select))
    (let ((s (pine/repl/session:open-session :mode "probe-text"
                                     :minors '("probe-list" "probe-select"))))
      (unwind-protect
           (progn
             (is (equal '(:from-select :from-list :from-mode)
                        (mapcar #'funcall (pine/repl/mode:claimants s :activate)))
                 "highest precedence first, then the mode chain")
             (is (eq :from-select (funcall (pine/repl/mode:handler s :activate)))))
        (pine/repl/session:close s)))))

(test a-mode-binds-a-chord-to-a-command-and-the-chain-resolves-it
  (with-modes
    (pine/repl/command:defcommand "probe-save" () () :saved)
    (pine/repl/command:defcommand "probe-eval" () () :evaluated)
    (unwind-protect
         (progn
           (pine/repl/mode:mode "probe-text")
           (pine/repl/mode:mode "probe-lisp" :parent "probe-text")
           (pine/repl/mode:bind "probe-text" "C-x C-s" "probe-save")
           (pine/repl/mode:bind "probe-lisp" "C-x C-e" "probe-eval")
           (let ((s (pine/repl/session:open-session :mode "probe-lisp")))
             (unwind-protect
                  (progn
                    (is (equal :evaluated
                               (pine/repl/command:run (pine/repl/mode:binding s "C-x C-e"))))
                    (is (equal :saved
                               (pine/repl/command:run (pine/repl/mode:binding s "C-x C-s")))
                        "a chord the mode does not bind comes from its parent")
                    (is (null (pine/repl/mode:binding s "C-c C-z"))))
               (pine/repl/session:close s))))
      (pine/repl/command:forget "probe-save")
      (pine/repl/command:forget "probe-eval"))))

(test a-minor-mode-binding-wins-over-the-mode
  (with-modes
    (pine/repl/command:defcommand "probe-mode-key" () () :mode)
    (pine/repl/command:defcommand "probe-minor-key" () () :minor)
    (unwind-protect
         (progn
           (pine/repl/mode:mode "probe-text")
           (pine/repl/mode:minor "probe-over" :precedence 20)
           (pine/repl/mode:bind "probe-text" "RET" "probe-mode-key")
           (pine/repl/mode:bind "probe-over" "RET" "probe-minor-key")
           (let ((s (pine/repl/session:open-session :mode "probe-text"
                                            :minors '("probe-over"))))
             (unwind-protect
                  (is (eq :minor (pine/repl/command:run (pine/repl/mode:binding s "RET"))))
               (pine/repl/session:close s))))
      (pine/repl/command:forget "probe-mode-key")
      (pine/repl/command:forget "probe-minor-key"))))

(test where-a-file-is-can-say-what-it-is
  (with-modes
    (pine/repl/mode:mode "probe-lisp" :claims '((:files "*.lisp" "*.asd")))
    (pine/repl/mode:mode "probe-config" :claims '((:paths "*/pine/*.lisp")))
    (is (equal "probe-config" (pine/repl/mode:name (pine/repl/mode:mode-for "/home/x/.config/pine/init.lisp")))
        ":paths is matched against the whole namestring and asked first")
    (is (equal "probe-lisp" (pine/repl/mode:name (pine/repl/mode:mode-for "/home/x/src/ns.lisp"))))
    (is (null (pine/repl/mode:mode-for "/home/x/notes.txt")))))

(test a-session-reads-a-setting-through-its-mode
  (with-modes
    (pine/repl/mode:mode "probe-text" :settings '(:tab-width 8))
    (pine/repl/mode:mode "probe-lisp" :parent "probe-text" :settings '(:indent 2))
    (let ((s (pine/repl/session:open-session :mode "probe-lisp")))
      (unwind-protect
           (progn
             (is (eql 2 (pine/repl/mode:setting s :indent)))
             (is (eql 8 (pine/repl/mode:setting s :tab-width)))
             (is (eql :none (pine/repl/mode:setting s :absent :none))))
        (pine/repl/session:close s)))))
