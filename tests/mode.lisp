(in-package :pine.test)

(def-suite* :pine.mode :in :pine)

(defmacro with-modes (&body body)
  `(unwind-protect (progn ,@body)
     (dolist (m (pine.repl:modes))
       (when (eql 0 (search "probe" (pine.repl:name m)))
         (pine.repl:unmode (pine.repl:name m))))))

(test a-mode-falls-back-to-its-parent
  (with-modes
    (pine.repl:mode "probe-text" :settings '(:tab-width 8 :indicator "Text"))
    (pine.repl:mode "probe-prog" :parent "probe-text" :settings '(:comment ";"))
    (pine.repl:mode "probe-lisp" :parent "probe-prog" :settings '(:indicator "Lisp"))
    (is (equal '("probe-lisp" "probe-prog" "probe-text")
               (mapcar #'pine.repl:name (pine.repl:chain "probe-lisp"))))
    (is (equal ";" (pine.repl:setting "probe-lisp" :comment))
        "a setting the mode does not carry comes from up the chain")
    (is (eql 8 (pine.repl:setting "probe-lisp" :tab-width)))
    (is (equal "Lisp" (pine.repl:setting "probe-lisp" :indicator))
        "the most specific mode that says wins")))

(test a-chain-someone-wrote-in-a-circle-ends
  (with-modes
    (pine.repl:mode "probe-a" :parent "probe-b")
    (pine.repl:mode "probe-b" :parent "probe-a")
    (is (= 2 (length (pine.repl:chain "probe-a"))))))

(test a-minor-mode-answers-before-the-mode-and-in-precedence-order
  (with-modes
    (pine.repl:mode "probe-text")
    (pine.repl:minor "probe-list" :precedence 15)
    (pine.repl:minor "probe-select" :precedence 20)
    (pine.repl:handle "probe-text" :activate (lambda () :from-mode))
    (pine.repl:handle "probe-list" :activate (lambda () :from-list))
    (pine.repl:handle "probe-select" :activate (lambda () :from-select))
    (let ((s (pine.repl:open-session :mode "probe-text"
                                     :minors '("probe-list" "probe-select"))))
      (unwind-protect
           (progn
             (is (equal '(:from-select :from-list :from-mode)
                        (mapcar #'funcall (pine.repl:claimants s :activate)))
                 "highest precedence first, then the mode chain")
             (is (eq :from-select (funcall (pine.repl:handler s :activate)))))
        (pine.repl:close s)))))

(test a-mode-binds-a-chord-to-a-command-and-the-chain-resolves-it
  (with-modes
    (pine.repl:defcommand "probe-save" () () :saved)
    (pine.repl:defcommand "probe-eval" () () :evaluated)
    (unwind-protect
         (progn
           (pine.repl:mode "probe-text")
           (pine.repl:mode "probe-lisp" :parent "probe-text")
           (pine.repl:bind "probe-text" "C-x C-s" "probe-save")
           (pine.repl:bind "probe-lisp" "C-x C-e" "probe-eval")
           (let ((s (pine.repl:open-session :mode "probe-lisp")))
             (unwind-protect
                  (progn
                    (is (equal :evaluated
                               (pine.repl:run (pine.repl:binding s "C-x C-e"))))
                    (is (equal :saved
                               (pine.repl:run (pine.repl:binding s "C-x C-s")))
                        "a chord the mode does not bind comes from its parent")
                    (is (null (pine.repl:binding s "C-c C-z"))))
               (pine.repl:close s))))
      (pine.repl:forget "probe-save")
      (pine.repl:forget "probe-eval"))))

(test a-minor-mode-binding-wins-over-the-mode
  (with-modes
    (pine.repl:defcommand "probe-mode-key" () () :mode)
    (pine.repl:defcommand "probe-minor-key" () () :minor)
    (unwind-protect
         (progn
           (pine.repl:mode "probe-text")
           (pine.repl:minor "probe-over" :precedence 20)
           (pine.repl:bind "probe-text" "RET" "probe-mode-key")
           (pine.repl:bind "probe-over" "RET" "probe-minor-key")
           (let ((s (pine.repl:open-session :mode "probe-text"
                                            :minors '("probe-over"))))
             (unwind-protect
                  (is (eq :minor (pine.repl:run (pine.repl:binding s "RET"))))
               (pine.repl:close s))))
      (pine.repl:forget "probe-mode-key")
      (pine.repl:forget "probe-minor-key"))))

(test where-a-file-is-can-say-what-it-is
  (with-modes
    (pine.repl:mode "probe-lisp" :claims '((:files "*.lisp" "*.asd")))
    (pine.repl:mode "probe-config" :claims '((:paths "*/pine/*.lisp")))
    (is (equal "probe-config" (pine.repl:name (pine.repl:mode-for "/home/x/.config/pine/init.lisp")))
        ":paths is matched against the whole namestring and asked first")
    (is (equal "probe-lisp" (pine.repl:name (pine.repl:mode-for "/home/x/src/ns.lisp"))))
    (is (null (pine.repl:mode-for "/home/x/notes.txt")))))

(test a-session-reads-a-setting-through-its-mode
  (with-modes
    (pine.repl:mode "probe-text" :settings '(:tab-width 8))
    (pine.repl:mode "probe-lisp" :parent "probe-text" :settings '(:indent 2))
    (let ((s (pine.repl:open-session :mode "probe-lisp")))
      (unwind-protect
           (progn
             (is (eql 2 (pine.repl:setting s :indent)))
             (is (eql 8 (pine.repl:setting s :tab-width)))
             (is (eql :none (pine.repl:setting s :absent :none))))
        (pine.repl:close s)))))
