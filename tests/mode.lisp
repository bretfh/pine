(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.mode :in :pine)

(defmacro with-modes (&body body)
  "A space holding the modes the doc writes."
  `(pine.ns:with-space ()
     (pine.ns:write /mode/prog
                    {:parent :text
                     :indent {:width 2}
                     :comment {:line ";"}})
     (pine.ns:write /mode/lisp
                    {:parent :prog
                     :grammar :commonlisp
                     :indicator "Lisp"
                     :files ["*.lisp" "*.asd" "*.cl"]
                     :comment {:line ";;"}})
     ,@body))

;;;; writing the mode is registering it

(test a-mode-is-a-map-and-writing-it-is-the-whole-of-declaring-it
  (with-modes
    (is (eq :commonlisp (pine.ns:read /mode/lisp/grammar)))
    (is (string= "Lisp" (pine.ns:read /mode/lisp/indicator)))
    (is (equal '(:lisp :prog :text) (pine.mode:chain :lisp)))))

(test a-chain-that-loops-ends-rather-than-hanging
  (pine.ns:with-space ()
    (pine.ns:write /mode/a {:parent :b})
    (pine.ns:write /mode/b {:parent :a})
    (is (equal '(:a :b) (pine.mode:chain :a)))))

;;;; :parent is what makes inheriting mean anything

(test a-setting-comes-from-the-first-mode-up-the-chain-that-says-it
  (with-modes
    (is (fset:equal? {:width 2} (pine.mode:setting :lisp :indent))
        "lisp says no width, so prog's stands")
    (is (fset:equal? {:line ";;"} (pine.mode:setting :lisp :comment))
        "lisp says its own")
    (is (null (pine.mode:setting :lisp :nothing-says-this)))
    (is (eq :fallback (pine.mode:setting :lisp :nothing-says-this :fallback)))))

;;;; which mode a file gets

(test a-mode-claims-its-own-file-types
  (with-modes
    (is (eq :lisp (pine.mode:for-file "/home/bfh/init.lisp")))
    (is (eq :lisp (pine.mode:for-file "pine.asd")))
    (is (null (pine.mode:for-file "notes.txt"))
        "nothing claims it, and that is not an error")))

(test a-glob-covers-a-run-of-anything-including-none
  (is (pine.mode:matches-p "*.lisp" "a.lisp"))
  (is (pine.mode:matches-p "*.lisp" ".lisp"))
  (is (pine.mode:matches-p "Makefile" "Makefile"))
  (is (pine.mode:matches-p "*test*" "my-test-file"))
  (is (not (pine.mode:matches-p "*.lisp" "a.lisp.bak"))))

;;;; which handler answers a verb

(test the-major-mode-answers-a-verb-it-claims
  (with-modes
    (pine.ns:write /mode/lisp/on/newline (pine.data:fn [buf] :lisp-newline))
    (pine.ns:write /buf/scratch {:mode :lisp})
    (is (eq :lisp-newline (funcall (pine.mode:handler "scratch" :newline) "scratch")))
    (is (null (pine.mode:handler "scratch" :nobody-claims-this))
        "no handler means the built-in verb answers")))

(test a-verb-falls-up-the-parent-chain
  (with-modes
    (pine.ns:write /mode/prog/on/newline (pine.data:fn [buf] :prog-newline))
    (pine.ns:write /buf/scratch {:mode :lisp})
    (is (eq :prog-newline (funcall (pine.mode:handler "scratch" :newline) "scratch")))))

(test a-minor-mode-is-asked-before-the-major-one
  (with-modes
    (pine.ns:write /minor/paren {:precedence 10})
    (pine.ns:write /minor/paren/on/insert (pine.data:fn [buf] :paren))
    (pine.ns:write /mode/lisp/on/insert (pine.data:fn [buf] :lisp))
    (pine.ns:write /buf/scratch {:mode :lisp :minor #{:paren}})
    (is (eq :paren (funcall (pine.mode:handler "scratch" :insert) "scratch")))))

(test minor-modes-are-asked-in-precedence-order
  (with-modes
    (pine.ns:write /minor/low {:precedence 1})
    (pine.ns:write /minor/high {:precedence 20})
    (pine.ns:write /minor/low/on/insert (pine.data:fn [buf] :low))
    (pine.ns:write /minor/high/on/insert (pine.data:fn [buf] :high))
    (pine.ns:write /buf/scratch {:mode :lisp :minor #{:low :high}})
    (is (equal '(:high :low) (pine.mode:minors "scratch")))
    (is (eq :high (funcall (pine.mode:handler "scratch" :insert) "scratch")))))

;;;; the dispatch reads back

(test what-a-mode-overrides-is-one-read
  (with-modes
    (pine.ns:write /mode/lisp/on/newline (pine.data:fn [buf] :a))
    (pine.ns:write /mode/lisp/on/indent-line (pine.data:fn [buf] :b))
    (is (= 2 (fset:size (pine.ns:read /mode/lisp/on/*)))
        "exactly what lisp-mode overrides, and nothing walks a class tree")))

(test every-mode-that-touches-a-verb-is-one-read
  (with-modes
    (pine.ns:write /mode/lisp/on/newline (pine.data:fn [buf] :a))
    (pine.ns:write /mode/prog/on/newline (pine.data:fn [buf] :b))
    (is (= 2 (fset:size (pine.ns:read /mode/**/on/newline))))))
