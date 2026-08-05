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
    (pine.ns:write /mode/lisp/on/newline (pine.data:fn [buf] (declare (ignore buf)) :lisp-newline))
    (pine.ns:write /buf/scratch {:mode :lisp})
    (is (eq :lisp-newline (funcall (pine.mode:handler "scratch" :newline) "scratch")))
    (is (null (pine.mode:handler "scratch" :nobody-claims-this))
        "no handler means the built-in verb answers")))

(test a-verb-falls-up-the-parent-chain
  (with-modes
    (pine.ns:write /mode/prog/on/newline (pine.data:fn [buf] (declare (ignore buf)) :prog-newline))
    (pine.ns:write /buf/scratch {:mode :lisp})
    (is (eq :prog-newline (funcall (pine.mode:handler "scratch" :newline) "scratch")))))

(test a-minor-mode-is-asked-before-the-major-one
  (with-modes
    (pine.ns:write /minor/paren {:precedence 10})
    (pine.ns:write /minor/paren/on/insert (pine.data:fn [buf] (declare (ignore buf)) :paren))
    (pine.ns:write /mode/lisp/on/insert (pine.data:fn [buf] (declare (ignore buf)) :lisp))
    (pine.ns:write /buf/scratch {:mode :lisp :minor #{:paren}})
    (is (eq :paren (funcall (pine.mode:handler "scratch" :insert) "scratch")))))

(test minor-modes-are-asked-in-precedence-order
  (with-modes
    (pine.ns:write /minor/low {:precedence 1})
    (pine.ns:write /minor/high {:precedence 20})
    (pine.ns:write /minor/low/on/insert (pine.data:fn [buf] (declare (ignore buf)) :low))
    (pine.ns:write /minor/high/on/insert (pine.data:fn [buf] (declare (ignore buf)) :high))
    (pine.ns:write /buf/scratch {:mode :lisp :minor #{:low :high}})
    (is (equal '(:high :low) (pine.mode:minors "scratch")))
    (is (eq :high (funcall (pine.mode:handler "scratch" :insert) "scratch")))))

;;;; layering: a mode lays itself over another rather than only replacing it

(test the-claimants-are-every-mode-that-answers-in-order
  (with-modes
    (pine.ns:write /minor/paren {:precedence 10})
    (pine.ns:write /minor/paren/on/insert
                   (pine.data:fn [buf] (declare (ignore buf)) :paren))
    (pine.ns:write /mode/lisp/on/insert
                   (pine.data:fn [buf] (declare (ignore buf)) :lisp))
    (pine.ns:write /mode/prog/on/insert
                   (pine.data:fn [buf] (declare (ignore buf)) :prog))
    (pine.ns:write /buf/scratch {:mode :lisp :minor #{:paren}})
    (is (equal '(:paren :lisp :prog)
               (mapcar (lambda (fn) (funcall fn "scratch"))
                       (pine.mode:claimants "scratch" :insert)))
        "the chain the verb goes down is not the minor mode, then the mode, then
what the mode falls back to")))

(test a-handler-that-writes-its-verb-reaches-the-mode-under-it
  "Writing the verb it claimed is how a handler says `and then the one below
me'. Only when they run out does the built-in answer, so a mode can adjust what
another does instead of having to do the whole job itself."
  (with-modes
    (pine.ts.syntax:declare-all)
      (pine.ns:raise :buf)
    (unwind-protect
         (let ((seen nil))
           (pine.ns:write /minor/shout {:precedence 10})
           (pine.ns:write /minor/shout/on/insert
                          (pine.data:fn [buf text]
                            (push :shout seen)
                            (fset:map ((pine.buf:at buf :text)
                                       (fset:seq :insert (string-upcase text))))))
           (pine.ns:write /mode/lisp/on/insert
                          (pine.data:fn [buf text]
                            (push :lisp seen)
                            (fset:map ((pine.buf:at buf :text)
                                       (fset:seq :insert
                                                 (concatenate 'string text "!"))))))
           (pine.ns:write /buf/layered/text "")
           (pine.ns:write /buf/layered {:mode :lisp :minor #{:shout}})
           (pine.ns:write (pine.buf:at "layered" :text) (fset:seq :insert "hi"))
           (is (equal '(:lisp :shout) seen)
               "the minor mode did not reach the major one under it")
           (is (equal "HI!" (pine.ns:read (pine.buf:at "layered" :text)))
               "each mode did not see what the one above it made of the write"))
      (pine.ns:lower :buf))))

(test overwrite-covers-what-it-types-and-lets-the-insert-through
  "The proof of layering, in the mode pine ships for it: overwrite takes the
characters the insert is about to cover and writes the verb again. Inserting is
still the job of whatever claims it underneath."
  (pine.ns:with-space ()
    (pine.ns:raise :mode)
    (pine.ts.syntax:declare-all)
      (pine.ns:raise :buf)
    (unwind-protect
         (progn
           (pine.ns:write /buf/over/text "abcd")
           (pine.ns:write /buf/over/point [0 1])
           (pine.ns:write /buf/over/minor #{:overwrite})
           (pine.ns:write (pine.buf:at "over" :text) (fset:seq :insert "XY"))
           (is (equal "aXYd" (pine.ns:read (pine.buf:at "over" :text)))
               "overwrite did not cover the characters it typed over"))
      (pine.ns:lower :buf)
      (pine.ns:lower :mode))))

;;;; the dispatch reads back

(test what-a-mode-overrides-is-one-read
  (with-modes
    (pine.ns:write /mode/lisp/on/newline (pine.data:fn [buf] (declare (ignore buf)) :a))
    (pine.ns:write /mode/lisp/on/indent-line (pine.data:fn [buf] (declare (ignore buf)) :b))
    (is (= 2 (fset:size (pine.ns:read /mode/lisp/on/*)))
        "exactly what lisp-mode overrides, and nothing walks a class tree")))

(test every-mode-that-touches-a-verb-is-one-read
  (with-modes
    (pine.ns:write /mode/lisp/on/newline (pine.data:fn [buf] (declare (ignore buf)) :a))
    (pine.ns:write /mode/prog/on/newline (pine.data:fn [buf] (declare (ignore buf)) :b))
    (is (= 2 (fset:size (pine.ns:read /mode/**/on/newline))))))

;;;; What a mode answers about a buffer, as against what it does to one.

(test a-mode-answers-a-verb-nothing-in-src-knows-about
  "A producer is a function at a path, so a mode written here answers for a
buffer without one line of pine having heard of it."
  (with-modes
    (pine.ns:write /mode/probe {:parent :text})
    (pine.ns:write /mode/probe/answers/definition
                   (pine.data:fn [buf of]
                     (list (list "/tmp/probe.lisp" 7 3 (or of buf)))))
    (pine.ns:write /buf/probe-buf/text ["(f)"])
    (pine.ns:write /buf/probe-buf/mode :probe)
    (is (equal '(("/tmp/probe.lisp" 7 3 "probe-buf"))
               (pine.mode:answer "probe-buf" :definition nil)))
    (is (equal '(("/tmp/probe.lisp" 7 3 "thing"))
               (pine.mode:answer "probe-buf" :definition "thing")))))

(test a-verb-is-read-off-the-buffer
  "A surface asks the buffer, not the mode: the path is the whole interface."
  (with-modes
    (pine.ts.syntax:declare-all)
      (pine.ns:raise :buf)
    (unwind-protect
         (progn
           (pine.ns:write /mode/probe {:parent :text})
           (pine.ns:write /mode/probe/answers/arglist
                          (pine.data:fn [buf of] (declare (ignore of))
                            (format nil "~a takes nothing" buf)))
           (pine.ns:write /buf/probe-buf/text ["(f)"])
           (pine.ns:write /buf/probe-buf/mode :probe)
           (is (equal "probe-buf takes nothing"
                      (pine.ns:read /buf/probe-buf/arglist))))
      (pine.ns:lower :buf))))

(test a-point-verb-takes-the-most-specific-answer-and-a-set-verb-merges
  (with-modes
    (pine.ns:write /mode/probe {:parent :text})
    (pine.ns:write /mode/text/answers/definition
                   (pine.data:fn [buf of] (declare (ignore buf of)) (list :from-text)))
    (pine.ns:write /mode/probe/answers/definition
                   (pine.data:fn [buf of] (declare (ignore buf of)) (list :from-probe)))
    (pine.ns:write /mode/text/answers/references
                   (pine.data:fn [buf of] (declare (ignore buf of)) (list :text-ref)))
    (pine.ns:write /mode/probe/answers/references
                   (pine.data:fn [buf of] (declare (ignore buf of)) (list :probe-ref)))
    (pine.ns:write /buf/probe-buf/text ["(f)"])
    (pine.ns:write /buf/probe-buf/mode :probe)
    (is (equal '(:from-probe) (pine.mode:answer "probe-buf" :definition nil)))
    (is (equal '(:probe-ref :text-ref)
               (pine.mode:answer "probe-buf" :references nil)))))
