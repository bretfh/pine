(in-package :pine/test)

(def-suite* :pine/ts :in :pine)

(defvar *runtime* nil)

(defun runtime ()
  (or *runtime*
      (let ((r (pine/ts/runtime:make-ts-runtime)))
        (pine/ts/runtime:ensure-ts r)
        (setf *runtime* r))))

(defun faces-of (source &optional (language :commonlisp))
  (pine/ts/syntax:compute-highlights (runtime) language source))

(test the-languages-pine-ships-are-declared
  (is (member :commonlisp (pine/ts/syntax:languages)))
  (is (member :scheme (pine/ts/syntax:languages)))
  (is (member :pine (pine/ts/syntax:languages))))

(test a-language-says-what-grammar-to-load
  (multiple-value-bind (lib fn) (pine/ts/syntax:grammar-of :commonlisp)
    (is (equal "libtree-sitter-commonlisp" lib))
    (is (equal "tree_sitter_commonlisp" fn)))
  (multiple-value-bind (lib fn) (pine/ts/syntax:grammar-of :pine)
    (is (equal "libtree-sitter-pine" lib))
    (is (equal "tree_sitter_pine" fn))))

(test a-dialect-inherits-the-language-it-is-a-dialect-of
  (let ((cl (pine/ts/syntax:for :commonlisp))
        (pine (pine/ts/syntax:for :pine)))
    (is-true (gethash "let" (pine/ts/highlight:lang-heads cl)))
    (is-true (gethash "let" (pine/ts/highlight:lang-heads pine))
             "pine inherits Common Lisp's head rules")
    (is-true (gethash "map_lit" (pine/ts/highlight:lang-nodes pine))
             "and adds its own node rules")
    (is (null (gethash "map_lit" (pine/ts/highlight:lang-nodes cl)))
        "without putting them back into what it inherited from")))

(test a-rule-is-a-plist-and-reads-with-the-same-accessor
  (let ((rule (gethash "str_lit" (pine/ts/highlight:lang-nodes
                                  (pine/ts/syntax:for :commonlisp)))))
    (is (eq :string (pine/data:at rule :face)))))

(test source-is-parsed-and-painted
  (let ((found (faces-of "(defun f (x) \"doc\" x)")))
    (is (consp found) "no highlights came back; is libtree-sitter-commonlisp loadable?")
    (is (find :keyword found :key #'fourth) "defun paints as a keyword")
    (is (find :string found :key #'fourth) "the docstring paints as a string")))

(test the-image-answers-for-a-head-nobody-wrote-down
  (let ((rule (pine/ts/highlight:head-rule (pine/ts/syntax:for :commonlisp)
                                           "with-open-file")))
    (is (eq :keyword (pine/data:at rule :face))
        "a macro with a &body is a keyword whose rest is a body")
    (is (eq :body (pine/data:at rule :rest)))))

(defmacro probe-two-then-body (a b &body forms)
  (declare (ignore a b))
  `(progn ,@forms))

(test a-macro-indents-by-where-its-own-body-begins
  (let ((rule (pine/ts/highlight:head-rule (pine/ts/syntax:for :commonlisp)
                                           "probe-two-then-body"
                                           :pine/test)))
    (is (eql 2 (pine/data:at rule :indent))
        "the macro's own lambda list says where its body begins")
    (is (eq :body (pine/data:at rule :rest)))))

(test what-the-image-never-heard-of-is-read-by-its-shape
  (let ((rule (pine/ts/highlight:head-rule (pine/ts/syntax:for :commonlisp)
                                           "defnothing-at-all")))
    (is (eq :keyword (pine/data:at rule :face))
        "a config's own macros are not defined until it runs")))

(test pine-source-parses-under-pines-own-grammar
  (let ((found (faces-of "(write /buf/scratch/text {:a 1})" :pine)))
    (is (consp found))
    (is (find :namespace found :key #'fourth) "a path segment is painted")))

(test the-byte-index-answers-both-ways-over-lines
  (let ((index (pine/ts/index:build-index (pine/data:seq "hello" "there"))))
    (is (eql 0 (pine/ts/index:line-start index 0)))
    (is (eql 6 (pine/ts/index:line-start index 1)))
    (multiple-value-bind (line col) (pine/ts/index:source-line-col index 8)
      (is (eql 1 line))
      (is (eql 2 col)))))

(test a-rule-map-is-a-map-and-a-constant-set-is-a-set
  (let* ((lang (pine/ts/syntax:for :commonlisp))
         (rule (gethash "str_lit" (pine/ts/highlight:lang-nodes lang))))
    (is-true (pine/data:mapp rule) "a node rule is a map, looked up by key")
    (is (eq :string (pine/data:at rule :face)))))

(test a-buffer-says-what-it-is-written-in
  (unwind-protect
       (progn
         (pine:start)
         (let ((b (pine/edit/buffer:make-buffer "probe-syntax")))
           (setf (pine/fs/node:contents b) "(in-package #:pine/test)
(defun f () 1)")
           (is (eq (find-package :pine/test) (pine/edit/language:package-of b))
               "the package its own (in-package) names")
           (is (null (pine/edit/language:readtable-of b))
               "and nothing else, where it says nothing else")
           (setf (pine/fs/node:contents b)
                 "(in-package #:pine/test)
(named-readtables:in-readtable pine/path/reader:syntax)
(defun f () /probe/one)")
           (is (eq (named-readtables:find-readtable 'pine/path/reader:syntax)
                   (pine/edit/language:readtable-of b))
               "the readtable its own (in-readtable) names")
           (is (eq :pine (pine/ts/parser::%grammar b))
               "so it is parsed as pine, whatever path it is under")
           (multiple-value-bind (package readtable) (pine/edit/language:reading b)
             (is (eq (find-package :pine/test) package))
             (is-true (pine/path/path:pathp
                       (eval (let ((*package* package) (*readtable* readtable))
                               (read-from-string "/probe/one"))))
                      "and a form in it reads in that readtable"))))
    (pine:stop)))

(test a-macro-of-the-buffers-own-package-indents-like-the-macro-it-is
  (unwind-protect
       (progn
         (pine:start)
         (let ((b (pine/edit/buffer:make-buffer "probe-indent")))
           (setf (pine/edit/buffer:mode-of b) "lisp")
           (setf (pine/fs/node:contents b) "(in-package #:pine/test)
(probe-two-then-body 1 2
x)")
           (is (eql 2 (pine/ts/parser:indent b 2 :width 2))
               "the parse reads the head in the buffer's own package")
           (setf (pine/fs/node:contents b) "(in-package #:cl-user)
(probe-two-then-body 1 2
x)")
           (pine/ts/parser:wait b)
           (is (> (pine/ts/parser:indent b 2 :width 2) 2)
               "and where it is not a macro it lines up like a call")))
    (pine:stop)))
