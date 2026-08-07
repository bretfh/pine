(defpackage #:pine.ts.syntax
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:hl #:pine.ts.highlight))
  (:export #:language #:for #:grammar-of #:declare-all
           #:compute-highlights #:hl-dump #:hl-dump-file))

(in-package #:pine.ts.syntax)
(named-readtables:in-readtable pine.path:syntax)

;;;; What a language is, as paths.
;;;;
;;;; /syntax/?name holds ordinary written data: the grammar to load, what each
;;;; node type paints, and what a symbol in head position does to the form it
;;;; heads. Ordinary, because the tree is the storage and there is no world
;;;; behind it -- so amending one macro is
;;;;
;;;;   (write /syntax/commonlisp/head/defcmd {:rest :body :indent 2})
;;;;
;;;; which is undoable, watchable, stored and visible in /was, like any write.
;;;; A provider here would have made the whole language one opaque object, and
;;;; amending one head would have meant rewriting it.
;;;;
;;;; One clause is a provider, and only one: /syntax/?lang/head/?name, whose
;;;; read falls back to asking the running image. That is a real world behind a
;;;; path, and it is what makes a user's own macros highlight without anyone
;;;; writing them down.

(defmacro language (options &rest clauses)
  "A language, as a map. OPTIONS carries :grammar and :indent; each clause is a
path and the map at it, so (/node/str_lit {:face :string}) and
(/head/let {...}) read the way a provider's clauses do."
  `(%language ,options
              (list ,@(loop :for (path map) :in clauses
                            :collect `(cons ,path ,map)))))

(defun %language (options clauses)
  (let ((out (if (fset:map? options) options (fset:empty-map))))
    (dolist (clause clauses out)
      (destructuring-bind (path . map) clause
        (setf out (%put out (p:keys path) map))))))

(defun %put (map keys value)
  (cond ((null keys) value)
        (t (let ((inner (fset:lookup map (first keys))))
             (fset:with map (first keys)
                        (%put (if (fset:map? inner) inner (fset:empty-map))
                              (rest keys) value))))))

;;;; Asking the image what a symbol is.
;;;;
;;;; This is what a text editor cannot do and pine can: the daemon holds the
;;;; code, so it knows that DEFCMD is a macro and that its lambda list has &BODY
;;;; third, which is exactly the number of arguments to align before indenting.
;;;; A user's own macros highlight and indent right without a word of config.

(defun %symbol (name package)
  "The symbol NAME names, looked for where the buffer would look."
  (let ((upper (string-upcase name)))
    (or (and package (find-symbol upper package))
        (find-symbol upper :cl)
        (find-symbol upper :cl-user))))

(defun %body-position (sym)
  "How many arguments a macro distinguishes before its body, or NIL when it
takes no body. &BODY in the lambda list is emacs's indent N, said by the macro
itself rather than guessed from its name."
  (let ((args (ignore-errors (sb-introspect:function-lambda-list sym))))
    (loop :for a :in args
          :for i :from 0
          :when (and (symbolp a) (string= "&BODY" (symbol-name a)))
            :do (return i))))

(defun %commonlisp-rule (name package)
  "What the image says about a head symbol, as a rule."
  (let ((sym (%symbol name package)))
    (cond
      ((null sym) (%by-name name))
      ((special-operator-p sym) {:face :keyword :rest :body})
      ((macro-function sym)
       ;; &BODY says how many arguments to align before indenting. A macro
       ;; without one may still take a body -- LOOP does -- so the shape of the
       ;; name is asked after the lambda list, not instead of it.
       (let ((n (%body-position sym)))
         (cond (n (fset:map (:face :keyword) (:rest :body) (:indent n)))
               ((%by-name name) {:face :keyword :rest :body})
               (t {:face :keyword}))))
      ((and (fboundp sym) (eq (symbol-package sym) (find-package :cl)))
       {:face :builtin})
      ((and (boundp sym) (constantp sym)) {:face :function-call :constant t})
      (t (%by-name name)))))

(defun %by-name (name)
  "What the shape of a name says when the image has never heard of it, which is
what editing a file this image has not loaded is. Not a guess pine can do
without: a config's own macros are not defined until it runs."
  (let ((n (length name)))
    (when (or (and (>= n 3) (string= "def" name :end2 3))
              (and (>= n 5) (string= "with-" name :end2 5))
              (and (>= n 3) (string= "do-" name :end2 3))
              (string= name "do")
              (string= name "loop"))
      {:face :keyword :rest :body})))

(defparameter *inferrers*
  (list (cons :commonlisp #'%commonlisp-rule))
  "How each language works out a head it was not told about. A language absent
here is told everything or nothing.")

;;;; Compiling a declaration into what the walk reads

(defun %hash (map)
  (let ((out (make-hash-table :test 'equal)))
    (when (fset:map? map)
      (fset:do-map (key value map)
        (setf (gethash (string-downcase (p:name key)) out) value)))
    out))

(defun %names (set)
  (let ((out (make-hash-table :test 'equal)))
    (when set
      (fset:do-set (name (if (fset:set? set) set (fset:convert 'fset:set set)))
        (setf (gethash (string-downcase (string name)) out) t)))
    out))

(defun %compile (name raw)
  (let* ((indent (fset:lookup raw :indent))
         (infer (cdr (assoc name *inferrers*))))
    (hl:make-language
     :grammar (fset:lookup raw :grammar)
     :indent-width (or (and (fset:map? indent) (fset:lookup indent :width)) 2)
     :nodes (%hash (fset:lookup raw :node))
     :heads (%hash (fset:lookup raw :head))
     :otherwise (fset:lookup raw :otherwise)
     :constants (%names (fset:lookup raw :constants))
     ;; the package comes from the buffer at the moment of asking, not from
     ;; here: the same language reads different packages in different buffers
     :infer infer
     :raw raw)))

(defun %compiled ()
  "Language name to what its declaration last compiled to. The space keeps it:
rules compiled from one space's tree are not another's, and a compiled language
is not a value."
  (or (ns:kept :syntax)
      (setf (ns:kept :syntax) (pine.data:table))))

(defun for (name)
  "NAME's compiled rules, or NIL when nothing declares it.

What was compiled comes back when the declaration is still the same object.
That check is exact rather than a guess: an fset map is immutable and every
write supersedes, so EQ on it answers whether anything about the language moved."
  (let ((raw (ns:read (p:path /syntax name))))
    (when (fset:map? raw)
      (let* ((table (%compiled))
             (had (pine.data:at table name)))
        (if (and had (eq raw (hl:lang-raw had)))
            had
            (pine.data:put table name (%compile name raw)))))))

(defun grammar-of (name)
  "(values LIBRARY FN-NAME) for NAME's grammar, from its own declaration."
  (let* ((lang (for name))
         (g (and lang (hl:lang-grammar lang))))
    (when (fset:map? g)
      (values (fset:lookup g :lib) (fset:lookup g :fn)))))


;;;; The paths

(defun provider ()
  (ns:provider
   (/syntax/?lang/head/?name
    {:out (pine.data:fn [held] held)
     :doc "how a form headed by that symbol is walked and indented"})
   (/syntax/?lang/node/?type
    {:doc "what the walk does when it reaches that node type"})
   (/syntax/?lang/grammar
    {:doc "{:lib :fn}: the shared library to load and the C entry in it"})
   (/syntax/?lang/indent
    {:doc "{:width N}: what a body indents by, when the mode does not say"})
   (/syntax/?lang
    {:doc "a language: its grammar, its node rules and its head rules"})
   (/syntax {:doc "the languages there are"})))

(ns:serve :syntax
  {:at [/syntax]
   :doc "languages, as paths: what to parse with and what to paint"
   :up (lambda ()
         (ns:write /syntax (provider))
         ;; a language that moved is one whose rules have to be built again
         (ns:watch /syntax/*
                   (pine.data:fn [v]
                     (declare (ignore v))
                     (pine.data:clear (%compiled))
                     {})
                   :as :syntax-compiled)
         ;; what the space keeps: the rules each declaration compiled to
         (pine.data:table))
   :down (lambda (rules)
           (declare (ignore rules))
           (ns:watch /syntax/* nil :as :syntax-compiled))})

;; How the runtime finds a grammar. A registry, not state: which function
;; answers is a property of the code that is loaded, and what it answers comes
;; out of whichever space is current when it is asked.
(setf pine.ts.runtime:*grammar-of* #'grammar-of)


;;;; Highlighting something that is not a buffer: a tool, a test, a snippet.

(defun declare-all ()
  "Bring up everything that declares a language. Which languages exist is what
is served under /syntax, so a tool does not carry a list of them and a config
that adds one is picked up here for free."
  (ns:up :syntax)
  (dolist (d (ns:served))
    (let ((name (fset:lookup d :name))
          (at (fset:convert 'list (or (fset:lookup d :at) (fset:empty-seq)))))
      (when (and (not (eq :syntax name))
                 (some (lambda (path) (p:prefixp /syntax path)) at))
        (ns:up name)))))

(defun %state (runtime name)
  (multiple-value-bind (lib fn) (grammar-of name)
    (when lib
      (pine.ts.runtime:make-parse-state runtime name lib fn :syntax (for name)))))

(defun compute-highlights (runtime language text)
  "Highlights for TEXT in one shot, with a parse state of its own."
  (let ((ps (%state runtime language)))
    (when ps
      (unwind-protect
           (progn
             (pine.ts.runtime:parse-lines!
              ps (fset:convert 'fset:seq
                               (uiop:split-string text :separator '(#\Newline))))
             (hl:parse-highlights ps))
        (pine.ts.runtime:free-parse-state ps)))))

(defun hl-dump (source &optional (language :commonlisp))
  "Print each highlighted token of SOURCE and the face it resolves to."
  (ns:with-space ()
   (declare-all)
   (let* ((runtime (pine.ts.runtime:make-ts-runtime))
          (ps (progn (pine.ts.runtime:ensure-ts runtime)
                     (%state runtime language))))
    (if (null ps)
        (format t "~&no grammar loaded for ~a~%" language)
        (let ((lines (coerce (uiop:split-string source :separator '(#\Newline))
                             'vector)))
          (pine.ts.runtime:parse-lines!
           ps (fset:convert 'fset:seq
                            (uiop:split-string source :separator '(#\Newline))))
          (dolist (h (hl:parse-highlights ps))
            (destructuring-bind (line start-col end-col face) h
              (let* ((text (if (< line (length lines)) (aref lines line) ""))
                     (end (min end-col (length text))))
                (format t "~&~2d:~2d  ~16a ~s~%" line start-col face
                        (subseq text start-col (max start-col end)))))))))))

(defun hl-dump-file (path &optional (language :commonlisp))
  (with-open-file (s path :if-does-not-exist nil)
    (if (null s)
        (format t "~&no such file: ~a~%" path)
        (let ((source (make-string (file-length s))))
          (hl-dump (subseq source 0 (read-sequence source s)) language)))))
