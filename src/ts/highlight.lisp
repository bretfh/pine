(defpackage #:pine.ts.highlight
  (:use #:cl #:pine.ts)
  (:export #:%align-column #:%byte->char #:%byte-col #:%byte-line #:%defish-p #:%enclosing-form #:%form-head-name #:%intern-table #:%node-first-char #:%opens-form-p #:*cl-builtins* #:*cl-class-definers* #:*cl-constants* #:*cl-flat-all-binders* #:*cl-flat-first-binders* #:*cl-nested-binders* #:*cl-special-forms* #:*cl-struct-definers* #:*cl-type-definers* #:*cl-var-definers* #:*scheme-keywords* #:body-form-p #:cl-head-face #:cl-head-kind #:cl-highlights #:delimiter-face #:hl-dump #:hl-dump-file #:lambda-list-keyword-p #:parse-indent #:scheme-highlights #:ts-field #:ts-named-children #:ts-type #:ts-type= #:walk-highlights))

(in-package #:pine.ts.highlight)

(declaim (optimize (speed 3) (safety 1)))

;;;; Single-pass highlighter. One walk of the parse tree assigns each token a
;;;; face from its node type and context (form head, quote state, form depth),
;;;; replacing the query + capture-name-to-face + reclassify stack. Positions
;;;; are converted from tree-sitter's UTF-8 bytes to character columns through
;;;; the line index as spans are emitted, so text is never sliced by a byte
;;;; offset. See design/treesitter.org.

;;;; Common Lisp classification tables

(defvar *cl-special-forms* (make-hash-table :test 'equal))
(defvar *cl-builtins* (make-hash-table :test 'equal))
(defvar *cl-constants* (make-hash-table :test 'equal))
(defvar *cl-nested-binders* (make-hash-table :test 'equal))
(defvar *cl-flat-all-binders* (make-hash-table :test 'equal))
(defvar *cl-flat-first-binders* (make-hash-table :test 'equal))
(defvar *cl-type-definers* (make-hash-table :test 'equal))
(defvar *cl-var-definers* (make-hash-table :test 'equal))

(defun %intern-table (table names)
  (dolist (name names table) (setf (gethash name table) t)))

;; Heads rendered as keywords: special operators plus the common macros.
(%intern-table *cl-special-forms*
  '("block" "catch" "eval-when" "flet" "function" "go" "if" "labels" "let"
    "let*" "load-time-value" "locally" "macrolet" "multiple-value-call"
    "multiple-value-prog1" "progn" "progv" "quote" "return-from" "setq"
    "symbol-macrolet" "tagbody" "the" "throw" "unwind-protect" "declaim"
    "declare" "proclaim" "cond" "when" "unless" "case" "ecase" "ccase"
    "typecase" "etypecase" "ctypecase" "and" "or" "not" "with-standard-io-syntax"
    "with-compilation-unit" "with-condition-restarts" "with-hash-table-iterator"
    "with-package-iterator" "with-simple-restart" "handler-case" "handler-bind"
    "restart-case" "restart-bind" "ignore-errors" "prog" "prog*" "prog1" "prog2"
    "setf" "psetf" "psetq" "rotatef" "shiftf" "push" "pop" "pushnew" "remf"
    "incf" "decf" "assert" "check-type" "trace" "untrace" "step" "time" "inspect"
    "export" "import" "shadow" "shadowing-import" "provide" "require" "in-package"
    "use-package" "define-setf-expander" "define-symbol-macro"
    "define-compiler-macro" "define-modify-macro" "define-method-combination"
    "loop" "return"))

(%intern-table *cl-builtins*
  '("car" "cdr" "caar" "cadr" "cddr" "cdar" "cons" "list" "list*" "append"
    "nconc" "reverse" "nreverse" "last" "butlast" "length" "nth" "nthcdr" "elt"
    "subseq" "copy-list" "first" "second" "third" "fourth" "fifth" "rest"
    "mapcar" "mapc" "mapcan" "maplist" "mapcon" "reduce" "remove" "remove-if"
    "remove-if-not" "delete" "find" "find-if" "position" "position-if" "member"
    "member-if" "assoc" "rassoc" "getf" "gethash" "funcall" "apply" "identity"
    "complement" "constantly" "eq" "eql" "equal" "equalp" "=" "/=" "<" ">" "<="
    ">=" "+" "-" "*" "/" "1+" "1-" "min" "max" "abs" "mod" "rem" "floor"
    "ceiling" "truncate" "round" "gcd" "lcm" "expt" "sqrt" "exp" "log" "zerop"
    "plusp" "minusp" "evenp" "oddp" "null" "atom" "listp" "consp" "numberp"
    "integerp" "floatp" "realp" "characterp" "stringp" "symbolp" "keywordp"
    "functionp" "arrayp" "vectorp" "hash-table-p" "make-array" "aref" "svref"
    "make-hash-table" "make-instance" "make-string" "make-list" "string"
    "string=" "string-equal" "string<" "string>" "concatenate" "format" "print"
    "princ" "prin1" "write" "write-string" "write-line" "terpri" "read"
    "read-line" "char" "code-char" "char-code" "char=" "values" "values-list"
    "multiple-value-list" "type-of" "typep" "coerce" "error" "warn" "signal"
    "gensym" "intern" "symbol-name" "symbol-value" "symbol-function" "boundp"
    "fboundp"))

(%intern-table *cl-constants*
  '("t" "nil" "pi" "most-positive-fixnum" "most-negative-fixnum"
    "most-positive-double-float" "most-negative-double-float"
    "most-positive-single-float" "most-negative-single-float"
    "most-positive-short-float" "most-negative-short-float"
    "most-positive-long-float" "most-negative-long-float"))

;; Binders the grammar parses as bare lists. NESTED hold ((var init) ...);
;; FLAT-ALL bind every symbol in their list; FLAT-FIRST bind only the first.
(%intern-table *cl-nested-binders*
  '("let" "let*" "do" "do*" "symbol-macrolet" "prog" "prog*" "compiler-let"))
(%intern-table *cl-flat-all-binders*
  '("lambda" "destructuring-bind" "multiple-value-bind" "with-slots"
    "with-accessors"))
(%intern-table *cl-flat-first-binders*
  '("dolist" "dotimes" "do-symbols" "do-external-symbols" "do-all-symbols"
    "with-open-file" "with-open-stream" "with-input-from-string"
    "with-output-to-string"))

;; Generic def-forms (no defun_header node): head keyword, then a named entity.
(defvar *cl-class-definers* (make-hash-table :test 'equal))
(defvar *cl-struct-definers* (make-hash-table :test 'equal))
(%intern-table *cl-class-definers* '("defclass" "define-condition"))
(%intern-table *cl-struct-definers* '("defstruct"))
(%intern-table *cl-type-definers* '("deftype"))
(%intern-table *cl-var-definers* '("defvar" "defparameter" "defconstant"))

(defun cl-head-kind (name)
  "The role of a form whose head symbol is NAME."
  (cond ((null name) nil)
        ((gethash name *cl-nested-binders*) :binder-nested)
        ((gethash name *cl-flat-all-binders*) :binder-flat-all)
        ((gethash name *cl-flat-first-binders*) :binder-flat-first)
        ((gethash name *cl-class-definers*) :def-class)
        ((gethash name *cl-struct-definers*) :def-struct)
        ((gethash name *cl-type-definers*) :def-type)
        ((gethash name *cl-var-definers*) :def-var)
        ((string= name "defpackage") :def-package)
        ((gethash name *cl-special-forms*) :special)
        ((gethash name *cl-builtins*) :builtin)
        (t :call)))

(defun cl-head-face (kind)
  (case kind
    (:builtin :builtin)
    (:call :function-call)
    (t :keyword)))

(defun lambda-list-keyword-p (text)
  (and (plusp (length text)) (char= (char text 0) #\&)))

(defun delimiter-face (depth)
  (intern (format nil "DELIMITER.~d" (mod depth 6)) :keyword))


;;;; Node helpers

(defun ts-type (node) (ts-node-type node))

(defun ts-field (node name)
  (let ((child (ts-node-child-by-field-name node name (length name))))
    (unless (ts-node-is-null child) child)))

(defun ts-named-children (node)
  (loop :for i :from 0 :below (ts-node-named-child-count node)
        :collect (ts-node-named-child node i)))

(defun ts-type= (node &rest types)
  (member (ts-node-type node) types :test #'string=))


;;;; Common Lisp walk

(defun cl-highlights (root text index &key lo-byte hi-byte)
  "Highlights (line start-col end-col face) for the CL parse tree ROOT over TEXT.
With LO-BYTE / HI-BYTE, subtrees wholly outside that byte window are skipped;
the descent from ROOT still runs, so context (quote state, depth, head kind)
stays exact for everything inside the window."
  (let ((acc nil))
    (labels
        ((char-at (byte)
           (multiple-value-bind (line col) (byte-to-line-col byte index text)
             (+ (cdr (aref index line)) col)))
         (down (node)
           (string-downcase (subseq text (char-at (ts-node-start-byte node))
                                    (char-at (ts-node-end-byte node)))))
         (emit (start-byte end-byte face)
           ;; zero-width spans paint nothing and would straddle the incremental
           ;; window boundary (a comment's extent ends at col 0 of the next
           ;; line), so they are never emitted
           (when face
             (multiple-value-bind (sl sc) (byte-to-line-col start-byte index text)
               (multiple-value-bind (el ec) (byte-to-line-col end-byte index text)
                 (if (= sl el)
                     (when (> ec sc)
                       (push (list sl sc ec face) acc))
                     (progn
                       (push (list sl sc 999 face) acc)
                       (loop :for l :from (1+ sl) :below el
                             :do (push (list l 0 999 face) acc))
                       (when (plusp ec)
                         (push (list el 0 ec face) acc))))))))
         (emit-node (node face)
           (emit (ts-node-start-byte node) (ts-node-end-byte node) face))
         (delimiters (node depth)
           (let ((s (ts-node-start-byte node)) (e (ts-node-end-byte node))
                 (face (delimiter-face depth)))
             (emit s (1+ s) face)
             (emit (1- e) e face)))
         (operand-face (node quoted)
           (cond (quoted :constant)
                 ((gethash (down node) *cl-constants*) :constant)
                 (t :variable)))
         (walk (node depth quoted)
           (when (and hi-byte
                      (or (>= (ts-node-start-byte node) hi-byte)
                          (<= (ts-node-end-byte node) lo-byte)))
             (return-from walk))
           (let ((type (ts-node-type node)))
             (cond
               ((member type '("comment" "block_comment" "dis_expr") :test #'string=)
                (emit-node node :comment))
               ((string= type "str_lit") (walk-string node))
               ((string= type "path_lit") (emit-node node :string))
               ((string= type "char_lit") (emit-node node :character))
               ((member type '("num_lit" "complex_num_lit") :test #'string=)
                (emit-node node :number))
               ((member type '("kwd_lit" "nil_lit") :test #'string=)
                (emit-node node :constant))
               ((string= type "sym_lit") (emit-node node (operand-face node quoted)))
               ((string= type "package_lit") (walk-package node quoted nil))
               ((string= type "list_lit") (walk-list node depth quoted))
               ((string= type "defun") (walk-defun node depth))
               ((string= type "loop_macro") (walk-loop node depth))
               ((member type '("quoting_lit" "syn_quoting_lit") :test #'string=)
                (walk-quote node depth t))
               ((member type '("unquoting_lit" "unquote_splicing_lit") :test #'string=)
                (walk-quote node depth nil))
               ((string= type "var_quoting_lit") (walk-function-quote node depth))
               ((string= type "for_clause") (walk-for-clause node depth))
               ((member type '("loop_keyword" "for_clause_word" "accumulation_verb")
                        :test #'string=)
                (emit-node node :keyword))
               (t (walk-children node depth quoted)))))
         (walk-children (node depth quoted)
           (dolist (child (ts-named-children node)) (walk child depth quoted)))
         (walk-string (node)
           (emit-node node :string)
           ;; format_specifier children paint over the string body
           (dolist (child (ts-named-children node)) (emit-node child :escape)))
         (walk-package (node quoted role-face)
           (let ((package (ts-field node "package"))
                 (symbol (ts-field node "symbol")))
             (when package (emit-node package :namespace))
             (when symbol
               (emit-node symbol (or role-face (operand-face symbol quoted))))))
         (walk-list (node depth quoted)
           (delimiters node depth)
           (let ((elements (ts-named-children node)))
             (cond ((null elements))
                   (quoted (dolist (e elements) (walk e (1+ depth) t)))
                   (t (walk-form node depth elements)))))
         (walk-form (node depth elements)
           (declare (ignore node))
           (let* ((head (first elements))
                  (name (head-name head))
                  (kind (cl-head-kind name)))
             (emit-head head kind depth)
             (loop :for element :in (rest elements)
                   :for i :from 1
                   :do (walk-element element i kind depth))))
         (head-name (head)
           (cond ((string= (ts-node-type head) "sym_lit") (down head))
                 ((string= (ts-node-type head) "package_lit")
                  (let ((symbol (ts-field head "symbol")))
                    (and symbol (down symbol))))
                 (t nil)))
         (emit-head (head kind depth)
           (cond ((string= (ts-node-type head) "sym_lit")
                  (emit-node head (cl-head-face kind)))
                 ((string= (ts-node-type head) "package_lit")
                  (walk-package head nil (cl-head-face kind)))
                 (t (walk head (1+ depth) nil))))
         (walk-element (element i kind depth)
           (cond
             ((and (= i 1) (member kind '(:def-type :def-class :def-struct)))
              (emit-name element :type))
             ((and (= i 1) (eq kind :def-var)) (emit-name element :variable))
             ((and (= i 1) (eq kind :def-package)) (emit-name element :namespace))
             ((and (= i 2) (eq kind :def-class)) (walk-type-list element depth))
             ((and (= i 3) (eq kind :def-class)) (walk-slot-list element depth))
             ((and (>= i 2) (eq kind :def-struct)) (walk-slot-spec element depth))
             ((and (= i 1) (eq kind :binder-nested))
              (walk-nested-bindings element depth))
             ((and (= i 1) (eq kind :binder-flat-all))
              (walk-flat-bindings element depth t))
             ((and (= i 1) (eq kind :binder-flat-first))
              (walk-flat-bindings element depth nil))
             (t (walk element (1+ depth) nil))))
         (walk-type-list (node depth)
           (if (string= (ts-node-type node) "list_lit")
               (progn
                 (delimiters node depth)
                 (dolist (e (ts-named-children node))
                   (if (string= (ts-node-type e) "sym_lit")
                       (emit-node e :type)
                       (walk e (1+ depth) nil))))
               (walk node depth nil)))
         (walk-slot-list (node depth)
           (if (string= (ts-node-type node) "list_lit")
               (progn
                 (delimiters node depth)
                 (dolist (e (ts-named-children node)) (walk-slot-spec e (1+ depth))))
               (walk node depth nil)))
         (walk-slot-spec (node depth)
           (cond
             ((string= (ts-node-type node) "sym_lit") (emit-node node :variable))
             ((string= (ts-node-type node) "list_lit")
              (delimiters node depth)
              (let ((elements (ts-named-children node)))
                (when elements
                  (bind-name (first elements))
                  (loop :for e :in (rest elements) :do (walk e (1+ depth) nil)))))
             (t (walk node depth nil))))
         (emit-name (node face)
           (cond ((string= (ts-node-type node) "sym_lit") (emit-node node face))
                 ((string= (ts-node-type node) "kwd_lit") (emit-node node :constant))
                 ((string= (ts-node-type node) "package_lit")
                  (walk-package node nil face))
                 (t (walk node 0 nil))))
         (bind-name (node)
           (if (string= (ts-node-type node) "sym_lit")
               (emit-node node :variable)
               (walk node 0 nil)))
         (walk-nested-bindings (node depth)
           (delimiters node depth)
           (dolist (binding (ts-named-children node))
             (if (string= (ts-node-type binding) "list_lit")
                 (walk-binding-pair binding (1+ depth))
                 (bind-name binding))))
         (walk-binding-pair (node depth)
           (delimiters node depth)
           (let ((elements (ts-named-children node)))
             (when elements
               (bind-name (first elements))
               (loop :for e :in (rest elements) :do (walk e (1+ depth) nil)))))
         (walk-flat-bindings (node depth all)
           (delimiters node depth)
           (let ((elements (ts-named-children node)))
             (cond ((null elements))
                   (all (dolist (e elements) (bind-or-keyword e depth)))
                   (t (bind-name (first elements))
                      (loop :for e :in (rest elements) :do (walk e (1+ depth) nil))))))
         (bind-or-keyword (node depth)
           (let ((type (ts-node-type node)))
             (cond
               ((and (string= type "sym_lit") (lambda-list-keyword-p (down node)))
                (emit-node node :keyword))
               ((string= type "sym_lit") (emit-node node :variable))
               ((string= type "list_lit") (walk-flat-bindings node depth t))
               (t (walk node depth nil)))))
         (walk-defun (node depth)
           (let ((children (ts-named-children node)))
             (when children (walk-defun-header (first children) depth))
             (loop :for c :in (rest children) :do (walk c (1+ depth) nil))))
         (walk-defun-header (header depth)
           (let ((keyword (ts-field header "keyword"))
                 (name (ts-field header "function_name"))
                 (lambda-list (ts-field header "lambda_list")))
             (when keyword (emit-node keyword :keyword))
             (when name (emit-name name :function-name))
             (when lambda-list (walk-lambda-list lambda-list depth))))
         (walk-lambda-list (node depth)
           (delimiters node depth)
           (dolist (element (ts-named-children node))
             (let ((type (ts-node-type element)))
               (cond
                 ((and (string= type "sym_lit") (lambda-list-keyword-p (down element)))
                  (emit-node element :keyword))
                 ((string= type "sym_lit") (emit-node element :variable-param))
                 ((string= type "list_lit") (walk-lambda-sublist element depth))
                 (t (walk element depth nil))))))
         (walk-lambda-sublist (node depth)
           (delimiters node depth)
           (let ((elements (ts-named-children node)))
             (when elements
               (bind-param (first elements))
               (let ((second (second elements)))
                 (when second
                   (if (string= (ts-node-type second) "sym_lit")
                       (emit-node second :type)      ; method specializer
                       (walk second depth nil))))     ; &optional/&key default value
               (loop :for e :in (cddr elements) :do (walk e depth nil)))))
         (bind-param (node)
           (if (string= (ts-node-type node) "sym_lit")
               (emit-node node :variable-param)
               (walk node 0 nil)))
         (walk-quote (node depth inner-quoted)
           (let ((value (ts-field node "value")))
             (if value
                 (progn
                   (emit (ts-node-start-byte node) (ts-node-start-byte value) :quote)
                   (walk value depth inner-quoted))
                 (walk-children node depth inner-quoted))))
         (walk-function-quote (node depth)
           (let ((value (ts-field node "value")))
             (cond
               ((null value) (walk-children node depth nil))
               ((string= (ts-node-type value) "package_lit")
                (emit (ts-node-start-byte node) (ts-node-start-byte value) :quote)
                (walk-package value nil :function-name))
               ((string= (ts-node-type value) "sym_lit")
                (emit (ts-node-start-byte node) (ts-node-start-byte value) :quote)
                (emit-node value :function-name))
               (t (emit (ts-node-start-byte node) (ts-node-start-byte value) :quote)
                  (walk value depth nil)))))
         (walk-for-clause (node depth)
           (let ((variable (ts-field node "variable")))
             (dolist (child (ts-named-children node))
               (if (and variable
                        (= (ts-node-start-byte child) (ts-node-start-byte variable)))
                   (emit-node child :variable-param)
                   (walk child depth nil)))))
         (walk-loop (node depth)
           (walk-children node depth nil)))
      (walk root 0 nil)
      (nreverse acc))))


;;;; Scheme walk. The grammar is not present in every environment; when it is,
;;;; its node types are comment/string/number/character/boolean/symbol plus the
;;;; list forms. Heads of forms in *scheme-keywords* read as keywords.

(defvar *scheme-keywords* (make-hash-table :test 'equal))
(%intern-table *scheme-keywords*
  '("define" "define-syntax" "define-record-type" "define-values" "lambda"
    "let" "let*" "letrec" "letrec*" "let-values" "let*-values" "if" "cond"
    "case" "when" "unless" "and" "or" "begin" "do" "set!" "quote" "quasiquote"
    "unquote" "delay" "parameterize" "guard" "syntax-rules" "else"))

(defun scheme-highlights (root text index &key lo-byte hi-byte)
  (let ((acc nil))
    (labels
        ((char-at (byte)
           (multiple-value-bind (line col) (byte-to-line-col byte index text)
             (+ (cdr (aref index line)) col)))
         (down (node)
           (string-downcase (subseq text (char-at (ts-node-start-byte node))
                                    (char-at (ts-node-end-byte node)))))
         (emit (start-byte end-byte face)
           (multiple-value-bind (sl sc) (byte-to-line-col start-byte index text)
             (multiple-value-bind (el ec) (byte-to-line-col end-byte index text)
               (if (= sl el)
                   (when (> ec sc)
                     (push (list sl sc ec face) acc))
                   (progn (push (list sl sc 999 face) acc)
                          (loop :for l :from (1+ sl) :below el
                                :do (push (list l 0 999 face) acc))
                          (when (plusp ec)
                            (push (list el 0 ec face) acc)))))))
         (emit-node (node face)
           (emit (ts-node-start-byte node) (ts-node-end-byte node) face))
         (delimiters (node depth)
           (let ((s (ts-node-start-byte node)) (e (ts-node-end-byte node))
                 (face (delimiter-face depth)))
             (emit s (1+ s) face)
             (emit (1- e) e face)))
         (symbol-face (node head)
           (cond ((not head) :variable)
                 ((gethash (down node) *scheme-keywords*) :keyword)
                 (t :function-call)))
         (walk (node depth head)
           (when (and hi-byte
                      (or (>= (ts-node-start-byte node) hi-byte)
                          (<= (ts-node-end-byte node) lo-byte)))
             (return-from walk))
           (let ((type (ts-node-type node)))
             (cond
               ((string= type "comment") (emit-node node :comment))
               ((string= type "string") (emit-node node :string))
               ((string= type "number") (emit-node node :number))
               ((string= type "character") (emit-node node :character))
               ((string= type "boolean") (emit-node node :constant))
               ((string= type "symbol") (emit-node node (symbol-face node head)))
               ((plusp (ts-node-named-child-count node))
                (delimiters node depth)
                (loop :for i :from 0 :below (ts-node-named-child-count node)
                      :do (walk (ts-node-named-child node i) (1+ depth) (zerop i))))
               (t nil)))))
      (walk root 0 nil)
      (nreverse acc))))


;;;; Dispatch

(defun walk-highlights (language root text index &key lo-byte hi-byte)
  "Highlights for LANGUAGE's parse tree ROOT over TEXT, using the line INDEX.
LO-BYTE / HI-BYTE restrict the walk to subtrees intersecting that byte window."
  (case language
    (:commonlisp (cl-highlights root text index :lo-byte lo-byte :hi-byte hi-byte))
    (:scheme (scheme-highlights root text index :lo-byte lo-byte :hi-byte hi-byte))
    (t nil)))


;;;; Indentation. Computes the cl-indent column for a line off the same
;;;; persistent tree the highlighter and motion use. A head that introduces a
;;;; body (a special form, a binder, a definer, a with-/do- form) indents its
;;;; body two past the open paren; anything else aligns its arguments under the
;;;; first one, or one past the paren when the head stands alone on its line.

(defun %defish-p (name)
  "Head names that indent a body regardless of the classification tables: the
def-/with-/do- families, so user macros (defcommand, with-widget, do-thing)
indent like the built-ins without being enumerated."
  (and (stringp name)
       (let ((n (length name)))
         (or (and (>= n 3) (string= "def" name :end2 3))
             (and (>= n 5) (string= "with-" name :end2 5))
             (and (>= n 3) (string= "do-" name :end2 3))
             (string= name "do")
             (string= name "loop")))))

(defun body-form-p (language head-name)
  "True when a form headed by HEAD-NAME indents its body (open-paren + 2) rather
than aligning arguments under the first one."
  (and head-name (stringp head-name)
       (or (%defish-p head-name)
           (case language
             (:commonlisp
              (member (cl-head-kind head-name)
                      '(:special :binder-nested :binder-flat-all :binder-flat-first
                        :def-class :def-struct :def-type :def-var :def-package)))
             (:scheme (and (gethash head-name *scheme-keywords*) t))
             (t nil)))))

(defun %byte->char (byte index text)
  (multiple-value-bind (line col) (byte-to-line-col byte index text)
    (+ (cdr (aref index line)) col)))

(defun %byte-col (byte index text)
  (nth-value 1 (byte-to-line-col byte index text)))

(defun %byte-line (byte index text)
  (nth-value 0 (byte-to-line-col byte index text)))

(defun %node-first-char (node index text)
  (let ((c (%byte->char (ts-node-start-byte node) index text)))
    (when (< c (length text)) (char text c))))

(defun %opens-form-p (node index text)
  (member (%node-first-char node index text) '(#\( #\[ #\{)))

(defun %enclosing-form (node lstart index text)
  "Nearest ancestor of NODE that opens with a bracket and begins before byte
LSTART (so its opener is on an earlier line than the line starting at LSTART).
The root/source_file node is excluded: it begins at byte 0, which is often a
paren, but it is not a form."
  ;; depth-capped: a well-formed tree bounds this by nesting depth, but the cap
  ;; guarantees a signal-free walk can never spin the buffer thread past the
  ;; debugger's reach (a hook catches signals, not loops).
  (loop for n = node then (ts-node-parent n)
        for depth from 0 below 4096
        until (ts-node-is-null n)
        when (and (< (ts-node-start-byte n) lstart)
                  (not (ts-node-is-null (ts-node-parent n)))
                  (%opens-form-p n index text))
          return n
        finally (return nil)))

(defun %form-head-name (form index text)
  "Downcased text of FORM's first named child when it is a single-line symbol,
else nil (a nested head is not a body operator)."
  (when (plusp (ts-node-named-child-count form))
    (let* ((head (ts-node-named-child form 0))
           (hs (ts-node-start-byte head))
           (he (ts-node-end-byte head)))
      (when (= (%byte-line hs index text) (%byte-line (max hs (1- he)) index text))
        (string-downcase (subseq text (%byte->char hs index text)
                                      (%byte->char he index text)))))))

(defun %align-column (form open-col index text)
  "Align under the first argument when it shares the head's line; otherwise one
past the open paren."
  (if (>= (ts-node-named-child-count form) 2)
      (let* ((head (ts-node-named-child form 0))
             (arg  (ts-node-named-child form 1))
             (hb (ts-node-start-byte head))
             (ab (ts-node-start-byte arg)))
        (if (= (%byte-line hb index text) (%byte-line ab index text))
            (%byte-col ab index text)
            (1+ open-col)))
      (1+ open-col)))

(defun parse-indent (ps line)
  "Target indentation column for LINE from PS's persistent tree, or nil to leave
the line as-is (inside a multiline string). 0 at top level. No reparse."
  (let ((tree (ps-tree ps)) (text (ps-text ps)) (lang (ps-language ps)))
    (when tree
      (handler-case
          (let* ((index (build-line-index text))
                 (line (max 0 (min line (1- (length index)))))
                 (lstart (car (aref index line)))
                 (root (ts-tree-root-node tree))
                 (node (ts-node-named-descendant-for-byte-range root lstart lstart)))
            (cond
              ((ts-node-is-null node) 0)
              ((and (string= (ts-node-type node) "str_lit")
                    (< (ts-node-start-byte node) lstart))
               nil)
              (t (let ((form (%enclosing-form node lstart index text)))
                   (if (null form)
                       0
                       (let ((open-col (%byte-col (ts-node-start-byte form) index text)))
                         (if (eql (%node-first-char form index text) #\()
                             (if (body-form-p lang (%form-head-name form index text))
                                 (+ open-col 2)
                                 (%align-column form open-col index text))
                             (1+ open-col))))))))
        (error () nil)))))


;;;; Development harness. Prints every highlighted token and its face; a check
;;;; that a rule resolves as intended without launching the editor.

(defun hl-dump (source &optional (language :commonlisp))
  "Print each highlighted token of SOURCE and the face it resolves to."
  (let* ((runtime (make-ts-runtime))
         (ps (progn (ensure-ts runtime) (make-parse-state runtime language))))
    (if (null ps)
        (format t "~&no grammar loaded for ~a~%" language)
        (let ((lines (coerce (uiop:split-string source :separator '(#\Newline))
                             'vector)))
          (reparse! ps source)
          (dolist (h (parse-highlights ps source))
            (destructuring-bind (line start-col end-col face) h
              (let* ((text (if (< line (length lines)) (aref lines line) ""))
                     (end (min end-col (length text))))
                (format t "~&~2d:~2d  ~16a ~s~%" line start-col face
                        (subseq text start-col (max start-col end))))))))))

(defun hl-dump-file (path &optional (language :commonlisp))
  (with-open-file (s path :if-does-not-exist nil)
    (if (null s)
        (format t "~&no such file: ~a~%" path)
        (let ((source (make-string (file-length s))))
          (hl-dump (subseq source 0 (read-sequence source s)) language)))))
