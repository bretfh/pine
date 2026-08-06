(defpackage #:pine.ts.highlight
  (:use #:cl #:pine.ts.runtime #:pine.ts.index)
  (:export
   ;; highlights from a parse tree
   #:parse-highlights #:walk-highlights
   ;; indentation from the same tree
   #:parse-indent #:body-form-p
   ;; what a language is, once compiled: the rules the walk follows
   #:language #:make-language #:languagep
   #:lang-nodes #:lang-heads #:lang-otherwise #:lang-indent-width #:lang-raw
   #:lang-grammar #:lang-constants #:lang-infer #:lang-memo
   #:head-rule #:node-rule #:+roles+
   #:delimiter-face #:lambda-list-keyword-p
   ;; node helpers over the runtime's C surface
   #:ts-type #:ts-type= #:ts-field #:ts-named-nodes))

(in-package #:pine.ts.highlight)

;;;; Single-pass highlighter. One walk of the parse tree assigns each token a
;;;; face from its node type and its context: the enclosing form's head, the
;;;; form depth, the quote state. Positions are converted from tree-sitter's
;;;; UTF-8 bytes to character columns through the line index as spans are
;;;; emitted, so text is never sliced by a byte offset.
;;;;
;;;; What the walk does at each node is a RULE, and the rules are data: a
;;;; language is a map of node type to rule and of head symbol to rule, written
;;;; at /syntax/?name and compiled here. What the walk carries is why this is a
;;;; walk rather than a table -- depth, quoting and the enclosing head are not
;;;; expressible as a pattern over one node.
;;;;
;;;; This file knows nothing about any particular language, and nothing about
;;;; the namespace. pine.ts.syntax puts the two together.

(declaim (optimize (speed 3) (safety 1)))

(defun lambda-list-keyword-p (text)
  (and (plusp (length text)) (char= (char text 0) #\&)))

(defun delimiter-face (depth)
  (intern (format nil "DELIMITER.~d" (mod depth 6)) :keyword))


;;;; Node helpers

(defun ts-type (node) (ts-node-type node))

(defun ts-field (node name)
  (let ((node (ts-node-by-field-name node name (length name))))
    (unless (ts-node-is-null node) node)))

(defun ts-named-nodes (node)
  (loop :for i :from 0 :below (ts-node-named-count node)
        :collect (ts-node-named-nth node i)))

(defun ts-type= (node &rest types)
  (member (ts-node-type node) types :test #'string=))


;;;; A compiled language

(defstruct (language (:conc-name lang-) (:predicate languagep))
  grammar                               ; {:lib :fn}, what runtime loads
  (indent-width 2)
  (nodes (make-hash-table :test 'equal)) ; node type -> rule
  (heads (make-hash-table :test 'equal)) ; head symbol name -> rule
  otherwise                             ; the rule for an unlisted node type
  constants                             ; head names that read as constants
  infer                                 ; (fn [name] rule), asked once per name
  (memo (make-hash-table :test 'equal))  ; what infer answered
  raw)                                  ; the written map, for EQ invalidation

(defun node-rule (lang type)
  (or (gethash type (lang-nodes lang)) (lang-otherwise lang)))

(defun head-rule (lang name &optional package)
  "What a form headed by NAME does to its elements.

The written rule first, then whatever the language can work out about the name
and remembers. A language with nothing to say answers nothing, and a head with
no rule is an ordinary call.

PACKAGE is where the buffer's text reads its symbols, because that is where the
symbol is: a macro a config defined lives in the config's package, not in CL.
The memo is keyed by it for the same reason, since the same name somewhere else
is a different symbol."
  (when name
    (let ((written (gethash name (lang-heads lang))))
      (or written
          (let ((memo (lang-memo lang))
                (key (cons name (and package (package-name package)))))
            (multiple-value-bind (known found) (gethash key memo)
              (cond (found (unless (eq known :none) known))
                    ((null (lang-infer lang)) (setf (gethash key memo) :none) nil)
                    (t (let ((answer (funcall (lang-infer lang) name package)))
                         (setf (gethash key memo) (or answer :none))
                         answer)))))))))


;;;; The walk. One visit per named node.

(defstruct (ctx (:conc-name ctx-) (:copier nil))
  syntax src (acc nil) (depth 0) (quoted nil) head (index 0) package
  lo-byte hi-byte)

(defun %deeper (ctx &key (depth (1+ (ctx-depth ctx))) (quoted (ctx-quoted ctx))
                         (head (ctx-head ctx)) (index (ctx-index ctx)))
  "CTX one level in. The accumulator is a slot of the copy, so what a nested
walk emits has to be carried back: EMIT pushes onto the ctx it was given, and
%WALK hands the list back up."
  (let ((next (copy-structure ctx)))
    (setf (ctx-depth next) depth (ctx-quoted next) quoted
          (ctx-head next) head (ctx-index next) index)
    next))

(defun %text (node ctx)
  (string-downcase (source-substring (ctx-src ctx) (ts-node-start-byte node)
                                     (ts-node-end-byte node))))

(defun %emit (start-byte end-byte face ctx)
  "Paint bytes START..END. A span crossing a line becomes one run per line; a
zero-width one paints nothing, since a comment's extent ends at column 0 of the
next line and that would straddle the incremental window's boundary."
  (when face
    (let ((src (ctx-src ctx)) (acc (ctx-acc ctx)))
      (multiple-value-bind (sl sc) (source-line-col src start-byte)
        (multiple-value-bind (el ec) (source-line-col src end-byte)
          (if (= sl el)
              (when (> ec sc) (push (list sl sc ec face) (cdr acc)))
              (progn
                (push (list sl sc 999 face) (cdr acc))
                (loop :for l :from (1+ sl) :below el
                      :do (push (list l 0 999 face) (cdr acc)))
                (when (plusp ec) (push (list el 0 ec face) (cdr acc))))))))))

(defun %emit-node (node face ctx)
  (%emit (ts-node-start-byte node) (ts-node-end-byte node) face ctx))

(defun %delimiters (node ctx &optional (open 1))
  "The opening and closing tokens, faced by form depth. OPEN is how wide the
opener is: a set opens with two characters."
  (let ((s (ts-node-start-byte node)) (e (ts-node-end-byte node))
        (face (delimiter-face (ctx-depth ctx))))
    (%emit s (+ s open) face ctx)
    (%emit (1- e) e face ctx)))

(defun %constant-p (name ctx)
  (let ((set (lang-constants (ctx-syntax ctx))))
    (and set (gethash name set) t)))

(defun %operand-face (node ctx)
  (cond ((ctx-quoted ctx) :constant)
        ((%constant-p (%text node ctx) ctx) :constant)
        (t :variable)))


;;;; Roles. A rule says which of these each position of a form is walked in.
;;;; Every one was a local function of the hand-written walk this replaces.

(defparameter +roles+
  '(:form :body :here :quoted :skip :operand :package :name :binding-name
    :bindings :binding :vars :var :lambda-list :types :slots :slot :path)
  "How a position of a form is walked. A rule composes these; anything in a
:shape or :fields position that is not one of them names a face to paint with.")

(defun %role (role node ctx)
  (case role
    ((nil :form) (%walk node (%deeper ctx)))
    (:body (%walk node (%deeper ctx)))
    ;; walked here rather than a level in: a defun's header and a loop's
    ;; clauses are part of the form, not nested inside it
    (:here (%walk node ctx))
    (:quoted (%walk node (%deeper ctx :quoted t)))
    (:skip nil)
    (:operand (%emit-node node (%operand-face node ctx) ctx))
    (:package (%package node nil ctx))
    (:path (%path node ctx))
    ;; the face a definition's name takes: what the head rule said, and
    ;; :function-name where there is no head rule to say (a defun's header)
    (:name (%name node (or (ctx-head ctx) :function-name) ctx))
    (:binding-name (%bind-name node ctx))
    (:bindings (%nested-bindings node ctx))
    (:binding (%binding-pair node ctx))
    (:vars (%flat-bindings node ctx t))
    (:var (%flat-bindings node ctx nil))
    (:lambda-list (%lambda-list node ctx))
    (:types (%type-list node ctx))
    (:slots (%slot-list node ctx))
    (:slot (%slot-spec node ctx))
    (t (if (keywordp role)
           (%emit-node node role ctx)
           (%walk node (%deeper ctx))))))

(defun %segment-face (text lastp)
  "What one segment of a path paints. A binder is a binder wherever it sits, a
pattern operator is an operator, and what leads up to the leaf recedes the way
a package prefix does."
  (let ((n (length text)))
    (cond ((and (>= n 1) (char= #\? (char text 0))) :variable-param)
          ((or (string= text "*") (string= text "**")) :keyword)
          ((or (string= text ".") (string= text "..")) :keyword)
          (lastp :constant)
          (t :namespace))))

(defun %path (node ctx)
  "A path is one object painted in parts. The whole node takes the separators'
face first and each part paints over it, which is what the per-column rule of
a later property winning is for."
  (%emit-node node :quote ctx)
  (let* ((parts (ts-named-nodes node))
         (last (car (last parts))))
    (dolist (part parts)
      (let ((type (ts-node-type part)))
        (cond
          ((string= type "path_segment")
           (%emit-node part (%segment-face (%text part ctx) (eq part last)) ctx))
          ((string= type "path_interpolation")
           (let ((value (ts-field part "value")))
             (%emit-node part :escape ctx)
             (when value (%walk value (%deeper ctx)))))
          ((string= type "path_alternation")
           (dolist (name (ts-named-nodes part))
             (%emit-node name :constant ctx)))
          ((string= type "path_constraint")
           (let ((key (ts-field part "key"))
                 (value (ts-field part "value")))
             (when key (%emit-node key :constant ctx))
             (when value (%walk value (%deeper ctx)))))
          (t (%walk part (%deeper ctx))))))))

(defun %name (node face ctx)
  (let ((type (ts-node-type node)))
    (cond ((string= type "sym_lit") (%emit-node node face ctx))
          ((string= type "kwd_lit") (%emit-node node :constant ctx))
          ((string= type "package_lit") (%package node face ctx))
          (t (%walk node (%deeper ctx :depth 0))))))

(defun %bind-name (node ctx)
  (if (string= (ts-node-type node) "sym_lit")
      (%emit-node node :variable ctx)
      (%walk node (%deeper ctx :depth 0))))

(defun %bind-param (node ctx)
  (if (string= (ts-node-type node) "sym_lit")
      (%emit-node node :variable-param ctx)
      (%walk node (%deeper ctx :depth 0))))

(defun %package (node face ctx)
  (let ((package (ts-field node "package"))
        (symbol (ts-field node "symbol")))
    (when package (%emit-node package :namespace ctx))
    (when symbol
      (%emit-node symbol (or face (%operand-face symbol ctx)) ctx))))

(defun %nested-bindings (node ctx)
  (%delimiters node ctx)
  (dolist (binding (ts-named-nodes node))
    (if (string= (ts-node-type binding) "list_lit")
        (%binding-pair binding (%deeper ctx))
        (%bind-name binding ctx))))

(defun %binding-pair (node ctx)
  (%delimiters node ctx)
  (let ((elements (ts-named-nodes node)))
    (when elements
      (%bind-name (first elements) ctx)
      (dolist (e (rest elements)) (%walk e (%deeper ctx))))))

(defun %flat-bindings (node ctx all)
  (%delimiters node ctx)
  (let ((elements (ts-named-nodes node)))
    (cond ((null elements))
          (all (dolist (e elements) (%bind-or-keyword e ctx)))
          (t (%bind-name (first elements) ctx)
             (dolist (e (rest elements)) (%walk e (%deeper ctx)))))))

(defun %bind-or-keyword (node ctx)
  (let ((type (ts-node-type node)))
    (cond ((and (string= type "sym_lit") (lambda-list-keyword-p (%text node ctx)))
           (%emit-node node :keyword ctx))
          ((string= type "sym_lit") (%emit-node node :variable ctx))
          ((string= type "list_lit") (%flat-bindings node ctx t))
          (t (%walk node ctx)))))

(defun %lambda-list (node ctx)
  (%delimiters node ctx)
  (dolist (element (ts-named-nodes node))
    (let ((type (ts-node-type element)))
      (cond ((and (string= type "sym_lit") (lambda-list-keyword-p (%text element ctx)))
             (%emit-node element :keyword ctx))
            ((string= type "sym_lit") (%emit-node element :variable-param ctx))
            ((string= type "list_lit") (%lambda-sublist element ctx))
            (t (%walk element ctx))))))

(defun %lambda-sublist (node ctx)
  (%delimiters node ctx)
  (let ((elements (ts-named-nodes node)))
    (when elements
      (%bind-param (first elements) ctx)
      (let ((second (second elements)))
        (when second
          ;; a method specializer, or an &optional/&key default value
          (if (string= (ts-node-type second) "sym_lit")
              (%emit-node second :type ctx)
              (%walk second ctx))))
      (dolist (e (cddr elements)) (%walk e ctx)))))

(defun %type-list (node ctx)
  (if (string= (ts-node-type node) "list_lit")
      (progn
        (%delimiters node ctx)
        (dolist (e (ts-named-nodes node))
          (if (string= (ts-node-type e) "sym_lit")
              (%emit-node e :type ctx)
              (%walk e (%deeper ctx)))))
      (%walk node ctx)))

(defun %slot-list (node ctx)
  (if (string= (ts-node-type node) "list_lit")
      (progn
        (%delimiters node ctx)
        (dolist (e (ts-named-nodes node)) (%slot-spec e (%deeper ctx))))
      (%walk node ctx)))

(defun %slot-spec (node ctx)
  (let ((type (ts-node-type node)))
    (cond ((string= type "sym_lit") (%emit-node node :variable ctx))
          ((string= type "list_lit")
           (%delimiters node ctx)
           (let ((elements (ts-named-nodes node)))
             (when elements
               (%bind-name (first elements) ctx)
               (dolist (e (rest elements)) (%walk e (%deeper ctx))))))
          (t (%walk node ctx)))))


;;;; Applying a rule

(defun %in-window-p (node ctx)
  (let ((hi (ctx-hi-byte ctx)))
    (or (null hi)
        (and (< (ts-node-start-byte node) hi)
             (> (ts-node-end-byte node) (ctx-lo-byte ctx))))))

(defun %at (rule key) (and rule (fset:lookup rule key)))

(defun %head-name (head ctx)
  "The symbol naming what a form is, downcased, or NIL when its head is not one."
  (let ((type (ts-node-type head)))
    (cond ((string= type "sym_lit") (%text head ctx))
          ((string= type "package_lit")
           (let ((symbol (ts-field head "symbol")))
             (and symbol (%text symbol ctx))))
          (t nil))))

(defun %emit-head (head rule ctx)
  (let ((face (or (%at rule :face) :function-call))
        (type (ts-node-type head)))
    (cond ((string= type "sym_lit") (%emit-node head face ctx))
          ((string= type "package_lit") (%package head face ctx))
          (t (%walk head (%deeper ctx))))))

(defun %walk-head-form (node ctx)
  "A form: its head says what its elements are. Quoted, it is data, and its head
is an element like any other."
  (let ((elements (ts-named-nodes node)))
    (cond
      ((null elements))
      ((ctx-quoted ctx)
       (dolist (e elements) (%walk e (%deeper ctx))))
      (t
       (let* ((head (first elements))
              (name (%head-name head ctx))
              (rule (head-rule (ctx-syntax ctx) name (ctx-package ctx)))
              (shape (%at rule :shape))
              (rest (or (%at rule :rest) :form))
              (named (%at rule :name-face)))
         (%emit-head head rule ctx)
         (loop :for element :in (rest elements)
               :for i :from 1
               :do (%role (or (and shape (fset:lookup shape i)) rest) element
                          (%deeper ctx :depth (ctx-depth ctx) :index i
                                       :head (or named :function-name)))))))))

(defun %apply-rule (rule node ctx)
  (let ((quote-spec (%at rule :quote))
        (face (%at rule :face))
        (role (%at rule :role)))
    (cond
      (quote-spec (%walk-quote node ctx quote-spec))
      (role (%role role node ctx))
      ;; a faced node is a token: what it paints is the whole answer
      (face (%emit-node node face ctx)
            (let ((inner (%at rule :inner)))
              (when inner
                (dolist (e (ts-named-nodes node)) (%emit-node e inner ctx)))))
      (t
       (let ((opens (%at rule :delimiters)))
         (when opens (%delimiters node ctx (if (integerp opens) opens 1))))
       (if (%at rule :head)
           (%walk-head-form node ctx)
           (let ((covered (%fields rule node ctx))
                 (shape (%at rule :shape))
                 (rest (or (%at rule :rest) :here)))
             (loop :for element :in (ts-named-nodes node)
                   :for i :from 0
                   :unless (member (ts-node-start-byte element) covered)
                     :do (%role (or (and shape (fset:lookup shape i)) rest) element
                                (%deeper ctx :depth (ctx-depth ctx)
                                             :index i)))))))))

(defun %fields (rule node ctx)
  "Walk RULE's named fields, answering where each began so the rest skips them."
  (let ((fields (%at rule :fields))
        (covered nil))
    (when (fset:map? fields)
      (fset:do-map (name role fields)
        ;; a field name is written either way round: "lambda_list" reads
        ;; plainly and :lambda_list is what a path-shaped map gives back
        (let ((it (ts-field node (string-downcase (string name)))))
          (when it
            (push (ts-node-start-byte it) covered)
            (%role role it ctx)))))
    covered))

(defun %walk-quote (node ctx spec)
  "A quote paints its marker and walks what it quotes. :INTO says whether the
inside is quoted, which a comma inside a backquote is not."
  (let* ((value (ts-field node (or (fset:lookup spec :value) "value")))
         (as (fset:lookup spec :as))
         (into (fset:lookup spec :into)))
    (if (null value)
        (dolist (e (ts-named-nodes node))
          (%walk e (%deeper ctx :depth (ctx-depth ctx) :quoted into)))
        (progn
          (%emit (ts-node-start-byte node) (ts-node-start-byte value) :quote ctx)
          (let ((type (ts-node-type value)))
            (cond ((and as (string= type "package_lit")) (%package value as ctx))
                  ((and as (string= type "sym_lit")) (%emit-node value as ctx))
                  (t (%walk value (%deeper ctx :depth (ctx-depth ctx)
                                                :quoted into)))))))))

(defun %walk (node ctx)
  (when (%in-window-p node ctx)
    (%apply-rule (node-rule (ctx-syntax ctx) (ts-node-type node)) node ctx)))

(defun walk-highlights (syntax root src &key lo-byte hi-byte forms package)
  "Highlights (line start-col end-col face) for the parse tree ROOT, following
SYNTAX's rules and reading source through SRC.

LO-BYTE / HI-BYTE restrict the walk to subtrees intersecting that byte window.
FORMS names the top-level forms to walk instead of descending from ROOT, which
is how a window avoids enumerating every form in the file; a top-level form has
no enclosing context, so depth and quote state start where they would anyway."
  (when (languagep syntax)
    (let ((ctx (make-ctx :syntax syntax :src src :acc (cons :acc nil)
                         :package package
                         :lo-byte lo-byte :hi-byte hi-byte)))
      (if forms
          (dolist (form forms) (%walk form ctx))
          (%walk root ctx))
      (nreverse (cdr (ctx-acc ctx))))))

(defun body-form-p (syntax head-name &optional package)
  "True when a form headed by HEAD-NAME indents its body rather than aligning
its arguments under the first one. One source with the walk: a rule that walks
what follows as a body is a rule that indents it as one."
  (and head-name (stringp head-name) (languagep syntax)
       (let ((rule (head-rule syntax head-name package)))
         (and rule
              (or (eq :body (fset:lookup rule :rest))
                  (let ((shape (fset:lookup rule :shape))
                        (found nil))
                    (when (fset:map? shape)
                      (fset:do-map (i role shape)
                        (declare (ignore i))
                        (when (eq :body role) (setf found t))))
                    found))
              t))))


;;;; Indentation. Computes the cl-indent column for a line off the same
;;;; persistent tree the highlighter and motion use. A head that introduces a
;;;; body (a special form, a binder, a definer, a with-/do- form) indents its
;;;; body two past the open paren; anything else aligns its arguments under the
;;;; first one, or one past the paren when the head stands alone on its line.

(defun %byte-col (byte src)
  (nth-value 1 (source-line-col src byte)))

(defun %byte-line (byte src)
  (nth-value 0 (source-line-col src byte)))

(defun %node-first-char (node src)
  (source-char-at src (ts-node-start-byte node)))

(defparameter +form-openers+
  '(("list_lit" . 1) ("defun" . 1) ("loop_macro" . 1) ("set_lit" . 2)
    ("map_lit" . 1) ("seq_lit" . 1))
  "Node types whose first token opens a form, and how wide that opener is.

A type table rather than a first-character test: a set opens with two
characters, and a first-character test cannot tell a form from anything else
that happens to start with a bracket.")

(defun %opener-width (node)
  "How wide NODE's opening token is, or NIL when NODE does not open a form."
  (cdr (assoc (ts-node-type node) +form-openers+ :test #'string=)))

(defun %enclosing-form (node lstart src)
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
                  (%opener-width n))
          return n
        finally (return nil)))

(defun %head-node (form)
  "The node naming what FORM is.

Usually the first named node, but not always: the grammar gives a DEFUN a header
of its own, and the head sits inside it under the KEYWORD field. Taking the
first named node there answers the whole header text, which only ever worked
because the body test was a string prefix."
  (when (plusp (ts-node-named-count form))
    (let ((first (ts-node-named-nth form 0)))
      (or (ts-field first "keyword") first))))

(defun %form-head-name (form src)
  "Downcased text of FORM's head when it is a single-line symbol, else nil (a
nested head is not a body operator)."
  (let ((head (%head-node form)))
    (when head
      (let ((hs (ts-node-start-byte head))
            (he (ts-node-end-byte head)))
        (when (= (%byte-line hs src) (%byte-line (max hs (1- he)) src))
          (string-downcase (source-substring src hs he)))))))

(defun %align-first (form open-col src)
  "Align under element 0 when it sits on the opener's line, else one past the
opener. A map and a seq have no head: every element is an element, so there is
no argument to align under and the first thing in it is the first thing."
  (if (plusp (ts-node-named-count form))
      (let* ((first (ts-node-named-nth form 0))
             (fb (ts-node-start-byte first)))
        (if (= (%byte-line (ts-node-start-byte form) src) (%byte-line fb src))
            (%byte-col fb src)
            (1+ open-col)))
      (1+ open-col)))

(defun %headless-p (form)
  "Whether FORM is a collection rather than a call. Its elements are elements."
  (member (ts-node-type form) '("map_lit" "seq_lit" "set_lit") :test #'string=))

(defun %align-column (form open-col src)
  "Align under the first argument when it shares the head's line; otherwise one
past the open paren."
  (if (>= (ts-node-named-count form) 2)
      (let* ((head (ts-node-named-nth form 0))
             (arg  (ts-node-named-nth form 1))
             (hb (ts-node-start-byte head))
             (ab (ts-node-start-byte arg)))
        (if (= (%byte-line hb src) (%byte-line ab src))
            (%byte-col ab src)
            (1+ open-col)))
      (1+ open-col)))

(defun %elements-before (form lstart)
  "How many of FORM's arguments begin before byte LSTART: which argument the
line being indented is."
  (loop :for i :from 1 :below (ts-node-named-count form)
        :count (< (ts-node-start-byte (ts-node-named-nth form i)) lstart)))

(defun parse-indent (ps line &key (width 2))
  "Target indentation column for LINE from PS's persistent tree, or nil to leave
the line as-is (inside a multiline string). 0 at top level. No reparse.

WIDTH is what the buffer's mode says a body indents by, so a mode that indents
by four does, rather than by the two that used to be written here."
  (let ((tree (ps-tree ps)) (src (ps-byte-index ps)) (lang (ps-syntax ps))
        (line (- line (pine.ts.runtime:ps-offset ps))))
    (when (and tree src (<= 0 line))
      (handler-case
          (let* ((line (max 0 (min line (1- (index-line-count src)))))
                 (lstart (line-start src line))
                 (root (ts-tree-root-node tree))
                 (node (ts-node-named-descendant-for-byte-range root lstart lstart)))
            (cond
              ((ts-node-is-null node) 0)
              ((and (string= (ts-node-type node) "str_lit")
                    (< (ts-node-start-byte node) lstart))
               nil)
              (t (let ((form (%enclosing-form node lstart src)))
                   (if (null form)
                       0
                       (let* ((open-col (%byte-col (ts-node-start-byte form) src))
                              (opener (%opener-width form))
                              (name (%form-head-name form src))
                              (rule (and lang name
                                         (head-rule lang name (ps-package ps))))
                              (distinguished (and rule (fset:lookup rule :indent))))
                         (cond
                           ;; a collection has no head, so its continuation
                           ;; lines align under its first element, not its
                           ;; second: there is no head to skip
                           ((%headless-p form) (%align-first form open-col src))
                           ;; a form that distinguishes its first N arguments
                           ;; aligns those and indents what follows. N is where
                           ;; &body sits in the macro's own lambda list. Only on
                           ;; a plain list, where element i really is argument i:
                           ;; a defun's name and lambda list live inside its
                           ;; header, so counting elements there counts nothing.
                           ((and distinguished
                                 (string= "list_lit" (ts-node-type form))
                                 (< (%elements-before form lstart) distinguished))
                            (%align-column form open-col src))
                           ((body-form-p lang name (ps-package ps))
                            (+ open-col width))
                           (t (%align-column form open-col src)))))))))
        ;; NIL is a real answer here: leave the line where it is. A failure is
        ;; not that answer, so it says so before falling back to it.
        (error (c)
          (pine.err:report-failure c (format nil "indenting line ~d" line))
          nil)))))


(defun %viewport-bytes (src from-line to-line)
  "The byte range covering lines FROM-LINE to TO-LINE inclusive."
  (let ((last (1- (index-line-count src))))
    (values (line-start src (max 0 (min from-line last)))
            (line-start src (1+ (max 0 (min to-line last)))))))

(defun %form-at-or-before (root byte count)
  "Index of the last of ROOT's named nodes starting at or before BYTE.

The nodes are in source order, so this is a binary search: finding the window
by descending from the root would touch every form in the file, and each node
call through the by-value TSNode binding allocates."
  (let ((lo 0) (hi (1- count)))
    (loop :while (< lo hi)
          :do (let ((mid (ceiling (+ lo hi) 2)))
                (if (<= (ts-node-start-byte (ts-node-named-nth root mid)) byte)
                    (setf lo mid)
                    (setf hi (1- mid)))))
    lo))

(defun %forms-in-window (root lo-byte hi-byte)
  "ROOT's top-level named nodes that intersect [LO-BYTE, HI-BYTE)."
  (let ((count (ts-node-named-count root)))
    (when (plusp count)
      (loop :for i :from (%form-at-or-before root lo-byte count) :below count
            :for form = (ts-node-named-nth root i)
            :while (< (ts-node-start-byte form) hi-byte)
            :when (> (ts-node-end-byte form) lo-byte)
              :collect form))))

(defun %hl-window (ps tree from-line to-line)
  "Walk the top-level forms covering lines FROM-LINE to TO-LINE, and cache them."
  (let ((src (ps-byte-index ps))
        (root (ts-tree-root-node tree)))
    (multiple-value-bind (lo-byte hi-byte) (%viewport-bytes src from-line to-line)
      (let ((hl (walk-highlights (ps-syntax ps) root src
                                 :lo-byte lo-byte :hi-byte hi-byte
                                 :package (ps-package ps)
                                 :forms (%forms-in-window root lo-byte hi-byte))))
        (setf (ps-hl-cache ps) hl (ps-hl-lines ps) (ps-lines ps)
              (ps-hl-window ps) (cons from-line to-line)
              (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
        hl))))

(defun %hl-window-incremental (ps tree from-line to-line)
  "Re-walk only the forms the edit touched and keep the rest of the window's
cached tuples. Sound because the caller has established that no line moved, so
every cached tuple outside the re-walked lines still describes its own line."
  (destructuring-bind (lo hi delta) (ps-hl-pending ps)
    (declare (ignore delta))
    (let* ((src (ps-byte-index ps))
           (root (ts-tree-root-node tree))
           (last (1- (index-line-count src)))
           (lo-byte (line-start src (max 0 (min lo last))))
           (hi-byte (line-start src (1+ (max 0 (min hi last)))))
           (forms nil))
      ;; Widen to whole forms and then to whole lines until stable, so that a
      ;; cached tuple is dropped exactly where a fresh one is emitted. Without
      ;; the line step a form starting on the same line another ends would have
      ;; its tuples dropped and never re-emitted. The range only grows and is
      ;; bounded by the file, so this terminates.
      (loop :for grew = nil
            :do (setf forms (%forms-in-window root lo-byte hi-byte))
                (dolist (form forms)
                  (let ((s (ts-node-start-byte form)) (e (ts-node-end-byte form)))
                    (when (< s lo-byte) (setf lo-byte s grew t))
                    (when (> e hi-byte) (setf hi-byte e grew t))))
                (let ((ll (line-start src (nth-value 0 (byte-line src lo-byte))))
                      (hh (line-start
                           src
                           (1+ (nth-value 0 (byte-line src (max lo-byte (1- hi-byte))))))))
                  (when (< ll lo-byte) (setf lo-byte ll grew t))
                  (when (> hh hi-byte) (setf hi-byte hh grew t)))
            :while grew)
      (let* ((lo-line (nth-value 0 (byte-line src lo-byte)))
             (hi-line (nth-value 0 (byte-line src (max lo-byte (1- hi-byte)))))
             (fresh (walk-highlights (ps-syntax ps) root src
                                     :lo-byte lo-byte :hi-byte hi-byte
                                     :package (ps-package ps) :forms forms))
             (kept (remove-if (lambda (tuple) (<= lo-line (first tuple) hi-line))
                              (ps-hl-cache ps)))
             (merged (nconc kept fresh)))
        (setf (ps-hl-cache ps) merged (ps-hl-lines ps) (ps-lines ps)
              (ps-hl-window ps) (cons from-line to-line)
              (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
        merged))))

(defun %window-edit-is-local-p (ps from-line to-line)
  "True when the pending edit moved no line and lands inside FROM-LINE..TO-LINE,
which is what makes the window's cached tuples still usable."
  (let ((pending (ps-hl-pending ps)))
    (and pending
         (destructuring-bind (lo hi delta) pending
           (and (zerop delta) (>= lo from-line) (<= hi to-line))))))

(defun %shift-tuples (tuples offset)
  "TUPLES with every line moved from band-relative to buffer coordinates."
  (if (zerop offset)
      tuples
      (mapcar (lambda (tuple)
                (list (+ (first tuple) offset) (second tuple)
                      (third tuple) (fourth tuple)))
              tuples)))

(defun %hl-full (ps tree)
  ;; a full walk asks about every line; a window asks about thirty
  (when (byte-index-pending (ps-byte-index ps))
    (setf (ps-byte-index ps) (compact-index (ps-byte-index ps))))
  (let ((hl (walk-highlights (ps-syntax ps) (ts-tree-root-node tree)
                             (ps-byte-index ps) :package (ps-package ps))))
    (setf (ps-hl-cache ps) hl (ps-hl-lines ps) (ps-lines ps)
          (ps-hl-window ps) nil
          (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
    hl))

(defun %hl-incremental (ps tree)
  "Re-walk only the top-level forms covering the recorded edit and merge with
the cached tuples: keep lines above, shift lines below by the edit's delta.
The re-walk window is widened to whole lines and whole top-level forms (to a
fixpoint), so cached tuples are dropped exactly where fresh ones are emitted."
  (destructuring-bind (lo hi delta) (ps-hl-pending ps)
    (let* ((src (ps-byte-index ps))
           (root (ts-tree-root-node tree))
           (nlines (index-line-count src))
           (lo-byte (line-start src (min lo (1- nlines))))
           (hi-byte (line-start src (1+ hi))))
      ;; widen to whole lines and whole top-level forms until stable; the
      ;; window only grows and is bounded by the file, so this terminates
      (loop for grew = nil
            do (loop for i from 0 below (ts-node-named-count root)
                     for node = (ts-node-named-nth root i)
                     for s = (ts-node-start-byte node)
                     for e = (ts-node-end-byte node)
                     do (when (and (< s hi-byte) (> e lo-byte)
                                   (or (< s lo-byte) (> e hi-byte)))
                          (setf lo-byte (min lo-byte s)
                                hi-byte (max hi-byte e)
                                grew t)))
               (let ((ll (line-start src (nth-value 0 (byte-line src lo-byte))))
                     (hl (line-start
                          src
                          (1+ (nth-value 0 (byte-line src (max lo-byte (1- hi-byte))))))))
                 (when (< ll lo-byte) (setf lo-byte ll grew t))
                 (when (> hl hi-byte) (setf hi-byte hl grew t)))
            while grew)
      ;; a window that swallowed most of the file: the merge bookkeeping buys
      ;; nothing over the plain full walk
      (when (>= (- hi-byte lo-byte) (* 3 (floor (max 1 (index-total src)) 4)))
        (return-from %hl-incremental (%hl-full ps tree)))
      (let* ((lo-line (nth-value 0 (byte-line src lo-byte)))
             (hi-line (nth-value 0 (byte-line src (max lo-byte (1- hi-byte)))))
             (old-hi-line (- hi-line delta))
             (fresh (walk-highlights (ps-syntax ps) root src
                                     :lo-byte lo-byte :hi-byte hi-byte :package (ps-package ps)))
             (merged nil))
        (dolist (tup (ps-hl-cache ps))
          (let ((line (first tup)))
            (when (< line lo-line)
              (push tup merged))))
        (setf merged (nreverse merged))
        (setf merged (nconc merged (copy-list fresh)))
        (let ((below nil))
          (dolist (tup (ps-hl-cache ps))
            (let ((line (first tup)))
              (when (> line old-hi-line)
                (push (list (+ line delta) (second tup) (third tup) (fourth tup))
                      below))))
          (setf merged (nconc merged (nreverse below))))
        (setf (ps-hl-cache ps) merged (ps-hl-lines ps) (ps-lines ps)
              (ps-hl-window ps) nil
              (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
        merged))))

(defun parse-highlights (ps &key from-line to-line)
  "Highlights (line start-col end-col face) from PS's persistent tree, in the
buffer's own line numbers.

The source is PS's own lines and byte index, the ones its tree was parsed from,
so a caller cannot hand this a text the tree does not describe. Cache identity
is EQ on the lines: the seq is immutable, so an unchanged buffer is one
comparison rather than a walk over the file.

With FROM-LINE and TO-LINE, walks only the tree covering those lines. The
descent from the root still runs, so quote state, form depth and head kind stay
exact. Without a range, the whole buffer is walked and cached, and when exactly
one edit was recorded since the last call only the changed top-level forms are
re-walked. Anything unexpected falls back to the full walk."
  (let* ((tree (ps-tree ps))
         (lines (ps-lines ps))
         (offset (ps-offset ps))
         (from-line (and from-line (- from-line offset)))
         (to-line (and to-line (- to-line offset))))
    (when (and tree (ps-byte-index ps))
      (handler-case
          (%shift-tuples
           (if (and from-line to-line)
               (let ((same-window (equal (ps-hl-window ps) (cons from-line to-line))))
                 (cond
                   ((and (ps-hl-cache ps) same-window (not (ps-hl-stale ps))
                         (null (ps-hl-pending ps))
                         (eq lines (ps-hl-lines ps)))
                    (ps-hl-cache ps))
                   ((and (ps-hl-cache ps) same-window (not (ps-hl-stale ps))
                         (%window-edit-is-local-p ps from-line to-line))
                    (%hl-window-incremental ps tree from-line to-line))
                   (t (%hl-window ps tree from-line to-line))))
               (cond
                 ((and (ps-hl-cache ps) (null (ps-hl-pending ps)) (not (ps-hl-stale ps))
                       (eq lines (ps-hl-lines ps)))
                  (ps-hl-cache ps))
                 ((and (ps-hl-cache ps) (ps-hl-pending ps) (not (ps-hl-stale ps)))
                  (%hl-incremental ps tree))
                 (t (%hl-full ps tree))))
           offset)
        ;; the cached tuples are the likeliest thing to be wrong, so they are
        ;; dropped and the tree walked once more; the condition is reported
        ;; either way, and a second failure is the walk's own
        (error (c)
          (pine.err:report-failure
           c (format nil "highlighting ~a, retrying without the cache"
                     (ps-language ps)))
          (setf (ps-hl-cache ps) nil (ps-hl-lines ps) nil
                (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
          (%shift-tuples
           (walk-highlights (ps-syntax ps) (ts-tree-root-node tree)
                            (ps-byte-index ps) :package (ps-package ps))
           offset))))))
