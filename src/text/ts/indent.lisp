(in-package #:pine/text/ts/highlight)

(defparameter +form-openers+
  '(("list_lit" . 1) ("defun" . 1) ("loop_macro" . 1) ("set_lit" . 2)
    ("map_lit" . 1) ("seq_lit" . 1))
  "Node types whose first token opens a form, and how wide that opener is.

A type table rather than a first-character test: a set opens with two
characters, and a first-character test cannot tell a form from anything else
that happens to start with a bracket.")

(defun %in-window-p (node ctx)
  (let ((hi (ctx-hi-byte ctx)))
    (or (null hi)
        (and (< (ts-node-start-byte node) hi)
             (> (ts-node-end-byte node) (ctx-lo-byte ctx))))))

(defun %at (rule key) (and rule (pl:lookup rule key)))

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
               :do (%role (or (and shape (pl:lookup shape i)) rest) element
                          (%deeper ctx :depth (ctx-depth ctx) :index i
                                       :head (or named :function-name)))))))))

(defun %apply-rule (rule node ctx)
  (let ((quote-spec (%at rule :quote))
        (face (%at rule :face))
        (role (%at rule :role)))
    (cond
      (quote-spec (%walk-quote node ctx quote-spec))
      (role (%role role node ctx))

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
                     :do (%role (or (and shape (pl:lookup shape i)) rest) element
                                (%deeper ctx :depth (ctx-depth ctx)
                                             :index i)))))))))

(defun %fields (rule node ctx)
  "Walk RULE's named fields, answering where each began so the rest skips them."
  (let ((fields (%at rule :fields))
        (covered nil))
    (when (pl:mapp fields)
      (pl:do-map (name role fields)

        (let ((it (ts-field node (string-downcase (string name)))))
          (when it
            (push (ts-node-start-byte it) covered)
            (%role role it ctx)))))
    covered))

(defun %walk-quote (node ctx spec)
  "A quote paints its marker and walks what it quotes. :INTO says whether the
inside is quoted, which a comma inside a backquote is not."
  (let* ((value (ts-field node (or (pl:lookup spec :value) "value")))
         (as (pl:lookup spec :as))
         (into (pl:lookup spec :into)))
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
              (or (eq :body (pl:lookup rule :rest))
                  (let ((shape (pl:lookup rule :shape))
                        (found nil))
                    (when (pl:mapp shape)
                      (pl:do-pairs (i role shape)
                        (when (eq :body role) (setf found t))))
                    found))
              t))))
(defun %byte-col (byte src)
  (nth-value 1 (source-line-col src byte)))

(defun %byte-line (byte src)
  (nth-value 0 (source-line-col src byte)))

(defun %node-first-char (node src)
  (source-char-at src (ts-node-start-byte node)))

(defun %opener-width (node)
  "How wide NODE's opening token is, or NIL when NODE does not open a form."
  (cdr (assoc (ts-node-type node) +form-openers+ :test #'string=)))

(defun %enclosing-form (node lstart)
  "Nearest ancestor of NODE that opens with a bracket and begins before byte
LSTART (so its opener is on an earlier line than the line starting at LSTART).
The root/source_file node is excluded: it begins at byte 0, which is often a
paren, but it is not a form."

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
        (line (- line (pine/text/ts/runtime:ps-offset ps))))
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
              (t (let ((form (%enclosing-form node lstart)))
                   (if (null form)
                       0
                       (let* ((open-col (%byte-col (ts-node-start-byte form) src))
                              (name (%form-head-name form src))
                              (rule (and lang name
                                         (head-rule lang name (ps-package ps))))
                              (distinguished (and rule (pl:lookup rule :indent))))
                         (cond

                           ((%headless-p form) (%align-first form open-col src))

                           ((and distinguished
                                 (string= "list_lit" (ts-node-type form))
                                 (< (%elements-before form lstart) distinguished))
                            (%align-column form open-col src))
                           ((body-form-p lang name (ps-package ps))
                            (+ open-col width))
                           (t (%align-column form open-col src)))))))))

        (error (c)
          (pine/run/fault:report c (format nil "indenting line ~d" line))
          nil)))))
