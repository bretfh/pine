(defpackage #:pine.ts.highlight
  (:use #:cl #:pine.ts.runtime #:pine.ts.index)
  (:local-nicknames (#:pl #:pine.run.plist))
  (:export

   #:parse-highlights #:walk-highlights

   #:parse-indent #:body-form-p

   #:language #:make-language #:languagep #:lang-name
   #:lang-nodes #:lang-heads #:lang-otherwise #:lang-indent-width #:lang-raw
   #:lang-grammar #:lang-constants #:lang-infer #:lang-memo
   #:head-rule #:node-rule #:+roles+
   #:delimiter-face #:lambda-list-keyword-p

   #:ts-type #:ts-type= #:ts-field #:ts-named-nodes))

(in-package #:pine.ts.highlight)

(defparameter +roles+
  '(:form :body :here :quoted :skip :operand :package :name :binding-name
    :bindings :binding :vars :var :lambda-list :types :slots :slot :path)
  "How a position of a form is walked. A rule composes these; anything in a
:shape or :fields position that is not one of them names a face to paint with.")

(declaim (optimize (speed 3) (safety 1)))

(defun lambda-list-keyword-p (text)
  (and (plusp (length text)) (char= (char text 0) #\&)))

(defun delimiter-face (depth)
  (intern (format nil "DELIMITER.~d" (mod depth 6)) :keyword))

(defun ts-type (node) (ts-node-type node))

(defun ts-field (node name)
  (let ((node (ts-node-by-field-name node name (length name))))
    (unless (ts-node-is-null node) node)))

(defun ts-named-nodes (node)
  (loop :for i :from 0 :below (ts-node-named-count node)
        :collect (ts-node-named-nth node i)))

(defun ts-type= (node &rest types)
  (member (ts-node-type node) types :test #'string=))

(defstruct (language (:conc-name lang-) (:predicate languagep))
  name
  grammar
  (indent-width 2)
  (nodes (make-hash-table :test 'equal))
  (heads (make-hash-table :test 'equal))
  otherwise
  constants
  infer
  (memo (make-hash-table :test 'equal))
  raw)

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

(defun %role (role node ctx)
  (case role
    ((nil :form) (%walk node (%deeper ctx)))
    (:body (%walk node (%deeper ctx)))

    (:here (%walk node ctx))
    (:quoted (%walk node (%deeper ctx :quoted t)))
    (:skip nil)
    (:operand (%emit-node node (%operand-face node ctx) ctx))
    (:package (%package node nil ctx))
    (:path (%path node ctx))

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
