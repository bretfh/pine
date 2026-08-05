(defpackage #:pine.ts.lang.commonlisp
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:syntax #:pine.ts.syntax))
  (:export #:commonlisp #:server))

(in-package #:pine.ts.lang.commonlisp)
(named-readtables:in-readtable pine.path:syntax)

;;;; Common Lisp, as a declaration.
;;;;
;;;; What is written here is only what the shape of the language says. Whether
;;;; a symbol is a macro, a special operator, a builtin or a constant is not
;;;; written down anywhere: the daemon holds the code, so it is asked. That is
;;;; the difference between this file and the ~250 hand-typed symbol names it
;;;; replaces -- the image knows all of CL, and it knows the user's macros too.

(defun commonlisp ()
  (syntax:language
   {:grammar {:lib "libtree-sitter-commonlisp" :fn "tree_sitter_commonlisp"}
    :indent {:width 2}
    :doc "Common Lisp"
    :constants #{"t" "nil" "pi"
                 "most-positive-fixnum" "most-negative-fixnum"
                 "most-positive-double-float" "most-negative-double-float"
                 "most-positive-single-float" "most-negative-single-float"
                 "most-positive-short-float" "most-negative-short-float"
                 "most-positive-long-float" "most-negative-long-float"}}

   ;; ---- what each node type paints -------------------------------------
   (/node/comment          {:face :comment})
   (/node/block_comment    {:face :comment})
   (/node/dis_expr         {:face :comment})
   (/node/char_lit         {:face :character})
   (/node/num_lit          {:face :number})
   (/node/complex_num_lit  {:face :number})
   (/node/kwd_lit          {:face :constant})
   (/node/nil_lit          {:face :constant})
   (/node/path_lit         {:face :string})
   ;; format directives are named nodes painted over the string body
   (/node/str_lit          {:face :string :inner :escape})
   ;; only the context can name a symbol's face
   (/node/sym_lit          {:role :operand})
   (/node/package_lit      {:role :package})
   ;; a list is a form: its delimiters by depth, then its head decides
   (/node/list_lit         {:delimiters t :head t})
   ;; a set has no head, so every element is an element
   (/node/set_lit          {:delimiters 2 :rest :form})
   ;; the grammar gives a defun a header of its own; it is part of the form
   (/node/defun            {:shape {0 :here} :rest :body})
   (/node/defun_header     {:fields {"keyword" :keyword
                                     "function_name" :name
                                     "lambda_list" :lambda-list}
                            :rest :skip})
   (/node/loop_macro       {:rest :here})
   (/node/loop_keyword     {:face :keyword})
   (/node/for_clause_word  {:face :keyword})
   (/node/accumulation_verb {:face :keyword})
   (/node/for_clause       {:fields {"variable" :variable-param} :rest :here})
   (/node/quoting_lit          {:quote {:value "value" :into t}})
   (/node/syn_quoting_lit      {:quote {:value "value" :into t}})
   (/node/unquoting_lit        {:quote {:value "value" :into nil}})
   (/node/unquote_splicing_lit {:quote {:value "value" :into nil}})
   (/node/var_quoting_lit      {:quote {:value "value" :into nil
                                        :as :function-name}})
   (/otherwise             {:rest :here})

   ;; ---- what a head does to the form it heads ---------------------------
   ;; only forms with a shape are here. Whether a head is a macro, a special
   ;; operator or a builtin is asked of the image.
   (/head/let              {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/let*             {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/do               {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/do*              {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/prog             {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/prog*            {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/compiler-let     {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/symbol-macrolet  {:face :keyword :shape {1 :bindings} :rest :body})

   (/head/lambda           {:face :keyword :shape {1 :vars} :rest :body})
   (/head/destructuring-bind  {:face :keyword :shape {1 :vars} :rest :body})
   (/head/multiple-value-bind {:face :keyword :shape {1 :vars} :rest :body})
   (/head/with-slots       {:face :keyword :shape {1 :vars} :rest :body})
   (/head/with-accessors   {:face :keyword :shape {1 :vars} :rest :body})

   (/head/dolist           {:face :keyword :shape {1 :var} :rest :body})
   (/head/dotimes          {:face :keyword :shape {1 :var} :rest :body})
   (/head/do-symbols       {:face :keyword :shape {1 :var} :rest :body})
   (/head/do-external-symbols {:face :keyword :shape {1 :var} :rest :body})
   (/head/do-all-symbols   {:face :keyword :shape {1 :var} :rest :body})
   (/head/with-open-file   {:face :keyword :shape {1 :var} :rest :body})
   (/head/with-open-stream {:face :keyword :shape {1 :var} :rest :body})
   (/head/with-input-from-string  {:face :keyword :shape {1 :var} :rest :body})
   (/head/with-output-to-string   {:face :keyword :shape {1 :var} :rest :body})

   (/head/defclass         {:face :keyword :name-face :type
                            :shape {1 :name 2 :types 3 :slots} :rest :body})
   (/head/define-condition {:face :keyword :name-face :type
                            :shape {1 :name 2 :types 3 :slots} :rest :body})
   (/head/defstruct        {:face :keyword :name-face :type
                            :shape {1 :name} :rest :slot})
   (/head/deftype          {:face :keyword :name-face :type
                            :shape {1 :name} :rest :body})
   (/head/defvar           {:face :keyword :name-face :variable
                            :shape {1 :name} :rest :body})
   (/head/defparameter     {:face :keyword :name-face :variable
                            :shape {1 :name} :rest :body})
   (/head/defconstant      {:face :keyword :name-face :variable
                            :shape {1 :name} :rest :body})
   (/head/defpackage       {:face :keyword :name-face :namespace
                            :shape {1 :name} :rest :body})
   (/head/in-package       {:face :keyword :name-face :namespace :shape {1 :name}})

   ;; the few the image cannot answer for: DECLARE has no function binding and
   ;; is not a special operator, so nothing about the running image says it is
   ;; anything at all
   (/head/declare          {:face :keyword})
   (/head/declaim          {:face :keyword :rest :body})
   (/head/proclaim         {:face :keyword :rest :body})
   (/head/otherwise        {:face :keyword})
   (/head/t                {:face :keyword})))

(defclass server (ns:server) ()
  (:default-initargs :name :syntax-commonlisp :serves (list /syntax/commonlisp)
                     :after (list :syntax))
  (:documentation "Common Lisp's rules, at /syntax/commonlisp."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /syntax/commonlisp (commonlisp))
  nil)

(ns:register (make-instance 'server))
