(defpackage #:pine/text/ts/lang/commonlisp
  (:use #:cl)
  (:local-nicknames (#:syntax #:pine/text/ts/syntax))
  (:export #:commonlisp))
(in-package #:pine/text/ts/lang/commonlisp)

(named-readtables:in-readtable pine/fs/reader:syntax)

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

   (/node/comment          {:face :comment})
   (/node/block_comment    {:face :comment})
   (/node/dis_expr         {:face :comment})
   (/node/char_lit         {:face :character})
   (/node/num_lit          {:face :number})
   (/node/complex_num_lit  {:face :number})
   (/node/kwd_lit          {:face :constant})
   (/node/nil_lit          {:face :constant})
   (/node/path_lit         {:face :string})

   (/node/str_lit          {:face :string :inner :escape})

   (/node/sym_lit          {:role :operand})
   (/node/package_lit      {:role :package})

   (/node/list_lit         {:delimiters t :head t})

   (/node/set_lit          {:delimiters 2 :rest :form})

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

   (/head/declare          {:face :keyword})
   (/head/declaim          {:face :keyword :rest :body})
   (/head/proclaim         {:face :keyword :rest :body})
   (/head/otherwise        {:face :keyword})
   (/head/t                {:face :keyword})))

(syntax:declare-language :commonlisp (commonlisp))
