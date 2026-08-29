(defpackage #:pine/text/ts/lang/commonlisp
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text) (#:d #:pine/data))
  (:export #:commonlisp))
(in-package #:pine/text/ts/lang/commonlisp)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defun commonlisp ()
  (text:language
   (d:map :grammar (d:map :lib "libtree-sitter-commonlisp" :fn "tree_sitter_commonlisp")
    :indent (d:map :width 2)
    :doc "Common Lisp"
    :constants (d:set "t" "nil" "pi"
                 "most-positive-fixnum" "most-negative-fixnum"
                 "most-positive-double-float" "most-negative-double-float"
                 "most-positive-single-float" "most-negative-single-float"
                 "most-positive-short-float" "most-negative-short-float"
                 "most-positive-long-float" "most-negative-long-float"))

   (/node/comment          (d:map :face :comment))
   (/node/block_comment    (d:map :face :comment))
   (/node/dis_expr         (d:map :face :comment))
   (/node/char_lit         (d:map :face :character))
   (/node/num_lit          (d:map :face :number))
   (/node/complex_num_lit  (d:map :face :number))
   (/node/kwd_lit          (d:map :face :constant))
   (/node/nil_lit          (d:map :face :constant))
   (/node/path_lit         (d:map :face :string))

   (/node/str_lit          (d:map :face :string :inner :escape))

   (/node/sym_lit          (d:map :role :operand))
   (/node/package_lit      (d:map :role :package))

   (/node/list_lit         (d:map :delimiters t :head t))

   (/node/set_lit          (d:map :delimiters 2 :rest :form))

   (/node/defun            (d:map :shape (d:map 0 :here) :rest :body))
   (/node/defun_header     (d:map :fields (d:map "keyword" :keyword
                                     "function_name" :name
                                     "lambda_list" :lambda-list)
                            :rest :skip))
   (/node/loop_macro       (d:map :rest :here))
   (/node/loop_keyword     (d:map :face :keyword))
   (/node/for_clause_word  (d:map :face :keyword))
   (/node/accumulation_verb (d:map :face :keyword))
   (/node/for_clause       (d:map :fields (d:map "variable" :variable-param) :rest :here))
   (/node/quoting_lit          (d:map :quote (d:map :value "value" :into t)))
   (/node/syn_quoting_lit      (d:map :quote (d:map :value "value" :into t)))
   (/node/unquoting_lit        (d:map :quote (d:map :value "value" :into nil)))
   (/node/unquote_splicing_lit (d:map :quote (d:map :value "value" :into nil)))
   (/node/var_quoting_lit      (d:map :quote (d:map :value "value" :into nil
                                        :as :function-name)))
   (/otherwise             (d:map :rest :here))

   (/head/let              (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/let*             (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/do               (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/do*              (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/prog             (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/prog*            (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/compiler-let     (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))
   (/head/symbol-macrolet  (d:map :face :keyword :shape (d:map 1 :bindings) :rest :body))

   (/head/lambda           (d:map :face :keyword :shape (d:map 1 :vars) :rest :body))
   (/head/destructuring-bind  (d:map :face :keyword :shape (d:map 1 :vars) :rest :body))
   (/head/multiple-value-bind (d:map :face :keyword :shape (d:map 1 :vars) :rest :body))
   (/head/with-slots       (d:map :face :keyword :shape (d:map 1 :vars) :rest :body))
   (/head/with-accessors   (d:map :face :keyword :shape (d:map 1 :vars) :rest :body))

   (/head/dolist           (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/dotimes          (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/do-symbols       (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/do-external-symbols (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/do-all-symbols   (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/with-open-file   (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/with-open-stream (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/with-input-from-string  (d:map :face :keyword :shape (d:map 1 :var) :rest :body))
   (/head/with-output-to-string   (d:map :face :keyword :shape (d:map 1 :var) :rest :body))

   (/head/defclass         (d:map :face :keyword :name-face :type
                            :shape (d:map 1 :name 2 :types 3 :slots) :rest :body))
   (/head/define-condition (d:map :face :keyword :name-face :type
                            :shape (d:map 1 :name 2 :types 3 :slots) :rest :body))
   (/head/defstruct        (d:map :face :keyword :name-face :type
                            :shape (d:map 1 :name) :rest :slot))
   (/head/deftype          (d:map :face :keyword :name-face :type
                            :shape (d:map 1 :name) :rest :body))
   (/head/defvar           (d:map :face :keyword :name-face :variable
                            :shape (d:map 1 :name) :rest :body))
   (/head/defparameter     (d:map :face :keyword :name-face :variable
                            :shape (d:map 1 :name) :rest :body))
   (/head/defconstant      (d:map :face :keyword :name-face :variable
                            :shape (d:map 1 :name) :rest :body))
   (/head/defpackage       (d:map :face :keyword :name-face :namespace
                            :shape (d:map 1 :name) :rest :body))
   (/head/in-package       (d:map :face :keyword :name-face :namespace :shape (d:map 1 :name)))

   (/head/handler-case     (d:map :face :keyword :indent 1 :rest :body))

   (/head/declare          (d:map :face :keyword))
   (/head/declaim          (d:map :face :keyword :rest :body))
   (/head/proclaim         (d:map :face :keyword :rest :body))
   (/head/otherwise        (d:map :face :keyword))
   (/head/t                (d:map :face :keyword))))

(text:declare-language :commonlisp (commonlisp))
