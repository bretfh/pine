(defpackage #:pine/text/ts/lang/scheme
  (:use #:cl)
  (:local-nicknames (#:syntax #:pine/text/ts/syntax))
  (:export #:scheme))
(in-package #:pine/text/ts/lang/scheme)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defun scheme ()
  (syntax:language
   {:grammar {:lib "libtree-sitter-scheme" :fn "tree_sitter_scheme"}
    :indent {:width 2}
    :doc "Scheme"
    :constants #{"else"}}

   (/node/comment   {:face :comment})
   (/node/string    {:face :string})
   (/node/number    {:face :number})
   (/node/character {:face :character})
   (/node/boolean   {:face :constant})
   (/node/symbol    {:role :operand})
   (/otherwise      {:delimiters t :head t})

   (/head/lambda    {:face :keyword :shape {1 :vars} :rest :body})
   (/head/define    {:face :keyword :name-face :function-name
                     :shape {1 :name} :rest :body})
   (/head/define-syntax  {:face :keyword :name-face :function-name
                          :shape {1 :name} :rest :body})
   (/head/define-values  {:face :keyword :shape {1 :vars} :rest :body})
   (/head/define-record-type {:face :keyword :rest :body})
   (/head/let       {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/let*      {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/letrec    {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/letrec*   {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/let-values  {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/let*-values {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/if        {:face :keyword :rest :body})
   (/head/cond      {:face :keyword :rest :body})
   (/head/case      {:face :keyword :rest :body})
   (/head/when      {:face :keyword :rest :body})
   (/head/unless    {:face :keyword :rest :body})
   (/head/and       {:face :keyword :rest :body})
   (/head/or        {:face :keyword :rest :body})
   (/head/begin     {:face :keyword :rest :body})
   (/head/do        {:face :keyword :rest :body})
   (/head/set!      {:face :keyword :rest :body})
   (/head/quote     {:face :keyword :rest :quoted})
   (/head/quasiquote {:face :keyword :rest :quoted})
   (/head/unquote   {:face :keyword :rest :body})
   (/head/delay     {:face :keyword :rest :body})
   (/head/parameterize {:face :keyword :shape {1 :bindings} :rest :body})
   (/head/guard     {:face :keyword :rest :body})
   (/head/syntax-rules {:face :keyword :rest :body})))

(syntax:declare-language :scheme (scheme))
