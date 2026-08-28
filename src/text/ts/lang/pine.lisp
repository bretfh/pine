(defpackage #:pine/text/ts/lang/pine
  (:use #:cl)
  (:local-nicknames (#:syntax #:pine/text))
  (:export #:pine))
(in-package #:pine/text/ts/lang/pine)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defun pine ()
  (syntax:language
   {:grammar {:lib "libtree-sitter-pine" :fn "tree_sitter_pine"}
    :indent {:width 2}
    :readtable 'pine/fs/reader:syntax
    :doc "pine"}

   (/node/map_lit  {:delimiters t :rest :form})
   (/node/seq_lit  {:delimiters t :rest :form})

   (/node/ns_path  {:role :path})
   (/node/unquote_lit
    {:fields {"marker" :escape "close" :escape} :rest :form})))

(syntax:declare-language :pine (pine) :parent :commonlisp)
