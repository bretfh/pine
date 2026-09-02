(defpackage #:pine/text/ts/lang/pine
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text) (#:d #:pine/data))
  (:export #:pine))
(in-package #:pine/text/ts/lang/pine)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defun pine ()
  (text:language
   (d:map :grammar (d:map :lib "libtree-sitter-pine" :fn "tree_sitter_pine")
    :indent (d:map :width 2)
    :readtable 'pine/fs/reader:syntax
    :doc "pine")

   (/node/map_lit  (d:map :delimiters t :rest :form))
   (/node/seq_lit  (d:map :delimiters t :rest :form))

   (/node/ns_path  (d:map :role :path))
   (/node/unquote_lit
    (d:map :fields (d:map "marker" :escape "close" :escape) :rest :form))))

(text:declare-language :pine (pine) :parent :commonlisp)
