(defpackage #:pine.ts.lang.pine
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:syntax #:pine.ts.syntax))
  (:export #:pine))

(in-package #:pine.ts.lang.pine)
(named-readtables:in-readtable pine.path:syntax)

;;;; Common Lisp as pine's own reader sees it.
;;;;
;;;; Everything Common Lisp says is true here, so this declaration is that one
;;;; plus what pine's readtable adds: brace maps, bracket seqs, paths and
;;;; interpolation. That it is a handful of node rules over another language's
;;;; map rather than a second walker is the whole point of rules being data.
;;;;
;;;; A path is painted the way a package_lit is: what leads up to the leaf
;;;; recedes, the leaf carries. The pattern operators are keywords because they
;;;; are operators, and a binder is a binder wherever it appears.

(defun pine ()
  (syntax:language
   (fset:with (pine.ts.lang.commonlisp:commonlisp)
              :grammar {:lib "libtree-sitter-pine" :fn "tree_sitter_pine"})

   ;; a map and a seq have no head: every element is an element
   (/node/map_lit  {:delimiters t :rest :form})
   (/node/seq_lit  {:delimiters t :rest :form})

   ;; a path is one object, painted in parts
   (/node/ns_path  {:role :path})
   (/node/unquote_lit
    {:fields {"marker" :escape "close" :escape} :rest :form})))

(ns:serve :syntax-pine
  {:at [/syntax/pine]
   :after [:syntax :syntax-commonlisp]
   :doc "pine's own reader, at /syntax/pine"
   :up (lambda () (ns:write /syntax/pine (pine)) nil)})
