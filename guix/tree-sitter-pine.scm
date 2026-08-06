(define-module (tree-sitter-pine)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (tree-sitter-pine))

(define-public tree-sitter-pine
  (package
    (name "tree-sitter-pine")
    (version "0.1.0")
    (source (local-file "../../tree-sitter-commonlisp/pine/src"
                        "tree-sitter-pine-src" #:recursive? #t))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "gcc" "-shared" "-fPIC" "-I" "." "parser.c"
                      "-o" "libtree-sitter-pine.so")))
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib/tree-sitter")))
                (mkdir-p lib)
                (install-file "libtree-sitter-pine.so" lib)))))))
    (home-page "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
    (synopsis "Tree-sitter grammar for Common Lisp under pine's reader")
    (description "Common Lisp as pine's readtable reads it: brace maps, bracket
seqs, namespace paths and interpolation.  A dialect of tree-sitter-commonlisp,
generated from the same checkout.")
    (license license:expat)))

tree-sitter-pine
