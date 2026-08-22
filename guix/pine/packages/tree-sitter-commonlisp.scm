(define-module (pine packages tree-sitter-commonlisp)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (tree-sitter-commonlisp))

(define-public tree-sitter-commonlisp
  (package
    (name "tree-sitter-commonlisp")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xg3ay8l62h7s35abkxi4gjfvndzdvvrpgh1z980q1ib5935sxf0"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "gcc" "-shared" "-fPIC" "-I" "src" "src/parser.c"
                      "-o" "libtree-sitter-commonlisp.so")))
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib/tree-sitter")))
                (mkdir-p lib)
                (install-file "libtree-sitter-commonlisp.so" lib)))))))
    (home-page "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
    (synopsis "Tree-sitter grammar for Common Lisp")
    (description "Common Lisp grammar for tree-sitter, built as a shared
library loadable by pine.")
    (license license:expat)))

tree-sitter-commonlisp
