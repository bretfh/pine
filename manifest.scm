(use-modules (guix profiles) (gnu packages))
(add-to-load-path (string-append (dirname (current-filename)) "/guix"))
(use-modules (tree-sitter-commonlisp) (pine-pty))

(concatenate-manifests
 (list (specifications->manifest
        (list "sbcl"
              "cairo"
              "tree-sitter"
              "wlr-protocols"
              "libxkbcommon"
              "sbcl-cl-xkb"
              "gcc-toolchain"
              "pkg-config"))
       (packages->manifest (list tree-sitter-commonlisp pine-pty))))
