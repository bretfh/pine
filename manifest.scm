(use-modules (guix profiles) (gnu packages))
(add-to-load-path (string-append (dirname (current-filename)) "/guix"))
(use-modules (tree-sitter-commonlisp) (pine-pty) (sento)
             (timer-wheel) (local-time-duration) (pure-tls)
             (cl-cancel) (precise-time) (river) (wayflan))

(concatenate-manifests
 (list (specifications->manifest
        (list "sbcl"
              ;; the wm harness (bench/wm-shot.sh): capture, a test client,
              ;; and synthetic key presses against headless river
              "grim"
              "foot"
              "wtype"
              "cairo"
              "tree-sitter"
              "wlr-protocols"
              "libxkbcommon"
              "gcc-toolchain"
              "pkg-config"
              ;; doc/*.dot -> the png and svg beside them: make docs
              "graphviz"
              "sbcl-alexandria"
              "sbcl-atomics"
              "sbcl-babel"
              "sbcl-binding-arrows"
              "sbcl-blackbird"
              "sbcl-bordeaux-threads"
              "sbcl-cffi"
              "sbcl-cl-base64"
              "sbcl-cl-cairo2"
              "sbcl-cl-ppcre"
              "sbcl-cl-ppcre-unicode"
              "sbcl-cl-unicode"
              "sbcl-cl-speedy-queue"
              "sbcl-cl-sqlite"
              "sbcl-iterate"
              "sbcl-cl-str"
              "sbcl-cl-xkb"
              "sbcl-closer-mop"
              "sbcl-documentation-utils"
              "sbcl-esrap"
              "sbcl-fiveam"
              "sbcl-net.didierverna.asdf-flv"
              "sbcl-global-vars"
              "sbcl-trivial-garbage"
              "sbcl-trivial-backtrace"
              "sbcl-trivial-indent"
              "sbcl-split-sequence"
              "sbcl-misc-extensions"
              "sbcl-mt19937"
              "sbcl-named-readtables"
              "sbcl-trivial-with-current-source-form"
              "sbcl-cl-change-case"
              "sbcl-vom"
              "sbcl-float-features"
              "sbcl-cl-colors2"
              "sbcl-cl-colors"
              "sbcl-let-plus"
              "sbcl-metabang-bind"
              "sbcl-plump"
              "sbcl-array-utils"
              "sbcl-anaphora"
              "sbcl-cl-utilities"
              "sbcl-flexi-streams"
              "sbcl-fset"
              "sbcl-idna"
              "sbcl-ironclad"
              "sbcl-jzon"
              "sbcl-local-time"
              "sbcl-log4cl"
              "sbcl-posix-shm"
              "sbcl-trivial-features"
              "sbcl-trivial-gray-streams"
              "sbcl-usocket"))
       (packages->manifest (list tree-sitter-commonlisp pine-pty sbcl-sento
                                 sbcl-timer-wheel sbcl-local-time-duration
                                 sbcl-pure-tls sbcl-cl-cancel
                                 sbcl-precise-time river-0.4
                                 sbcl-wayflan))))
