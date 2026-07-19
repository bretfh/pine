(use-modules (guix profiles) (gnu packages))
(add-to-load-path (string-append (dirname (current-filename)) "/guix"))
(use-modules (tree-sitter-commonlisp) (pine-pty)
             (timer-wheel) (local-time-duration) (pure-tls))

(concatenate-manifests
 (list (specifications->manifest
        (list "sbcl"
              "gtk"
              "gobject-introspection"
              "tree-sitter"
              "gtk4-layer-shell"
              "wlr-protocols"
              "libxkbcommon"
              "sbcl-cl-xkb"
              "gcc-toolchain"
              "pkg-config"
              ;; pine's Lisp deps straight from guix (no ocicl). sento +
              ;; sento-remoting are in-repo (vendor/); these satisfy their deps
              ;; and pine's own. The three libs guix lacks (timer-wheel,
              ;; local-time-duration, pure-tls) are the local packages below.
              ;; core / daemon:
              "sbcl-fset"
              "sbcl-alexandria"
              "sbcl-bordeaux-threads"
              "sbcl-closer-mop"
              "sbcl-cffi"
              "sbcl-jzon"              ; provides com.inuoe.jzon
              "sbcl-log4cl"
              "sbcl-cl-speedy-queue"
              "sbcl-cl-str"            ; provides str
              "sbcl-blackbird"
              "sbcl-binding-arrows"
              "sbcl-atomics"
              "sbcl-flexi-streams"
              "sbcl-usocket"
              ;; wayland + cairo backend (the wl-editor / wl-desktop path):
              "sbcl-wayflan"           ; provides wayflan-client
              "sbcl-posix-shm"
              "sbcl-cl-cairo2"))
       (packages->manifest (list tree-sitter-commonlisp pine-pty
                                 sbcl-timer-wheel sbcl-local-time-duration
                                 sbcl-pure-tls))))
