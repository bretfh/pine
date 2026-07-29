(define-module (pine)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages xdisorg)
  #:use-module (sento)
  #:use-module (timer-wheel)
  #:use-module (local-time-duration)
  #:use-module (pure-tls)
  #:use-module (cl-cancel)
  #:use-module (precise-time)
  #:use-module (tree-sitter-commonlisp)
  #:use-module (pine-pty)
  #:use-module (river)
  #:use-module (wayflan)
  #:export (pine))

;; pine built as a guix package: the same save-lisp-and-die image `make bin'
;; produces, from this repo's source, with a wrapper carrying the runtime
;; environment (dlopened libs + tree-sitter grammars) the Makefile wrapper
;; carries via GUIX_ENVIRONMENT. trivial-build-system: no strip phase, which
;; would discard the core appended to the ELF.

(define (pine-source-select? file stat)
  (let ((name (basename file)))
    (not (member name '(".git" "www" ".pine.bin" "pine" ".cache")))))

(define (S name) (specification->package name))

(define %cl-deps
  ;; mirrors manifest.scm's sbcl closure
  (map S (list "sbcl-alexandria" "sbcl-atomics" "sbcl-babel"
               "sbcl-binding-arrows" "sbcl-blackbird" "sbcl-bordeaux-threads"
               "sbcl-cffi" "sbcl-cl-base64" "sbcl-cl-cairo2" "sbcl-cl-ppcre"
               "sbcl-cl-ppcre-unicode" "sbcl-cl-unicode" "sbcl-cl-speedy-queue"
               "sbcl-cl-sqlite" "sbcl-iterate" "sbcl-cl-str" "sbcl-cl-xkb"
               "sbcl-closer-mop" "sbcl-documentation-utils" "sbcl-esrap"
               "sbcl-fiveam" "sbcl-net.didierverna.asdf-flv" "sbcl-global-vars"
               "sbcl-trivial-garbage" "sbcl-trivial-backtrace"
               "sbcl-trivial-indent" "sbcl-split-sequence"
               "sbcl-misc-extensions" "sbcl-mt19937" "sbcl-named-readtables"
               "sbcl-trivial-with-current-source-form" "sbcl-cl-change-case"
               "sbcl-vom" "sbcl-float-features" "sbcl-cl-colors2"
               "sbcl-cl-colors" "sbcl-let-plus" "sbcl-metabang-bind"
               "sbcl-plump" "sbcl-array-utils" "sbcl-anaphora"
               "sbcl-cl-utilities" "sbcl-flexi-streams" "sbcl-fset"
               "sbcl-idna" "sbcl-ironclad" "sbcl-jzon" "sbcl-local-time"
               "sbcl-log4cl" "sbcl-posix-shm" "sbcl-trivial-features"
               "sbcl-trivial-gray-streams" "sbcl-usocket")))

(define %local-deps
  (list sbcl-sento sbcl-timer-wheel sbcl-local-time-duration sbcl-pure-tls
        sbcl-cl-cancel sbcl-precise-time sbcl-wayflan))

(define-public pine
  (package
    (name "pine")
    (version "0.0.1")
    (source (local-file ".." "pine-source"
                        #:recursive? #t
                        #:select? pine-source-select?))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils) (guix build union))
      #:builder
      #~(begin
          (use-modules (guix build utils) (guix build union))
          (union-build "/tmp/env"
                       (list #$@%cl-deps #$@%local-deps
                             #$tree-sitter-commonlisp #$pine-pty
                             #$river-0.4
                             #$(S "wlr-protocols")
                             #$(S "libffi") #$(S "linux-libre-headers")
                             #$cairo #$tree-sitter #$libxkbcommon))
          (copy-recursively #$(package-source this-package) "/tmp/src")
          (setenv "HOME" "/tmp")
          (setenv "GUIX_ENVIRONMENT" "/tmp/env")
          (setenv "LD_LIBRARY_PATH" "/tmp/env/lib")
          ;; cffi-libffi grovels: gcc compiles a probe against libffi
          (setenv "PATH" (string-append #$(file-append (S "gcc-toolchain") "/bin")
                                        ":" (or (getenv "PATH") "")))
          (setenv "C_INCLUDE_PATH" "/tmp/env/include")
          (setenv "LIBRARY_PATH" "/tmp/env/lib")
          (setenv "CL_SOURCE_REGISTRY"
                  "/tmp/src//:/tmp/env/share/common-lisp//")
          ;; fasls and groveled wrappers (posix-shm's wrapper.so) go into the
          ;; output: the saved image reloads foreign libs by recorded path,
          ;; so those paths must survive the build.
          (setenv "ASDF_OUTPUT_TRANSLATIONS"
                  (string-append "/:" #$output "/lib/asdf-cache/"))
          (mkdir-p #$output)
          (with-directory-excursion "/tmp/src"
            (invoke #$(file-append (S "sbcl") "/bin/sbcl")
                    "--dynamic-space-size" "4096"
                    "--no-userinit" "--non-interactive"
                    "--eval" "(require :asdf)"
                    "--eval" "(asdf:load-system :pine/wayland)"
                    "--eval" "(sb-ext:save-lisp-and-die \".pine.bin\"
                                :toplevel (function pine.cli:main)
                                :executable t)"))
          (mkdir-p #$output)
          (union-build (string-append #$output "/env")
                       (list #$cairo #$tree-sitter #$tree-sitter-commonlisp
                             #$pine-pty #$libxkbcommon #$(S "libffi")))
          (let ((bin (string-append #$output "/bin"))
                (libexec (string-append #$output "/libexec")))
            (mkdir-p bin)
            (mkdir-p libexec)
            (copy-file "/tmp/src/.pine.bin"
                       (string-append libexec "/pine.bin"))
            (chmod (string-append libexec "/pine.bin") #o555)
            (call-with-output-file (string-append bin "/pine")
              (lambda (port)
                (format port "#!~a~%export GUIX_ENVIRONMENT=~s~%~
export LD_LIBRARY_PATH=~s${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}~%~
exec ~s \"$@\"~%"
                        #$(file-append (S "bash-minimal") "/bin/sh")
                        (string-append #$output "/env")
                        (string-append #$output "/env/lib")
                        (string-append #$output "/libexec/pine.bin"))))
            (chmod (string-append bin "/pine") #o555)))))
    (home-page "https://github.com/bfh/pine")
    (synopsis "Pine Is Not Emacs")
    (description "Pine is a multi-process Lisp environment: a headless
daemon holding buffers as actors over immutable state, with editor,
desktop, and window manager frontends as separate supervised processes
over remoting.")
    (license license:expat)))
