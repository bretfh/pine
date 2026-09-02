(define-module (pine packages pine)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-13)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages xdisorg)
  #:use-module (pine packages sento)
  #:use-module (pine packages timer-wheel)
  #:use-module (pine packages local-time-duration)
  #:use-module (pine packages pure-tls)
  #:use-module (pine packages cl-cancel)
  #:use-module (pine packages precise-time)
  #:use-module (pine packages tree-sitter-commonlisp)
  #:use-module (pine packages wayflan)
  #:export (pine %pine-lisp-inputs))

(define (S name) (specification->package name))

(define %pine-lisp-inputs
  (append
   (map S (list "sbcl-alexandria" "sbcl-bordeaux-threads" "sbcl-closer-mop"
                "sbcl-named-readtables" "sbcl-cffi" "sbcl-fset" "sbcl-usocket"
                "sbcl-cl-sqlite" "sbcl-jzon" "sbcl-cl-cairo2" "sbcl-cl-xkb"
                "sbcl-posix-shm" "sbcl-log4cl"))
   (list sbcl-sento sbcl-timer-wheel sbcl-local-time-duration sbcl-pure-tls
         sbcl-cl-cancel sbcl-precise-time sbcl-wayflan)))

(define (pine-source-select? file stat)
  (not (or (member (basename file)
                   '(".git" ".cache" "www" "systems" "ocicl" ".pine.bin"))
           (string-suffix? "/lib/tree-sitter" file)
           (string-suffix? "/lib/libpine-pty.so" file)
           (string-suffix? "/lib/pine-pty-helper" file)
           (and (string-suffix? "/pine" file)
                (eq? 'regular (stat:type stat))))))

(define-public pine
  (package
    (name "pine")
    (version "0.0.1")
    (source (local-file "../../.." "pine-source"
                        #:recursive? #t
                        #:select? pine-source-select?))
    (build-system asdf-build-system/sbcl)
    (outputs '("out" "bin"))
    (arguments
     (list
      #:asd-systems ''("pine")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'build-owned-c
            (lambda _
              (let* ((lib (string-append #$output "/lib"))
                     (ts (string-append lib "/tree-sitter"))
                     (libexec (string-append #$output "/libexec"))
                     (helper (string-append libexec "/pine-pty-helper")))
                (mkdir-p lib)
                (mkdir-p ts)
                (mkdir-p libexec)
                (substitute* "lib/pty-helper.c"
                  (("\"/bin/sh\"")
                   (string-append "\"" #$(file-append bash-minimal "/bin/sh")
                                  "\"")))
                (invoke "gcc" "-O2" "lib/pty-helper.c" "-o" helper)
                (invoke "gcc" "-shared" "-fPIC"
                        (string-append "-DPINE_PTY_HELPER=\"" helper "\"")
                        "lib/pty.c" "-lutil"
                        "-o" (string-append lib "/libpine-pty.so"))
                (invoke "gcc" "-shared" "-fPIC" "-I" "grammar/pine/src"
                        "grammar/pine/src/parser.c"
                        "-o" (string-append ts "/libtree-sitter-pine.so")))))
          (add-after 'build-owned-c 'fix-library-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; every one of these is named through cffi's :default, which
              ;; appends the suffix itself, so what goes in is extensionless.
              (let* ((bare (lambda (path)
                             (substring path 0 (- (string-length path) 3))))
                     (found (lambda (suffix)
                              (bare (search-input-file inputs suffix)))))
                (substitute* "vt/pty.lisp"
                  (("\"libpine-pty\"")
                   (string-append "\"" #$output "/lib/libpine-pty\"")))
                (substitute* "src/systems/text/ts/runtime.lisp"
                  (("\"libtree-sitter\"")
                   (string-append "\"" (found "/lib/libtree-sitter.so") "\"")))
                (substitute* "src/systems/text/ts/lang/pine.lisp"
                  (("\"libtree-sitter-pine\"")
                   (string-append "\"" #$output
                                  "/lib/tree-sitter/libtree-sitter-pine\"")))
                (substitute* "src/systems/text/ts/lang/commonlisp.lisp"
                  (("\"libtree-sitter-commonlisp\"")
                   (string-append
                    "\""
                    (found "/lib/tree-sitter/libtree-sitter-commonlisp.so")
                    "\"")))
                (substitute* "src/systems/text/ts/lang/scheme.lisp"
                  (("\"libtree-sitter-scheme\"")
                   (string-append
                    "\""
                    (found "/lib/tree-sitter/libtree-sitter-scheme.so")
                    "\""))))))
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (setenv "HOME" "/tmp")
              (build-program (string-append (assoc-ref outputs "bin")
                                            "/bin/pine")
                             outputs
                             #:dependencies '("pine/all")
                             #:entry-program '((pine/cli:main arguments) 0)))))))
    (native-inputs (list gcc-toolchain))
    (inputs
     (append (list cairo
                   sqlite
                   libffi
                   libxkbcommon
                   tree-sitter
                   tree-sitter-commonlisp
                   (S "tree-sitter-scheme"))
             %pine-lisp-inputs))
    (home-page "https://github.com/bretfh/pine")
    (synopsis "Pine Is Not Emacs")
    (description "Pine is a Common Lisp environment in which everything
addressable is a node in one namespace: documents, devices, running jobs,
surfaces and other machines, answering the same four verbs.  It ships a
daemon, a command line client, an editor, a terminal, a desktop and a window
manager, each a system that can be loaded and dropped at runtime.")
    (license license:expat)))
