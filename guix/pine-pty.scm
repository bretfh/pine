(define-module (pine-pty)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (pine-pty))

(define-public pine-pty
  (package
    (name "pine-pty")
    (version "0.0.1")
    (source (local-file "../lib/pty.c" "pine-pty.c"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'unpack)
          (replace 'build
            (lambda _
              (copy-file #$(local-file "../lib/pty.c") "pty.c")
              (copy-file #$(local-file "../lib/pty-helper.c") "pty-helper.c")
              (invoke "gcc" "-O2" "pty-helper.c" "-o" "pine-pty-helper")
              (invoke "gcc" "-shared" "-fPIC"
                      (string-append "-DPINE_PTY_HELPER=\"" #$output
                                     "/bin/pine-pty-helper\"")
                      "pty.c" "-lutil" "-o" "libpine-pty.so")))
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib"))
                    (bin (string-append #$output "/bin")))
                (mkdir-p lib)
                (mkdir-p bin)
                (install-file "libpine-pty.so" lib)
                (install-file "pine-pty-helper" bin)))))))
    (home-page "https://example.com/pine")
    (synopsis "PTY spawn helper for pine")
    (description "A small shared library that spawns a command under a
pseudo-terminal, for pine's terminal buffers, and the helper it spawns.")
    (license license:expat)))

pine-pty
