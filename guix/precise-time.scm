(define-module (precise-time)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (sbcl-precise-time))

;; Shinmera's high-resolution / monotonic time. Transitive dep of pure-tls
;; (via cl-cancel). Not in guix. Pinned to the upstream commit that carries the
;; source ocicl builds (the ocicl commit predates a repo move).
(define-public sbcl-precise-time
  (package
    (name "sbcl-precise-time")
    (version "20260218-e0bf77d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Shinmera/precise-time")
                    (commit "e0bf77d7c200913c0b7920363c26e89ab68c03ad")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1avkcd5qn0d0y232nr4xx9l3cgdjdzm6fn6pzi2jcjg2r8nx4y56"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("precise-time") #:tests? #f))
    (inputs (list (specification->package "sbcl-documentation-utils")
                  (specification->package "sbcl-cffi")
                  (specification->package "sbcl-trivial-features")))
    (home-page "https://github.com/Shinmera/precise-time")
    (synopsis "High-resolution and monotonic time for Common Lisp")
    (description "Access to the platform's high-resolution and monotonic
clocks via CFFI.  A transitive dependency of pure-tls through cl-cancel.")
    (license license:zlib)))

sbcl-precise-time
