(define-module (pine packages precise-time)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:export (sbcl-precise-time))

(define-public sbcl-precise-time
  (package
    (name "sbcl-precise-time")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shinmera/precise-time")
                    (commit "e0bf77d7c200913c0b7920363c26e89ab68c03ad")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1avkcd5qn0d0y232nr4xx9l3cgdjdzm6fn6pzi2jcjg2r8nx4y56"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:tests? #f))
    (inputs (list sbcl-documentation-utils sbcl-trivial-features sbcl-cffi))
    (home-page "https://github.com/shinmera/precise-time")
    (synopsis "Precise time measurement for Common Lisp")
    (description "Access to the operating system's most precise monotonic and
real time counters.")
    (license license:zlib)))

sbcl-precise-time
