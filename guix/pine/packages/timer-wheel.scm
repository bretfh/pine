(define-module (pine packages timer-wheel)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:export (sbcl-timer-wheel))

(define-public sbcl-timer-wheel
  (package
    (name "sbcl-timer-wheel")
    (version "0.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/npatrick04/timer-wheel")
                    (commit "7b4bf7193f2af96386e8d9f89f79c72e76b6b2f9")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0w94q4h5n646l3iaywq1akag38vg2bwlrg7q6w5g7dj9lv4gl2iy"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:tests? #f))
    (inputs (list sbcl-bordeaux-threads))
    (home-page "https://github.com/npatrick04/timer-wheel")
    (synopsis "Timer wheel for Common Lisp")
    (description "A hierarchical timer wheel scheduling timeouts in O(1).")
    (license license:expat)))

sbcl-timer-wheel
