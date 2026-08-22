(define-module (pine packages local-time-duration)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:export (sbcl-local-time-duration))

(define-public sbcl-local-time-duration
  (package
    (name "sbcl-local-time-duration")
    (version "0.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/enaeher/local-time-duration")
                    (commit "fa20a4a03a1ee076eada649796e2f2345c930c21")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0f13mg18lv31lclz9jvqyj8d85p1jj1366nlld8m3dxnnwsbbkd6"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:tests? #f
                     #:asd-systems ''("local-time-duration")))
    (inputs (list sbcl-local-time sbcl-alexandria sbcl-esrap))
    (home-page "https://github.com/enaeher/local-time-duration")
    (synopsis "Duration arithmetic over local-time")
    (description "Durations and duration arithmetic on top of local-time
timestamps.")
    (license license:expat)))

sbcl-local-time-duration
