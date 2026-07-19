(define-module (local-time-duration)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (sbcl-local-time-duration))

;; sento dep. Only the core system is built (the cl-postgres glue asd is
;; skipped so we don't pull cl-postgres).
(define-public sbcl-local-time-duration
  (package
    (name "sbcl-local-time-duration")
    (version "20240503-fa20a4a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/enaeher/local-time-duration")
                    (commit "fa20a4a03a1ee076eada649796e2f2345c930c21")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0f13mg18lv31lclz9jvqyj8d85p1jj1366nlld8m3dxnnwsbbkd6"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("local-time-duration")))
    (inputs (list (specification->package "sbcl-local-time")
                  (specification->package "sbcl-alexandria")
                  (specification->package "sbcl-esrap")))
    (home-page "https://github.com/enaeher/local-time-duration")
    (synopsis "Duration arithmetic on top of local-time")
    (description "Operations on time durations, built on the local-time
library.  A sento dependency.")
    (license license:expat)))

sbcl-local-time-duration
