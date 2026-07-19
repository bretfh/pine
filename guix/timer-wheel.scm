(define-module (timer-wheel)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (sbcl-timer-wheel))

;; sento's ask-timeout wheel. Not in guix; pinned to the commit ocicl uses.
(define-public sbcl-timer-wheel
  (package
    (name "sbcl-timer-wheel")
    (version "20240503-6cdcb93")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/npatrick04/timer-wheel")
                    (commit "6cdcb93b2cdc45b5dc963d061f96a0801b61aa83")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "12pc1dpnkwj43n1sdqhg8n8h0mb16zcx4wxly85b7bqf00s962bc"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("timer-wheel")))
    (inputs (list (specification->package "sbcl-bordeaux-threads")))
    (home-page "https://github.com/npatrick04/timer-wheel")
    (synopsis "Portable single-layer timer wheel for Common Lisp")
    (description "A portable single-layer timer wheel implementation, used by
sento for ask timeouts.")
    (license license:expat)))

sbcl-timer-wheel
