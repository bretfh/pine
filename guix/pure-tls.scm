(define-module (pure-tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages)
  #:use-module (cl-cancel)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (sbcl-pure-tls))

;; sento-remoting's TLS 1.3 transport (pure CL, no OpenSSL). Not in guix.
(define-public sbcl-pure-tls
  (package
    (name "sbcl-pure-tls")
    (version "20260215-d78526b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atgreen/pure-tls")
                    (commit "d78526befdbe9a23d82f8cfa39eed349e3a37848")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0f6n6m27glj5fxwr4q8n3w8z5hllf6i14cwvdsr6nshiphyzp1cm"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("pure-tls") #:tests? #f))
    (inputs (list (specification->package "sbcl-ironclad")
                  (specification->package "sbcl-trivial-gray-streams")
                  (specification->package "sbcl-flexi-streams")
                  (specification->package "sbcl-alexandria")
                  (specification->package "sbcl-cl-base64")
                  (specification->package "sbcl-trivial-features")
                  (specification->package "sbcl-idna")
                  (specification->package "sbcl-bordeaux-threads")
                  (specification->package "sbcl-usocket")
                  sbcl-cl-cancel))
    (home-page "https://github.com/atgreen/pure-tls")
    (synopsis "Pure Common Lisp TLS 1.3 implementation")
    (description "A TLS 1.3 (RFC 8446) implementation in pure Common Lisp with
no foreign libraries, used by sento-remoting as its transport.")
    (license license:expat)))

sbcl-pure-tls
