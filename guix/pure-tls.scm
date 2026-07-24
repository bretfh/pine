(define-module (pure-tls)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (cl-cancel)
  #:export (sbcl-pure-tls))

(define-public sbcl-pure-tls
  (package
    (name "sbcl-pure-tls")
    (version "0.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atgreen/pure-tls")
                    (commit "d985aeaed8336dc6f88eadef5b80741fe22f4b21")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "11cslc6n3n3b3ba0fyi4c64xv041hi3f2z9ngkl8s2y7d17sjm1h"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:tests? #f))
    (inputs (list sbcl-ironclad sbcl-trivial-gray-streams sbcl-flexi-streams
                  sbcl-alexandria sbcl-cl-base64 sbcl-trivial-features
                  sbcl-idna sbcl-bordeaux-threads sbcl-usocket sbcl-cl-cancel))
    (home-page "https://github.com/atgreen/pure-tls")
    (synopsis "TLS 1.3 in pure Common Lisp")
    (description "A TLS 1.3 client and server implementation with no foreign
dependencies.")
    (license license:expat)))

sbcl-pure-tls
