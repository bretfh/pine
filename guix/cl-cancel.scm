(define-module (cl-cancel)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (precise-time)
  #:export (sbcl-cl-cancel))

(define-public sbcl-cl-cancel
  (package
    (name "sbcl-cl-cancel")
    (version "0.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atgreen/cl-cancel")
                    (commit "bec34fb37fe713746bdeefaf542f578d174d9ffa")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1qjh94wrbmypziwayhzh1nbr9319pwdi722i95lc8qcrvvb1brrk"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:tests? #f))
    (inputs (list sbcl-bordeaux-threads sbcl-atomics sbcl-precise-time))
    (home-page "https://github.com/atgreen/cl-cancel")
    (synopsis "Cancellation contexts for Common Lisp")
    (description "Deadline and cancellation propagation for threads and
streams.")
    (license license:expat)))

sbcl-cl-cancel
