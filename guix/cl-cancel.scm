(define-module (cl-cancel)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages)
  #:use-module (precise-time)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (sbcl-cl-cancel))

;; atgreen's cancellation / deadline primitives. Dep of pure-tls, not in guix.
(define-public sbcl-cl-cancel
  (package
    (name "sbcl-cl-cancel")
    (version "20260206-bec34fb")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atgreen/cl-cancel")
                    (commit "bec34fb37fe713746bdeefaf542f578d174d9ffa")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1qjh94wrbmypziwayhzh1nbr9319pwdi722i95lc8qcrvvb1brrk"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:asd-systems ''("cl-cancel") #:tests? #f))
    (inputs (list (specification->package "sbcl-bordeaux-threads")
                  (specification->package "sbcl-atomics")
                  sbcl-precise-time))
    (home-page "https://github.com/atgreen/cl-cancel")
    (synopsis "Cancellation, deadlines, and cancellable streams for Common Lisp")
    (description "Cancellation tokens, deadlines, and cancellable streams,
used by pure-tls.")
    (license license:expat)))

sbcl-cl-cancel
