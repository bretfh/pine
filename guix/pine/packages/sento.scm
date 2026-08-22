(define-module (pine packages sento)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (pine packages timer-wheel)
  #:use-module (pine packages local-time-duration)
  #:use-module (pine packages pure-tls)
  #:export (sbcl-sento))

(define-public sbcl-sento
  (package
    (name "sbcl-sento")
    (version "3.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdbergmann/cl-gserver")
                    (commit "a889b8375709ca9396bba5d06b6e460689a23716")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1w3dgz517jw9zpfcck827gq2h4gpy173gylcg6anp55x153h8pz5"))))
    (build-system asdf-build-system/sbcl)
    (arguments (list #:tests? #f
                     #:asd-systems ''("sento" "sento-remoting")))
    (inputs (list sbcl-alexandria sbcl-log4cl sbcl-bordeaux-threads
                  sbcl-cl-speedy-queue sbcl-cl-str sbcl-blackbird
                  sbcl-binding-arrows sbcl-atomics sbcl-timer-wheel
                  sbcl-local-time-duration sbcl-flexi-streams sbcl-usocket
                  sbcl-pure-tls))
    (home-page "https://github.com/mdbergmann/cl-gserver")
    (synopsis "Actor framework for Common Lisp")
    (description "Sento is an actor framework with actors, agents, futures,
and a remoting extension for actors across images and hosts.")
    (license license:asl2.0)))

sbcl-sento
