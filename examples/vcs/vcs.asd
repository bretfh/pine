(asdf:defsystem #:vcs
                :description "An app that brings a device of its own"
                :depends-on (#:pine/host #:pine/edit)
                :components ((:file "vcs")))
