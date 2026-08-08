(asdf:defsystem #:pine
                :description "Pine Is Not Emacs"
                :author "Bret Horne"
                :license "GPL"
                :version "0.0.1"
                :depends-on (#:sento
                             #:sento-remoting
                             #:usocket
                             #:fset
                             #:alexandria
                             #:bordeaux-threads
                             #:closer-mop
                             #:named-readtables
                             #:cffi
                             #:cffi-libffi
                             #:com.inuoe.jzon
                             #:sqlite)
                :in-order-to ((asdf:test-op (asdf:test-op #:pine/test)))
                :serial t
                :pathname "src/"
                :components
                ((:module "repl"
                          :serial t
                          :components ((:file "package") (:file "command")
                                       (:file "mode") (:file "session")))))

(asdf:defsystem #:pine/test
                :depends-on (#:pine #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite") (:file "style") (:file "repl") (:file "mode"))
                :perform (asdf:test-op (o c)
                                       (unless (uiop:symbol-call :fiveam :run! :pine)
                                         (error "pine tests failed"))))

(asdf:defsystem #:pine/vt
                :description "Native terminal emulator"
                :depends-on (#:cffi)
                :serial t
                :pathname "vt/"
                :components ((:file "package")
                             (:file "types")
                             (:file "color")
                             (:file "sgr")
                             (:file "ops")
                             (:file "write")
                             (:file "parser")
                             (:file "render")
                             (:file "input")
                             (:file "pty")))
