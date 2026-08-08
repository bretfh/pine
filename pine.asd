(asdf:defsystem #:pine
                :description "Pine Is Not Emacs"
                :author "Bret Horne"
                :license "GPL"
                :version "0.0.1"
                :depends-on (#:alexandria
                             #:bordeaux-threads
                             #:closer-mop
                             #:named-readtables
                             #:cffi
                             #:sqlite
                             #:uiop)
                :in-order-to ((asdf:test-op (asdf:test-op #:pine/test)))
                :serial t
                :pathname "src/"
                :components
                ((:module "run"
                          :serial t
                          :components ((:file "cell") (:file "mailbox")
                                       (:file "task")))
                 (:module "fs"
                          :serial t
                          :components ((:file "node") (:file "computed")
                                       (:file "tree") (:file "mount")))
                 (:module "world"
                          :serial t
                          :components ((:file "world") (:file "store")))
                 (:module "proc"
                          :serial t
                          :components ((:file "process") (:file "lisp")
                                       (:file "supervisor")))
                 (:module "repl"
                          :serial t
                          :components ((:file "command") (:file "mode")
                                       (:file "session")))
                 (:module "path"
                          :serial t
                          :components ((:file "path") (:file "reader")
                                       (:file "place")))
                 (:module "ui"
                          :serial t
                          :components ((:file "node") (:file "face")
                                       (:file "raster") (:file "css")
                                       (:file "style") (:file "build")
                                       (:file "layout") (:file "hit") (:file "cells")
                                       (:file "wire") (:file "paths")))
                 (:file "boot")))

(asdf:defsystem #:pine/test
                :depends-on (#:pine #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite") (:file "style")
                             (:file "run") (:file "fs") (:file "world")
                             (:file "proc") (:file "repl") (:file "mode") (:file "path") (:file "ui") (:file "boot"))
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
