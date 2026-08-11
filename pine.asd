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
                             #:cffi-libffi
                             #:fset
                             #:sento
                             #:sento-remoting
                             #:usocket
                             #:sqlite
                             #:com.inuoe.jzon
                             #:sb-posix
                             #:pine/vt
                             #:sb-introspect
                             #:uiop)
                :in-order-to ((asdf:test-op (asdf:test-op #:pine/test)))
                :serial t
                :pathname "src/"
                :components
                ((:file "data")
                 (:module "run"
                          :serial t
                          :components ((:file "libs") (:file "task") (:file "fault")
                                       (:file "timer") (:file "agent")
                                       (:file "log")))
                 (:module "fs"
                          :serial t
                          :components ((:file "node") (:file "computed")
                                       (:file "tree") (:file "mount")
                                       (:file "watch")))
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
                                       (:file "session") (:file "shell")))
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
                 (:module "ts"
                          :serial t
                          :components ((:file "index") (:file "runtime") (:file "parse") (:file "edit")
                                       (:file "highlight") (:file "indent") (:file "walk")
                                       (:file "syntax")
                                       (:module "lang"
                                                :serial t
                                                :components ((:file "commonlisp")
                                                             (:file "scheme")
                                                             (:file "pine")))))
                 (:module "provider"
                          :serial t
                          :components ((:file "out") (:file "sh") (:file "live") (:file "env")
                                       (:file "clock") (:file "sys")
                                       (:file "audio") (:file "screen")
                                       (:file "power") (:file "net")
                                       (:file "media") (:file "wm")))
                 (:module "edit"
                          :serial t
                          :components ((:file "text") (:file "history") (:file "buffer")
                                       (:file "language")
                                       (:file "window") (:file "prompt")))
                 (:module "parser"
                          :serial t :pathname "ts/"
                          :components ((:file "parser")))
                 (:module "net"
                          :serial t
                          :components ((:file "server") (:file "attach") (:file "agent")
                                       (:file "control")))
                 (:module "surface"
                          :serial t :pathname "app/"
                          :components ((:file "surface")))
                 (:module "editing"
                          :serial t :pathname "edit/"
                          :components ((:file "key") (:file "render")
                                       (:file "term") (:file "commands")
                                       (:file "motion") (:file "isearch") (:file "listing") (:file "repl") (:file "debugger")
                                       (:file "defaults") (:file "eval")
                                       (:file "session")))
                 (:module "app"
                          :serial t
                          :components ((:file "desktop") (:file "wm")
                                       (:file "compositor")))
                 (:file "frontend")
                 (:file "boot")
                 (:file "cli")))

(asdf:defsystem #:pine/test
                :depends-on (#:pine #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite") (:file "style")
                             (:file "run") (:file "vt") (:file "layout") (:file "fs") (:file "world")
                             (:file "proc") (:file "repl") (:file "mode") (:file "path") (:file "ui") (:file "ts") (:file "edit") (:file "eval") (:file "isearch") (:file "prompt") (:file "provider") (:file "term") (:file "net") (:file "control") (:file "desktop") (:file "stress") (:file "boot"))
                :perform (asdf:test-op (o c)
                                       (unless (uiop:symbol-call :fiveam :run! :pine)
                                         (error "pine tests failed"))))

(asdf:defsystem #:pine/cairo
                :depends-on (#:pine #:cl-cairo2)
                :serial t
                :pathname "src/cairo/"
                :components ((:file "grid") (:file "paint") (:file "calendar")
                             (:file "shot")))

(asdf:defsystem #:pine/wayland
                :depends-on (#:pine/cairo #:wayflan-client #:posix-shm #:cl-xkb)
                :serial t
                :pathname "src/"
                :components
                ((:module "protocol"
                          :serial t :pathname "wayland/protocol/"
                          :components ((:file "wlr-layer-shell") (:file "river-wm")
                                       (:file "river-xkb") (:file "river-layer-shell")))
                 (:module "wayland"
                          :serial t
                          :components ((:file "connection") (:file "surface")
                                       (:file "input")))
                 (:module "app"
                          :serial t :pathname "wayland/app/"
                          :components ((:file "editor") (:file "keys")
                                       (:file "desktop")
                                       (:file "chrome") (:file "chord")
                                       (:file "state")
                                       (:file "wm")))))

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
