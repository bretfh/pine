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
                             #:sqlite
                             #:pine/vt)
                :in-order-to ((asdf:test-op (asdf:test-op #:pine/test)))
                :serial t
                :pathname "src/"
                :components
                (
                 ;; The order is the dependency order: every file is written in
                 ;; terms of the ones before it, and nothing names anything that
                 ;; loads later. tests/packages.lisp holds it to that.
                 ;;
                 ;; the data model, then the vocabularies that are nothing but
                 ;; places in it
                 (:file "data")
                 (:file "path")
                 (:file "ns")
                 (:file "mode")
                 (:file "win")
                 (:file "cmd")
                 ;; what the tree keeps, says, and is told
                 (:file "store")
                 (:file "log")
                 (:file "doc")
                 (:file "err")
                 (:file "proc")
                 (:module "core"
                          :serial t
                          :components ((:file "server") (:file "actor") (:file "agent")
                                       (:file "hooks") (:file "attach")))
                 (:file "host")
                 (:module "provider"
                          :serial t
                          :components ((:file "file") (:file "sh") (:file "env")
                                       (:file "out")
                                       (:file "clock") (:file "procfs")
                                       (:file "pipewire") (:file "backlight")
                                       (:file "logind") (:file "networkmanager")
                                       (:file "mpris") (:file "niri")))
                 (:module "ts"
                          :serial t
                          :components ((:file "index") (:file "runtime") (:file "highlight")
                                       (:file "syntax")
                                       (:module "lang"
                                                :serial t
                                                :components ((:file "commonlisp")
                                                             (:file "scheme")))
                                       (:file "parser")))
                 (:module "face"
                          :serial t :pathname "ui/"
                          :components ((:file "face") (:file "css")))
                 (:file "text")
                 (:file "buf")
                 (:file "key")
                 (:file "term")
                 ;; the widget model first, then what resolves a style for one,
                 ;; then what builds, measures, renders and ships them
                 (:module "ui"
                          :serial t
                          :components ((:file "node") (:file "style") (:file "build")
                                       (:file "raster") (:file "layout") (:file "cells")
                                       (:file "wire") (:file "paths")))
                 (:file "view")
                 ;; what a content type looks like as rows: every pane in pine
                 ;; asks this, so nothing has to be an editor to show a buffer
                 (:file "pane")
                 (:file "echo")
                 (:file "eval")
                 (:file "kill")
                 (:file "desktop")
                 (:file "wm")
                 (:module "editor"
                          :serial t
                          :components ((:file "frame") (:file "render")
                                       (:file "isearch") (:file "help") (:file "debugger")
                                       (:file "repl") (:file "session") (:file "win") (:file "commands")))
                 (:file "frontend")
                 (:file "boot")
                 (:file "cli")
                 (:file "user")))

(asdf:defsystem #:pine/test
                :description "The pine test suite."
                ;; the wayland and cairo backings load headless, and the package tests can
                ;; only check the packages that are present
                :depends-on (#:pine #:pine/cairo #:pine/wayland #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite") (:file "fixtures")
                             (:file "data") (:file "path") (:file "ns")
                             (:file "store") (:file "err")
                             (:file "doc")
                             (:file "proc") (:file "host") (:file "mode")
                             (:file "win")
                             (:file "provider")
                             (:file "buf")
                             (:file "vt") (:file "index") (:file "ts")
                             (:file "layout") (:file "style") (:file "wire")
                             (:file "keys")
                             (:file "isearch") (:file "repl") (:file "liveness") (:file "async")
                             (:file "term") (:file "wm")
                             (:file "packages") (:file "editor") (:file "agent")
                             (:file "frontend") (:file "cli")
                             ;; the :pine.stress suite, run by `make stress', not by test-op
                             (:file "stress"))
                ;; run! is explain! over run: it prints the report and answers the status.
                ;; The error is what carries a failure out to the shell as an exit code.
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

(asdf:defsystem #:pine/cairo
                :description "Cairo backend for the widget engine: the paint-px pass rendering
the layout tree to a cairo context, styled by the shared pine.style resolver
(which lives in :pine's layout module)."
                :depends-on (#:pine #:cl-cairo2)
                :serial t
                :pathname "src/cairo/"
                :components ((:file "grid") (:file "paint") (:file "shot")))

(asdf:defsystem #:pine/wayland
                :description "The wayland backing: the protocol bindings pine generates from
the compositor's own XML, and the client that paints the pine.layout tree onto
wayland surfaces through the :pine/cairo backend."
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
                          :components ((:file "connection") (:file "surface") (:file "input")))
                 (:module "app"
                          :serial t :pathname "wayland/app/"
                          :components ((:file "editor") (:file "keys") (:file "desktop") (:file "wm")))))
