(asdf:defsystem #:pine
  :description "Pine Is Not Emacs"
  :author "Bret Horne"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:sento
               #:sento-remoting
               #:fset
               #:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:cffi
               #:cffi-libffi
               #:com.inuoe.jzon
               #:pine/vt)
  :in-order-to ((asdf:test-op (asdf:test-op #:pine/test)))
  :serial t
  :pathname "src/"
  :components
  ((:file "package")
   (:module "core"
    :serial t
    :components ((:file "server") (:file "eval") (:file "actor") (:file "agent")
                (:file "jobs") (:file "event") (:file "hooks") (:file "attach")))
   (:module "buffer"
    :serial t
    :components ((:file "buffer") (:file "window") (:file "face") (:file "rules")))
   (:module "client"
    :serial t
    :components ((:file "client")))
   (:module "state"
    :serial t
    :components ((:file "ref") (:file "var")))
   (:module "input"
    :serial t
    :components ((:file "key") (:file "keymap") (:file "command")))
   (:module "mode"
    :serial t
    :components ((:file "mode") (:file "edit")))
   (:module "layout"
    :serial t
    :components ((:file "style") (:file "layout")))
   (:module "source"
    :serial t
    :components ((:file "sources")))
   (:module "term"
    :serial t
    :components ((:file "term")))
   (:module "ts"
    :serial t
    :components ((:file "ts") (:file "highlight")))
   (:module "render"
    :serial t
    :components ((:file "render")))
   (:module "editor"
    :serial t
    :components ((:file "echo") (:file "kill-ring") (:file "completion")
                (:file "minibuffer") (:file "isearch") (:file "file")
                (:file "repl") (:file "editor")
                (:file "session")))
   (:module "desktop"
    :serial t
    :components ((:file "desktop")))
   (:file "user")
   (:file "main")))

(asdf:defsystem #:pine/test
  :description "The pine test suite."
  :depends-on (#:pine #:fiveam)
  :serial t
  :pathname "tests/"
  :components ((:file "suite") (:file "model") (:file "editor") (:file "agent"))
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :pine.test :run-tests)
               (error "pine tests failed"))))

(asdf:defsystem #:pine/vt
  :description "Self-contained terminal emulator (ex hemlock.term) + native pty."
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
  :components ((:file "display") (:file "paint") (:file "shot")))

(asdf:defsystem #:pine/wayland
  :description "Wayland client: wlr-layer-shell bindings + shm/cairo surfaces
painting the pine.layout tree through the :pine/cairo backend. Uses the wayflan
library for the raw protocol."
  :depends-on (#:pine/cairo #:wayflan-client #:posix-shm #:cl-xkb)
  :serial t
  :pathname "src/wayland/"
  :components ((:file "layer-shell") (:file "surface") (:file "input") (:file "client")
               (:file "editor") (:file "editor-keys")))
