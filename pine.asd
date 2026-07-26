(asdf:defsystem #:pine
  :description "Pine Is Not Emacs"
  :author "Bret Horne"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:sento
               #:sento-remoting
               #:usocket
               #:fset
               #:alexandria
               #:bordeaux-threads
               #:closer-mop
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
   (:module "core"
    :serial t
    :components ((:file "server") (:file "eval") (:file "actor") (:file "agent")
                (:file "jobs") (:file "event") (:file "hooks") (:file "attach")))
   ;; keys and modes are the bottom of the editing stack: the buffer actor
   ;; dispatches a message through its mode, so modes load before buffers.
   (:module "input"
    :serial t :pathname "editor/"
    :components ((:file "key") (:file "keymap")))
   ;; the echo line: no dependencies at all, and the command loop reports
   ;; through it, so it sits under everything that has something to say.
   (:module "echo"
    :serial t :pathname "editor/"
    :components ((:file "echo")))
   (:module "mode"
    :serial t :pathname "editor/"
    :components ((:file "mode")))
   (:module "ts"
    :serial t
    :components ((:file "runtime") (:file "highlight")))
   (:module "face"
    :serial t :pathname "ui/"
    :components ((:file "face") (:file "rules")))
   (:module "text"
    :serial t
    :components ((:file "buffer") (:file "window")))
   (:module "state"
    :serial t
    :components ((:file "store") (:file "ref") (:file "var") (:file "world")))
   (:module "frame"
    :serial t :pathname "editor/"
    :components ((:file "frame")))
   (:module "input-dispatch"
    :serial t :pathname "editor/"
    :components ((:file "command")))
   (:module "ui"
    :serial t
    :components ((:file "style") (:file "node") (:file "build") (:file "raster")
                (:file "layout") (:file "cells") (:file "wire")))
   (:file "source")
   ;; the base/text verb methods, once everything they reach for exists
   (:module "edit"
    :serial t :pathname "editor/"
    :components ((:file "edit")))
   (:file "term")
   (:module "ui-render"
    :serial t :pathname "ui/"
    :components ((:file "render")))
   (:module "editor"
    :serial t
    :components ((:file "ask") (:file "motion") (:file "kill-ring")
                (:file "layout") (:file "completion")
                (:file "minibuffer") (:file "isearch") (:file "file")
                (:file "target") (:file "repl") (:file "overwrite")
                (:file "window") (:file "help") (:file "debugger")
                (:file "evaluate") (:file "commands") (:file "session")))
   (:file "desktop")
   (:file "wm")
   (:file "frontend")
   ;; main declares :pine, and the user vocabulary imports from it
   (:file "pine")
   (:file "user")))

(asdf:defsystem #:pine/test
  :description "The pine test suite."
  :depends-on (#:pine #:fiveam)
  :serial t
  :pathname "tests/"
  :components ((:file "suite") (:file "model") (:file "editor") (:file "agent")
               (:file "frontend") (:file "deps"))
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :pine.test :run-tests)
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
