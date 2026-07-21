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
   (:module "session"
    :serial t
    :components ((:file "client")))
   (:module "keymap"
    :serial t
    :components ((:file "key") (:file "keymap") (:file "command") (:file "mode")
                (:file "variable")))
   (:module "widget"
    :serial t
    :components ((:file "cell") (:file "sources") (:file "layout")))
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
    :components ((:file "echo") (:file "kill-ring") (:file "complete")
                (:file "prompt") (:file "isearch") (:file "file") (:file "repl")
                (:file "editor") (:file "editor-session")))
   (:module "desktop"
    :serial t
    :components ((:file "desktop") (:file "palette")))
   (:file "user")
   (:file "main")))

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
  :description "GTK-free cairo backend for the widget engine: theme-rules as a
style resolver + a paint-cairo pass rendering the layout tree to a cairo context."
  :depends-on (#:pine #:cl-cairo2)
  :serial t
  :pathname "src/cairo/"
  :components ((:file "cell") (:file "style") (:file "paint") (:file "shot")))

(asdf:defsystem #:pine/wayland
  :description "GTK-free wayland client: wlr-layer-shell bindings + shm/cairo
surfaces painting the pine.layout tree through the :pine/cairo backend. Uses the
wayflan library for the raw protocol."
  :depends-on (#:pine/cairo #:wayflan-client #:posix-shm #:cl-xkb)
  :serial t
  :pathname "src/wayland/"
  :components ((:file "layer-shell") (:file "surface") (:file "input") (:file "client")
               (:file "editor") (:file "editor-keys")))

(asdf:defsystem #:pine/gtk
  :depends-on (#:pine #:pine/cairo #:cl-gtk4 #:cl-gdk4 #:cl-cairo2)
  :serial t
  :pathname "src/"
  :components
  ((:module "app"
    :serial t
    :components ((:file "paint") (:file "desktop-app") (:file "editor-app")))))
