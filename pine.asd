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
               #:closer-mop)
  :serial t
  :pathname "src/"
  :components
  ((:file "package")
   (:file "key")
   (:file "keymap")
   (:file "server")
   (:file "actor")
   (:file "event")
   (:file "hooks")
   (:file "face")
   (:file "buffer")
   (:file "window")
   (:file "client")
   (:file "echo")
   (:file "command")
   (:file "layout")
   (:file "mode")
   (:file "kill-ring")
   (:file "complete")
   (:file "prompt")
   (:file "file")
   (:file "shell")
   (:file "ts-stub")
   (:file "render")
   (:file "editor")
   (:file "user")
   (:file "main")))

(asdf:defsystem #:pine/gtk
  :depends-on (#:pine #:cl-gtk4 #:cl-gdk4 #:cl-cairo2)
  :serial t
  :pathname "src/"
  :components ((:file "surface")
               (:file "gtk")))
