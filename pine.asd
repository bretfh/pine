;;;; pine system definition.
;;;;
;;;; src/ is copied from hecl and still uses (in-package :hecl.*).  It loads
;;;; once Phase 1 renames the packages hecl.* -> pine.*.  The GUI/FFI files
;;;; are commented until their rework (see README Port plan):
;;;;   render.lisp  QML       -> GtkDrawingArea draw callback
;;;;   ts.lisp      ECL FFI   -> cl-tree-sitter
;;;;   term.lisp    ghostty   -> pine.vt (vt/, hemlock.term)
;;;;   bridge.lisp  dropped   (QML only)

(asdf:defsystem #:pine
  :description "pine - a parallel Lisp computing interface (Pine Is Not Emacs)"
  :author "Bret Horne"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:sento
               #:sento-remoting
               #:fset
               #:alexandria
               #:bordeaux-threads)
  :serial t
  :pathname "src/"
  :components
  ((:file "package")
   (:file "server")
   (:file "actor")
   (:file "event")
   (:file "hooks")
   (:file "face")
   (:file "buffer")
   (:file "window")
   (:file "client")
   (:file "layout")
   (:file "mode")
   (:file "keymap")
   (:file "kill-ring")
   (:file "complete")
   (:file "prompt")
   (:file "file")
   (:file "shell")
   (:file "editor")
   (:file "user")
   (:file "main")
   ;; --- reworked for SBCL+GTK before these load (see README): ---
   ;; (:file "render")   ; -> GtkDrawingArea
   ;; (:file "ts")       ; -> cl-tree-sitter
   ;; (:file "term")     ; -> pine.vt
   ))
