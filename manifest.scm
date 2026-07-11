;;; pine native toolchain for Guix.  guix shell -m manifest.scm
;;; CL libraries come via ocicl; this manifest is only the impl and C deps.
;;; gtk4-layer-shell is left out until the DE phase.

(specifications->manifest
 (list "sbcl"
       "gtk"                    ; GTK 4.x in Guix (gtk+ is the old 2/3 line)
       "gobject-introspection"  ; cl-gtk4 reads .typelib at load
       "tree-sitter"
       "gcc-toolchain"
       "pkg-config"
       "gnu-make"
       "git"
       ;; "gtk4-layer-shell"    ; DE phase: bars/panels/overlays
       ))
