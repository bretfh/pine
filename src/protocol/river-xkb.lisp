;;;; river_xkb_bindings_v1: compositor-side keyboard chords delivered to
;;;; the window manager.

(in-package #:pine.protocol)

(xyz.shunter.wayflan.client.scanner:wl-include
  (merge-pathnames "share/river-protocols/stable/river-xkb-bindings-v1.xml"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "GUIX_ENVIRONMENT")
                        (error "GUIX_ENVIRONMENT unset: build inside guix shell -m manifest.scm"))))
  :export t)
