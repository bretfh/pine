(in-package #:pine/wayland/protocol)

(xyz.shunter.wayflan.client.scanner:wl-include
  (merge-pathnames "share/river-protocols/stable/river-xkb-bindings-v1.xml"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "GUIX_ENVIRONMENT")
                        (error "GUIX_ENVIRONMENT unset: build inside guix shell -m manifest.scm"))))
  :export t)
