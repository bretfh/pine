(defpackage #:pine/wayland/protocol
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:documentation "Wayland protocol bindings, generated from the XML the
compositor ships: wlr layer shell, and river's window management, xkb bindings
and layer shell."))
(in-package #:pine/wayland/protocol)

(xyz.shunter.wayflan.client.scanner:wl-include
  (merge-pathnames "share/wlr-protocols/unstable/wlr-layer-shell-unstable-v1.xml"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "GUIX_ENVIRONMENT")
                        (error "GUIX_ENVIRONMENT unset: build inside guix shell -m manifest.scm"))))
  :export t)
