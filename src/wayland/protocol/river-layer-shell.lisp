;;;; river_layer_shell_v1: how a window manager tells river it supports
;;;; layer surfaces. River closes every layer surface immediately while no
;;;; manager has bound this.

(in-package #:pine.wayland.protocol)

(xyz.shunter.wayflan.client.scanner:wl-include
  (merge-pathnames "share/river-protocols/stable/river-layer-shell-v1.xml"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "GUIX_ENVIRONMENT")
                        (error "GUIX_ENVIRONMENT unset: build inside guix shell -m manifest.scm"))))
  :export t)
