(defpackage #:pine/wayland/protocol
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell))
(in-package #:pine/wayland/protocol)

(xyz.shunter.wayflan.client.scanner:wl-include
  (asdf:system-relative-pathname
   :pine "protocol/wlr-layer-shell-unstable-v1.xml")
  :export t)
