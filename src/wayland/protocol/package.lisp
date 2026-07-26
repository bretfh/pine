;;;; The wayland backing's packages. They cannot live with the rest of pine's
;;;; because they rest on wayflan, which only this system loads.

(defpackage #:pine.wayland.protocol
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:documentation "Wayland protocol bindings, generated from the XML the
compositor ships: wlr layer shell, and river's window management, xkb bindings
and layer shell."))
