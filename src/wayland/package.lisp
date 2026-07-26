;;;; The wayland backing's packages. They cannot live with the rest of pine's
;;;; because they rest on wayflan, which only this system loads.

(defpackage #:pine.protocol
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:documentation "Wayland protocol bindings, generated from the XML the
compositor ships: wlr layer shell, and river's window management, xkb bindings
and layer shell."))

(defpackage #:pine.wayland
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell #:pine.protocol)
  (:local-nicknames (#:a #:alexandria) (#:c #:cl-cairo2) (#:shm #:posix-shm)
                    (#:node #:pine.ui.node) (#:lay #:pine.ui.layout)
                    (#:uiw #:pine.ui.wire) (#:paint #:pine.cairo.paint)
                    (#:ev #:pine.core.eval) (#:wire #:xyz.shunter.wayflan.wire))
  (:export #:run-desktop #:run-editor #:run-wm #:*keyboard-handler*)
  (:documentation "The wayland backing: the compositor connection and its event
pump, shm and cairo surfaces, pointer input, and the three programs pine runs
as wayland clients."))
