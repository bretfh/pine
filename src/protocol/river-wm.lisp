;;;; river_window_manager_v1: the window management state machine, its
;;;; manage and render sequences, windows, outputs, seats, nodes,
;;;; decorations and shell surfaces. design/wm.org is the contract.
;;;;
;;;; Read before the protocols that reference river_output_v1 and
;;;; river_seat_v1, which the scanner interns as it meets them.

(in-package #:pine.protocol)

(xyz.shunter.wayflan.client.scanner:wl-include
  (merge-pathnames "share/river-protocols/stable/river-window-management-v1.xml"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "GUIX_ENVIRONMENT")
                        (error "GUIX_ENVIRONMENT unset: build inside guix shell -m manifest.scm"))))
  :export t)

