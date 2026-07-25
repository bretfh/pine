;;;; river xkb bindings, generated like river-wm.lisp. A separate file
;;;; because the scanner references river_seat_v1 from the window management
;;;; protocol: pine.river-wm must be loaded -- its exports established --
;;;; before this package's forms are read.

(defpackage #:pine.river-xkb
  (:use #:cl #:wayflan-client #:pine.river-wm)
  (:documentation "river_xkb_bindings_v1 client bindings: compositor-side
keyboard chords delivered to the window manager."))
(in-package #:pine.river-xkb)

(xyz.shunter.wayflan.client.scanner:wl-include
  (merge-pathnames "share/river-protocols/stable/river-xkb-bindings-v1.xml"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "GUIX_ENVIRONMENT")
                        (error "GUIX_ENVIRONMENT unset: build inside guix shell -m manifest.scm"))))
  :export t)
