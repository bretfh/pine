(defpackage #:pine/wayland/app/state
  (:use #:cl)
  (:export #:wm #:make-wm #:wm-display #:wm-manager #:wm-bindings-global
           #:wm-layer-shell #:wm-layer-focus #:wm-windows #:wm-outputs #:wm-seats
           #:wm-bindings #:wm-staged #:wm-rects #:wm-focus-id #:wm-border
           #:wm-staged-focus #:wm-dirty #:wm-pending #:wm-backing #:wm-pump
           #:wm-focus #:wm-ref #:wm-sys #:wm-done
           #:win #:make-win #:win-proxy #:win-node #:win-id #:win-title
           #:win-app-id #:win-width #:win-height #:win-hint #:win-framed
           #:out #:make-out #:out-proxy #:out-x #:out-y #:out-width #:out-height
           #:out-shell #:out-area
           #:rect #:find-win #:screen #:tiled))
(in-package #:pine/wayland/app/state)

(defstruct wm
  display
  manager
  bindings-global
  layer-shell
  (layer-focus :none)
  (windows nil)
  (outputs nil)
  (seats nil)
  (bindings nil)
  (staged nil)
  (rects nil)
  (focus-id nil)
  (border nil)
  (staged-focus nil)
  (dirty nil)
  (pending nil)
  backing
  pump
  (focus nil)
  (ref nil)
  (sys nil)
  (done nil))

(defstruct win
  proxy node id title app-id width height
  hint
  framed)

(defstruct out
  proxy x y width height
  shell
  area)

(defun rect (wm w)
  "(values X Y WIDTH HEIGHT) for W from the daemon's arrangement, or nil."
  (let ((entry (and (win-id w) (assoc (win-id w) (wm-rects wm) :test #'equal))))
    (when entry
      (values-list (rest entry)))))

(defun find-win (wm proxy)
  (find proxy (wm-windows wm) :key #'win-proxy :test #'eq))

(defun screen (wm)
  "The output windows tile onto: the first one announced."
  (first (last (wm-outputs wm))))

(defun tiled (wm)
  "Windows in tree order (oldest first), the order they tile in."
  (reverse (wm-windows wm)))
