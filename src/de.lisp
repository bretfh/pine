(defpackage #:pine.de
  (:use #:cl #:gtk4)
  (:export #:make-bar #:*bar-enabled*))

(in-package #:pine.de)

(defvar *bar-enabled* t)
(defvar *bar-client* nil)

;;;; gtk4-layer-shell — hand-bound. Turns a GtkWindow into an anchored desktop
;;;; surface. It must be loaded (LD_PRELOAD) before GTK inits its Wayland
;;;; backend or these calls are silent no-ops (see the make dev target).

(cffi:define-foreign-library libgtk4-layer-shell
  (t (:default "libgtk4-layer-shell")))

(defvar *ls-loaded* nil)
(defun ensure-layer-shell ()
  (unless *ls-loaded*
    (cffi:load-foreign-library 'libgtk4-layer-shell)
    (setf *ls-loaded* t)))

(cffi:defcfun ("gtk_layer_init_for_window" %init) :void (window :pointer))
(cffi:defcfun ("gtk_layer_set_layer" %set-layer) :void (window :pointer) (layer :int))
(cffi:defcfun ("gtk_layer_set_anchor" %set-anchor) :void
  (window :pointer) (edge :int) (anchor :boolean))
(cffi:defcfun ("gtk_layer_auto_exclusive_zone_enable" %auto-zone) :void (window :pointer))
(cffi:defcfun ("gtk_layer_set_namespace" %set-namespace) :void
  (window :pointer) (namespace :string))

;; layer: 0 background 1 bottom 2 top 3 overlay.  edge: 0 left 1 right 2 top 3 bottom.
(defun configure-bar (window)
  (ensure-layer-shell)
  (let ((p (gobject-introspection-wrapper:object-pointer window)))
    (%init p)
    (%set-namespace p "pine-bar")
    (%set-layer p 2)
    (%set-anchor p 0 t)
    (%set-anchor p 1 t)
    (%set-anchor p 2 t)
    (%auto-zone p)))


;;;; The bar is a reactive-view: its thunk builds a pine.layout tree that reads
;;;; cells (fed by pine.source), so a cell change re-renders it. Widget clicks
;;;; are in-process closures (pine.layout:action), hit-tested by column -- no
;;;; nREPL, no eww-update string protocol.

(defun %text (s) (make-instance 'pine.layout:text-node :content s))

(defun ws-node (w)
  "A clickable workspace glyph: focusing it runs niri in-process."
  (make-instance 'pine.layout:action
    :callback (let ((idx (getf w :idx)))
                (lambda ()
                  (ignore-errors
                    (uiop:launch-program
                     (list "niri" "msg" "action" "focus-workspace"
                           (princ-to-string idx))))))
    :child (%text (format nil "~a~a" (getf w :idx) (if (getf w :focused) "*" "")))))

(defun %cell (name default)
  (let ((c (pine.cell:find-cell name))) (if c (pine.cell:cell-ref c) default)))

(defun bar-root (cli)
  (let* ((label (%cell :bar-label "pine"))
         (wss (%cell :workspaces nil))
         (buf (and cli (pine.client:current-buffer cli)))
         (name (or (and buf (ignore-errors (pine.buffer:ask buf :name))) "scratch"))
         (clock (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
                  (declare (ignore s))
                  (format nil "~2,'0d:~2,'0d" h m))))
    (make-instance 'pine.layout:hstack :spacing 2
      :children (append (list (%text label) (%text "|"))
                        (mapcar #'ws-node wss)
                        (list (%text "|") (%text name) (%text "|") (%text clock))))))

(defparameter *bar-cols* 200)
(defvar *bar-view* nil)
(defvar *bar-root* nil)
(defvar *bar-line* "")

(defun build-bar-line (cli)
  "The view thunk: build + render the bar tree, keep the root for hit-testing,
return the text line."
  (let* ((root (bar-root cli))
         (lay (make-instance 'pine.layout:layout :root root :width *bar-cols*)))
    (setf *bar-root* root
          *bar-line* (or (first (pine.layout:render-layout lay)) ""))))

(defun click-callback-at (node col)
  "Callback of the pine.layout:action whose column span contains COL, or nil."
  (typecase node
    (pine.layout:action
     (if (and (= 0 (pine.layout:start-line node))
              (<= (pine.layout:start-col node) col)
              (< col (pine.layout:end-col node)))
         (pine.layout:callback node)
         nil))
    ((or pine.layout:hstack pine.layout:vstack)
     (some (lambda (c) (click-callback-at c col)) (pine.layout:children node)))
    (t nil)))


;;;; Drawing + input

(defparameter *bar-cell-w* 9d0)   ; measured from the font on first paint
(defparameter *bar-font* 14d0)
(defparameter *bar-ascent* 15d0)
(defparameter *bar-x0* 8d0)
(defvar *bar-measured* nil)

(cffi:defcallback %bar-draw :void ((area :pointer) (cr :pointer)
                                   (width :int) (height :int) (data :pointer))
  (declare (ignore area data))
  (let ((cairo:*context* (make-instance 'cairo:context :pointer cr
                                        :width width :height height :pixel-based-p nil)))
    (cairo:set-source-rgb 0.09d0 0.09d0 0.14d0)
    (cairo:paint)
    (cairo:select-font-face "monospace" :normal :normal)
    (cairo:set-font-size *bar-font*)
    ;; hit-testing must use the font's real advance, not a guess, or click
    ;; columns drift off the rendered glyphs.
    (unless *bar-measured*
      (multiple-value-bind (xb yb w h xadv) (cairo:text-extents "MMMMMMMMMM")
        (declare (ignore xb yb w h))
        (when (plusp xadv) (setf *bar-cell-w* (/ xadv 10d0))))
      (let ((fe (cairo:get-font-extents)))
        (setf *bar-ascent* (cairo:font-ascent fe)))
      (setf *bar-measured* t))
    (setf *bar-cols* (max 10 (floor width *bar-cell-w*)))
    (let ((line (if *bar-view* (pine.cell:render-view *bar-view*) (build-bar-line *bar-client*))))
      (cairo:set-source-rgb 0.80d0 0.84d0 0.96d0)
      (cairo:move-to *bar-x0* *bar-ascent*)
      (cairo:show-text line))))

(defun on-bar-click (x)
  ;; run the handler off the UI thread through pine.eval, so a slow or broken
  ;; widget onclick can't hang or crash the bar (or the editor sharing the loop).
  (let* ((col (floor (- x *bar-x0*) *bar-cell-w*))
         (cb (and *bar-root* (click-callback-at *bar-root* col))))
    (when cb (pine.eval:evaluate-thunk cb :package (find-package :pine-user)))))

(defun make-bar (app cli)
  (setf *bar-client* cli)
  (pine.cell:defcell :bar-label "pine")
  (ignore-errors
   (pine.source:start-niri-source
    (pine.server:actor-system (pine.client:server-of cli))))
  (let ((window (make-application-window :application app))
        (area (make-drawing-area)))
    (setf *bar-view*
          (pine.cell:make-view
           (lambda () (build-bar-line cli))
           (lambda () (run-in-main-event-loop () (widget-queue-draw area)))))
    (setf (drawing-area-content-height area) 22
          (drawing-area-draw-func area) (list (cffi:callback %bar-draw)
                                              (cffi:null-pointer) (cffi:null-pointer))
          (window-child window) area)
    (let ((click (make-gesture-click)))
      (connect click "pressed"
               (lambda (gesture n-press x y)
                 (declare (ignore gesture n-press y))
                 (on-bar-click x)))
      (widget-add-controller area click))
    (configure-bar window)
    (window-present window)
    (timeout-add 1000 (lambda () (widget-queue-draw area) t))
    window))
