(in-package #:pine.de)

;;;; The desktop's surfaces. Per the architecture: many surfaces, one substrate.
;;;; A <layer-shell-surface> is a pine.surface:surface, the same CLOS protocol
;;;; the editor's <gtk-surface> implements -- present / request-redraw /
;;;; surface-metrics -- so the bar, each panel, and the echo strip are surfaces
;;;; on the one core, not a parallel app. Each wraps a gtk4-layer-shell window
;;;; whose GtkDrawingArea paints a reactive pine.layout tree through the cairo
;;;; painter. Nothing here runs a widget's code on the main thread without a
;;;; guard, so a broken widget reports to the echo area rather than dropping the
;;;; whole image (editor included) into the debugger.

(defvar *bar-enabled* t)
(defvar *desktop-client* nil)

(defmacro %guard (what &body body)
  "Run BODY; on any error report it to the echo area and continue. WHAT names the
site. The seam that keeps a DE fault off the main loop."
  `(handler-case (progn ,@body)
     (error (e)
       (ignore-errors (pine.echo:message (format nil "de ~a: ~a" ,what e)))
       nil)))

;;;; Transparency. GTK paints the window's own (opaque) theme background before
;;;; the drawing area, so a translucent cairo fill would show through onto grey,
;;;; not the compositor. One CSS provider on the display makes window +
;;;; drawing-area backgrounds transparent, letting the blur be the glass.

(defvar *css-installed* nil)
(defun ensure-transparent-bg ()
  (unless *css-installed*
    (let ((provider (make-css-provider)))
      (css-provider-load-from-data
       provider "window, .background, drawingarea {
                   background: transparent; background-color: transparent; }")
      (style-context-add-provider-for-display (gdk4:display-default) provider 800)
      (setf *css-installed* t))))

;;;; gtk4-layer-shell, hand-bound. Turns a GtkWindow into an anchored surface.
;;;; Must be loaded (LD_PRELOAD) before GTK inits Wayland or the calls no-op.

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
(defun configure-window (window kind)
  "Anchor WINDOW per KIND: :bar full-height left strip, :echo full-width bottom
strip, :panel bottom-left overlay."
  (ensure-layer-shell)
  (let ((p (gobject-introspection-wrapper:object-pointer window)))
    (%init p)
    (ecase kind
      (:bar   (%set-namespace p "pine-bar")   (%set-layer p 2)
              (%set-anchor p 0 t) (%set-anchor p 2 t) (%set-anchor p 3 t) (%auto-zone p))
      (:echo  (%set-namespace p "pine-echo")  (%set-layer p 2)
              (%set-anchor p 0 t) (%set-anchor p 1 t) (%set-anchor p 3 t) (%auto-zone p))
      (:panel (%set-namespace p "pine-panel") (%set-layer p 3)
              (%set-anchor p 0 t) (%set-anchor p 3 t)))))

;;;; Reactive + shell helpers a widget uses.

(defun cell (name default)
  "Read reactive cell NAME, DEFAULT if it has no value yet."
  (let ((c (pine.cell:find-cell name))) (if c (pine.cell:cell-ref c) default)))

(defun launch (&rest args)
  "Launch a program off the UI thread, errors swallowed."
  (ignore-errors (uiop:launch-program args)))

(defun sh (cmd)
  "A click thunk that runs shell CMD off the UI thread."
  (lambda () (ignore-errors (uiop:launch-program (list "sh" "-c" cmd)))))

(defun toggle (name)
  "A click thunk that toggles panel NAME on the UI thread."
  (lambda () (run-in-main-event-loop () (toggle-panel name))))

;;;; The surface. A pine.surface:surface subclass; KIND selects chrome and the
;;;; arrange origin, BUILDER is the () -> pine.layout tree it renders reactively.

(defclass layer-shell-surface (pine.surface:surface)
  ((name    :initarg :name    :accessor ls-name)
   (kind    :initarg :kind    :accessor ls-kind)
   (window  :initarg :window  :accessor ls-window)
   (area    :initarg :area    :accessor ls-area)
   (builder :initarg :builder :accessor ls-builder)
   (view    :accessor ls-view    :initform nil)
   (root    :accessor ls-root    :initform nil)
   (hover   :accessor ls-hover   :initform nil)   ; (y . x) pixel, or nil
   (font-px :initarg :font-px :accessor ls-font-px :initform 13)
   (width   :accessor ls-width   :initform 0)     ; last drawn pixel size
   (height  :accessor ls-height  :initform 0)
   (visible :accessor ls-visible :initform t)))

(defmethod pine.surface:present ((s layer-shell-surface))
  (window-present (ls-window s))
  (setf (ls-visible s) t))

(defmethod pine.surface:request-redraw ((s layer-shell-surface))
  (let ((area (ls-area s)))
    (when area (run-in-main-event-loop () (widget-queue-draw area)))))

(defmethod pine.surface:surface-metrics ((s layer-shell-surface))
  ;; the DE paints a pixel layout tree, not a cell grid; report the pixel size
  ;; as 1x1 cells so the protocol still has an answer.
  (values 1 1 (ls-width s) (ls-height s)))

(defvar *surfaces* (make-hash-table)
  "drawing-area pointer address -> <layer-shell-surface>.")
(defvar *panels* (make-hash-table :test 'equal)
  "panel name -> <layer-shell-surface>.")

(defun surface-for-area (area) (gethash (cffi:pointer-address area) *surfaces*))

(defun surface-build (s)
  "Build S's widget tree (tracking the cells it reads) and store it."
  (setf (ls-root s) (funcall (ls-builder s))))

(defun render-surface (s width height)
  "Chrome for S's kind, then build + lay out + paint its tree in pixels."
  (setf (ls-width s) width (ls-height s) height)
  (ecase (ls-kind s)
    ((:bar :panel)
     (multiple-value-bind (r g b) (%rgb *chrome-bg*)
       (cairo:set-source-rgba r g b *bg-alpha*))
     (let ((in (if (eq (ls-kind s) :bar) 3d0 4d0)))
       (%rounded-rect in in (- width (* 2 in)) (- height (* 2 in)) 12d0))
     (cairo:fill-path))
    (:echo
     (multiple-value-bind (r g b) (%rgb *chrome-bg*)
       (cairo:set-source-rgba r g b *bg-alpha*))
     (cairo:paint)))
  (cairo:select-font-face pine.surface:*font-family* :normal :normal)
  (if (ls-view s) (pine.cell:render-view (ls-view s)) (surface-build s))
  (let ((pine.layout:*text-size* #'%cairo-text-size)
        (pine.layout:*default-font-px* (ls-font-px s))
        (root (ls-root s)))
    (when root
      (ecase (ls-kind s)
        (:bar
         (pine.layout:measure root (- width 8) height)
         (pine.layout:arrange root 4d0 4d0
                              (coerce (- width 8) 'double-float)
                              (coerce (- height 8) 'double-float)))
        (:panel
         (multiple-value-bind (mw mh) (pine.layout:measure root (- width 16) 100000)
           (declare (ignore mw))
           (pine.layout:arrange root 8d0 8d0
                                (coerce (- width 16) 'double-float)
                                (coerce mh 'double-float))))
        (:echo
         (pine.layout:measure root width height)
         (pine.layout:arrange root 64d0 0d0
                              (coerce (- width 64) 'double-float)
                              (coerce height 'double-float))))
      (when (ls-hover s)
        (let ((n (pine.layout:node-at root (car (ls-hover s)) (cdr (ls-hover s)))))
          (when n (setf (pine.layout:hovered n) t))))
      (paint-cairo root))))

(cffi:defcallback %ls-draw :void ((area :pointer) (cr :pointer)
                                  (width :int) (height :int) (data :pointer))
  (declare (ignore data))
  (%guard "draw"
    (let ((s (surface-for-area area)))
      (when s
        (let ((cairo:*context* (make-instance 'cairo:context :pointer cr
                                              :width width :height height
                                              :pixel-based-p nil)))
          (render-surface s width height))))))

(defvar *local-agent* nil
  "The local agent's info, resolved once at desktop start. Clients tell it a
thunk job directly; the click path must never do a registry ask on the UI
thread (that blocks GTK up to the ask timeout).")

(defun on-surface-click (s x y)
  ;; look up the widget's thunk (guarded), then TELL the local agent (async, no
  ;; ask) to run it through pine.eval off its mailbox thread -- one addressable
  ;; eval path (agent-eval :local) that never blocks the UI thread.
  (%guard "click"
    (let ((thunk (and (ls-root s)
                      (pine.layout:click-thunk (ls-root s) (round y) (round x)))))
      (when thunk
        (if *local-agent*
            (pine.actor:agent-run nil *local-agent* thunk :package :pine-user)
            ;; no agent resolved yet: evaluate-thunk is itself non-blocking
            ;; (spawns the eval thread) so this fallback can't hang either.
            (pine.eval:evaluate-thunk thunk :package (find-package :pine-user)))))))

(defun on-surface-motion (s x y)
  (%guard "motion"
    (let ((pos (cons (round y) (round x))))
      (when (eq (ls-kind s) :bar)
        (pine.source:set! :hint
                          (or (and (ls-root s)
                                   (pine.layout:hint-at (ls-root s) (round y) (round x)))
                              "")))
      (unless (equal pos (ls-hover s))
        (setf (ls-hover s) pos)
        (widget-queue-draw (ls-area s))))))

(defun on-surface-leave (s)
  (%guard "leave"
    (setf (ls-hover s) nil)
    (when (eq (ls-kind s) :bar) (pine.source:set! :hint ""))
    (widget-queue-draw (ls-area s))))

(defun make-surface (app &key name kind builder width height (font-px 13))
  "Create (but do not present) a layer-shell surface: window + drawing area wired
to the shared draw/click/motion path, its reactive view tracking the cells its
BUILDER reads."
  (let* ((window (make-application-window :application app))
         (area (make-drawing-area))
         (s (make-instance 'layer-shell-surface :name name :kind kind
                           :builder builder :window window :area area :font-px font-px)))
    (when width  (setf (drawing-area-content-width area) width))
    (when height (setf (drawing-area-content-height area) height))
    (setf (drawing-area-draw-func area)
          (list (cffi:callback %ls-draw) (cffi:null-pointer) (cffi:null-pointer))
          (window-child window) area)
    (setf (gethash (cffi:pointer-address
                    (gobject-introspection-wrapper:object-pointer area))
                   *surfaces*)
          s)
    (setf (ls-view s)
          (pine.cell:make-view (lambda () (surface-build s))
                               (lambda () (pine.surface:request-redraw s))))
    (let ((click (make-gesture-click)))
      (connect click "pressed"
               (lambda (g n x y) (declare (ignore g n)) (on-surface-click s x y)))
      (widget-add-controller area click))
    (let ((motion (make-event-controller-motion)))
      (connect motion "motion" (lambda (c x y) (declare (ignore c)) (on-surface-motion s x y)))
      (connect motion "leave"  (lambda (c) (declare (ignore c)) (on-surface-leave s)))
      (widget-add-controller area motion))
    (configure-window window kind)
    s))

;;;; The registry: a config declares the bar builder and its panels; start-desktop
;;;; realises them once it has the GtkApplication.

(defvar *bar-builder* nil)
(defvar *bar-width* 44)
(defun set-bar! (builder &key (width 44))
  "Register BUILDER (a thunk -> node tree) as the bar's content."
  (setf *bar-builder* builder *bar-width* width))

(defvar *panel-specs* nil "list of (name builder width height), last wins.")
(defun defpanel (name builder &key (width 400) (height 320))
  "Register an overlay panel NAME built by BUILDER, shown with (toggle NAME)."
  (setf *panel-specs*
        (cons (list name builder width height)
              (remove name *panel-specs* :key #'first :test #'equal))))

(defun toggle-panel (name)
  "Show panel NAME, hiding any other open panel. Runs on the UI thread."
  (%guard "toggle"
    (let ((p (gethash name *panels*)))
      (when p
        (if (ls-visible p)
            (setf (widget-visible-p (ls-window p)) nil (ls-visible p) nil)
            (progn
              (maphash (lambda (k q)
                         (declare (ignore k))
                         (when (and (ls-visible q) (not (eq q p)))
                           (setf (widget-visible-p (ls-window q)) nil (ls-visible q) nil)))
                       *panels*)
              (pine.surface:present p)))))))

;;;; The echo strip: a bottom full-width surface showing the :hint cell (set by
;;;; bar hover). Part of the framework -- generic across any config.

(defun echo-builder ()
  (row :pad-x 12 :pad-y 4
    (label (cell :hint "") :face :comment :font-px 13)))

(defvar *bar-surface* nil)

(defun start-desktop (app cli)
  "Realise the registered desktop on APP for client CLI: transparency, data
sources, the bar, its panels, and the echo strip."
  (setf *desktop-client* cli)
  (ensure-transparent-bg)
  ;; resolve the local agent once here (a single startup ask, off any hot path)
  ;; so the click path only ever tells it -- never asks the registry on the UI thread.
  (setf *local-agent*
        (ignore-errors (pine.actor:find-agent (pine.client:server-of cli) "local")))
  (ignore-errors
   (pine.source:start-sources (pine.client:server-of cli)))
  (setf pine.layout:*hover-face* :hover)
  (pine.cell:defcell :hint "")
  (when *bar-builder*
    (let ((bar (make-surface app :name "bar" :kind :bar
                             :builder *bar-builder* :width *bar-width* :font-px 15)))
      (setf *bar-surface* bar)
      (pine.surface:present bar)))
  (dolist (spec *panel-specs*)
    (destructuring-bind (name builder width height) spec
      (let ((p (make-surface app :name name :kind :panel :builder builder
                             :width width :height height :font-px 13)))
        (setf (ls-visible p) nil (gethash name *panels*) p))))
  (let ((echo (make-surface app :name "echo" :kind :echo
                            :builder #'echo-builder :height 22 :font-px 13)))
    (pine.surface:present echo))
  ;; the clock updates each minute; a slow tick keeps it and any polled cell fresh.
  (when *bar-surface*
    (timeout-add 1000 (lambda () (widget-queue-draw (ls-area *bar-surface*)) t)))
  *bar-surface*)

;;;; The reusable widget vocabulary (eww's nm-card / nm-head shapes). A card is a
;;;; rounded padded box; a header is an accent icon plus a title and status line;
;;;; a pill is a rounded padded hover-highlighted button. A config composes these.

(defun card (&rest nodes)
  "A rounded, padded content box."
  (apply #'column :radius 8 :fill "#322f34" :pad 12 :spacing 6 nodes))

(defun header (glyph title sub &optional (sub-face :string))
  "A card header: an accent glyph, a title, and a status line."
  (card
   (row :spacing 14 :align :center
     (icon glyph :face :accent :font-px 22)
     (column :spacing 2
       (label title :face :default :font-px 17)
       (label sub :face sub-face :font-px 13)))))

(defun pill (&rest args)
  "A rounded, padded, hover-highlighted button (a list row / action button)."
  (apply #'button :pad-x 12 :pad-y 10 :radius 8 args))
