(defpackage #:pine.de
  (:use #:cl #:gtk4)
  (:shadowing-import-from #:pine.layout
                #:defwidget #:column #:row #:label #:icon #:button #:boxed
                #:centered #:gap #:rule #:meter #:rows #:choice)
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
    (%set-anchor p 0 t)   ; left
    (%set-anchor p 2 t)   ; top
    (%set-anchor p 3 t)   ; bottom  -> a full-height strip on the left edge
    (%auto-zone p)))


;;;; The bar is a reactive-view: its thunk builds a pine.layout tree that reads
;;;; cells (fed by pine.source), so a cell change re-renders it. Widget clicks
;;;; are in-process closures (pine.layout:action), hit-tested by column -- no
;;;; nREPL, no eww-update string protocol.

(defun %cell (name default)
  (let ((c (pine.cell:find-cell name))) (if c (pine.cell:cell-ref c) default)))

(defun %sh (cmd)
  "A thunk that runs shell CMD off the UI thread (safe from a click handler)."
  (lambda () (ignore-errors (uiop:launch-program (list "sh" "-c" cmd)))))

(defun %toggle (name)
  "A thunk that toggles panel NAME on the UI thread."
  (lambda () (run-in-main-event-loop () (toggle-panel name))))

;; nerd-font glyph codepoints (numeric so the source stays ASCII)
(defparameter *g-overview* #xF02C1)
(defparameter *g-search*   #x0F002)
(defparameter *g-apps*     #xF003B)
(defparameter *g-term*     #x0F120)
(defparameter *g-web*      #x0F268)
(defparameter *g-files*    #x0F07B)
(defparameter *g-edit*     #x0F121)
(defparameter *g-vol*      #x0F028)
(defparameter *g-media*    #x0F001)
(defparameter *g-net*      #x0F1EB)
(defparameter *g-system*   #x0F007)

;;;; The sidebar, declared as composable widgets. Each defwidget is a component
;;;; (like an eww defwidget); it reads cells, so a cell change re-renders it.

(defwidget ws-item (w)
  (icon (princ-to-string (getf w :idx))
        :on-click (%sh (format nil "niri msg action focus-workspace ~a" (getf w :idx)))
        :face (cond ((getf w :urgent)  :constant)
                    ((getf w :focused) :function-name)
                    (t                 :comment))))

(defwidget sidebar-top ()
  (column :align :center :spacing 1
    (icon *g-overview* :on-click (%sh "niri msg action toggle-overview") :face :comment)
    (icon *g-search*   :on-click (%sh "cd ~ && setsid -f bb ~/.config/eww/niri-window-switch.bb")
                       :face :comment)
    (mapcar #'ws-item (%cell :workspaces nil))))

(defwidget sidebar-apps ()
  (column :align :center :spacing 1
    (icon *g-apps*  :on-click (%sh "setsid -f fuzzel"))
    (icon *g-term*  :on-click (%sh "setsid -f alacritty"))
    (icon *g-web*   :on-click (%sh "setsid -f google-chrome"))
    (icon *g-files* :on-click (%sh "setsid -f nautilus"))
    (icon *g-edit*  :on-click (%sh "cd ~ && emacsclient -c -n"))))

(defwidget sidebar-tray ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (column :align :center :spacing 1
      (icon *g-vol*    :on-click (%toggle "audio")   :face :string)
      (icon *g-media*  :on-click (%toggle "media")   :face :string)
      (icon *g-net*    :on-click (%toggle "network") :face :string)
      (label (format nil "~2,'0d" h) :face :variable-param)
      (label (format nil "~2,'0d" m) :face :comment)
      (icon *g-system* :on-click (%toggle "calendar") :face :function-name))))

(defwidget sidebar ()
  (column :align :center
    (sidebar-top)
    (gap)
    (sidebar-apps)
    (gap)
    (sidebar-tray)))

(defparameter *bar-cols* 4)
(defparameter *bar-rows* 40)
(defvar *bar-view* nil)
(defvar *bar-root* nil)
(defvar *bar-cells* #())
(defvar *bar-count* 0)

(defun build-bar-cells (cli)
  "The view thunk: build + render the sidebar to a styled cell grid at the
surface's full height, keeping the root for hit-testing. Returns the cells."
  (declare (ignore cli))
  (let* ((root (sidebar))
         (lay (make-instance 'pine.layout:layout :root root
                             :width *bar-cols* :height *bar-rows*)))
    (setf *bar-root* root)
    (multiple-value-bind (lines cells n) (pine.layout:render-layout-grid lay)
      (declare (ignore lines))
      (setf *bar-cells* cells *bar-count* n)
      cells)))



;;;; Drawing + input

(defparameter *bar-cell-w* 9d0)   ; measured from the font on first paint
(defparameter *bar-cell-h* 20d0)
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
        (setf *bar-ascent* (cairo:font-ascent fe)
              *bar-cell-h* (cairo:font-height fe)))
      (setf *bar-measured* t))
    (setf *bar-cols* (max 1 (floor width *bar-cell-w*))
          *bar-rows* (max 1 (floor height *bar-cell-h*)))
    ;; render-view runs the thunk (build-bar-cells) with cell tracking, so the
    ;; bar re-subscribes to exactly the cells it read and redraws on change.
    (if *bar-view* (pine.cell:render-view *bar-view*) (build-bar-cells *bar-client*))
    (pine.surface:paint-cell-grid *bar-cells* *bar-count*
                                  *bar-cell-w* *bar-cell-h* *bar-ascent* *bar-x0*)))

(defun on-bar-click (x y)
  ;; run the handler off the UI thread through pine.eval, so a slow or broken
  ;; widget onclick can't hang or crash the bar (or the editor sharing the loop).
  (let* ((col (floor (- x *bar-x0*) *bar-cell-w*))
         (line (floor y *bar-cell-h*))
         (thunk (and *bar-root* (pine.layout:click-thunk *bar-root* line col))))
    (when thunk (pine.eval:evaluate-thunk thunk :package (find-package :pine-user)))))

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
           (lambda () (build-bar-cells cli))
           (lambda () (run-in-main-event-loop () (widget-queue-draw area)))))
    (setf (drawing-area-content-width area) 36   ; narrow sidebar; height stretches
          (drawing-area-draw-func area) (list (cffi:callback %bar-draw)
                                              (cffi:null-pointer) (cffi:null-pointer))
          (window-child window) area)
    (let ((click (make-gesture-click)))
      (connect click "pressed"
               (lambda (gesture n-press x y)
                 (declare (ignore gesture n-press))
                 (on-bar-click x y)))
      (widget-add-controller area click))
    (configure-bar window)
    (register-panel app "calendar" #'calendar-panel :width 240 :height 120)
    (window-present window)
    (timeout-add 1000 (lambda () (widget-queue-draw area) t))
    window))


;;;; Panels — layer-shell overlay surfaces rendering a widget tree to cells.
;;;; Each is a second surface on the same substrate: a builder thunk -> node
;;;; tree -> cells, painted by the shared painter, clicks through pine.eval. One
;;;; panel shows at a time; toggling one closes the others.

(defstruct panel name builder window area view root (cells #()) (count 0)
                 (width 400) (height 320) (visible nil))

(defvar *panels* (make-hash-table :test 'equal))
(defvar *panels-by-area* (make-hash-table))

(defun configure-panel (window)
  "Anchor WINDOW as a bottom-left overlay layer-shell surface."
  (ensure-layer-shell)
  (let ((p (gobject-introspection-wrapper:object-pointer window)))
    (%init p)
    (%set-namespace p "pine-panel")
    (%set-layer p 3)          ; overlay
    (%set-anchor p 0 t)       ; left
    (%set-anchor p 3 t)))     ; bottom

(defun build-panel-cells (panel)
  (let* ((root (funcall (panel-builder panel)))
         (cols (max 10 (floor (panel-width panel) *bar-cell-w*)))
         (lay (make-instance 'pine.layout:layout :root root :width cols)))
    (setf (panel-root panel) root)
    (multiple-value-bind (lines cells n) (pine.layout:render-layout-grid lay)
      (declare (ignore lines))
      (setf (panel-cells panel) cells (panel-count panel) n)
      cells)))

(defun %ensure-metrics ()
  (unless *bar-measured*
    (multiple-value-bind (xb yb w h xadv) (cairo:text-extents "MMMMMMMMMM")
      (declare (ignore xb yb w h))
      (when (plusp xadv) (setf *bar-cell-w* (/ xadv 10d0))))
    (let ((fe (cairo:get-font-extents)))
      (setf *bar-ascent* (cairo:font-ascent fe)
            *bar-cell-h* (cairo:font-height fe)))
    (setf *bar-measured* t)))

(cffi:defcallback %panel-draw :void ((area :pointer) (cr :pointer)
                                     (width :int) (height :int) (data :pointer))
  (declare (ignore data))
  (let ((panel (gethash (cffi:pointer-address area) *panels-by-area*)))
    (when panel
      (let ((cairo:*context* (make-instance 'cairo:context :pointer cr
                                            :width width :height height :pixel-based-p nil)))
        (cairo:set-source-rgb 0.10d0 0.10d0 0.15d0)
        (cairo:paint)
        (cairo:select-font-face "monospace" :normal :normal)
        (cairo:set-font-size *bar-font*)
        (%ensure-metrics)
        (if (panel-view panel)
            (pine.cell:render-view (panel-view panel))
            (build-panel-cells panel))
        (pine.surface:paint-cell-grid (panel-cells panel) (panel-count panel)
                                      *bar-cell-w* *bar-cell-h* *bar-ascent* 8d0)))))

(defun on-panel-click (panel x y)
  (let* ((col (floor (- x 8d0) *bar-cell-w*))
         (line (floor y *bar-cell-h*))
         (thunk (and (panel-root panel)
                     (pine.layout:click-thunk (panel-root panel) line col))))
    (when thunk (pine.eval:evaluate-thunk thunk :package (find-package :pine-user)))))

(defun register-panel (app name builder &key (width 400) (height 320))
  "Create a hidden overlay panel NAME whose content is BUILDER (a function of the
client returning a node tree). Show it with toggle-panel."
  (let* ((window (make-application-window :application app))
         (area (make-drawing-area))
         (panel (make-panel :name name :builder builder :window window :area area
                            :width width :height height)))
    (setf (drawing-area-content-width area) width
          (drawing-area-content-height area) height
          (drawing-area-draw-func area) (list (cffi:callback %panel-draw)
                                              (cffi:null-pointer) (cffi:null-pointer))
          (window-child window) area)
    (setf (gethash (cffi:pointer-address
                    (gobject-introspection-wrapper:object-pointer area))
                   *panels-by-area*)
          panel)
    (setf (panel-view panel)
          (pine.cell:make-view (lambda () (build-panel-cells panel))
                               (lambda () (run-in-main-event-loop ()
                                            (widget-queue-draw area)))))
    (let ((click (make-gesture-click)))
      (connect click "pressed"
               (lambda (gesture n-press x y)
                 (declare (ignore gesture n-press))
                 (on-panel-click panel x y)))
      (widget-add-controller area click))
    (configure-panel window)
    (setf (gethash name *panels*) panel)
    panel))

(defun show-panel (panel)
  (window-present (panel-window panel))
  (setf (panel-visible panel) t))

(defun hide-panel (panel)
  (setf (widget-visible-p (panel-window panel)) nil
        (panel-visible panel) nil))

(defun toggle-panel (name)
  "Show panel NAME, hiding any other open panel. Must run on the UI thread."
  (let ((panel (gethash name *panels*)))
    (when panel
      (if (panel-visible panel)
          (hide-panel panel)
          (progn
            (maphash (lambda (k p)
                       (declare (ignore k))
                       (when (and (panel-visible p) (not (eq p panel)))
                         (hide-panel p)))
                     *panels*)
            (show-panel panel))))))

(defwidget calendar-panel ()
  "Placeholder panel: today's date. Exercises the panel surface end to end."
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (column :align :center
      (label (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d) :face :function-name)
      (label "calendar" :face :comment))))
