(in-package #:pine.desktop-app)

(defmacro %guard (what &body body)
  `(handler-case (progn ,@body)
     (error (e) (ignore-errors (format *error-output* "desktop ~a: ~a~%" ,what e)) nil)))

;;;; Stylesheet, authored in Lisp -> GTK CSS.

(defun p (role) (pine.buffer:color role))
(defun glass (role &optional (a (pine.buffer:metric :opacity 0.4)))
  (multiple-value-bind (r g b) (pine.buffer:hex-rgb (p role))
    (format nil "rgba(~d, ~d, ~d, ~a)" r g b a)))
(defun rad () (format nil "~apx" (pine.buffer:metric :radius 8)))
(defun mono () (format nil "~s, monospace" (pine.buffer:metric :font "Maple Mono NF")))

(defun theme-rules ()
  `(;; global reset: strip GTK4's default chrome off every widget (eww's
    ;; `* { all: unset }') -- borders, shadows, focus rings, backgrounds. Specific
    ;; rules add back only what they want. Font + base colour go on the window so
    ;; they inherit (a `*' colour would break per-widget colour inheritance).
    ("*" (:border-width "0" :border-style "none" :box-shadow "none" :outline-style "none"
          :background-color "transparent" :background-image "none"))
    ("button" (:min-width "0" :min-height "0" :padding "0"))
    ("button, scale" (:transition "background-color 0.25s, color 0.25s"))
    ;; window/decoration carry GTK4's default padding + the CSD shadow-margin;
    ;; zero them so the surface glass reaches the screen edge (no top/side gap).
    ("window, .background, .surface, decoration"
     (:background-color "transparent" :padding "0" :margin "0"))
    ("window" (:font-family ,(mono) :font-size "13px" :color ,(p :fg)))
    ;; bar
    (".bar" (:background-color ,(glass :bg) :color ,(p :fg) :padding "8px 0 0 0"))
    (".bgroup" (:border-radius ,(rad) :padding "6px 4px"))
    (".viewer, .picker, .launch, .app, .icon, .net, .media, .ctl, .ws"
     (:color ,(p :fg-alt) :min-width "28px"
      :padding "8px 0" :border-radius ,(rad) :margin "3px 0" :font-size "15px"))
    (".net" (:color ,(p :cyan)))
    (".ctl" (:color ,(p :accent)))
    (".ws-current" (:color ,(p :accent-fg) :background-color ,(p :accent) :min-width "28px"
                    :padding "8px 0" :border-radius ,(rad) :margin "3px 0"))
    (".ws" (:font-size "12px"))
    (".viewer:hover, .picker:hover, .launch:hover, .app:hover, .icon:hover, .net:hover, .media:hover, .ctl:hover, .clock:hover, .ws:hover"
     (:background-color ,(p :bg-active) :color ,(p :accent-fg)))
    (".clock" (:color ,(p :fg) :margin-top "6px"))
    (".clock .hour" (:font-size "15px" :font-weight "bold"))
    (".clock .min" (:color ,(p :fg-dim) :font-size "15px"))
    (".corner-sq" (:background-image ,(format nil "linear-gradient(135deg, ~a, ~a)"
                                              (p :accent) (p :magenta))
                   :color ,(p :accent-fg) :min-width "28px" :min-height "28px"
                   :border-radius "9px" :font-size "15px"))
    ;; per-glyph offsets (nerd-font glyphs are not centred in their em box)
    (".viewer .bar-glyph" (:margin-right "2px"))
    (".picker .bar-glyph" (:margin-right "2px"))
    (".launch .bar-glyph" (:margin-right "0px"))
    (".term .bar-glyph" (:margin-right "0px"))
    (".web .bar-glyph" (:margin-right "0px"))
    (".files .bar-glyph" (:margin-right "0px"))
    (".edit .bar-glyph" (:margin-right "0px"))
    (".icon .bar-glyph" (:margin-right "3px"))
    (".media .bar-glyph" (:margin-right "5px"))
    (".net .bar-glyph" (:margin-right "6px"))
    (".ctl .bar-glyph" (:margin-right "4px"))
    (".workspaces .ws .bar-glyph" (:margin-left "2px"))
    (".corner-sq .bar-glyph" (:color ,(p :accent-fg) :font-size "15px"))
    ;; calendar
    (".cal-box" (:background-color ,(glass :bg-dim) :border-radius ,(rad) :padding "10px"))
    ("calendar" (:color ,(p :fg)))
    ("calendar:selected" (:background-color ,(p :accent) :color ,(p :accent-fg) :border-radius "6px"))
    ;; popup chrome
    (".netmenu" (:background-color ,(glass :bg) :color ,(p :fg)
                 :border-width "1px" :border-style "solid" :border-color ,(p :border)
                 :border-radius ,(rad) :padding "8px" :margin "8px"
                 :box-shadow ,(format nil "0 0 5px 0 ~a" (p :shadow))
                 :min-width "360px"))
    (".nm-card" (:background-color ,(glass :bg-dim) :border-radius ,(rad) :padding "12px" :margin "6px"))
    (".nm-head" (:padding "12px 14px"))
    (".nm-head-ico" (:font-size "22px" :color ,(p :accent) :margin-right "14px"))
    (".nm-title" (:font-size "17px" :font-weight "bold" :color ,(p :fg)))
    (".nm-sub" (:font-size "13px"))
    (".nm-sub.on" (:color ,(p :green)))
    (".nm-sub.off" (:color ,(p :fg-dim)))
    (".nm-subhead" (:font-size "13px" :color ,(p :fg-dim) :margin "2px 4px 8px 4px"))
    (".nm-list-card" (:padding "6px"))
    (".nm-scroll" (:min-height "14rem"))
    (".nm-row" (:border-radius ,(rad) :padding "10px 12px" :margin "2px"
                :transition "background-color 0.2s"))
    (".nm-row:hover" (:background-color ,(p :bg-active)))
    (".nm-row.active" (:background-color ,(p :bg-alt)))
    (".nm-row.sel" (:background-color ,(p :bg-active) :box-shadow ,(format nil "inset 3px 0 0 0 ~a" (p :accent))))
    (".nm-sig" (:font-size "14px" :color ,(p :fg-dim)))
    (".nm-sig.hi" (:color ,(p :green)))
    (".nm-sig.mid" (:color ,(p :yellow)))
    (".nm-sig.lo" (:color ,(p :red)))
    (".nm-name" (:font-size "14px" :color ,(p :fg)))
    (".nm-name.active" (:color ,(p :green) :font-weight "bold"))
    (".nm-lock" (:font-size "12px" :color ,(p :fg-dim)))
    (".nm-check" (:font-size "13px" :color ,(p :green)))
    (".nm-actions" (:padding "6px 4px 2px 4px"))
    (".nm-btn" (:background-color ,(p :bg-active) :color ,(p :fg) :padding "10px 20px"
                :border-radius ,(rad) :transition "background-color 0.2s, color 0.2s"))
    (".nm-btn:hover" (:background-color ,(p :bg-alt)))
    (".nm-btn.go" (:background-color ,(p :accent) :color ,(p :accent-fg)))
    (".nm-btn.no" (:color ,(p :red)))
    ;; menu sliders (audio, media) -- the * reset already stripped the Adwaita chrome
    (".menu-slider trough" (:background-color ,(p :bg-alt) :min-height "8px" :border-radius "5px"))
    (".menu-slider trough highlight" (:background-color ,(p :accent) :border-radius "5px"))
    (".menu-slider slider" (:min-width "16px" :min-height "16px" :border-radius "8px" :background-color ,(p :fg)))
    ;; audio
    (".audio-mute" (:color ,(p :cyan) :font-size "20px" :padding "0 4px"))
    (".audio-mute:hover" (:color ,(p :accent)))
    (".audio-pct" (:color ,(p :fg-dim) :min-width "38px"))
    ;; media
    (".media-info" (:padding "2px 2px 12px 2px"))
    (".media-art" (:min-width "64px" :min-height "64px" :border-radius ,(rad) :background-color ,(p :bg-active)))
    (".media-art-ico" (:font-size "26px" :color ,(p :fg-dim)))
    (".media-title" (:font-size "15px" :font-weight "bold" :color ,(p :fg)))
    (".media-artist" (:color ,(p :fg-dim)))
    (".media-times" (:padding "6px 2px 2px 2px"))
    (".media-time" (:font-size "11px" :color ,(p :fg-dim)))
    (".media-ctrl" (:padding "10px 0 2px 0"))
    (".media-btn" (:color ,(p :fg) :font-size "20px" :padding "6px"))
    (".media-btn:hover" (:color ,(p :accent)))
    (".media-none-box" (:padding "8px"))
    (".media-none" (:color ,(p :fg-dim) :padding "14px"))
    ;; control panel
    (".ctlpanel-box" (:padding "4px"))
    (".ctl-card" (:background-color ,(glass :bg-dim) :border-radius ,(rad) :padding "14px" :margin "6px"))
    (".ctl-profile" (:padding "2px"))
    (".ctl-pfp" (:background-color ,(p :accent) :border-radius "100%" :min-width "58px"
                 :min-height "58px" :margin-right "16px"))
    (".ctl-pfp-ico" (:font-size "28px" :color ,(p :accent-fg)))
    (".ctl-user" (:font-size "24px" :font-weight "bold" :color ,(p :accent)))
    (".ctl-uptime" (:color ,(p :fg-dim)))
    (".ctl-power" (:background-color ,(glass :bg) :border-radius ,(rad) :padding "12px 8px" :margin-top "14px"))
    (".ctl-confirm" (:background-color ,(glass :bg) :border-radius ,(rad) :padding "10px 8px" :margin-top "14px"))
    (".ctl-confirm-lbl" (:color ,(p :fg) :font-size "14px" :font-weight "bold"))
    (".ctl-cf" (:font-size "18px" :padding "4px 14px"))
    (".ctl-cf.yes" (:color ,(p :green)))
    (".ctl-cf.no" (:color ,(p :red)))
    (".ctl-cf:hover" (:color ,(p :fg)))
    (".pw-a" (:font-size "22px" :padding "4px 12px"))
    (".pw-a.lock" (:color ,(p :blue)))
    (".pw-a.logout" (:color ,(p :yellow)))
    (".pw-a.reboot" (:color ,(p :magenta)))
    (".pw-a.suspend" (:color ,(p :cyan)))
    (".pw-a.off" (:color ,(p :red)))
    (".pw-a:hover" (:color ,(p :fg)))
    (".ctl-rings" (:padding "4px 0"))
    (".ctl-ring-box" (:background-color ,(glass :bg) :border-radius ,(rad) :padding "10px 8px" :margin "0 4px"))
    (".ctl-ring-ico" (:font-size "18px" :margin "14px"))
    (".ctl-ring-box.cpu .ctl-ring-ico" (:margin-left "11px" :margin-right "17px"))
    (".ctl-ring-box.ram .ctl-ring-ico" (:margin-left "11px" :margin-right "17px"))
    (".ctl-ring-box.disk .ctl-ring-ico" (:margin-left "11px" :margin-right "17px"))
    (".ctl-ring-lbl" (:font-size "11px" :margin-top "6px" :color ,(p :fg-dim)))
    (".ctl-ring-box.cpu .ctl-ring-ico, .ctl-ring-box.cpu .ctl-ring-lbl" (:color ,(p :red)))
    (".ctl-ring-box.ram .ctl-ring-ico, .ctl-ring-box.ram .ctl-ring-lbl" (:color ,(p :blue)))
    (".ctl-ring-box.disk .ctl-ring-ico, .ctl-ring-box.disk .ctl-ring-lbl" (:color ,(p :green)))
    (".ctl-ring-box.temp .ctl-ring-ico, .ctl-ring-box.temp .ctl-ring-lbl" (:color ,(p :yellow)))
    (".ctl-srow" (:padding "4px 8px"))
    (".ctl-sico.vol" (:color ,(p :accent) :font-size "17px"))
    (".ctl-sico.bri" (:color ,(p :yellow) :font-size "17px"))
    (".ctl-scale trough" (:background-color ,(p :bg) :min-height "10px" :min-width "180px" :border-radius "50px"))
    (".ctl-scale.vol trough highlight" (:background-color ,(p :accent) :border-radius "10px"))
    (".ctl-scale.bri trough highlight" (:background-color ,(p :yellow) :border-radius "10px"))
    (".ctl-scale slider" (:min-width "14px" :min-height "14px" :border-radius "7px" :background-color ,(p :fg)))
    ;; echo
    (".echo" (:background-color "transparent"))
    (".echo-lead" (:min-width "44px" :background-color "transparent"))
    (".echo-body" (:background-color ,(glass :bg)))
    (".echo-text" (:color ,(p :fg) :font-size "13px" :padding "0 12px"))
    (".echo-stat" (:color ,(p :fg-dim) :font-size "12px" :padding "0 16px 0 8px"))))

(defun theme->css (rules)
  (with-output-to-string (s)
    (loop for (sel props) in rules do
      (format s "~a { " sel)
      (loop for (k v) on props by #'cddr
            do (format s "~a: ~a; " (string-downcase (symbol-name k)) v))
      (format s "}~%"))))

(defvar *theme-installed* nil)
(defun install-theme ()
  (unless *theme-installed*
    (let ((prov (gtk4:make-css-provider)))
      (gtk4:css-provider-load-from-string prov (theme->css (theme-rules)))
      (gtk4:style-context-add-provider-for-display (gdk4:display-default) prov 800))
    (setf *theme-installed* t)))

;;;; gtk4-layer-shell, hand-bound.

(cffi:define-foreign-library libgtk4-layer-shell (t (:default "libgtk4-layer-shell")))
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
(cffi:defcfun ("gtk_layer_set_namespace" %set-namespace) :void (window :pointer) (namespace :string))

(defun configure-window (window kind)
  (ensure-layer-shell)
  (let ((p (gobject-introspection-wrapper:object-pointer window)))
    (%init p)
    (ecase kind
      (:bar   (%set-namespace p "gtk-layer-shell") (%set-layer p 2)
              (%set-anchor p 0 t) (%set-anchor p 2 t) (%set-anchor p 3 t) (%auto-zone p))
      (:echo  (%set-namespace p "gtk-layer-shell") (%set-layer p 2)
              (%set-anchor p 0 t) (%set-anchor p 1 t) (%set-anchor p 3 t) (%auto-zone p))
      (:panel (%set-namespace p "gtk-layer-shell") (%set-layer p 3)
              (%set-anchor p 0 t) (%set-anchor p 3 t)))))

;;;; Rings: one GtkDrawingArea per ring, params keyed by the area pointer.

(defvar *rings* (make-hash-table))

(cffi:defcallback %ring-draw :void ((area :pointer) (cr :pointer)
                                    (width :int) (height :int) (data :pointer))
  (declare (ignore data))
  (let ((p (gethash (cffi:pointer-address area) *rings*)))
    (when p
      (destructuring-bind (frac th ar ag ab tr tg tb) p
        (let* ((cairo:*context* (make-instance 'cairo:context :pointer cr
                                               :width width :height height :pixel-based-p nil))
               (th (coerce th 'double-float))
               (rad (- (/ (min width height) 2d0) (/ th 2d0) 1d0))
               (cx (/ width 2d0)) (cy (/ height 2d0))
               (start (* -0.5d0 pi)) (end (+ start (* frac 2d0 pi))))
          (cairo:set-line-width th) (cairo:set-line-cap :round)
          (cairo:set-source-rgb tr tg tb)
          (cairo:new-sub-path) (cairo:arc cx cy rad 0d0 (* 2d0 pi)) (cairo:stroke)
          (cairo:set-source-rgb ar ag ab)
          (cairo:new-sub-path) (cairo:arc cx cy rad start end) (cairo:stroke))))))

(defun %ring (props nodes)
  (let* ((da (gtk4:make-drawing-area))
         (v (getf props :value 0)) (mn (getf props :min 0)) (mx (getf props :max 100))
         (span (max 1 (- mx mn)))
         (frac (max 0d0 (min 1d0 (/ (float (- v mn) 1d0) span)))))
    (setf (gtk4:drawing-area-content-width da) 58
          (gtk4:drawing-area-content-height da) 58)
    (multiple-value-bind (ar ag ab) (%face-rgb01 (getf props :arc-face))
      (multiple-value-bind (tr tg tb) (%face-rgb01 (getf props :track-face))
        (setf (gethash (cffi:pointer-address (gobject-introspection-wrapper:object-pointer da))
                       *rings*)
              (list frac (getf props :thickness 5) ar ag ab tr tg tb))))
    (setf (gtk4:drawing-area-draw-func da)
          (list (cffi:callback %ring-draw) (cffi:null-pointer) (cffi:null-pointer)))
    (let ((inner (build-widget (first nodes))))
      (if inner
          (let ((ov (gtk4:make-overlay)))
            (setf (gtk4:overlay-child ov) da
                  (gtk4:widget-halign inner) gtk4:+align-center+
                  (gtk4:widget-valign inner) gtk4:+align-center+)
            (gtk4:overlay-add-overlay ov inner)
            ov)
          da))))

(defun %centerbox (props nodes)
  (let* ((vert (eq (getf props :orient :v) :v))
         (cb (gtk4:make-center-box))
         (s (build-widget (first nodes)))
         (c (build-widget (second nodes)))
         (e (build-widget (third nodes))))
    (setf (gtk4:orientable-orientation cb)
          (if vert gtk4:+orientation-vertical+ gtk4:+orientation-horizontal+))
    (when s (setf (gtk4:center-box-start-widget cb) s))
    (when c (setf (gtk4:center-box-center-widget cb) c))
    (when e (setf (gtk4:center-box-end-widget cb) e))
    cb))

;;;; wire tree -> GTK widget tree.

(defvar *client-ref* nil)
(defun send-action (id args)
  (when (and id *client-ref*)
    (ignore-errors (sento.actor:tell *client-ref* (list :widget-action :id id :args args)))))

(defun send-hint (text)
  (when *client-ref*
    (ignore-errors (sento.actor:tell *client-ref* (list :hint :text text)))))

(defun %hint-controller (w hint)
  (let ((m (gtk4:make-event-controller-motion)))
    (gtk4:connect m "enter" (lambda (c x y) (declare (ignore c x y)) (send-hint hint)))
    (gtk4:connect m "leave" (lambda (c) (declare (ignore c)) (send-hint "")))
    (gtk4:widget-add-controller w m)))

(defun %box (orient props nodes)
  (let ((b (gtk4:make-box :orientation orient :spacing (or (getf props :spacing) 0)))
        (align (getf props :align))
        (vert (= orient gtk4:+orientation-vertical+)))
    (dolist (c nodes)
      (let ((cw (build-widget c)))
        (when cw
          (cond
            ((and (consp c) (eq (first c) :gap))
             (if vert (setf (gtk4:widget-vexpand-p cw) t) (setf (gtk4:widget-hexpand-p cw) t)))
            (t (case align
                 (:center (if vert (setf (gtk4:widget-halign cw) gtk4:+align-center+)
                              (setf (gtk4:widget-valign cw) gtk4:+align-center+)))
                 (:end    (if vert (setf (gtk4:widget-halign cw) gtk4:+align-end+)
                              (setf (gtk4:widget-valign cw) gtk4:+align-end+)))
                 (:stretch (setf (gtk4:widget-hexpand-p cw) t))
                 (t (if vert (setf (gtk4:widget-halign cw) gtk4:+align-start+)
                        (setf (gtk4:widget-valign cw) gtk4:+align-start+))))))
          (gtk4:box-append b cw))))
    b))

(defun %button (props nodes)
  (let* ((cform (first nodes))
         (b (gtk4:make-button))
         (inner (build-widget cform))
         (id (getf props :action)))
    (when inner
      (setf (gtk4:button-child b) inner)
      (if (and (consp cform) (eq (first cform) :label))
          (setf (gtk4:widget-halign inner) gtk4:+align-center+
                (gtk4:widget-valign inner) gtk4:+align-center+)
          (setf (gtk4:widget-hexpand-p inner) t)))
    (when id (gtk4:connect b "clicked"
                           (lambda (w) (declare (ignore w)) (send-action id nil))))
    b))

(defun %scale (props)
  (let* ((mn (getf props :min 0)) (mx (getf props :max 100)) (v (getf props :value 0))
         (sc (gtk4:make-scale :min (float mn 1d0) :max (float (max mx (1+ mn)) 1d0) :step 1d0
                              :orientation gtk4:+orientation-horizontal+))
         (id (getf props :action)))
    (setf (gtk4:range-value sc) (float v 1d0)
          (gtk4:scale-draw-value-p sc) nil
          (gtk4:widget-hexpand-p sc) t)
    (when id (gtk4:connect sc "value-changed"
                           (lambda (w) (send-action id (list (round (gtk4:range-value w)))))))
    sc))

(defun build-widget (form)
  (when form
    (destructuring-bind (type props &rest nodes) form
      (let ((w (case type
                 (:label (let ((l (gtk4:make-label :str (getf props :content ""))))
                           (setf (gtk4:label-xalign l) 0.0) l))
                 (:row      (%box gtk4:+orientation-horizontal+ props nodes))
                 ((:column :grid) (%box gtk4:+orientation-vertical+ props nodes))
                 (:box      (%box gtk4:+orientation-horizontal+ props nodes))
                 (:centered (let* ((b (%box gtk4:+orientation-vertical+ props nodes))
                                   (inner (gtk4:widget-first-child b)))
                              (setf (gtk4:widget-halign b) gtk4:+align-center+
                                    (gtk4:widget-valign b) gtk4:+align-center+)
                              (when inner
                                (setf (gtk4:widget-halign inner) gtk4:+align-center+
                                      (gtk4:widget-valign inner) gtk4:+align-center+
                                      (gtk4:widget-hexpand-p inner) t
                                      (gtk4:widget-vexpand-p inner) t))
                              b))
                 (:centerbox (%centerbox props nodes))
                 (:gap      (gtk4:make-box :orientation gtk4:+orientation-horizontal+ :spacing 0))
                 (:rule     (gtk4:make-box :orientation gtk4:+orientation-horizontal+ :spacing 0))
                 (:action   (%button props nodes))
                 (:choice   (%box gtk4:+orientation-horizontal+ props nodes))
                 (:meter    (%scale props))
                 (:ring     (%ring props nodes))
                 (:calendar (let ((c (gtk4:make-calendar)))
                              (ignore-errors
                               (setf (gtk4:calendar-year c) (getf props :year 2000)
                                     (gtk4:calendar-month c) (max 0 (1- (getf props :month 1)))
                                     (gtk4:calendar-day c) (getf props :day 1)))
                              c))
                 (:picture  (let ((path (getf props :path "")))
                              (if (plusp (length path))
                                  (gtk4:make-picture :filename path)
                                  (gtk4:make-box :orientation gtk4:+orientation-horizontal+ :spacing 0))))
                 (:viewport (let ((sw (gtk4:make-scrolled-window)))
                              (setf (gtk4:scrolled-window-child sw)
                                    (%box gtk4:+orientation-vertical+ (list* :align :stretch props) nodes)
                                    (gtk4:widget-vexpand-p sw) t
                                    (gtk4:scrolled-window-policy sw)
                                    (list gtk4:+policy-type-never+ gtk4:+policy-type-automatic+))
                              (let ((h (getf props :height)))
                                (when h (setf (gtk4:scrolled-window-min-content-height sw) h)))
                              sw))
                 (t (gtk4:make-box :orientation gtk4:+orientation-horizontal+ :spacing 0)))))
        (when w
          (let ((cls (getf props :class)))
            (when cls
              (dolist (c (uiop:split-string cls :separator " "))
                (when (plusp (length c)) (gtk4:widget-add-css-class w c)))))
          (when (getf props :expand) (setf (gtk4:widget-hexpand-p w) t))
          (let ((hint (getf props :hint))) (when hint (%hint-controller w hint))))
        w))))

;;;; Surfaces.

(defstruct (surface (:constructor %make-surface)) name window kind content)

(defvar *sys* nil)
(defvar *surfaces* (make-hash-table :test 'equal))

(defparameter *panels* '("calendar" "audio" "network" "media" "ctl"))

(defun rebuild (name form)
  (let ((surf (gethash name *surfaces*)))
    (when surf
      (run-in-main-event-loop ()
        (%guard "rebuild"
          (let ((frame (gtk4:make-box :orientation gtk4:+orientation-vertical+ :spacing 0))
                (w (build-widget form)))
            (gtk4:widget-add-css-class frame "surface")
            (gtk4:widget-add-css-class frame (string-downcase (symbol-name (surface-kind surf))))
            (when w
              (case (surface-kind surf)
                (:bar  (setf (gtk4:widget-vexpand-p w) t))
                (:echo (setf (gtk4:widget-hexpand-p w) t)))
              (gtk4:box-append frame w))
            (setf (surface-content surf) w
                  (gtk4:window-child (surface-window surf)) frame)))))))

(defun add-surface (window name kind)
  (setf (gethash name *surfaces*)
        (%make-surface :name name :window window :kind kind)))

(defun toggle-panel (name show)
  (let ((surf (gethash name *surfaces*)))
    (when surf
      (if show
          (gtk4:window-present (surface-window surf))
          (setf (gtk4:widget-visible-p (surface-window surf)) nil)))))

(define-application (:name %app :id "org.pine.desktop")
  (define-main-window (window (make-application-window :application *application*))
    (install-theme)
    (add-surface window "bar" :bar)
    (setf (gtk4:widget-size-request window) '(32 -1))
    (configure-window window :bar)
    (window-present window)
    (let ((ew (make-application-window :application *application*)))
      (add-surface ew "echo" :echo)
      (setf (gtk4:widget-size-request ew) '(-1 28))
      (configure-window ew :echo)
      (window-present ew))
    (dolist (name *panels*)
      (let ((pw (make-application-window :application *application*)))
        (add-surface pw name :panel)
        (configure-window pw :panel)))
    (timeout-add 1000 (lambda ()
                        (let ((b (gethash "bar" *surfaces*)))
                          (when (and *client-ref* b (null (surface-content b)))
                            (ignore-errors (sento.actor:tell *client-ref* (list :refresh)))))
                        t))))

(defun run-desktop-app (&key (host "127.0.0.1") (port 17000))
  (unless pine.server:*server* (setf pine.server:*server* (make-instance 'pine.server:server)))
  (ignore-errors (pine.buffer:install-default-faces))
  (setf *sys* (sento.actor-system:make-actor-system
               '(:dispatchers (:shared (:workers 2 :strategy :random)))))
  (sento.remoting:enable-remoting *sys* :host "127.0.0.1" :port 0)
  (sento.actor-context:actor-of *sys* :name "display"
    :receive
    (lambda (msg)
      (case (first msg)
        (:attached
         (destructuring-bind (&key id client-uri) (rest msg)
           (declare (ignore id))
           (setf *client-ref* (sento.remoting:make-remote-ref *sys* client-uri))))
        (:widgets
         (destructuring-bind (&key surface tree) (rest msg)
           (rebuild surface tree)))
        (:panel
         (destructuring-bind (&key name show) (rest msg)
           (run-in-main-event-loop () (toggle-panel name show)))))
      nil))
  (pine.attach:attach-to-daemon *sys*
    (format nil "sento://~a:~d/user/attach" host port)
    (format nil "sento://127.0.0.1:~d/user/display" (sento.remoting:remoting-port *sys*))
    :kind :desktop)
  (%app))
