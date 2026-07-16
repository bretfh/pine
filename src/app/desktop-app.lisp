(in-package #:pine.desktop-app)

(defmacro %guard (what &body body)
  `(handler-case (progn ,@body)
     (error (e) (ignore-errors (format *error-output* "desktop ~a: ~a~%" ,what e)) nil)))

;;;; Stylesheet: the rules live in pine.buffer (one source, shared with the
;;;; cairo backend); here we only flatten them to a GTK CSS string.

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
      (gtk4:css-provider-load-from-string prov (theme->css (pine.buffer:theme-rules)))
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
