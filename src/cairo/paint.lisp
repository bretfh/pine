(defpackage #:pine/cairo/paint
  (:use #:cl #:pine/ui/node)
  (:export #:measure-tree #:paint-arranged #:paint-tree #:render-tree-to-png #:with-cairo-layout))
(in-package #:pine/cairo/paint)

(defvar *cairo-font* "Maple Mono NF")

(defvar *style-cache* nil "Per-render eq map node -> resolved style.")

(defparameter +cal-months+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(export '(paint-px render-tree-to-png paint-tree paint-arranged measure-tree
          with-cairo-layout *cairo-font*))

(defun cairo-text-size (text font-px)
  (cairo:select-font-face *cairo-font* :normal :normal)
  (cairo:set-font-size (float font-px 1d0))
  (let ((fe (cairo:get-font-extents)))
    (multiple-value-bind (xb yb w h ax) (cairo:text-extents text)
      (declare (ignore xb yb w h))
      (values (max 1 (ceiling ax))
              (max 1 (ceiling (+ (cairo:font-ascent fe) (cairo:font-descent fe))))))))

(defun styled (n chain hover)
  "The resolved style for N given its ancestor class CHAIN (root-first). The
non-hover style is cached for the render; hover re-resolves."
  (let ((full (append chain (list (pine/ui/cells:node-classes n)))))
    (if hover
        (pine/ui/style:resolve full :hover t)
        (or (and *style-cache* (gethash n *style-cache*))
            (let ((st (pine/ui/style:resolve full)))
              (when *style-cache* (setf (gethash n *style-cache*) st))
              st)))))

(defun node-chain (n chain) (append chain (list (pine/ui/cells:node-classes n))))

(defun apply-styles! (n chain)
  "Fold CSS padding / min-size / font-size into the node before layout, so
measure/arrange (which honour pad/min/font-px) produce the styled pixel sizes.
Caches the resolved style for the paint pass."
  (let* ((full (node-chain n chain))
         (st (pine/ui/style:resolve full)))
    (when *style-cache* (setf (gethash n *style-cache*) st))
    (when (pine/ui/style:st-pad-x st) (setf (pad-x n) (pine/ui/style:st-pad-x st)))
    (when (pine/ui/style:st-pad-y st) (setf (pad-y n) (pine/ui/style:st-pad-y st)))
    (when (pine/ui/style:st-min-w st) (setf (min-w n) (max (min-w n) (pine/ui/style:st-min-w st))))
    (when (pine/ui/style:st-min-h st) (setf (min-h n) (max (min-h n) (pine/ui/style:st-min-h st))))
    (when (pine/ui/style:st-margin st) (setf (node-margin n) (pine/ui/style:st-margin st)))
    (when (and (pine/ui/style:st-font-px st) (null (font-px n)))
      (setf (font-px n) (pine/ui/style:st-font-px st)))
    (dolist (c (pine/ui/layout:nodes-of n)) (apply-styles! c full))))

(defun set-hex (hex)
  (multiple-value-bind (r g b) (pine/ui/face:hex-rgb hex)
    (when r (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)))))

(defun set-face-rgb (face)
  (destructuring-bind (r g b) (pine/ui/face:face-fg face)
    (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0))))

(defun content-color (n st)
  "(values r g b) 0..1 for a node's text: its style colour, else its face fg,
else the theme default."
  (cond ((pine/ui/style:st-fg st) (values-list (pine/ui/style:st-fg st)))
        (t (destructuring-bind (r g b) (pine/ui/face:face-fg (or (face n) :default))
             (values (/ r 255.0) (/ g 255.0) (/ b 255.0))))))

(defun rounded-rect (x y w h r)
  (let ((x (float x 1d0)) (y (float y 1d0)) (w (float w 1d0)) (h (float h 1d0))
        (r (float (min r (/ (min w h) 2d0)) 1d0)))
    (if (<= r 0d0)
        (cairo:rectangle x y w h)
        (progn
          (cairo:new-sub-path)
          (cairo:arc (+ x w (- r)) (+ y r)       r (* -0.5d0 pi) 0d0)
          (cairo:arc (+ x w (- r)) (+ y h (- r)) r 0d0 (* 0.5d0 pi))
          (cairo:arc (+ x r)       (+ y h (- r)) r (* 0.5d0 pi) pi)
          (cairo:arc (+ x r)       (+ y r)       r pi (* 1.5d0 pi))
          (cairo:close-path)))))

(defun radius-px (radius w h)
  (cond ((eq radius :round) (/ (min w h) 2.0))
        ((numberp radius) radius)
        (t 0)))

(defun node-rect (n)
  (values (start-col n) (start-line n)
          (- (end-col n) (start-col n)) (1+ (- (end-line n) (start-line n)))))

(defun fill-gradient (x y w h stops)
  "Fill the current path with a 135deg linear gradient (top-left to bottom-right)
between the two STOPS ((r g b) (r g b))."
  (let ((p (cairo:create-linear-pattern (float x 1d0) (float y 1d0)
                                        (float (+ x w) 1d0) (float (+ y h) 1d0))))
    (destructuring-bind (r0 g0 b0) (first stops)
      (cairo:pattern-add-color-stop-rgb p 0d0 r0 g0 b0))
    (destructuring-bind (r1 g1 b1) (second stops)
      (cairo:pattern-add-color-stop-rgb p 1d0 r1 g1 b1))
    (cairo:set-source p)
    (cairo:fill-path)
    (cairo:destroy p)))

(defun draw-shadow (st x y w h r)
  "A cheap feathered drop shadow: fill the rounded shape blurred outward in
concentric low-alpha layers, so the panel reads as floating over the desktop."
  (let ((shadow (pine/ui/style:st-shadow st)))
    (when shadow
      (destructuring-bind (ox oy blur color) shadow
        (destructuring-bind (sr sg sb sa) color
          (let ((steps (max 1 (min 12 (round blur)))))
            (loop for i from steps downto 1
                  for grow = (float i 1d0)
                  do (cairo:set-source-rgba sr sg sb (* (or sa 1.0) 0.14))
                     (rounded-rect (- (+ x ox) grow) (- (+ y oy) grow)
                                   (+ w (* 2 grow)) (+ h (* 2 grow)) (+ r grow))
                     (cairo:fill-path))))))))

(defun draw-chrome (st x y w h)
  (when (and (plusp w) (plusp h))
    (let ((r (radius-px (pine/ui/style:st-radius st) w h)))
      (draw-shadow st x y w h r)
      (cond
        ((pine/ui/style:st-gradient st)
         (rounded-rect x y w h r)
         (fill-gradient x y w h (pine/ui/style:st-gradient st)))
        ((pine/ui/style:st-bg st)
         (rounded-rect x y w h r)
         (destructuring-bind (rr gg bb aa) (pine/ui/style:st-bg st)
           (cairo:set-source-rgba rr gg bb aa))
         (cairo:fill-path)))
      (when (and (pine/ui/style:st-border-color st) (plusp (pine/ui/style:st-border-w st)))
        (rounded-rect x y w h r)
        (apply #'cairo:set-source-rgb (pine/ui/style:st-border-color st))
        (cairo:set-line-width (float (pine/ui/style:st-border-w st) 1d0))
        (cairo:stroke))
      (let ((inset (pine/ui/style:st-inset st)))
        (when inset
          (destructuring-bind (ir ig ib iw) inset
            (cairo:rectangle (float x 1d0) (float y 1d0) (float iw 1d0) (float h 1d0))
            (cairo:set-source-rgb ir ig ib)
            (cairo:fill-path)))))))

(defgeneric paint-px (node chain)
  (:documentation "Draw arranged NODE and its subtree to cairo:*context*. CHAIN
is the ancestor class-set list, root-first, for style resolution."))

(defmethod paint-px :before ((n node) chain)
  (let ((st (styled n chain (hovered n))))
    (multiple-value-bind (x y w h) (node-rect n)
      (draw-chrome st x y w h))))

(defun paint-nodes (n chain list)
  (let ((cc (node-chain n chain))) (dolist (c list) (when c (paint-px c cc)))))

(defmethod paint-px ((n node) chain) (paint-nodes n chain (pine/ui/layout:nodes-of n)))

(defmethod paint-px ((n scroll) chain)
  "Clip to the viewport rect before painting the (taller, offset) content."
  (multiple-value-bind (x y w h) (node-rect n)
    (cairo:save)
    (cairo:rectangle (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
    (cairo:clip)
    (paint-nodes n chain (pine/ui/layout:nodes-of n))
    (cairo:restore)))

(defmethod paint-px ((n text-node) chain)
  (paint-glyph-run n chain (content n)))

(defun paint-glyph-run (n chain text)
  (when (plusp (length text))
    (let* ((st (styled n chain (hovered n)))
           (fpx (or (font-px n) (pine/ui/style:st-font-px st) pine/ui/layout:*default-font-px*)))
      (cairo:select-font-face *cairo-font* :normal (if (pine/ui/style:st-bold st) :bold :normal))
      (cairo:set-font-size (float fpx 1d0))
      (multiple-value-bind (r g b) (content-color n st) (cairo:set-source-rgb r g b))
      (multiple-value-bind (x y w h) (node-rect n)
        (declare (ignore w))
        (let* ((fe (cairo:get-font-extents))
               (asc (cairo:font-ascent fe))
               (lh (+ asc (cairo:font-descent fe)))
               (top (+ y (max 0 (/ (- h lh) 2d0)))))
          (cairo:move-to (float x 1d0) (float (+ top asc) 1d0))
          (cairo:show-text text))))))

(defmethod paint-px ((n separator) chain)
  (declare (ignore chain))
  (multiple-value-bind (x y w h) (node-rect n)
    (set-face-rgb (or (face n) :comment))
    (cairo:set-line-width 1d0)
    (if (sep-vertical n)
        (progn
          (cairo:move-to (float (+ x (/ w 2d0)) 1d0) (float y 1d0))
          (cairo:line-to (float (+ x (/ w 2d0)) 1d0) (float (+ y h) 1d0)))
        (progn
          (cairo:move-to (float x 1d0) (float (+ y (/ h 2d0)) 1d0))
          (cairo:line-to (float (+ x w) 1d0) (float (+ y (/ h 2d0)) 1d0))))
    (cairo:stroke)))

(defmethod paint-px ((n slider) chain)
  (declare (ignore chain))
  (multiple-value-bind (x y w h) (node-rect n)
    (let* ((frac (slider-fraction n))
           (th 10) (ty (+ y (floor (- h th) 2)))
           (cls (pine/ui/cells:node-classes n))
           (fill-role (if (member "bri" cls :test #'string=) :yellow :accent))
           (fillw (max th (round (* frac w)))))
      (rounded-rect x ty w th (/ th 2.0)) (set-hex (pine/ui/face:color :bg)) (cairo:fill-path)
      (when (plusp frac)
        (rounded-rect x ty fillw th (/ th 2.0))
        (set-hex (pine/ui/face:color fill-role)) (cairo:fill-path))
      (let ((kx (max (+ x 7) (min (- (+ x w) 7) (+ x (round (* frac w)))))))
        (cairo:new-sub-path)
        (cairo:arc (float kx 1d0) (float (+ ty (/ th 2.0)) 1d0) 7d0 0d0 (* 2d0 pi))
        (set-hex (pine/ui/face:color :fg)) (cairo:fill-path)))))

(defmethod paint-px ((n pine/ui/node:ring) chain)
  (multiple-value-bind (x y w h) (node-rect n)
    (let* ((d (min w h)) (th (float (thickness n) 1d0))
           (cx (float (+ x (/ w 2.0)) 1d0)) (cy (float (+ y (/ h 2.0)) 1d0))
           (rad (max 1d0 (- (/ d 2.0) (/ th 2.0) 1)))
           (frac (ring-fraction n))
           (start (* -0.5d0 pi)) (end (+ start (* frac 2d0 pi))))
      (cairo:set-line-width th) (cairo:set-line-cap :round)
      (set-face-rgb (track-face n))
      (cairo:new-sub-path) (cairo:arc cx cy rad 0d0 (* 2d0 pi)) (cairo:stroke)
      (when (plusp frac)
        (set-face-rgb (arc-face n))
        (cairo:new-sub-path) (cairo:arc cx cy rad start end) (cairo:stroke))))
  (when (node n) (paint-nodes n chain (list (node n)))))

(defmethod paint-px ((n picture) chain)
  (declare (ignore chain))
  (let ((path (pic-path n)))
    (when (and path (plusp (length path)) (probe-file path))
      (ignore-errors
        (multiple-value-bind (x y w h) (node-rect n)
          (let* ((img (cairo:image-surface-create-from-png path))
                 (iw (cairo:image-surface-get-width img))
                 (ih (cairo:image-surface-get-height img)))
            (cairo:save)
            (cairo:translate (float x 1d0) (float y 1d0))
            (when (and (plusp iw) (plusp ih))
              (cairo:scale (/ w iw 1d0) (/ h ih 1d0)))
            (cairo:set-source-surface img 0 0) (cairo:paint)
            (cairo:restore)))))))

(defun cairo-cell-metrics (fpx)
  "cell-w cell-h ascent, identical to what CAIRO-TEXT-SIZE (the measure hook)
reports for one cell, so a window measures and paints at the same grid."
  (cairo:select-font-face *cairo-font* :normal :normal)
  (cairo:set-font-size (float fpx 1d0))
  (let ((fe (cairo:get-font-extents)))
    (multiple-value-bind (xb yb w h ax) (cairo:text-extents "M")
      (declare (ignore xb yb w h))
      (values (float (max 1 (ceiling ax)) 1d0)
              (float (max 1 (ceiling (+ (cairo:font-ascent fe) (cairo:font-descent fe)))) 1d0)
              (cairo:font-ascent fe)))))

(defmethod paint-px ((n view-node) chain)
  (multiple-value-bind (x y w h) (node-rect n)
    (let* ((style (styled n chain (hovered n)))
           (said (pine/ui/style:st-bg style)))
      (destructuring-bind (br bg bb)
          (if said
              (mapcar (lambda (c) (round (* 255 c))) (subseq said 0 3))
              (pine/ui/face:face-bg :window))
        (cairo:set-source-rgba (/ br 255.0) (/ bg 255.0) (/ bb 255.0)
                               (float (or (pine/ui/style:st-opacity style)
                                          (view-opacity n))
                                      1d0))))
    (cairo:rectangle (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
    (cairo:fill-path)
    (let ((fpx (or (font-px n) pine/ui/layout:*default-font-px*))
          (over (pine/ui/layout:view-overlay-count n)))
      (multiple-value-bind (cw ch asc) (cairo-cell-metrics fpx)

        (when (plusp over)
          (let ((top (- y (* over ch)))
                (orows (subseq (view-rows n) 0 over)))
            (destructuring-bind (br bg bb) (or (pine/ui/face:face-bg :completion)
                                               (pine/ui/face:face-bg :window)
                                               (list 0 0 0))
              (cairo:set-source-rgba (/ br 255.0) (/ bg 255.0) (/ bb 255.0) 0.95d0))
            (cairo:rectangle (float x 1d0) (float top 1d0)
                             (float w 1d0) (float (* over ch) 1d0))
            (cairo:fill-path)
            (cairo:save)
            (cairo:translate 0d0 (float top 1d0))
            (pine/cairo/grid:paint-rows orows cw ch asc (float x 1d0))
            (cairo:restore)))
        (cairo:save)
        (cairo:rectangle (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
        (cairo:clip)
        (cairo:translate 0d0 (float y 1d0))
        (pine/cairo/grid:paint-rows (subseq (view-rows n) over) cw ch asc (float x 1d0))

        (when (>= (view-crow n) 0)
          (destructuring-bind (br bg bb) (pine/ui/face:face-bg :cursor)
            (cairo:set-source-rgb (/ br 255.0) (/ bg 255.0) (/ bb 255.0)))
          (cairo:rectangle (+ (float x 1d0) (* (view-ccol n) cw)) (* (view-crow n) ch) cw ch)
          (cairo:set-line-width 1.5d0) (cairo:stroke))
        (cairo:restore)))))

(defmacro with-cairo-layout (&body body)
  "Bind the dynamic state the cairo layout pass needs: the theme font, the
*text-size* hook (pixel measurement), and a fresh per-render style cache."
  `(let ((*cairo-font* (pine/ui/face:metric :font "Maple Mono NF"))
         (pine/ui/layout:*text-size* #'cairo-text-size)
         (*style-cache* (make-hash-table :test 'eq)))
     ,@body))

(defun measure-tree (node avail-w)
  "Fold styles and measure NODE under a throwaway context; (values w h). Use
inside WITH-CAIRO-LAYOUT."
  (let ((s (cairo:create-image-surface :argb32 8 8)))
    (cairo:with-context ((cairo:create-context s))
      (apply-styles! node nil)
      (pine/ui/layout:measure node avail-w 100000))))

(defun paint-arranged (node)
  "Paint NODE at the rects it already carries -- the daemon's one arrange,
shipped over the wire -- resolving styles but running no second layout."
  (apply-styles! node nil)
  (paint-px node nil))

(defun paint-tree (node width height)
  "Measure/arrange NODE to fill WIDTH x HEIGHT and paint it to the bound cairo
context. For a fixed-size surface (a layer surface the compositor sized). The
caller sets up WITH-CAIRO-LAYOUT, the context, and any wallpaper/clear."
  (apply-styles! node nil)
  (pine/ui/layout:measure node width height)
  (pine/ui/layout:arrange node 0 0 width height)
  (paint-px node nil))

(defun render-tree-to-png (node path &key (pad 20) (avail-w 420) (bg :bg-alt))
  "Render the widget NODE tree to a PNG at PATH, on a BG palette-role wallpaper
so the glass surfaces read. Returns (:w W :h H :path PATH)."
  (with-cairo-layout
    (multiple-value-bind (mw mh) (measure-tree node avail-w)
      (let* ((w (+ mw (* 2 pad))) (h (+ mh (* 2 pad)))
             (surface (cairo:create-image-surface :argb32 w h)))
        (cairo:with-context ((cairo:create-context surface))
          (multiple-value-bind (r g b) (pine/ui/face:hex-rgb (pine/ui/face:color bg))
            (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)) (cairo:paint))
          (pine/ui/layout:arrange node pad pad mw mh)
          (paint-px node nil))
        (cairo:surface-write-to-png surface path)
        (list :w w :h h :path path)))))
