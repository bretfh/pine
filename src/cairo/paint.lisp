(in-package :pine.layout)

;;;; The cairo backend for the widget engine. measure/arrange already run in
;;;; pixels when *text-size* is bound; here we bind it to a cairo text measurer
;;;; and add PAINT-CAIRO, a second paint pass that draws each arranged node to a
;;;; cairo context -- rounded backgrounds, borders, themed text, sliders, and
;;;; rings -- styled by pine.style resolving the shared theme-rules. No GTK, no
;;;; cell raster: the same tree the daemon builds renders straight to pixels.

(export '(paint-cairo render-tree-to-png paint-tree measure-tree
          with-cairo-layout *cairo-font*))

(defvar *cairo-font* "Maple Mono NF")
(defvar *style-cache* nil "Per-render eq map node -> resolved style.")

;;;; Text measurement: the *text-size* hook, so measure lays out in pixels.

(defun cairo-text-size (text font-px)
  (cairo:select-font-face *cairo-font* :normal :normal)
  (cairo:set-font-size (float font-px 1d0))
  (let ((fe (cairo:get-font-extents)))
    (multiple-value-bind (xb yb w h ax) (cairo:text-extents text)
      (declare (ignore xb yb w h))
      (values (max 1 (ceiling ax))
              (max 1 (ceiling (+ (cairo:font-ascent fe) (cairo:font-descent fe))))))))

;;;; Style plumbing.

(defun node-classes (n)
  (let ((c (css-class n)))
    (if c (remove "" (uiop:split-string c :separator '(#\space)) :test #'string=) nil)))

(defun styled (n chain hover)
  "The resolved style for N given its ancestor class CHAIN (root-first). The
non-hover style is cached for the render; hover re-resolves."
  (let ((full (append chain (list (node-classes n)))))
    (if hover
        (pine.style:resolve full :hover t)
        (or (and *style-cache* (gethash n *style-cache*))
            (let ((st (pine.style:resolve full)))
              (when *style-cache* (setf (gethash n *style-cache*) st))
              st)))))

(defun child-chain (n chain) (append chain (list (node-classes n))))

(defun apply-styles! (n chain)
  "Fold CSS padding / min-size / font-size into the node before layout, so
measure/arrange (which honour pad/min/font-px) produce the styled pixel sizes.
Caches the resolved style for the paint pass."
  (let* ((full (child-chain n chain))
         (st (pine.style:resolve full)))
    (when *style-cache* (setf (gethash n *style-cache*) st))
    (when (pine.style:st-pad-x st) (setf (pad-x n) (pine.style:st-pad-x st)))
    (when (pine.style:st-pad-y st) (setf (pad-y n) (pine.style:st-pad-y st)))
    (when (pine.style:st-min-w st) (setf (min-w n) (max (min-w n) (pine.style:st-min-w st))))
    (when (pine.style:st-min-h st) (setf (min-h n) (max (min-h n) (pine.style:st-min-h st))))
    (when (pine.style:st-margin st) (setf (node-margin n) (pine.style:st-margin st)))
    (when (and (pine.style:st-font-px st) (null (font-px n)))
      (setf (font-px n) (pine.style:st-font-px st)))
    (dolist (c (nodes-of n)) (apply-styles! c full))))

;;;; Cairo helpers.

(defun set-hex (hex)
  (multiple-value-bind (r g b) (pine.buffer:hex-rgb hex)
    (when r (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)))))

(defun set-face-rgb (face)
  (destructuring-bind (r g b) (pine.buffer:face-fg face)
    (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0))))

(defun content-color (n st)
  "(values r g b) 0..1 for a node's text: its style colour, else its face fg,
else the theme default."
  (cond ((pine.style:st-fg st) (values-list (pine.style:st-fg st)))
        (t (destructuring-bind (r g b) (pine.buffer:face-fg (or (face n) :default))
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
  (let ((shadow (pine.style:st-shadow st)))
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
    (let ((r (radius-px (pine.style:st-radius st) w h)))
      (draw-shadow st x y w h r)
      (cond
        ((pine.style:st-gradient st)
         (rounded-rect x y w h r)
         (fill-gradient x y w h (pine.style:st-gradient st)))
        ((pine.style:st-bg st)
         (rounded-rect x y w h r)
         (destructuring-bind (rr gg bb aa) (pine.style:st-bg st)
           (cairo:set-source-rgba rr gg bb aa))
         (cairo:fill-path)))
      (when (and (pine.style:st-border-color st) (plusp (pine.style:st-border-w st)))
        (rounded-rect x y w h r)
        (apply #'cairo:set-source-rgb (pine.style:st-border-color st))
        (cairo:set-line-width (float (pine.style:st-border-w st) 1d0))
        (cairo:stroke))
      (let ((inset (pine.style:st-inset st)))
        (when inset
          (destructuring-bind (ir ig ib iw) inset
            (cairo:rectangle (float x 1d0) (float y 1d0) (float iw 1d0) (float h 1d0))
            (cairo:set-source-rgb ir ig ib)
            (cairo:fill-path)))))))

;;;; PAINT-CAIRO: chrome via :before, content/recursion in the primary method.

(defgeneric paint-cairo (node chain)
  (:documentation "Draw arranged NODE and its subtree to cairo:*context*. CHAIN
is the ancestor class-set list, root-first, for style resolution."))

(defmethod paint-cairo :before ((n node) chain)
  (let ((st (styled n chain (hovered n))))
    (multiple-value-bind (x y w h) (node-rect n)
      (draw-chrome st x y w h))))

(defmethod paint-cairo ((n node) chain) (declare (ignore chain)) nil)

(defun paint-children (n chain list)
  (let ((cc (child-chain n chain))) (dolist (c list) (when c (paint-cairo c cc)))))

(defmethod paint-cairo ((n vstack) chain) (paint-children n chain (nodes n)))
(defmethod paint-cairo ((n hstack) chain) (paint-children n chain (nodes n)))
(defmethod paint-cairo ((n centerbox) chain) (paint-children n chain (%cb-parts n)))
(defmethod paint-cairo ((n grid) chain) (paint-children n chain (apply #'append (cells n))))
(defmethod paint-cairo ((n list-node) chain) (paint-children n chain (rendered n)))
(defmethod paint-cairo ((n box) chain) (paint-children n chain (list (node n))))
(defmethod paint-cairo ((n center) chain) (paint-children n chain (list (node n))))

(defmethod paint-cairo ((n scroll) chain)
  "Clip to the viewport rect before painting the (taller, offset) content."
  (multiple-value-bind (x y w h) (node-rect n)
    (cairo:save)
    (cairo:rectangle (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
    (cairo:clip)
    (paint-children n chain (list (node n)))
    (cairo:restore)))
(defmethod paint-cairo ((n action) chain) (paint-children n chain (list (node n))))
(defmethod paint-cairo ((n selectable) chain) (paint-children n chain (list (node n))))

(defmethod paint-cairo ((n text-node) chain)
  (paint-glyph-run n chain (content n)))
(defmethod paint-cairo ((n field) chain)
  (paint-glyph-run n chain (content n)))

(defun paint-glyph-run (n chain text)
  (when (plusp (length text))
    (let* ((st (styled n chain (hovered n)))
           (fpx (or (font-px n) (pine.style:st-font-px st) *default-font-px*)))
      (cairo:select-font-face *cairo-font* :normal (if (pine.style:st-bold st) :bold :normal))
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

(defmethod paint-cairo ((n separator) chain)
  (declare (ignore chain))
  (multiple-value-bind (x y w h) (node-rect n)
    (set-face-rgb (or (face n) :comment))
    (cairo:set-line-width 1d0)
    (cairo:move-to (float x 1d0) (float (+ y (/ h 2d0)) 1d0))
    (cairo:line-to (float (+ x w) 1d0) (float (+ y (/ h 2d0)) 1d0))
    (cairo:stroke)))

(defmethod paint-cairo ((n slider) chain)
  (declare (ignore chain))
  (multiple-value-bind (x y w h) (node-rect n)
    (let* ((frac (slider-fraction n))
           (th 10) (ty (+ y (floor (- h th) 2)))
           (cls (node-classes n))
           (fill-role (if (member "bri" cls :test #'string=) :yellow :accent))
           (fillw (max th (round (* frac w)))))
      (rounded-rect x ty w th (/ th 2.0)) (set-hex (pine.buffer:color :bg)) (cairo:fill-path)
      (when (plusp frac)
        (rounded-rect x ty fillw th (/ th 2.0))
        (set-hex (pine.buffer:color fill-role)) (cairo:fill-path))
      (let ((kx (max (+ x 7) (min (- (+ x w) 7) (+ x (round (* frac w)))))))
        (cairo:new-sub-path)
        (cairo:arc (float kx 1d0) (float (+ ty (/ th 2.0)) 1d0) 7d0 0d0 (* 2d0 pi))
        (set-hex (pine.buffer:color :fg)) (cairo:fill-path)))))

(defmethod paint-cairo ((n ring) chain)
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
  (when (node n) (paint-children n chain (list (node n)))))

(defmethod paint-cairo ((n picture) chain)
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

;;;; A window (a buffer or terminal rendered as rows) blits through the shared
;;;; painter, offset to the node's rect. One leaf, the whole text area.

(defun cairo-cell-metrics (fpx)
  (cairo:select-font-face *cairo-font* :normal :normal)
  (cairo:set-font-size (float fpx 1d0))
  (multiple-value-bind (xb yb w h xadv) (cairo:text-extents "MMMMMMMMMM")
    (declare (ignore xb yb w h))
    (let ((fe (cairo:get-font-extents)))
      (values (/ (max xadv 1d0) 10d0) (cairo:font-height fe) (cairo:font-ascent fe)))))

(defmethod paint-cairo ((n window) chain)
  (declare (ignore chain))
  (multiple-value-bind (x y w h) (node-rect n)
    (destructuring-bind (br bg bb) (pine.buffer:face-bg :window)
      (cairo:set-source-rgb (/ br 255.0) (/ bg 255.0) (/ bb 255.0)))
    (cairo:rectangle (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
    (cairo:fill-path)
    (let ((fpx (or (font-px n) *default-font-px*)))
      (multiple-value-bind (cw ch asc) (cairo-cell-metrics fpx)
        (cairo:save)
        (cairo:rectangle (float x 1d0) (float y 1d0) (float w 1d0) (float h 1d0))
        (cairo:clip)
        (cairo:translate 0d0 (float y 1d0))
        (pine.surface:paint-rows (window-rows n) cw ch asc (float x 1d0))
        ;; the point (a hollow caret), when this window carries one
        (when (>= (window-crow n) 0)
          (destructuring-bind (br bg bb) (pine.buffer:face-bg :cursor)
            (cairo:set-source-rgb (/ br 255.0) (/ bg 255.0) (/ bb 255.0)))
          (cairo:rectangle (+ (float x 1d0) (* (window-ccol n) cw)) (* (window-crow n) ch) cw ch)
          (cairo:set-line-width 1.5d0) (cairo:stroke))
        (cairo:restore)))))

;;;; Calendar: a month grid. Row 0 the month/year title, row 1 the weekday
;;;; header, rows 2..7 the day cells; the current day gets an accent pill.

(defun %days-in-month (mo y)
  (if (= mo 2)
      (if (or (and (zerop (mod y 4)) (plusp (mod y 100))) (zerop (mod y 400))) 29 28)
      (aref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- mo))))

(defun %first-dow (mo y)
  (multiple-value-bind (s m h d dm yr dow)
      (decode-universal-time (encode-universal-time 0 0 12 1 mo y))
    (declare (ignore s m h d dm yr))
    (mod (1+ dow) 7)))                                  ; 0 = Sunday column

(defparameter +cal-months+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defmethod measure ((n calendar) aw ah)
  (declare (ignore aw ah))
  (let* ((fpx (or (font-px n) *default-font-px*))
         (cw (+ 10 (nth-value 0 (%text-size "00" fpx))))
         (ch (+ 8 (%line-h fpx))))
    (values (* 7 cw) (* 8 ch))))

(defun draw-cell-text (s cx cy cw ch role &optional bold)
  (cairo:select-font-face *cairo-font* :normal (if bold :bold :normal))
  (multiple-value-bind (r g b) (pine.buffer:hex-rgb (pine.buffer:color role))
    (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)))
  (multiple-value-bind (xb yb tw th ax) (cairo:text-extents s)
    (declare (ignore xb yb tw th))
    (let* ((fe (cairo:get-font-extents)) (asc (cairo:font-ascent fe))
           (lh (+ asc (cairo:font-descent fe))))
      (cairo:move-to (float (+ cx (/ (- cw ax) 2.0)) 1d0)
                     (float (+ cy (/ (- ch lh) 2.0) asc) 1d0))
      (cairo:show-text s))))

(defmethod paint-cairo ((n calendar) chain)
  (declare (ignore chain))
  (multiple-value-bind (x y w h) (node-rect n)
    (let* ((fpx (or (font-px n) *default-font-px*))
           (cw (/ w 7.0)) (ch (/ h 8.0))
           (year (cal-year n)) (mo (cal-month n)) (day (cal-day n))
           (ndays (%days-in-month mo year)) (fdow (%first-dow mo year)))
      (cairo:set-font-size (float fpx 1d0))
      (draw-cell-text (format nil "~a ~d" (aref +cal-months+ (1- mo)) year)
                      x y w ch :accent t)
      (loop for lbl in '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa") for col from 0
            do (draw-cell-text lbl (+ x (* col cw)) (+ y ch) cw ch :fg-dim))
      (loop for d from 1 to ndays
            for pos = (+ fdow (1- d))
            for cx = (+ x (* (mod pos 7) cw))
            for cy = (+ y (* (+ 2 (floor pos 7)) ch))
            do (when (= d day)
                 (rounded-rect (+ cx 2) (+ cy 2) (- cw 4) (- ch 4) 6)
                 (set-hex (pine.buffer:color :accent)) (cairo:fill-path))
               (draw-cell-text (princ-to-string d) cx cy cw ch
                               (if (= d day) :accent-fg :fg))))))

;;;; Render entry: a node tree -> a PNG. This is the eyes for the backend --
;;;; render a real panel headless and look at it.

(defmacro with-cairo-layout (&body body)
  "Bind the dynamic state the cairo layout pass needs: the theme font, the
*text-size* hook (pixel measurement), and a fresh per-render style cache."
  `(let ((*cairo-font* (pine.buffer:metric :font "Maple Mono NF"))
         (*text-size* #'cairo-text-size)
         (*style-cache* (make-hash-table :test 'eq)))
     ,@body))

(defun measure-tree (node avail-w)
  "Fold styles and measure NODE under a throwaway context; (values w h). Use
inside WITH-CAIRO-LAYOUT."
  (let ((s (cairo:create-image-surface :argb32 8 8)))
    (cairo:with-context ((cairo:create-context s))
      (apply-styles! node nil)
      (measure node avail-w 100000))))

(defun measure-scratch (node avail-w)
  (measure-tree node avail-w))

(defun paint-tree (node width height)
  "Measure/arrange NODE to fill WIDTH x HEIGHT and paint it to the bound cairo
context. For a fixed-size surface (a layer surface the compositor sized). The
caller sets up WITH-CAIRO-LAYOUT, the context, and any wallpaper/clear."
  (apply-styles! node nil)
  (measure node width height)
  (arrange node 0 0 width height)
  (paint-cairo node nil))

(defun render-tree-to-png (node path &key (pad 20) (avail-w 420) (bg :bg-alt))
  "Render the widget NODE tree to a PNG at PATH, on a BG palette-role wallpaper
so the glass surfaces read. Returns (:w W :h H :path PATH)."
  (with-cairo-layout
    (multiple-value-bind (mw mh) (measure-tree node avail-w)
      (let* ((w (+ mw (* 2 pad))) (h (+ mh (* 2 pad)))
             (surface (cairo:create-image-surface :argb32 w h)))
        (cairo:with-context ((cairo:create-context surface))
          (multiple-value-bind (r g b) (pine.buffer:hex-rgb (pine.buffer:color bg))
            (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)) (cairo:paint))
          (arrange node pad pad mw mh)
          (paint-cairo node nil))
        (cairo:surface-write-to-png surface path)
        (list :w w :h h :path path)))))
