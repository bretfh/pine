(in-package #:pine.de)

;;;; The cairo painter. An arranged widget tree (absolute pixel rects, from
;;;; pine.layout under *text-size* = %cairo-text-size) is drawn here: each node's
;;;; chrome (hover highlight, rounded/gradient fill) then its per-type content
;;;; (text at its font-px, a slider as a trough + highlight). This is the DE's
;;;; render half; the layer surfaces below feed it a context and a tree.

(defparameter *bg-alpha* 0.42d0
  "Panel translucency. The compositor's blur behind it is the glass.")
(defparameter *chrome-bg* "#232025"
  "The panel background colour, drawn at *bg-alpha*.")

(defun %rgb (hex)
  "The (values r g b) in 0..1 of a #rrggbb colour."
  (multiple-value-bind (r g b) (pine.layout:hex-rgb hex)
    (values (/ r 255d0) (/ g 255d0) (/ b 255d0))))

(defun %rounded-rect (x y w h r)
  "Append a rounded-rectangle sub-path (a plain rect when R <= 0)."
  (let ((r (min r (/ w 2d0) (/ h 2d0))))
    (if (<= r 0d0)
        (cairo:rectangle x y w h)
        (let ((q (/ pi 2)))
          (cairo:new-sub-path)
          (cairo:arc (- (+ x w) r) (+ y r)       r (- q) 0d0)
          (cairo:arc (- (+ x w) r) (- (+ y h) r) r 0d0 q)
          (cairo:arc (+ x r)       (- (+ y h) r) r q pi)
          (cairo:arc (+ x r)       (+ y r)       r pi (* 3 q))
          (cairo:close-path)))))

(defun %cairo-text-size (text font-px)
  "Text's (values w h) in pixels at FONT-PX -- pine.layout's *text-size* hook."
  (cairo:set-font-size (coerce font-px 'double-float))
  (multiple-value-bind (xb yb w h xadv) (cairo:text-extents text)
    (declare (ignore xb yb w h))
    (values (ceiling (max xadv 1))
            (ceiling (+ (cairo:font-height (cairo:get-font-extents)) 4)))))

(defun %face-rgb01 (face)
  "The foreground colour of FACE in 0..1, or a light default."
  (let ((f (and face (ignore-errors (pine.buffer:find-face face)))))
    (if (and f (pine.buffer:fg f))
        (%rgb (pine.buffer:fg f))
        (values 0.94d0 0.83d0 0.77d0))))

(defun %px-bounds (n)
  "The arranged node N's (values x y w h) as doubles."
  (values (coerce (pine.layout:start-col n) 'double-float)
          (coerce (pine.layout:start-line n) 'double-float)
          (coerce (- (pine.layout:end-col n) (pine.layout:start-col n)) 'double-float)
          (coerce (1+ (- (pine.layout:end-line n) (pine.layout:start-line n))) 'double-float)))

(defun paint-cairo (node)
  "Paint the arranged NODE tree: chrome, then per-type content, then children."
  (when node
    (multiple-value-bind (x y w h) (%px-bounds node)
      (when (pine.layout:hovered node)
        (multiple-value-bind (r g b) (%rgb "#5b595e")
          (cairo:set-source-rgba r g b 0.9d0))
        (%rounded-rect x y w h 6d0) (cairo:fill-path))
      (let ((fill (pine.layout:fill-of node)) (gr (pine.layout:grad node))
            (rad (coerce (pine.layout:radius node) 'double-float)))
        (when (or fill gr)
          (%rounded-rect x y w h rad)
          (if gr
              (let ((p (cairo:create-linear-pattern x y (+ x w) (+ y h))))
                (multiple-value-bind (r g b) (%rgb (or fill gr))
                  (cairo:pattern-add-color-stop-rgb p 0d0 r g b))
                (multiple-value-bind (r g b) (%rgb gr)
                  (cairo:pattern-add-color-stop-rgb p 1d0 r g b))
                (cairo:set-source p) (cairo:fill-path))
              (multiple-value-bind (r g b) (%rgb fill)
                (cairo:set-source-rgb r g b) (cairo:fill-path)))))
      (typecase node
        (pine.layout:text-node
         (let ((s (pine.layout:content node)))
           (when (plusp (length s))
             (cairo:set-font-size
              (coerce (or (pine.layout:font-px node) pine.layout:*default-font-px*)
                      'double-float))
             (multiple-value-bind (r g b) (%face-rgb01 (pine.layout:face node))
               (cairo:set-source-rgb r g b))
             (cairo:move-to x (+ y (cairo:font-ascent (cairo:get-font-extents))))
             (cairo:show-text s))))
        (pine.layout:slider
         (let* ((frac (pine.layout:slider-fraction node))
                (ty (+ y (/ h 2d0) -4d0)))
           (multiple-value-bind (r g b) (%rgb "#3b393e") (cairo:set-source-rgb r g b))
           (%rounded-rect x ty w 8d0 4d0) (cairo:fill-path)
           (multiple-value-bind (r g b) (%rgb "#675072") (cairo:set-source-rgb r g b))
           (%rounded-rect x ty (* w frac) 8d0 4d0) (cairo:fill-path)))))
    (dolist (c (pine.layout:nodes-of node)) (paint-cairo c))))
