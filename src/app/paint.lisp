(defpackage #:pine.desktop-app
  (:use #:cl #:gtk4)
  (:export #:run-desktop-app))

(in-package #:pine.desktop-app)

;;;; The desktop app's cairo painter. An arranged widget tree (absolute pixel
;;;; rects, from pine.layout under *text-size* = %cairo-text-size) is drawn here:
;;;; each node's chrome (hover highlight, rounded/gradient fill) then its per-type
;;;; content (text at its font-px, a slider as a trough + highlight). The trees
;;;; come from the daemon over the wire; this process only paints them.

(defun %rgb (hex)
  "The (values r g b) in 0..1 of a #rrggbb colour."
  (multiple-value-bind (r g b) (pine.buffer:hex-rgb hex)
    (values (/ r 255d0) (/ g 255d0) (/ b 255d0))))

(defun %role (role)
  "The (values r g b) in 0..1 of a palette ROLE in the active theme."
  (%rgb (pine.buffer:color role)))

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
  "The foreground colour of FACE in 0..1, from the active theme."
  (destructuring-bind (r g b) (pine.buffer:face-fg face)
    (values (/ r 255d0) (/ g 255d0) (/ b 255d0))))

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
        (multiple-value-bind (r g b) (%role 'bg-active)
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
           (multiple-value-bind (r g b) (%role 'bg-alt) (cairo:set-source-rgb r g b))
           (%rounded-rect x ty w 8d0 4d0) (cairo:fill-path)
           (multiple-value-bind (r g b) (%role 'accent) (cairo:set-source-rgb r g b))
           (%rounded-rect x ty (* w frac) 8d0 4d0) (cairo:fill-path)))
        (pine.layout:ring
         (let* ((th (coerce (pine.layout:thickness node) 'double-float))
                (rad (- (/ (min w h) 2d0) (/ th 2d0) 1d0))
                (cx (+ x (/ w 2d0))) (cy (+ y (/ h 2d0)))
                (start (* -0.5d0 pi))
                (end (+ start (* (pine.layout:ring-fraction node) 2d0 pi))))
           (cairo:set-line-width th)
           (cairo:set-line-cap :round)
           (multiple-value-bind (r g b) (%face-rgb01 (pine.layout:track-face node))
             (cairo:set-source-rgb r g b))
           (cairo:new-sub-path) (cairo:arc cx cy rad 0d0 (* 2d0 pi)) (cairo:stroke)
           (multiple-value-bind (r g b) (%face-rgb01 (pine.layout:arc-face node))
             (cairo:set-source-rgb r g b))
           (cairo:new-sub-path) (cairo:arc cx cy rad start end) (cairo:stroke)))))
    (dolist (c (pine.layout:nodes-of node)) (paint-cairo c))))
