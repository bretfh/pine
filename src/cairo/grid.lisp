(defpackage #:pine.cairo.grid
  (:use #:cl)
  (:export #:paint-cell-grid #:paint-rows #:render-grid-to-png #:*font-family*))

(in-package #:pine.cairo.grid)

(defparameter *font-family* "Maple Mono NF")

(defun %cell-metrics (font-px)
  "cell-w cell-h ascent for *font-family* at FONT-PX in the current context."
  (cairo:select-font-face *font-family* :normal :normal)
  (cairo:set-font-size font-px)
  (multiple-value-bind (xb yb w h xadv) (cairo:text-extents "MMMMMMMMMM")
    (declare (ignore xb yb w h))
    (let ((fe (cairo:get-font-extents)))
      (values (/ (max xadv 1d0) 10d0) (cairo:font-height fe) (cairo:font-ascent fe)))))

(defun render-grid-to-png (rows path &key (font-px 15d0) (x0 6d0))
  "Paint wire ROWS (pine.ui.render:frame->rows output) to a PNG at PATH. Headless
eyes for the editor frame: no window, an offscreen cairo image surface."
  (multiple-value-bind (cell-w cell-h ascent)
      (cairo:with-png-file ("/tmp/pine-metrics.png" :argb32 8 8) (%cell-metrics font-px))
    (let* ((cols (reduce #'max rows :initial-value 1
                         :key (lambda (r) (length (car r)))))
           (w (max 1 (ceiling (+ (* cols cell-w) (* 2 x0)))))
           (h (max 1 (ceiling (* (length rows) cell-h)))))
      (cairo:with-png-file (path :argb32 w h)
        (destructuring-bind (r g b) (pine.ui.face:face-bg :window)
          (cairo:set-source-rgb (/ r 255d0) (/ g 255d0) (/ b 255d0)))
        (cairo:paint)
        (cairo:select-font-face *font-family* :normal :normal)
        (cairo:set-font-size font-px)
        (paint-rows rows cell-w cell-h ascent x0))
      path)))

(defun %select-attr-font (attr)
  (cairo:select-font-face *font-family*
                          (if (logtest attr 2) :italic :normal)
                          (if (logtest attr 1) :bold :normal)))

(defun paint-rows (grid cell-w cell-h ascent x0)
  ;; backgrounds
  (loop for row in grid for r from 0
        for text = (car row) for runs = (cdr row) do
    (loop for (run . more) on runs do
      (destructuring-bind (col fr fg fb br bg bb attr) run
        (declare (ignore fr fg fb attr))
        (let ((end (if more (car (first more)) (length text))))
          (when (>= br 0)
            (cairo:set-source-rgb (/ br 255d0) (/ bg 255d0) (/ bb 255d0))
            (cairo:rectangle (+ x0 (* col cell-w)) (* r cell-h)
                             (* (- end col) cell-w) cell-h)
            (cairo:fill-path))))))
  ;; text runs
  (loop for row in grid for r from 0
        for text = (car row) for runs = (cdr row)
        for len = (length text) do
    (loop for (run . more) on runs do
      (destructuring-bind (col fr fg fb br bg bb attr) run
        (declare (ignore br bg bb))
        (let ((end (min (if more (car (first more)) len) len))
              (attr (or attr 0)))
          (when (< col end)
            (%select-attr-font attr)
            (cairo:set-source-rgb (/ fr 255d0) (/ fg 255d0) (/ fb 255d0))
            (let ((x (+ x0 (* col cell-w))) (y (+ (* r cell-h) ascent)))
              (cairo:move-to x y)
              (cairo:show-text (subseq text col end))
              (when (logtest attr 4)
                (cairo:rectangle x (+ y 1d0) (* (- end col) cell-w) 1d0)
                (cairo:fill-path)))))))))

(defun paint-cell-grid (cells n cell-w cell-h ascent x0)
  (loop for i from 0 below n by 10
        for br = (svref cells (+ i 6))
        when (and (integerp br) (>= br 0))
          do (cairo:set-source-rgb (/ br 255d0)
                                   (/ (svref cells (+ i 7)) 255d0)
                                   (/ (svref cells (+ i 8)) 255d0))
             (cairo:rectangle (+ x0 (* (svref cells (+ i 1)) cell-w))
                              (* (svref cells i) cell-h) cell-w cell-h)
             (cairo:fill-path))
  (loop for i from 0 below n by 10
        for attr = (let ((a (svref cells (+ i 9)))) (if (integerp a) a 0))
        do (%select-attr-font attr)
           (cairo:set-source-rgb (/ (svref cells (+ i 3)) 255d0)
                                 (/ (svref cells (+ i 4)) 255d0)
                                 (/ (svref cells (+ i 5)) 255d0))
           (let ((x (+ x0 (* (svref cells (+ i 1)) cell-w)))
                 (y (+ (* (svref cells i) cell-h) ascent)))
             (cairo:move-to x y)
             (cairo:show-text (string (code-char (svref cells (+ i 2)))))
             (when (logtest attr 4)
               (cairo:rectangle x (+ y 1d0) cell-w 1d0)
               (cairo:fill-path)))))

