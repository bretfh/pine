(defpackage #:pine.surface
  (:use #:cl)
  (:export #:surface #:surface-on-key #:surface-on-resize
           #:present #:paint-frame #:request-redraw #:surface-metrics
           #:paint-cell-grid #:paint-rows #:*font-family*))

(in-package #:pine.surface)

(defparameter *font-family* "Maple Mono NF")

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

(defclass surface ()
  ((on-key    :initarg :on-key    :accessor surface-on-key    :initform nil)
   (on-resize :initarg :on-resize :accessor surface-on-resize :initform nil)))

(defgeneric present (surface))
(defgeneric paint-frame (surface frame))
(defgeneric request-redraw (surface))
(defgeneric surface-metrics (surface)
  (:documentation "Values: cell-width cell-height cols rows."))
