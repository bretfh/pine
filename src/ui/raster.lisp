(defpackage #:pine.ui.raster
  (:use #:cl #:pine.ui.node)
  (:export #:%cell-off #:%face-cell-rgb #:%in-raster #:%with-clip #:blit-row #:make-raster #:raster #:raster-cells #:raster-clip #:raster-cols #:raster-p #:raster-put #:raster-put-bg #:raster-put-rgb #:raster-rows))

(in-package #:pine.ui.raster)

;;;; Faces -> cell colours

(defun %face-cell-rgb (designator)
  "(values fr fg fb br bg bb attr) for a face DESIGNATOR; bg -1 means none, attr
is the packed bold/italic/underline bits. DESIGNATOR is a face name resolved
through the active theme, or a precomputed (FG BG ATTR) tuple -- FG/BG (r g b)
lists or nil -- installed by RESOLVE-STYLES! for the cell render."
  (if (consp designator)
      (destructuring-bind (fg bg attr) designator
        (let ((f (or fg (pine.text.buffer:face-fg :default))))
          (values (first f) (second f) (third f)
                  (if bg (first bg) -1) (if bg (second bg) -1) (if bg (third bg) -1)
                  (or attr 0))))
      (let ((fg (pine.text.buffer:face-fg designator))
            (bg (pine.text.buffer:face-bg designator))
            (attr (let ((f (ignore-errors (pine.text.buffer:find-face designator))))
                    (if f (pine.text.buffer:face-attr-bits f) 0))))
        (values (first fg) (second fg) (third fg)
                (if bg (first bg) -1) (if bg (second bg) -1) (if bg (third bg) -1)
                attr))))


;;;; Raster -- a cell buffer widgets paint into. Cells are the flat 10-slot
;;;; [row col code fr fg fb br bg bb bold] format the surface painter draws.

(defstruct (raster (:constructor %make-raster)) cols rows cells (clip nil))

(defun make-raster (cols rows)
  (let* ((n (* cols rows)) (v (make-array (* 10 n))))
    (dotimes (i n)
      (let ((off (* 10 i)))
        (setf (svref v off) (floor i cols)
              (svref v (+ off 1)) (mod i cols)
              (svref v (+ off 2)) 32
              (svref v (+ off 3)) 205 (svref v (+ off 4)) 214 (svref v (+ off 5)) 244
              (svref v (+ off 6)) -1 (svref v (+ off 7)) -1 (svref v (+ off 8)) -1
              (svref v (+ off 9)) 0)))
    (%make-raster :cols cols :rows rows :cells v)))

(declaim (inline %cell-off %in-raster))
(defun %cell-off (r row col) (* 10 (+ (* row (raster-cols r)) col)))
(defun %in-raster (r row col)
  (and (>= row 0) (< row (raster-rows r)) (>= col 0) (< col (raster-cols r))
       (let ((c (raster-clip r)))
         (or (null c)
             (and (>= col (first c)) (< col (third c))
                  (>= row (second c)) (< row (fourth c)))))))

(defmacro %with-clip ((r x0 y0 x1 y1) &body body)
  "Restrict raster writes to the rect [X0 X1) x [Y0 Y1) within BODY."
  (let ((rr (gensym)) (old (gensym)))
    `(let* ((,rr ,r) (,old (raster-clip ,rr)))
       (setf (raster-clip ,rr) (list ,x0 ,y0 ,x1 ,y1))
       (unwind-protect (progn ,@body) (setf (raster-clip ,rr) ,old)))))

(defun raster-put (r row col ch face)
  (when (%in-raster r row col)
    (multiple-value-bind (fr fg fb br bg bb attr) (%face-cell-rgb face)
      (let ((off (%cell-off r row col)) (v (raster-cells r)))
        (setf (svref v (+ off 2)) (char-code ch)
              (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
              (svref v (+ off 9)) attr)
        (when (>= br 0)
          (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))))

(defun raster-put-bg (r row col br bg bb)
  (when (%in-raster r row col)
    (let ((off (%cell-off r row col)) (v (raster-cells r)))
      (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))

(defun raster-put-rgb (r row col ch fr fg fb br bg bb attr)
  "Write a cell with explicit colours (not a face), for blitting cell rows."
  (when (%in-raster r row col)
    (let ((off (%cell-off r row col)) (v (raster-cells r)))
      (setf (svref v (+ off 2)) (char-code ch)
            (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
            (svref v (+ off 9)) (or attr 0))
      (when (and (integerp br) (>= br 0))
        (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb)))))

(defun blit-row (r row col0 text runs)
  "Blit one (TEXT . RUNS) row into raster R at ROW, starting COL0. Each run is
(col fr fg fb br bg bb attr), its colours extending to the next run's col."
  (loop for (run . more) on runs do
    (destructuring-bind (col fr fg fb br bg bb attr) run
      (let ((end (if more (car (first more)) (length text))))
        (loop for c from col below (min end (length text))
              do (raster-put-rgb r row (+ col0 c) (char text c) fr fg fb br bg bb attr))))))
