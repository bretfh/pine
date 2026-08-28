(defpackage #:pine/ui/grid
  (:use #:cl)
  (:local-nicknames (#:face #:pine/ui/face))
  (:export #:medium #:grid #:make-grid #:cols #:flat #:clip
           #:put #:put-bg #:put-rgb #:blit #:with-clip #:by-row #:ink))
(in-package #:pine/ui/grid)

(defclass medium () ()
  (:documentation "What a widget is painted onto. PAINT specializes on it, so there
is one paint generic and not one per back end: a grid of character cells here, a
cairo canvas in pine/canvas, and anything else somebody writes."))

(defclass grid (medium)
  ((cols  :initarg :cols  :reader cols)
   (lines :initarg :lines :reader lines)
   (cells :initarg :cells :reader flat)
   (clip  :initform nil   :accessor clip))
  (:documentation "A grid of character cells: ten slots each, flat, because a paint
touches every one of them."))

(defun ink (it)
  "(values fr fg fb br bg bb attr) for a face designator: a face name resolved
through the theme, or an already-worked-out (FG BG ATTR). A face with no foreground
of its own falls back to the plain one -- that fallback belongs here, where a cell
is actually being coloured, and not in what a face is."
  (flet ((plain () (or (face:unhex (face:fg (face:in-force face:+plain+)))
                       '(205 214 244))))
    (if (consp it)
        (destructuring-bind (fg bg attr) it
          (let ((f (or fg (plain))))
            (values (first f) (second f) (third f)
                    (if bg (first bg) -1) (if bg (second bg) -1)
                    (if bg (third bg) -1)
                    (or attr 0))))
        (let* ((f (face:in-force it))
               (fg (or (and f (face:unhex (face:fg f))) (plain)))
               (bg (and f (face:unhex (face:bg f)))))
          (values (first fg) (second fg) (third fg)
                  (if bg (first bg) -1) (if bg (second bg) -1)
                  (if bg (third bg) -1)
                  (face:attrs f))))))

(defun make-grid (cols lines)
  (let* ((n (* cols lines)) (v (make-array (* 10 n))))
    (dotimes (i n)
      (let ((off (* 10 i)))
        (setf (svref v off) (floor i cols)
              (svref v (+ off 1)) (mod i cols)
              (svref v (+ off 2)) 32
              (svref v (+ off 3)) 205 (svref v (+ off 4)) 214 (svref v (+ off 5)) 244
              (svref v (+ off 6)) -1 (svref v (+ off 7)) -1 (svref v (+ off 8)) -1
              (svref v (+ off 9)) 0)))
    (make-instance 'grid :cols cols :lines lines :cells v)))

(declaim (inline %offset %inside))

(defun %offset (g line col) (* 10 (+ (* line (cols g)) col)))

(defun %inside (g line col)
  (and (>= line 0) (< line (lines g)) (>= col 0) (< col (cols g))
       (let ((c (clip g)))
         (or (null c)
             (and (>= col (first c)) (< col (third c))
                  (>= line (second c)) (< line (fourth c)))))))

(defmacro with-clip ((g x0 y0 x1 y1) &body body)
  "Keep writes inside the rect [X0 X1) x [Y0 Y1)."
  (let ((it (gensym)) (had (gensym)))
    `(let* ((,it ,g) (,had (clip ,it)))
       (setf (clip ,it) (list ,x0 ,y0 ,x1 ,y1))
       (unwind-protect (progn ,@body) (setf (clip ,it) ,had)))))

(defun put (g line col ch it)
  (when (%inside g line col)
    (multiple-value-bind (fr fg fb br bg bb attr) (ink it)
      (let ((off (%offset g line col)) (v (flat g)))
        (setf (svref v (+ off 2)) (char-code ch)
              (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
              (svref v (+ off 9)) attr)
        (when (>= br 0)
          (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg
                (svref v (+ off 8)) bb))))))

(defun put-bg (g line col br bg bb)
  (when (%inside g line col)
    (let ((off (%offset g line col)) (v (flat g)))
      (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))

(defun put-rgb (g line col ch fr fg fb br bg bb attr)
  (when (%inside g line col)
    (let ((off (%offset g line col)) (v (flat g)))
      (setf (svref v (+ off 2)) (char-code ch)
            (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
            (svref v (+ off 9)) (or attr 0))
      (when (and (integerp br) (>= br 0))
        (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg
              (svref v (+ off 8)) bb)))))

(defun blit (g line col0 text runs)
  "Blit one (TEXT . RUNS) row in at LINE from COL0. A run is (col fr fg fb br bg bb
attr) and its colours reach to the next run's column."
  (loop :for (run . more) :on runs
        :do (destructuring-bind (col fr fg fb br bg bb attr) run
              (let ((end (if more (car (first more)) (length text))))
                (loop :for c :from col :below (min end (length text))
                      :do (put-rgb g line (+ col0 c) (char text c)
                                   fr fg fb br bg bb attr))))))

(defun by-row (g)
  "The grid scanned back out as (TEXT . RUNS) rows -- the shape that crosses the
wire, so one format serves the frame, the chrome and a buffer's lines."
  (let ((cols (cols g)) (v (flat g)))
    (loop :for line :from 0 :below (lines g)
          :collect
          (let ((text (make-string cols :initial-element #\space))
                (runs nil) (had nil))
            (dotimes (c cols)
              (let* ((off (%offset g line c))
                     (now (list (svref v (+ off 3)) (svref v (+ off 4))
                                (svref v (+ off 5)) (svref v (+ off 6))
                                (svref v (+ off 7)) (svref v (+ off 8))
                                (svref v (+ off 9)))))
                (setf (char text c) (code-char (svref v (+ off 2))))
                (unless (equal now had)
                  (push (list* c now) runs)
                  (setf had now))))
            (cons text (nreverse runs))))))
