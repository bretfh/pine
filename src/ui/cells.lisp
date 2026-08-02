(defpackage #:pine.ui.cells
  (:use #:cl #:pine.ui.node #:pine.ui.raster)
  (:export #:class-names #:node-classes #:render #:rows-of))

(in-package #:pine.ui.cells)

;;;; The cell render: the same node tree the desktop paints in pixels,
;;;; rendered to styled cells for a buffer or the chrome. Styling is the ONE
;;;; resolution: a node's CSS classes through pine.ui.style (the desktop's
;;;; theme-rules), else its face name through the theme faces -- both terminate
;;;; in the same (fg bg attr) run values. The arranged tree rides along so any
;;;; rendered (line col) maps back to its node with no side table.

(defgeneric class-names (c)
  (:documentation "A :class value as a list of class-name strings.")
  (:method ((c null)) nil))

(defmethod class-names ((c symbol))
  (list (string-downcase (symbol-name c))))

(defmethod class-names ((c string))
  (remove "" (uiop:split-string c :separator '(#\space)) :test #'string=))

(defmethod class-names ((c cons))
  (mapcan #'class-names c))

(defun node-classes (n)
  (class-names (css-class n)))

(defun %scale-rgb (c)
  "pine.ui.style colours are 0..1; cells carry 0..255."
  (list (round (* 255 (first c))) (round (* 255 (second c))) (round (* 255 (third c)))))

(defun %node-cell-style (n full)
  "The (FG BG ATTR) cell tuple for N from CSS matched on the class chain FULL,
falling back per-part to N's face name; nil when CSS contributes nothing (the
face name, if any, then resolves as usual at paint)."
  (let* ((st (pine.ui.style:resolve full))
         (css-fg (pine.ui.style:st-fg st))
         (css-bg (pine.ui.style:st-bg st))
         (bold (pine.ui.style:st-bold st))
         (name (and (keywordp (face n)) (face n))))
    (when (or css-fg css-bg bold)
      (list (if css-fg (%scale-rgb css-fg) (and name (pine.ui.face:face-fg name)))
            (if css-bg (%scale-rgb css-bg) (and name (pine.ui.face:face-bg name)))
            (logior (if bold 1 0)
                    (if name
                        (pine.ui.face:face-attr-bits (pine.ui.face:find-face name))
                        0))))))

(defun resolve-styles! (root &optional chain)
  "Resolve every node's cell colours before a cell render: CSS classes win,
the node's face name fills the gaps, and the result lands in the face slot as a
precomputed (FG BG ATTR) tuple. Colour/weight only -- CSS box properties are
pixel values and do not apply to cell layout. A selected selectable resolves
with a \"sel\" class appended, so \".foo.sel\" rules style the selection. Runs
at build time; the rendered tree is read-only afterwards."
  (labels ((walk (n chain)
             (let* ((classes (append (node-classes n)
                                     (and (typep n 'selectable) (selectedp n)
                                          (list "sel"))))
                    (full (append chain (list classes))))
               (when classes
                 (let ((tuple (%node-cell-style n full)))
                   (when tuple (setf (face n) tuple))))
               (dolist (c (pine.ui.layout:nodes-of n)) (walk c full)))))
    (walk root chain))
  root)

(defun rows-of (r)
  "Scan raster R into rows (TEXT . RUNS), run = (col fr fg fb br bg bb attr) --
the same run format frame->rows ships on the wire, so one format serves the
frame, the chrome, and layout buffer rows."
  (let ((cols (raster-cols r)) (v (raster-cells r)))
    (loop for row from 0 below (raster-rows r) collect
      (let ((text (make-string cols :initial-element #\space)) (runs nil) (prev nil))
        (dotimes (c cols)
          (let* ((off (cell-offset r row c))
                 (style (list (svref v (+ off 3)) (svref v (+ off 4)) (svref v (+ off 5))
                              (svref v (+ off 6)) (svref v (+ off 7)) (svref v (+ off 8))
                              (svref v (+ off 9)))))
            (setf (char text c) (code-char (svref v (+ off 2))))
            (unless (equal style prev)
              (push (list* c style) runs)
              (setf prev style))))
        (cons text (nreverse runs))))))

(defun render (root width &key height selection)
  "Render ROOT to cells: flag SELECTION (the nth selectable), resolve styles,
lay out at WIDTH, paint, and return (values rows tree) -- rows in the wire row
format, TREE the arranged root whose rects make node-at a position->node map.
The returned tree is read-only: selection is an input here, never a mutation of
a published tree. Layout buffers use static children (vstack), not list-node
item functions, which build only at measure."
  (pine.ui.face:with-faces
   (let ((pine.ui.layout:*text-size* nil))
    (when selection
      (loop for s in (pine.ui.layout:collect-selectables root) for i from 0
            do (setf (selectedp s) (= i selection))))
    (resolve-styles! root)
    (multiple-value-bind (mw mh) (pine.ui.layout:measure root width (or height 1000))
      (declare (ignore mw))
      (let* ((h (max 1 (or height mh)))
             (r (make-raster width h)))
        (pine.ui.layout:arrange root 0 0 width h)
        (pine.ui.layout:paint root r)
        (values (rows-of r) root))))))
