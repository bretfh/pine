;;;; Headless product shots. Stage a clean editor scene (real code highlighted by
;;;; pine's own tree-sitter + theme, a proper completion box, an eval result in
;;;; the echo line) and render it through the same cairo path the wayflan client
;;;; uses. No window, no daemon, no display.
;;;;   guix shell --rebuild-cache -m manifest.scm -- \
;;;;     sh -c 'LD_LIBRARY_PATH=$GUIX_ENVIRONMENT/lib sbcl --non-interactive \
;;;;       --eval "(asdf:load-system :pine/cairo)" --load www/shots.lisp'

(in-package :cl-user)

(pine.cairo::ensure-env)
(pine.cairo::seed-cells)
(defvar *rt* (pine.ts:make-ts-runtime))

(defun face->run (col face &optional (attr 0))
  (destructuring-bind (r g b) (pine.buffer:face-fg face)
    (list col r g b -1 -1 -1 attr)))

(defun face-attr (face)
  (let ((f (pine.buffer:find-face face)))
    (cond ((and f (pine.buffer:italic f)) 2)
          ((and f (pine.buffer:bold f)) 1)
          (t 0))))

(defun split-lines (text)
  (loop with start = 0
        for nl = (position #\Newline text :start start)
        collect (subseq text start (or nl (length text)))
        while nl do (setf start (1+ nl))))

(defun code-rows (text)
  (let* ((ps (pine.ts:make-parse-state *rt* :commonlisp))
         (lines (coerce (split-lines text) 'vector))
         (byline (make-hash-table)))
    (pine.ts:parse-full! ps text)
    (dolist (h (pine.ts:parse-highlights ps text))
      (destructuring-bind (l sc ec face) h
        (push (list sc ec face) (gethash l byline))))
    (loop for i from 0 below (length lines)
          for line = (aref lines i)
          for len = (length line)
          for spans = (sort (copy-list (gethash i byline)) #'< :key #'first)
          collect (cons line
                        (let ((runs (list (face->run 0 :default))))
                          (dolist (s spans)
                            (destructuring-bind (sc ec face) s
                              (let ((sc (max 0 (min sc len))) (ec (max 0 (min ec len))))
                                (when (< sc ec)
                                  (push (face->run sc face (face-attr face)) runs)
                                  (push (face->run ec :default) runs)))))
                          (nreverse runs))))))

;;;; Widgets faked at the row level, but each is a clean uniform block so the
;;;; background paints as a solid rectangle (no ragged edges).

(defun pad (s n) (if (< (length s) n)
                     (concatenate 'string s (make-string (- n (length s)) :initial-element #\Space))
                     (subseq s 0 n)))

(defparameter +popw+ 40)

(defparameter +popindent+ 18)

(defun comp-row (label type selected)
  (let* ((box (pad (format nil "  ~a~a" (pad label 16) type) +popw+))
         (s (concatenate 'string (make-string +popindent+ :initial-element #\Space) box))
         (fg (if selected '(28 24 30) '(200 202 216)))     ; dark on the warm bar / light on panel
         (bg (if selected '(243 192 154) '(59 56 65))))
    (cons s (list (face->run 0 :default)
                  (append (list +popindent+) fg bg (list 0))))))

(defvar *code*
  "(defpackage :orbit
  (:use :cl)
  (:export #:step! #:energy))
(in-package :orbit)

;; a tiny leapfrog n-body core -- see design/perf.org for the numbers
(defstruct body (x 0d0) (y 0d0) (vx 0d0) (vy 0d0) (mass 1d0))

(defparameter *dt* 0.01d0
  \"seconds advanced per integration step\")

(defun kick (a b)
  (let ((dx (- (body-x b) (body-x a)))
        (dy (- (body-y b) (body-y a))))
    (/ *dt* (expt (+ (* dx dx) (* dy dy)) 1.5d0))))

(defun step! (bodies)
  \"advance every body one step, then drift\"
  (declare (type simple-vector bodies))")

(defun editor-scene ()
  (append
   (code-rows *code*)
   (list
    ;; the line being typed, with the block cursor sitting after (body-
    (cons "    (incf (body-vx a) (body-"
          (list (face->run 0 :default) (face->run 5 :keyword) (face->run 9 :default)
                (face->run 10 :function-call) (face->run 17 :default)
                (face->run 19 :variable) (face->run 20 :default)
                (face->run 22 :function-call)))
    ;; completion box, flush under the cursor column
    (comp-row "body-x"    "double-float" t)
    (comp-row "body-y"    "double-float" nil)
    (comp-row "body-vx"   "double-float" nil)
    (comp-row "body-vy"   "double-float" nil)
    (comp-row "body-mass" "double-float" nil))
   (loop repeat 2 collect (cons "" (list (face->run 0 :default))))
   (list
    (cons (pad " orbit.lisp        Lisp        (18,26)        wayflan-cairo" 78)
          (list (list 0 254 222 255 103 80 114 0)))
    (cons " C-x C-e   =>   -0.169083134"
          (list (list 0 63 196 137 -1 -1 -1 0)
                (list 15 205 214 244 -1 -1 -1 0))))))

(defun render-scene (rows path w h &key (crow 18) (ccol 26))
  (let ((sess (pine.editor::make-sess :rows rows :crow crow :ccol ccol))
        (builder (gethash "editor"
                          (symbol-value (find-symbol "*SURFACES*" :pine.desktop)))))
    (let ((pine.editor::*editor-session* sess))
      (pine.layout:with-cairo-layout
        (let ((surface (cairo:create-image-surface :argb32 w h))
              (tree (funcall builder nil)))
          (cairo:with-context ((cairo:create-context surface))
            (multiple-value-bind (r g b) (pine.buffer:hex-rgb (pine.buffer:color :bg))
              (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)) (cairo:paint))
            (pine.layout:paint-tree tree w h))
          (cairo:surface-write-to-png surface path))))
    (format t "~&wrote ~a  (~dx~d, ~d rows)~%" path w h (length rows))))

(render-scene (editor-scene) "/tmp/pine-shot-editor.png" 900 620 :crow 19 :ccol 28)
(sb-ext:exit)
