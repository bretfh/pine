(defpackage #:pine.editor.overwrite
  (:use #:cl)
  (:export #:mount-mode))

(in-package #:pine.editor.overwrite)
(named-readtables:in-readtable pine.path:syntax)

;;;; overwrite: a minor mode that claims :insert. Typed text takes the place of
;;;; what is under point instead of pushing it along, which is one line written
;;;; back and point moved on -- a map of writes, the way any mode changes a
;;;; verb.

(defun %overwritten (line col text)
  "LINE with TEXT written over the characters at COL."
  (let ((before (subseq line 0 (min col (length line))))
        (after (subseq line (min (length line) (+ col (length text))))))
    (concatenate 'string before text after)))

(defun mount-mode ()
  (pine.ns:write
   /minor/overwrite
   {:precedence 10
    :indicator "Ovwrt"
    :on {:insert
         (pine.data:fn [buf text]
           (let* ((point (pine.ns:read (pine.buf:at buf :point)))
                  (line (or (fset:lookup point 0) 0))
                  (col (or (fset:lookup point 1) 0))
                  (at (pine.path:path (pine.buf:at buf :line)
                                      (princ-to-string line)))
                  (had (or (pine.ns:read at) "")))
             (fset:map (at (%overwritten had col text))
                       ((pine.buf:at buf :point)
                        (fset:seq line (+ col (length text)))))))}}))
