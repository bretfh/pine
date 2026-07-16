(defpackage #:pine.palette
  (:use #:cl)
  (:export #:candidate #:make-candidate #:cand-label #:cand-annot #:cand-type #:cand-value
           #:register-source #:source-candidates #:filter-candidates
           #:register-action #:candidate-actions
           #:palette-tree))
(in-package #:pine.palette)

;;;; A completion facility as data: named sources produce annotated candidates,
;;;; a filter narrows them, and per-type actions (embark) act on the selected
;;;; one. palette-tree renders the whole thing as a widget tree, so completion is
;;;; a surface built from the same language as everything else.

(defstruct (candidate (:conc-name cand-))
  label (annot "") (type :item) value)

;;;; Sources: name -> a function of the query returning a list of candidates.

(defvar *sources* (make-hash-table :test 'eq))
(defun register-source (name fn) (setf (gethash name *sources*) fn))
(defun source-candidates (name &optional (query ""))
  (let ((fn (gethash name *sources*)))
    (if fn (funcall fn query) nil)))

(defun filter-candidates (candidates query)
  "Keep candidates whose label contains QUERY (case-insensitive subsequence)."
  (if (zerop (length query))
      candidates
      (let ((q (string-downcase query)))
        (remove-if-not (lambda (c) (search q (string-downcase (cand-label c)))) candidates))))

;;;; Actions (embark): candidate type -> list of (key-label . function-of-value).

(defvar *actions* (make-hash-table :test 'eq))
(defun register-action (type key fn) (push (cons key fn) (gethash type *actions*)))
(defun candidate-actions (cand)
  (and cand (gethash (cand-type cand) *actions*)))

;;;; Rendering: the palette as a widget tree (vertico + marginalia + embark).

(defun %type-glyph (type)
  (case type
    (:command #x0F0493) (:buffer #x0F0219) (:file #x0F0210)
    (:window #x0F02D1) (:system #x0F0709) (t #x0F0766)))

(defun %row (cand selected)
  (pine.layout:row :class (if selected "nm-row sel" "nm-row") :align :center :spacing 12
    (pine.layout:icon (%type-glyph (cand-type cand)) :class "nm-sig")
    (pine.layout:label (cand-label cand) :expand 1
                       :class (if selected "nm-name active" "nm-name"))
    (pine.layout:label (cand-annot cand) :class "nm-lock")))

(defun palette-tree (candidates &key (query "") (selected 0) (title "1247 items"))
  "Build the palette widget tree for CANDIDATES, QUERY typed so far, SELECTED
index highlighted. The action bar shows the embark actions of the selected one."
  (let* ((sel (and candidates (nth (max 0 (min selected (1- (length candidates)))) candidates)))
         (acts (candidate-actions sel)))
    (pine.layout:column :class "netmenu" :align :stretch
      (pine.layout:row :class "nm-card nm-head" :align :center :spacing 10
        (pine.layout:icon #x0F0349 :class "nm-head-ico")
        (pine.layout:label query :class "nm-title")
        (pine.layout:label "█" :class "nm-title")
        (pine.layout:label title :class "nm-subhead"))
      (apply #'pine.layout:column :class "nm-card nm-list-card" :align :stretch :spacing 3
        (loop for c in candidates for i from 0 collect (%row c (= i selected))))
      (apply #'pine.layout:row :class "nm-actions" :align :center :spacing 16
        (pine.layout:label "RET run" :class "nm-name active")
        (append
         (loop for (k . nil) in acts collect (pine.layout:label k :class "nm-subhead"))
         (list (pine.layout:gap :expand 1)
               (pine.layout:label "embark" :class "nm-sig hi")))))))
