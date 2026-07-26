(defpackage #:pine.ui.build
  (:use #:cl #:pine.ui.node)
  (:shadow #:centerbox #:ring)
  (:export #:boxed #:cal #:centerbox #:centered #:choice #:column #:gap #:icon #:label #:meter #:pic #:ring #:row #:rows #:rule #:viewport #:window))

(in-package #:pine.ui.build)

;;;; Declarative constructor DSL. Each widget is a terse function: leading
;;;; :keyword value pairs are props, the remaining arguments are nodes (a
;;;; node that is itself a list is spliced, so (mapcar ...) works like eww's
;;;; `for'). Trees read like markup -- (column :spacing 1 (label "a") (row ...))
;;;; -- and defwidget names a reusable component. This is the eww analog.

(defun %parse-args (args)
  "(values plist nodes): peel leading keyword/value prop pairs, then treat the
rest as nodes, dropping nils and splicing lists."
  (let ((props nil) (rest args))
    (loop while (and rest (keywordp (car rest)) (cdr rest))
          do (push (pop rest) props) (push (pop rest) props))
    (values (nreverse props)
            (loop for c in rest when c append (if (listp c) c (list c))))))

(defun label (text &rest props)
  "A text run. Props are any node style: :face :font-px :pad :min-w :radius ..."
  (apply #'make-instance 'text-node :content (or text "") props))

(defun icon (glyph &rest props)
  "A glyph (a codepoint or string). With :on-click it becomes a clickable cell:
:face/:font-px style the glyph, the rest (:min-w :pad :radius :hint) style the
clickable node, which centres the glyph.
(icon #xF120 :on-click thunk :hint \"Term\" :min-w 28 :pad-y 8 :radius 8 :font-px 15)"
  (let ((g (if (integerp glyph) (string (code-char glyph)) (string glyph)))
        (click (getf props :on-click)))
    (if click
        (let ((lbl (make-instance 'text-node :content g :class (getf props :glyph-class)
                                  :face (getf props :face) :font-px (getf props :font-px)))
              (p (copy-list props)))
          (remf p :face) (remf p :font-px) (remf p :on-click) (remf p :glyph-class)
          (apply #'make-instance 'action :callback click :node lbl p))
        (make-instance 'text-node :content g :class (getf props :class)
                       :face (getf props :face) :font-px (getf props :font-px)))))

(defun column (&rest args)
  "A vertical box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'vstack :nodes items props)))

(defun row (&rest args)
  "A horizontal box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'hstack :nodes items props)))

(defun centerbox (&key orient class hint expand start center end)
  "eww centerbox: three slots pinned start / centre / end along ORIENT (:v or :h).
The centre floats in the slack; the ends stay anchored, so an oversize start
never pushes the end past the surface. (centerbox :orient :v :start .. :end ..)"
  (make-instance 'pine.ui.node:centerbox :orient (or orient :v) :class class :hint hint
                 :expand (or expand 0) :start start :center center :end end))

(defun button (&rest args)
  "A clickable wrapper carrying any node style. It centres its one node.
(button :on-click thunk :hint \"...\" :pad-x 12 :radius 8 (label \"go\"))"
  (multiple-value-bind (props items) (%parse-args args)
    (let ((click (getf props :on-click)))
      (remf props :on-click)
      (apply #'make-instance 'action :callback click :node (first items) props))))

(defun boxed (&rest args)
  "A fixed-width cell. Props :width :align :pad :face; one node."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'box :node (first items) props)))

(defun centered (&rest args)
  "Centre one node in the space it is given."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'center :node (first items) props)))

(defun viewport (&rest args)
  "A clipped, scrollable window onto a taller node. Props :height :offset."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'scroll :node (first items) props)))

(defun gap (&rest props)
  "Flexible empty space. (gap :expand 2)"
  (apply #'make-instance 'spacer props))

(defun rule (&rest props)
  "A separator line. (rule :char #\\= :face :comment)"
  (apply #'make-instance 'separator props))

(defun meter (&rest props)
  "A slider/gauge. (meter :value v :min 0 :max 100 :track 12 :on-change fn)"
  (apply #'make-instance 'slider props))

(defun ring (&rest args)
  "A circular-progress gauge. Props :value :min :max :thickness :diameter
:arc-face :track-face; the optional one node is centred inside the ring."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:ring :node (first items) props)))

(defun cal (&rest props)
  "A month calendar. Props :year :month :day."
  (apply #'make-instance 'calendar props))

(defun pic (path &rest props)
  "An image loaded from file PATH."
  (apply #'make-instance 'picture :path (or path "") props))

(defun window (rows &rest props)
  "A leaf rendering already-laid-out cell ROWS (each (text . runs)) -- an Emacs
window onto a buffer or a terminal. Props may set :crow / :ccol for the point,
plus any node style. Measure/arrange are O(1); paint blits the rows."
  (apply #'make-instance 'window-node :rows rows props))

(defun rows (items item-fn &rest props)
  "A vertical list built by mapping ITEM-FN over ITEMS. (rows nets #'net-row)"
  (apply #'make-instance 'list-node :items items :item-fn item-fn props))

(defun choice (&rest args)
  "A selectable row (keyboard-navigable). (choice :data d (label ...))"
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'selectable :node (first items) props)))
