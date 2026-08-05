(defpackage #:pine.ui.build
  (:use #:cl #:pine.ui.node)
  (:shadow #:centerbox #:ring #:box #:center #:scroll #:slider #:calendar
           #:image #:cells #:stack)
  (:export #:box #:button #:calendar #:cells #:centerbox #:center #:choice
           #:column #:gap #:icon #:image #:label #:ring #:row
           #:rows #:rule #:scroll #:slider #:grid #:stack
           #:field #:acting #:shown))

(in-package #:pine.ui.build)

;;;; Declarative constructor DSL. Each widget is a terse function: leading
;;;; :keyword value pairs are props, the remaining arguments are nodes (a
;;;; node that is itself a list is spliced, so (mapcar ...) works like eww's
;;;; `for'). Trees read like markup -- (column :spacing 1 (label "a") (row ...))
;;;; -- and defwidget names a reusable component. This is the eww analog.


(defun %parse-args (args)
  "(values props nodes): peel the leading keyword/value prop pairs into a map,
then treat the rest as nodes, dropping nils and splicing lists."
  (let ((props (fset:empty-map)) (rest args))
    (loop while (and rest (keywordp (car rest)) (cdr rest))
          do (let ((key (pop rest)))
               (setf props (fset:with props key (pop rest)))))
    (values props
            (loop for c in rest when c append (if (listp c) c (list c))))))

(defun shown (x)
  "What a slot shows. A path in place of a value is read, so a widget slot and
the place it shows are one thing."
  (cond ((pine.path:pathp x) (let ((v (pine.ns:read x))) (if (null v) "" v)))
        ((null x) "")
        (t x)))

(defun acting (click)
  "What a :click does: a command path, a write-map, or a function."
  (cond ((null click) nil)
        ((functionp click) click)
        (t (lambda () (pine.cmd:run click)))))

(defun %click (props)
  "The click thunk PROPS names, from :click or :on-click, gated by :confirm."
  (let ((thunk (acting (or (fset:lookup props :click)
                           (fset:lookup props :on-click))))
        (ask (fset:lookup props :confirm)))
    (cond ((null thunk) nil)
          ((null ask) thunk)
          (t (lambda ()
               (pine.ns:write (pine.path:parse "/echo")
                              (fset:map (:prompt (format nil "~a (y or n) " ask))
                                        (:then (lambda (answer)
                                                 (when (and (stringp answer)
                                                            (plusp (length answer))
                                                            (char-equal #\y (char answer 0)))
                                                   (funcall thunk)))))))))))

(defun %without (props &rest keys)
  "PROPS as initargs, with KEYS taken out: what a widget passes on after it has
read the props that are its own."
  (let ((out props))
    (dolist (k keys) (setf out (fset:less out k)))
    (pine.data:plist out)))

(defun %without-key (props key)
  (loop :for (k v) :on props :by #'cddr
        :unless (eq k key) :append (list k v)))

(defun label (text &rest props)
  "A text run. TEXT may be a path, which is read.

Whatever it answers is shown as text. A slot takes a path in place of a value,
and /sys/cpu holds a number, so a label that only took strings would make every
config write PRINC-TO-STRING around half of them."
  (apply #'make-instance 'text-node
         :content (let ((it (shown text)))
                    (if (stringp it) it (princ-to-string it)))
         props))

(defun icon (glyph &rest raw-props)
  "A glyph (a codepoint or string). With :on-click it becomes a clickable cell:
:face/:font-px style the glyph, the rest (:min-w :pad :radius :hint) style the
clickable node, which centres the glyph.
(icon #xF120 :on-click thunk :hint \"Term\" :min-w 28 :pad-y 8 :radius 8 :font-px 15)"
  (let* ((raw (shown glyph))
         (g (if (integerp raw) (string (code-char raw)) (string raw)))
         (props (pine.data:pairs raw-props))
         (click (%click props)))
    (if click
        (let ((lbl (make-instance 'text-node :content g
                                             :class (fset:lookup props :glyph-class)
                                             :face (fset:lookup props :face)
                                             :font-px (fset:lookup props :font-px)))
              (p (%without props :face :font-px :on-click :click :confirm
                           :glyph-class)))
          (apply #'make-instance 'action :callback click :node lbl p))
        (make-instance 'text-node :content g :class (fset:lookup props :class)
                                  :face (fset:lookup props :face)
                                  :font-px (fset:lookup props :font-px)))))

(defun column (&rest args)
  "A vertical box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'vstack :nodes items (pine.data:plist props))))

(defun row (&rest args)
  "A horizontal box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'hstack :nodes items (pine.data:plist props))))

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
    (let ((click (%click props)))
      (apply #'make-instance 'action :callback click :node (first items)
             (%without props :on-click :click :confirm)))))

(defun box (&rest args)
  "A fixed-width cell. Props :width :align :pad :face; one node."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:box :node (first items) (pine.data:plist props))))

(defun center (&rest args)
  "Centre one node in the space it is given."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:center :node (first items) (pine.data:plist props))))

(defun scroll (&rest args)
  "A clipped, scrollable window onto a taller node. Props :height :offset."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:scroll :node (first items) (pine.data:plist props))))

(defun gap (&rest props)
  "Flexible empty space. (gap :expand 2)"
  (apply #'make-instance 'spacer props))

(defun rule (&rest props)
  "A separator line. (rule :char #\\= :face :comment)"
  (apply #'make-instance 'separator props))

(defun slider (&rest args)
  "A slider. Given a path as its subject it shows that path and dragging writes
it, so there is no :value and no :on-change."
  (let ((subject (first args)))
    (cond ((pine.path:pathp subject)
           (apply #'make-instance 'pine.ui.node:slider
                  :value (or (pine.ns:read subject) 0)
                  :on-change (lambda (v) (pine.ns:write subject v))
                  (rest args)))
          ((keywordp subject) (apply #'make-instance 'pine.ui.node:slider args))
          (t (apply #'make-instance 'pine.ui.node:slider :value subject (rest args))))))

(defun field (subject &rest raw-props)
  "A one-line editable field over the path it edits.

The path is the whole of it: what it shows is what that path holds, and what is
typed into it is written back there. No :value and no :on-change."
  (let ((props (pine.data:pairs raw-props)))
    (apply #'make-instance 'text-node
           :content (princ-to-string (shown subject))
           :of (when (pine.path:pathp subject) subject)
           :on-change (when (pine.path:pathp subject)
                        (lambda (v) (pine.ns:write subject v)))
           :class (or (fset:lookup props :class) "field")
           (%without props :class))))

(defun stack (&rest args)
  "Nodes in one place, the last on top: each is given the whole rect and they
are painted in the order they were written."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:stack :nodes items
           (pine.data:plist props))))

(defun grid (&rest args)
  "A column of rows, COLUMNS wide."
  (multiple-value-bind (props items) (%parse-args args)
    (let ((columns (max 1 (or (fset:lookup props :columns) 1))))
      (apply #'make-instance 'vstack
             :nodes (loop :for rest = items :then (nthcdr columns rest)
                          :while rest
                          :collect (apply #'make-instance 'hstack
                                          :nodes (subseq rest 0 (min columns (length rest)))
                                          (%without props :columns)))
             (%without props :columns)))))

(defun ring (&rest args)
  "A circular gauge. Given a path as its subject it shows that path."
  (let ((subject (first args)))
    (if (keywordp subject)
        (multiple-value-bind (props items) (%parse-args args)
          (apply #'make-instance 'pine.ui.node:ring :node (first items) (pine.data:plist props)))
        (multiple-value-bind (props items) (%parse-args (rest args))
          (apply #'make-instance 'pine.ui.node:ring
                 :node (first items)
                 :value (if (pine.path:pathp subject)
                            (or (pine.ns:read subject) 0)
                            subject)
                 (pine.data:plist props))))))

(defun calendar (&rest props)
  "A month calendar. Props :year :month :day."
  (apply #'make-instance 'pine.ui.node:calendar props))

(defun image (path &rest props)
  "An image. PATH may be a path, which is read."
  (apply #'make-instance 'picture :path (princ-to-string (shown path)) props))

(defun cells (rows &rest props)
  "A leaf holding already-laid-out cell ROWS, each (text . runs). :AS names the
class it is, :OF the content it shows, :CROW and :CCOL the caret. Measure and
arrange are O(1); paint blits the rows."
  (apply #'make-instance
         (or (getf props :as) 'pine.ui.node:view-node)
         :rows rows (%without-key props :as)))


(defun rows (items item-fn &rest props)
  "A vertical list over a pattern, whose matches are the rows, or over a list
of values. Over a pattern ITEM-FN takes nothing and reads /.; over values it
takes the value and its index.

Over a pattern the row remembers the path it was built for. That is what makes
the listing a listing of things rather than of lines: the selection, the keys a
mode binds and a click all reach the same place, and none of them needs a table
saying which line was which."
  (let ((over-paths (pine.path:pathp items)))
    (apply #'make-instance 'list-node
           :items (if over-paths
                      (pine.data:keys (pine.ns:read items (fset:empty-map)))
                      items)
           :item-fn (if over-paths
                        (lambda (item &optional index)
                          (declare (ignore index))
                          (let ((pine.path:*here*
                                  (if (pine.path:pathp item)
                                      item
                                      (pine.path:here))))
                            (let ((row (funcall item-fn)))
                              (when (and row (null (pine.ui.node:of row)))
                                (setf (pine.ui.node:of row) pine.path:*here*))
                              row)))
                        item-fn)
           props)))

(defun choice (&rest args)
  "A selectable row. :click takes a command path, a write-map or a function.

The click is its own slot. A row that is clickable still keeps its :data and
still knows what it stands for, which is the whole of what makes a listing act
on things rather than on lines."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'selectable :node (first items)
           :click (%click props)
           (%without props :click :on-click :confirm))))
