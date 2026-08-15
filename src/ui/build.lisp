(defpackage #:pine.ui.build
  (:use #:cl #:pine.ui.node)
  (:shadow #:centerbox #:ring #:box #:center #:scroll #:slider #:calendar
           #:image #:cells #:stack)
  (:export #:*asking* #:*editing* #:here #:box #:button #:calendar #:cells #:centerbox #:center #:choice
           #:column #:gap #:icon #:image #:label #:ring #:row
           #:rows #:rule #:scroll #:slider #:grid #:stack
           #:field #:acting #:shown #:placep #:held))

(in-package #:pine.ui.build)

(defvar *here* nil)
(defvar *asking* nil)
(defvar *editing* nil)

(defun %parse-args (args)
  "(values props nodes): peel the leading keyword/value prop pairs into a map,
then treat the rest as nodes, dropping nils and splicing lists."
  (let ((props nil) (rest args))
    (loop while (and rest (keywordp (car rest)) (cdr rest))
          do (let ((key (pop rest)))
               (setf props (append props (list key (pop rest))))))
    (values props
            (loop for c in rest when c append (if (listp c) c (list c))))))

(defun here ()
  "The path a row is being built for, inside ROWS. What a listing's row reads
to say what it stands for."
  *here*)

(defun placep (x)
  "Whether X is somewhere a value is kept rather than a value: a node, or a
path naming one."
  (or (pine/fs/node:nodep x) (pine.path.path:pathp x)))

(defun held (x)
  (if (pine/fs/node:nodep x)
      (pine/fs/node:contents x)
      (pine.path.place:contents x)))

(defun (setf held) (value x)
  (if (pine/fs/node:nodep x)
      (setf (pine/fs/node:contents x) value)
      (setf (pine.path.place:contents x) value)))

(defun shown (x)
  "What a slot shows. A place in place of a value is read, so a widget slot and
the place it shows are one thing."
  (cond ((placep x) (let ((v (held x))) (if (null v) "" v)))
        ((null x) "")
        (t x)))

(defun %writing (m)
  "A write-map as a thunk: every place in it takes the value beside it, so a
config can say what a click does without writing a closure."
  (lambda ()
    (pine/data:do-pairs (where value m)
      (setf (held where) value))
    t))

(defun acting (click)
  "What a :click does: a function, a write-map, a node, a path, or a command's
name. A config holds nodes -- (at (root *world*) \"wm\" \"workspaces\" n) is one --
so clicking one writes it, the way clicking a path writes what it names."
  (cond ((null click) nil)
        ((functionp click) click)
        ((pine/data:mapp click) (%writing click))
        ((placep click) (lambda () (setf (held click) t)))
        (t (lambda () (pine.repl.command:run click)))))

(defun %click (props)
  "The click thunk PROPS names, from :click or :on-click, gated by :confirm."
  (let ((thunk (acting (or (getf props :click)
                           (getf props :on-click))))
        (ask (getf props :confirm)))
    (cond ((null thunk) nil)
          ((null ask) thunk)
          (t (lambda ()
               (if *asking*
                   (funcall *asking* ask thunk)
                   (pine/run/log:note "~a: nothing here can ask" ask)))))))

(defun %without (props &rest keys)
  "PROPS as initargs, with KEYS taken out: what a widget passes on after it has
read the props that are its own."
  (let ((out props))
    (dolist (k keys) (setf out (%without-key out k)))
    out))

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
         (props raw-props)
         (click (%click props)))
    (if click
        (let ((lbl (make-instance 'text-node :content g
                                             :class (getf props :glyph-class)
                                             :face (getf props :face)
                                             :font-px (getf props :font-px)))
              (p (%without props :face :font-px :on-click :click :confirm
                           :glyph-class)))
          (apply #'make-instance 'action :callback click :node lbl p))
        (make-instance 'text-node :content g :class (getf props :class)
                                  :face (getf props :face)
                                  :font-px (getf props :font-px)))))

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
    (let ((click (%click props)))
      (apply #'make-instance 'action :callback click :node (first items)
             (%without props :on-click :click :confirm)))))

(defun box (&rest args)
  "A fixed-width cell. Props :width :align :pad :face; one node."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:box :node (first items) props)))

(defun center (&rest args)
  "Centre one node in the space it is given."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:center :node (first items) props)))

(defun scroll (&rest args)
  "A clipped, scrollable window onto a taller node. Props :height :offset."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:scroll :node (first items) props)))

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
    (cond ((placep subject)
           (apply #'make-instance 'pine.ui.node:slider
                  :value (or (held subject) 0)
                  :on-change (lambda (v) (setf (held subject) v))
                  (rest args)))
          ((keywordp subject) (apply #'make-instance 'pine.ui.node:slider args))
          (t (apply #'make-instance 'pine.ui.node:slider :value subject (rest args))))))

(defun field (subject &rest raw-props)
  "A one-line editable field over the path it edits.

The path is the whole of it: what it shows is what that path holds, and what is
typed into it is written back there. No :value and no :on-change."
  (let ((props raw-props))
    (apply #'make-instance 'text-node
           :content (princ-to-string (shown subject))
           :of (when (placep subject) subject)
           :on-change (when (placep subject)
                        (lambda (v) (setf (held subject) v)))
           :class (or (getf props :class) "field")
           (%without props :class))))

(defun stack (&rest args)
  "Nodes in one place, the last on top: each is given the whole rect and they
are painted in the order they were written."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'pine.ui.node:stack :nodes items
           props)))

(defun grid (&rest args)
  "A column of rows, COLUMNS wide."
  (multiple-value-bind (props items) (%parse-args args)
    (let ((columns (max 1 (or (getf props :columns) 1))))
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
          (apply #'make-instance 'pine.ui.node:ring :node (first items) props))
        (multiple-value-bind (props items) (%parse-args (rest args))
          (apply #'make-instance 'pine.ui.node:ring
                 :node (first items)
                 :value (if (placep subject) (or (held subject) 0) subject)
                 props)))))

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
  (let ((over-paths (pine.path.path:pathp items)))
    (apply #'make-instance 'list-node
           :items (if over-paths
                      (let ((n (pine.path.place:at items)))
                        (and n (mapcar (lambda (each)
                                         (pine.path.path:parse
                                          (pine/fs/node:full-name each)))
                                       (pine/fs/node:nodes n))))
                      items)
           :item-fn (if over-paths
                        (lambda (item &optional index)
                          (declare (ignore index))
                          (let ((*here*
                                  (if (pine.path.path:pathp item)
                                      item
                                      *here*)))
                            (let ((row (funcall item-fn)))
                              (when (and row (null (pine.ui.node:of row)))
                                (setf (pine.ui.node:of row) *here*))
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
