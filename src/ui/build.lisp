(defpackage #:pine/ui/build
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:path #:pine/fs/path) (#:w #:pine/ui/widget)
                    (#:command #:pine/run/command) (#:log #:pine/run/log))
  (:shadow #:box #:center #:stack)
  (:export #:label #:field #:icon #:button #:column #:row #:stack #:box #:center
           #:centerbox #:scroll #:gap #:rule #:slider #:ring #:grid #:choice
           #:calendar #:image #:cells #:rows
           #:placep #:held #:shown #:acting #:here #:*asking* #:*editing*))
(in-package #:pine/ui/build)

(defvar *here* nil)
(defvar *asking* nil)
(defvar *editing* nil)

(defun here ()
  "The path a row is being built for, inside ROWS. What a listing's row reads to say
what it stands for."
  *here*)

(defun placep (it)
  "Whether IT is somewhere a value is kept rather than a value: a node, or a path
naming one."
  (or (node:nodep it) (path:pathp it)))

(defun held (it)
  (if (node:nodep it) (node:contents it) (path:contents it)))

(defun (setf held) (value it)
  (if (node:nodep it)
      (setf (node:contents it) value)
      (setf (path:contents it) value)))

(defun shown (it)
  "What a slot shows. A place in place of a value is read, so a widget slot and the
place it shows are one thing."
  (cond ((placep it) (let ((v (held it))) (if (null v) "" v)))
        ((null it) "")
        (t it)))

(defun %writing (m)
  (lambda ()
    (d:do-pairs (where value m) (setf (held where) value))
    t))

(defun acting (click)
  "What a :click does: a function, a write-map, a node, a path, or a command's name.
A config holds nodes, so clicking one writes it, the way clicking a path writes what
it names."
  (cond ((null click) nil)
        ((functionp click) click)
        ((d:mapp click) (%writing click))
        ((placep click) (lambda () (setf (held click) t)))
        (t (lambda () (command:run click)))))

(defun %click (props)
  (let ((thunk (acting (or (getf props :click) (getf props :on-click))))
        (ask (getf props :confirm)))
    (cond ((null thunk) nil)
          ((null ask) thunk)
          (t (lambda ()
               (if *asking*
                   (funcall *asking* ask thunk)
                   (log:note "~a: nothing here can ask" ask)))))))

(defun %without (props &rest keys)
  (loop :for (k v) :on props :by #'cddr
        :unless (member k keys) :append (list k v)))

(defun %split (args)
  "(values props parts): peel the leading keyword pairs, then take the rest as
parts, dropping nils and splicing lists."
  (let ((props nil) (rest args))
    (loop :while (and rest (keywordp (car rest)) (cdr rest))
          :do (let ((key (pop rest))) (setf props (append props (list key (pop rest))))))
    (values props
            (loop :for c :in rest :when c :append (if (listp c) c (list c))))))

(defun label (text &rest props)
  "A run of text. TEXT may be a place, which is read: /sys/cpu holds a number, and a
label that only took strings would make every config write PRINC-TO-STRING."
  (apply #'make-instance 'w:label
         :content (let ((it (shown text)))
                    (if (stringp it) it (princ-to-string it)))
         props))

(defun field (subject &rest props)
  "A one-line editable field over the place it edits. The place is the whole of it:
what it shows is what that place holds, and what is typed is written back there."
  (apply #'make-instance 'w:label
         :content (princ-to-string (shown subject))
         :of (when (placep subject) subject)
         :changed (when (placep subject)
                    (lambda (v) (setf (held subject) v)))
         :class (or (getf props :class) "field")
         (%without props :class)))

(defun icon (glyph &rest props)
  "A glyph, from a codepoint or a string. With a click it becomes a clickable cell
that centres the glyph."
  (let* ((raw (shown glyph))
         (g (if (integerp raw) (string (code-char raw)) (string raw)))
         (thunk (%click props)))
    (if thunk
        (apply #'make-instance 'w:action
               :click thunk
               :parts (list (make-instance 'w:label :content g
                                           :class (getf props :glyph-class)
                                           :face (getf props :face)
                                           :font (getf props :font)))
               (%without props :face :font :on-click :click :confirm :glyph-class))
        (make-instance 'w:label :content g :class (getf props :class)
                               :face (getf props :face) :font (getf props :font)))))

(defun button (&rest args)
  "A clickable wrapper carrying any style. It centres what it holds."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:action :click (%click props)
           :parts (list (first parts))
           (%without props :on-click :click :confirm))))

(defun column (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:column :parts parts props)))

(defun row (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:row :parts parts props)))

(defun stack (&rest args)
  "Parts in one place, the last on top."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:stack :parts parts props)))

(defun box (&rest args)
  "A cell of a fixed width."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:box :parts (list (first parts)) props)))

(defun center (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:center :parts (list (first parts)) props)))

(defun centerbox (&key upright class hint expand start center end)
  "Three slots pinned start, middle and end. The middle floats in the slack; the
ends stay anchored, so an oversize start never pushes the end off the surface."
  (make-instance 'w:centerbox :upright (if (null upright) t upright)
                              :class class :hint hint :expand (or expand 0)
                              :start start :middle center :end end))

(defun scroll (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:scroll :parts (list (first parts)) props)))

(defun gap (&rest props) (apply #'make-instance 'w:gap props))

(defun rule (&rest props) (apply #'make-instance 'w:rule props))

(defun slider (&rest args)
  "A slider. Given a place as its subject it shows that place and dragging writes
it, so there is no :value and no :changed."
  (let ((subject (first args)))
    (cond ((placep subject)
           (apply #'make-instance 'w:slider
                  :value (or (held subject) 0)
                  :changed (lambda (v) (setf (held subject) v))
                  (cl:rest args)))
          ((keywordp subject) (apply #'make-instance 'w:slider args))
          (t (apply #'make-instance 'w:slider :value subject (cl:rest args))))))

(defun ring (&rest args)
  (let ((subject (first args)))
    (if (keywordp subject)
        (multiple-value-bind (props parts) (%split args)
          (apply #'make-instance 'w:ring :parts (list (first parts)) props))
        (multiple-value-bind (props parts) (%split (cl:rest args))
          (apply #'make-instance 'w:ring
                 :parts (list (first parts))
                 :value (if (placep subject) (or (held subject) 0) subject)
                 props)))))

(defun grid (&rest args)
  "A column of rows, COLUMNS wide."
  (multiple-value-bind (props parts) (%split args)
    (let ((n (max 1 (or (getf props :columns) 1))))
      (apply #'make-instance 'w:column
             :parts (loop :for rest := parts :then (nthcdr n rest)
                          :while rest
                          :collect (apply #'make-instance 'w:row
                                          :parts (subseq rest 0 (min n (length rest)))
                                          (%without props :columns)))
             (%without props :columns)))))

(defun choice (&rest args)
  "A row of a listing. Its click is its own slot, so a clickable row still knows
what it stands for."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'w:choice :parts (list (first parts))
           :click (%click props)
           (%without props :click :on-click :confirm))))

(defun calendar (&rest props) (apply #'make-instance 'w:calendar props))

(defun image (where &rest props)
  (apply #'make-instance 'w:picture :path (princ-to-string (shown where)) props))

(defun cells (rows &rest props)
  "A leaf holding rows that are already laid out. Measure and arrange are one step
and paint blits them."
  (apply #'make-instance 'w:cells :rows rows props))

(defun rows (items builder &rest props)
  "A column over a pattern, whose matches are the rows, or over a list of values.
Over a pattern the row remembers the path it was built for: that is what makes a
listing a listing of things rather than of lines, so the selection, the keys a mode
binds and a click all reach the same place.

The rows are built here rather than at measure, so nothing is written into a tree
that has already been handed out."
  (let* ((over-paths (path:pathp items))
         (all (if over-paths
                  (let ((n (path:at items)))
                    (and n (mapcar (lambda (each)
                                     (path:parse (node:full-name each)))
                                   (node:nodes n))))
                  items)))
    (apply #'make-instance 'w:column
           :parts (loop :for item :in all
                        :for i :from 0
                        :collect (if over-paths
                                     (let ((*here* (if (path:pathp item) item *here*)))
                                       (let ((made (funcall builder)))
                                         (when (and made (null (w:of made)))
                                           (setf (w:of made) *here*))
                                         made))
                                     (funcall builder item i)))
           props)))
