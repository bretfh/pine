(in-package #:pine/ui)

(defvar *here* nil)

(defgeneric confirming (where question thunk)
  (:documentation "Ask QUESTION before doing THUNK, of whoever there is to ask.

Answered above, by whatever has somewhere to put a question. With nobody to ask
there is nobody to say yes, so it is said rather than done.")
  (:method (where question thunk)
    (declare (ignore where thunk))
    (log:note "~a: nothing here can ask" question)))

(defun here ()
  "The path a row is being built for, inside ROWS. What a listing's row reads to say
what it stands for."
  *here*)

(deftype somewhere ()
  "What names a place rather than being a value: a node, or a path naming one."
  '(or node:node path:path))

(defun placep (it) (typep it 'somewhere))

(defgeneric held (it)
  (:documentation "What IT holds. A widget slot takes a place or a value and shows
what it finds, so this is the one question every builder asks of what it was
handed, and a value answers it by being one.")
  (:method ((it node:node)) (node:contents it))
  (:method ((it path:path))
    (let ((n (tree:at it))) (and n (node:contents n))))
  (:method (it) it))

(defgeneric (setf held) (value it)
  (:method (value (it node:node)) (setf (node:contents it) value))
  (:method (value (it path:path))
    (setf (node:contents (tree:ensure it)) value)))

(defun %shown (it)
  "What a slot shows: what HELD answers, with nothing shown as the empty string.
SURFACE:SHOWN is a different question, and this one stays private rather than
taking its word."
  (let ((v (held it))) (if (null v) "" v)))

(defun %writing (m)
  (lambda ()
    (d:do-pairs (where value m) (setf (held where) value))
    t))

(defgeneric acting (click)
  (:documentation "What a :click does. A config holds nodes, so clicking one
writes it, the way clicking a path writes what it names; anything else is a
command's name.")
  (:method ((click null)) nil)
  (:method ((click function)) click)
  (:method ((click node:node)) (lambda () (setf (held click) t)))
  (:method ((click path:path)) (lambda () (setf (held click) t)))
  (:method (click)
    (if (d:mapp click)
        (%writing click)
        (lambda () (command:run click)))))

(defun %click (props)
  (let ((thunk (acting (or (getf props :click) (getf props :on-click))))
        (ask (getf props :confirm)))
    (cond ((null thunk) nil)
          ((null ask) thunk)
          (t (lambda () (confirming command:*at* ask thunk))))))

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
  (apply #'make-instance 'label
         :content (let ((it (%shown text)))
                    (if (stringp it) it (princ-to-string it)))
         props))

(defun field (subject &rest props)
  "A one-line editable field over the place it edits. The place is the whole of it:
what it shows is what that place holds, and what is typed is written back there."
  (apply #'make-instance 'label
         :content (princ-to-string (%shown subject))
         :of (when (placep subject) subject)
         :changed (when (placep subject)
                    (lambda (v) (setf (held subject) v)))
         :class (or (getf props :class) "field")
         (%without props :class)))

(defun icon (glyph &rest props)
  "A glyph, from a codepoint or a string. With a click it becomes a clickable cell
that centres the glyph."
  (let* ((raw (%shown glyph))
         (g (if (integerp raw) (string (code-char raw)) (string raw)))
         (thunk (%click props)))
    (if thunk
        (apply #'make-instance 'action
               :click thunk
               :parts (list (make-instance 'label :content g
                                           :class (getf props :glyph-class)
                                           :face (getf props :face)
                                           :font (getf props :font)))
               (%without props :face :font :on-click :click :confirm :glyph-class))
        (make-instance 'label :content g :class (getf props :class)
                               :face (getf props :face) :font (getf props :font)))))

(defun button (&rest args)
  "A clickable wrapper carrying any style. It centres what it holds."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'action :click (%click props)
           :parts (list (first parts))
           (%without props :on-click :click :confirm))))

(defun column (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'column :parts parts props)))

(defun row (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'row :parts parts props)))

(defun stack (&rest args)
  "Parts in one place, the last on top."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'stack :parts parts props)))

(defun box (&rest args)
  "A cell of a fixed width."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'box :parts (list (first parts)) props)))

(defun center (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'center :parts (list (first parts)) props)))

(defun centerbox (&key (upright :yes) class hint expand start center end)
  "Three slots pinned start, middle and end. The middle floats in the slack; the
ends stay anchored, so an oversize start never pushes the end off the surface.

UPRIGHT is :YES or :NO. It was a boolean whose NIL was read as unset and turned
back into true, so the one thing it could not be asked for was the other way up."
  (make-instance 'centerbox :upright (ecase upright (:yes t) (:no nil))
                              :class class :hint hint :expand (or expand 0)
                              :start start :middle center :end end))

(defun scroll (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'scroll :parts (list (first parts)) props)))

(defun gap (&rest props) (apply #'make-instance 'gap props))

(defun rule (&rest props) (apply #'make-instance 'rule props))

(defun slider (&rest args)
  "A slider. Given a place as its subject it shows that place and dragging writes
it, so there is no :value and no :changed."
  (let ((subject (first args)))
    (cond ((placep subject)
           (apply #'make-instance 'slider
                  :value (or (held subject) 0)
                  :changed (lambda (v) (setf (held subject) v))
                  (cl:rest args)))
          ((keywordp subject) (apply #'make-instance 'slider args))
          (t (apply #'make-instance 'slider :value subject (cl:rest args))))))

(defun ring (&rest args)
  (let ((subject (first args)))
    (if (keywordp subject)
        (multiple-value-bind (props parts) (%split args)
          (apply #'make-instance 'ring :parts (list (first parts)) props))
        (multiple-value-bind (props parts) (%split (cl:rest args))
          (apply #'make-instance 'ring
                 :parts (list (first parts))
                 :value (if (placep subject) (or (held subject) 0) subject)
                 props)))))

(defun grid (&rest args)
  "A column of rows, COLUMNS wide."
  (multiple-value-bind (props parts) (%split args)
    (let ((n (max 1 (or (getf props :columns) 1))))
      (apply #'make-instance 'column
             :parts (loop :for rest := parts :then (nthcdr n rest)
                          :while rest
                          :collect (apply #'make-instance 'row
                                          :parts (subseq rest 0 (min n (length rest)))
                                          (%without props :columns)))
             (%without props :columns)))))

(defun choice (&rest args)
  "A row of a listing. Its click is its own slot, so a clickable row still knows
what it stands for."
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'choice :parts (list (first parts))
           :click (%click props)
           (%without props :click :on-click :confirm))))

(defun calendar (&rest props) (apply #'make-instance 'calendar props))

(defun image (where &rest props)
  (apply #'make-instance 'picture :path (princ-to-string (%shown where)) props))

(defun cells (rows &rest props)
  "A leaf holding rows that are already laid out. Measure and arrange are one step
and paint blits them."
  (apply #'make-instance 'cells :rows rows props))

(defun rows (items builder &rest props)
  "A column over a pattern, whose matches are the rows, or over a list of values.
Over a pattern the row remembers the path it was built for: that is what makes a
listing a listing of things rather than of lines, so the selection, the keys a mode
binds and a click all reach the same place.

The rows are built here rather than at measure, so nothing is written into a tree
that has already been handed out."
  (let* ((over-paths (path:pathp items))
         (all (if over-paths
                  (let ((n (tree:at items)))
                    (and n (mapcar (lambda (each)
                                     (path:path (node:full-name each)))
                                   (node:nodes n))))
                  items)))
    (apply #'make-instance 'column
           :parts (loop :for item :in all
                        :for i :from 0
                        :collect (if over-paths
                                     (let ((*here* (if (path:pathp item) item *here*)))
                                       (let ((made (funcall builder)))
                                         (when (and made (null (of made)))
                                           (setf (of made) *here*))
                                         made))
                                     (funcall builder item i)))
           props)))

