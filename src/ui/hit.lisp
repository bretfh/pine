(in-package #:pine.ui.layout)

(defun node-parent (root node)
  "NODE's parent under ROOT, or nil for the root itself."
  (labels ((walk (n)
             (let ((nodes (nodes-of n)))
               (if (member node nodes :test #'eq)
                   n
                   (some #'walk nodes)))))
    (unless (eq root node) (walk root))))

(defun replace-node (parent old new)
  "Swap OLD for NEW among PARENT's stacked nodes. True when it happened."
  (when (typep parent '(or vstack hstack))
    (setf (nodes parent) (substitute new old (nodes parent) :test #'eq))
    t))

(defun remove-with-divider (parent node)
  "Remove NODE from PARENT's nodes along with its adjacent divider.

The divider taken is the one before NODE, else the one after, so a split's
separator leaves with it."
  (let* ((nodes (nodes parent))
         (i (position node nodes :test #'eq))
         (prev (and i (plusp i) (nth (1- i) nodes)))
         (next (and i (nth (1+ i) nodes)))
         (divider (cond ((typep prev 'separator) prev)
                        ((typep next 'separator) next))))
    (setf (nodes parent)
          (remove-if (lambda (k) (or (eq k node) (eq k divider))) nodes))))

(defun split-node (root leaf new orient &key divider)
  "Put NEW beside LEAF along ORIENT (:column below, :row beside), DIVIDER
between them. NEW joins the parent as a flat sibling when the parent already
stacks that way, so repeated splits stay flat instead of nesting deeper each
time; otherwise LEAF wraps in a fresh stack. Returns the root, which changes
when LEAF was itself the root. Returns nil when the tree cannot take the
split."
  (let* ((weight (max 1 (expand-of leaf)))
         (parent (node-parent root leaf))
         (same (and parent (typep parent (ecase orient
                                           (:column 'vstack)
                                           (:row 'hstack))))))
    (setf (expand-of new) weight)
    (cond
      (same
       (setf (nodes parent)
             (loop for k in (nodes parent)
                   append (cond ((not (eq k leaf)) (list k))
                                (divider (list k divider new))
                                (t (list k new)))))
       root)
      (t
       (setf (expand-of leaf) weight)
       (let* ((nodes (if divider (list leaf divider new) (list leaf new)))
              (container (ecase orient
                           (:column (apply #'pine.ui.build:column :align :stretch
                                                    :expand weight nodes))
                           (:row (apply #'pine.ui.build:row :align :stretch :spacing 0
                                             :expand weight nodes)))))
         (cond ((eq root leaf) container)
               ((and parent (replace-node parent leaf container)) root)
               (t nil)))))))

(defun remove-node (root leaf)
  "Drop LEAF and its divider from the tree, splicing out a container left
holding a single node. Returns the root, which changes when the splice
reaches it, or nil when LEAF cannot be removed."
  (let ((parent (node-parent root leaf)))
    (when (typep parent '(or vstack hstack))
      (remove-with-divider parent leaf)
      (let ((nodes (nodes parent)))
        (cond
          ((and (null (rest nodes)) (first nodes))
           (let ((node (first nodes))
                 (grandparent (node-parent root parent)))
             (setf (expand-of node) (expand-of parent))
             (cond (grandparent (replace-node grandparent parent node) root)
                   (t node))))
          (t root))))))

(defun %node-contains (n line col)
  (and (<= (start-line n) line) (<= line (end-line n))
       (<= (start-col n) col) (< col (end-col n))))

(defmethod node-at ((n action) line col)
  (when (%node-contains n line col) (or (call-next-method) n)))

(defmethod node-at ((n selectable) line col)
  (when (%node-contains n line col) (or (call-next-method) n)))

(defmethod node-at ((n slider) line col)
  (when (%node-contains n line col) n))

(defmethod node-at ((n text-node) line col)
  "A run of text answers a hit only when it is a place. A field is what you
click to change; a label is not, and one that took every hit over its own text
would swallow the clicks of whatever it sits inside."
  (when (and (on-change n) (%node-contains n line col)) n))

(defmethod node-at ((n scroll) line col)
  (when (%node-contains n line col) (call-next-method)))

(defun action-at (node line col)
  "The callback of the action under (LINE COL), or nil."
  (let ((hit (node-at node line col)))
    (when (typep hit 'action) (callback hit))))

(defun hint-at (root line col)
  "The hover hint of the node under (LINE COL), or nil."
  (let ((n (node-at root line col)))
    (and n (hint n))))

(defmethod nodes-of ((n vstack)) (nodes n))
(defmethod nodes-of ((n hstack)) (nodes n))
(defmethod nodes-of ((n stack)) (nodes n))
(defmethod nodes-of ((n centerbox)) (centerbox-parts n))
(defmethod nodes-of ((n list-node)) (rendered n))
(defmethod nodes-of ((n box)) (and (node n) (list (node n))))
(defmethod nodes-of ((n center)) (and (node n) (list (node n))))
(defmethod nodes-of ((n action)) (and (node n) (list (node n))))
(defmethod nodes-of ((n selectable)) (and (node n) (list (node n))))
(defmethod nodes-of ((n scroll)) (and (node n) (list (node n))))
(defmethod nodes-of ((n ring)) (and (node n) (list (node n))))

(defgeneric clicked (node col)
  (:documentation "The nullary thunk clicking NODE at COL means. A widget that
does something when it is clicked answers it here.")
  (:method (node col) (declare (ignore node col)) nil))

(defmethod clicked ((node action) col)
  (declare (ignore col))
  (callback node))

(defmethod clicked ((node selectable) col)
  "A row of a listing does what its :click says. Without one it does nothing on
a click, and choosing it is the listing's business rather than the pointer's."
  (declare (ignore col))
  (click node))

(defmethod clicked ((node slider) col)
  (let ((fn (on-change node))
        (v (slider-value-at node col)))
    (when fn (lambda () (funcall fn v)))))

(defmethod clicked ((node text-node) col)
  "A field: ask for the new value, seeded with the one it is showing, and hand
what is typed to the same path it reads. The prompt is the one at /echo, so a
field needs no editing of its own and no focus to hold."
  (declare (ignore col))
  (let ((write-back (on-change node))
        (had (content node))
        (ask (hint node)))
    (when write-back
      (lambda ()
        (if pine.ui.build:*editing*
            (funcall pine.ui.build:*editing*
                     (format nil "~a " (or ask "New value:")) had write-back)
            (pine/run/log:note "nothing here can ask for a value"))))))

(defun click-thunk (root line col)
  "A nullary thunk for a click at (LINE COL) on arranged ROOT, or nil where
nothing there does anything."
  (clicked (node-at root line col) col))

(defgeneric placep (n)
  (:documentation "Whether the selection can land on N. A row of a listing can
be chosen; a field is a place you can put a value in; nothing else is either.")
  (:method (n) (declare (ignore n)) nil)
  (:method ((n selectable)) t)
  (:method ((n text-node)) (and (on-change n) t)))

(defun collect-selectables (n)
  "Every place in N the selection can land on, in tree order. A row is a leaf
choice, so the walk does not descend past one."
  (let ((result nil))
    (labels ((walk (x)
               (when x
                 (if (placep x)
                     (push x result)
                     (mapc #'walk (nodes-of x))))))
      (walk n))
    (nreverse result)))
