(defpackage #:pine.view
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:show #:server #:rendered #:view-of #:selection-path
           #:node-at-point))

(in-package #:pine.view)
(named-readtables:in-readtable pine.path:syntax)

(defun view-of (name)
  "NAME's widget tree, or NIL when it is not a tool buffer."
  (ns:read (pine.buf:at name :view)))

(defun %rows-text (rows)
  "ROWS as text: what a tool buffer reads as, so point, motion and search work
in one like anywhere else."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (row) (string-right-trim " " (car row))) rows)))

(defun %width (name)
  "The width the window showing NAME has."
  (or (loop :for w :in (pine.win:windows)
            :when (fset:equal? (pine.buf:at name) (ns:read (p:child w "buf")))
              :return (ns:read (p:child w "width")))
      (ns:read /win/width)
      80))

(defun rendered (name &key selection)
  "NAME's view rendered at its window's width: (values rows tree)."
  (let ((tree (view-of name)))
    (when tree
      (pine.ui.cells:render tree (%width name)
                            :selection (or selection (%index name))))))

(defun %index (name)
  "Which row is selected, as a position in the tree. Where the selection is a
place, which row holds it is this buffer's own business."
  (or (pine.buf:asked name :selection) 0))

(defun %selectables (name)
  (let ((tree (view-of name)))
    (when tree (pine.ui.layout:collect-selectables tree))))

(defun selection-path (name)
  "The path the selected row carries, or NIL when it carries none."
  (let ((node (nth (%index name) (%selectables name))))
    (when node
      (let ((d (pine.ui.node:data node)))
        (when (p:pathp d) d)))))

(defun %select (name delta)
  "Move the selection by DELTA, wrapping. The rows render again with it, so the
text, the place the selection names and point all move together."
  (let ((n (length (%selectables name))))
    (when (plusp n)
      (setf (pine.buf:asked name :selection) (mod (+ (%index name) delta) n))
      (multiple-value-bind (rows tree) (rendered name)
        (let* ((node (nth (%index name)
                          (pine.ui.layout:collect-selectables tree)))
               (line (if node (pine.ui.node:start-line node) 0)))
          (ns:write
           (fset:map ((pine.buf:at name :selection) (selection-path name))
                     ((pine.buf:at name :text) (%rows-text rows))
                     ((pine.buf:at name :point) (fset:seq line 0))))))))
  nil)

(defgeneric activates-to (node)
  (:documentation "The thunk NODE activates to. A widget with something to do
when it is chosen answers it here; anything else answers for its children.")
  (:method ((node null)) nil)
  (:method (node) (some #'activates-to (pine.ui.layout:nodes-of node))))

(defmethod activates-to ((node pine.ui.node:action))
  (pine.ui.node:callback node))

(defmethod activates-to ((node pine.ui.node:selectable))
  (let ((d (pine.ui.node:data node)))
    (if (functionp d) d (call-next-method))))

(defun node-at-point (&optional (name (pine.buf:name-of (ns:read /buf/current))))
  "The node under point in NAME's rendered view, or NIL."
  (multiple-value-bind (rows tree) (rendered name)
    (declare (ignore rows))
    (when tree
      (let ((point (ns:read (pine.buf:at name :point))))
        (pine.ui.layout:node-at tree
                                (or (fset:lookup point 0) 0)
                                (or (fset:lookup point 1) 0))))))

(defun %activate (name)
  "Run what the row under point activates, else the selected row's."
  (let ((thunk (or (activates-to (node-at-point name))
                   (activates-to (nth (%index name) (%selectables name))))))
    (if thunk
        (funcall thunk)
        (ns:write /echo/hint "nothing to activate here")))
  nil)

(defun %text (name)
  (%rows-text (rendered name)))

(defclass server (ns:server) ()
  (:default-initargs :name :view :serves (list /minor/view))
  (:documentation "What a tool buffer is: a widget tree at a path, read as
rows, with the minor mode that moves between them."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  "Give a buffer the verbs a view needs, and the minor mode whose keys move
between rows. A mode that claims :activate answers before the built-in one,
which is what makes a tool buffer's Return its own."
  (ns:write /minor/view {:precedence 15})
  (setf pine.buf:*verbs*
        (fset:map (:activate (lambda (name) (%activate name)))
                  (:select (lambda (name delta) (%select name delta)))))
  ;; the view moved, so the buffer reads as its rows. Derived from the view,
  ;; so it is not stored and comes back by being rendered again.
  (ns:watch /buf/*/view
                 (pine.data:fn [v]
                   (let ((name (p:leaf (p:parent (ns:here)))))
                     (if v
                         (fset:map ((pine.buf:at name :text) (%text name)))
                         {})))
                 :as :view-text))

(defun show (name view &key (mode :text) (switch t))
  "Open NAME as a tool buffer showing VIEW, a function of the buffer name.

The view is written as an expression, so a tool buffer built from paths follows
them: whatever VIEW read is what rebuilds its rows.

Without SWITCH the buffer is made and left where it is, for a surface that
wants to exist before anyone looks at it."
  (pine.buf:make name)
  (ns:write (pine.buf:at name :mode) mode)
  (ns:write (pine.buf:at name :view) (funcall view name) :keep nil)
  (ns:write (pine.buf:at name :minor) [:conj :view])
  (setf (pine.buf:asked name :selection) 0)
  (ns:write (pine.buf:at name :selection) (selection-path name))
  (when switch (ns:write /buf/current (pine.buf:at name)))
  name)

(ns:register (make-instance 'server))

