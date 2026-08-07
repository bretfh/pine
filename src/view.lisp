(defpackage #:pine.view
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:show #:rendered #:view-of #:selection-path
           #:node-at-point))

(in-package #:pine.view)
(named-readtables:in-readtable pine.path:syntax)

(defun view-of (name)
  "NAME's widget tree, or NIL when it is not a UI buffer."
  (ns:read (pine.buf:at name :view)))

(defun %rows-text (rows)
  "ROWS as text: what a UI buffer reads as, so point, motion and search work
in one like anywhere else."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (row) (string-right-trim " " (car row))) rows)))

(defun %width (name)
  "The width the window showing NAME has."
  (or (loop :for w :in (pine.win:windows)
            :when (fset:equal? (pine.buf:at name) (ns:read (p:path w "buf")))
              :return (ns:read (p:path w "width")))
      (ns:read /win/width)
      80))

(defstruct (render (:constructor %render (rows tree view width selection))
                   (:copier nil))
  rows tree view width selection)

(defun %renders ()
  "The last render of each UI buffer. A rendered tree is not a value: it is what
the render produced, and it is here so that asking what is under point is a
lookup of a tree that has already been arranged rather than a second render of
the same one."
  (or (ns:kept :view)
      (setf (ns:kept :view)
            (sento.atomic:make-atomic-reference :value (fset:empty-map)))))

(defun %forget-render (name)
  (sento.atomic:atomic-swap (%renders) (lambda (all) (fset:less all name))))

(defun rendered (name &key selection)
  "NAME's view rendered at its window's width: (values rows tree).

What came back last time comes back again when the view, the width and the
selection are all the one it was made from. Those are everything the render
reads, so a render that would come out differently is never the one kept: the
view is written as an expression and a fresh tree arrives whenever anything it
read moved."
  (let ((tree (view-of name)))
    (when tree
      (let ((width (%width name))
            (chosen (or selection (%index name)))
            (last (fset:lookup (sento.atomic:atomic-get (%renders)) name)))
        (if (and last
                 (eq tree (render-view last))
                 (eql width (render-width last))
                 (eql chosen (render-selection last)))
            (values (render-rows last) (render-tree last))
            (multiple-value-bind (rows arranged)
                (pine.ui.cells:render tree width :selection chosen)
              (sento.atomic:atomic-swap
               (%renders)
               (lambda (all)
                 (fset:with all name (%render rows arranged tree width chosen))))
              (values rows arranged)))))))

(defun %index (name)
  "Which row is selected, as a position in the tree. Where the selection is a
place, which row holds it is this buffer's own business."
  (or (pine.buf:asked name :selection) 0))

(defun %selectables (name)
  (let ((tree (view-of name)))
    (when tree (pine.ui.layout:collect-selectables tree))))

(defun %given (node)
  "What the author attached to NODE, where the kind of node takes one. A field
is a place without being a row, so what a row carries is not asked of it."
  (when (typep node 'pine.ui.node:selectable) (pine.ui.node:data node)))

(defun stands-for (node)
  "The path NODE is a projection of: the one it was built for, else one it was
handed as its data. NIL when it stands for nothing addressable."
  (when node
    (let ((built-for (pine.ui.node:of node))
          (given (%given node)))
      (cond ((p:pathp built-for) built-for)
            ((p:pathp given) given)))))

(defun selection-path (name)
  "The path the selected row stands for, or NIL when it stands for nothing.

This is what a key a mode binds acts on: (write ${(read /buf/?name/selection)}
[:restart]) reaches the process that row is showing, and nothing had to remember
which line it was on."
  (stands-for (nth (%index name) (%selectables name))))

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
when it is chosen answers it here; anything else answers for the nodes under it.")
  (:method ((node null)) nil)
  (:method (node) (some #'activates-to (pine.ui.layout:nodes-of node))))

(defmethod activates-to ((node pine.ui.node:action))
  (pine.ui.node:callback node))

(defmethod activates-to ((node pine.ui.node:text-node))
  "A field: Return on one does what clicking it does, so an editable region in a
UI buffer is reached from the keyboard and the pointer by the same route."
  (pine.ui.layout:clicked node 0))

(defmethod activates-to ((node pine.ui.node:selectable))
  "What choosing a row runs: its :click, else a function it was handed as data,
else whatever is inside it. A row that stands for a path runs nothing here; the
mode's own keys act on the path, which is what /buf/?name/selection is for."
  (or (pine.ui.node:click node)
      (let ((given (pine.ui.node:data node)))
        (if (functionp given) given (call-next-method)))))

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

(ns:serve :view
  {:at [/minor/list]
   :doc "what a UI buffer is: a widget tree at a path, read as rows, with the
minor mode that moves between them"
   ;; a mode that claims :activate answers before the built-in one, which is
   ;; what makes a UI buffer's Return its own
   :up (lambda ()
         (ns:write /minor/list {:precedence 15})
         (setf pine.buf:*verbs*
               {:activate (lambda (name) (%activate name))
                :select (lambda (name delta) (%select name delta))})
         ;; the view moved, so the buffer reads as its rows and the selection
         ;; stands for whatever the row under it now shows. Both are read off
         ;; the one tree, so they move together and neither is stored: they come
         ;; back by being rendered again. A buffer that became a view through
         ;; its mode gets its selection here rather than only through SHOW,
         ;; which is the route a config never takes.
         (ns:watch /buf/*/view
                   (pine.data:fn [v]
                     (let ((name (p:leaf (p:parent (ns:here)))))
                       (if v
                           (fset:map ((pine.buf:at name :text) (%text name))
                                     ((pine.buf:at name :selection)
                                      (selection-path name)))
                           (progn (%forget-render name) {}))))
                   :as :view-text)
         ;; the last render of each UI buffer, which is this space's and not the
         ;; image's: two spaces show different buffers at different widths
         (sento.atomic:make-atomic-reference :value (fset:empty-map)))})

(defun show (name view &key (mode :text) (switch t))
  "Open NAME as a UI buffer showing VIEW, a function of the buffer name.

The view is written as an expression, so a UI buffer built from paths follows
them: whatever VIEW read is what rebuilds its rows.

Without SWITCH the buffer is made and left where it is, for a surface that
wants to exist before anyone looks at it."
  (pine.buf:make name)
  (ns:write (pine.buf:at name :mode) mode)
  (ns:write (pine.buf:at name :view) (funcall view name) :keep nil)
  (ns:write (pine.buf:at name :minor) [:conj :list])
  (setf (pine.buf:asked name :selection) 0)
  (ns:write (pine.buf:at name :selection) (selection-path name))
  (when switch (ns:write /buf/current (pine.buf:at name)))
  name)


