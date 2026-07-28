(defpackage #:pine.editor.view
  (:use #:cl)
  (:export #:show #:install #:rendered #:view-of #:selection-path
           #:node-at-point))

(in-package #:pine.editor.view)
(named-readtables:in-readtable pine.path:syntax)

;;;; A tool buffer is a buffer with a view: a widget tree at /buf/?name/view,
;;;; written as an expression, so it is computed again whenever anything it
;;;; read moves. Nothing is stored twice -- the buffer holds no rows, no
;;;; arranged tree and no copy of what it is showing -- and the window renders
;;;; the tree at the width it has when it paints, so there is nothing to
;;;; reproject when the frame resizes.
;;;;
;;;; What a row activates and how the selection moves are the buffer's built-in
;;;; verbs, installed here because they need the widgets, which /buf sits under.

(defun view-of (name)
  "NAME's widget tree, or NIL when it is not a tool buffer."
  (pine.ns:read (pine.buf:at name :view)))

(defun %width (name)
  "The width the window showing NAME has, or the frame's."
  (let ((client pine.editor.frame:*client*))
    (or (when client
          (loop :for w :in (pine.editor.frame:windows client)
                :when (equal name (pine.text.window:window-name w))
                  :return (pine.text.window:win-width w)))
        (when client
          (pine.text.window:frame-cols (pine.editor.frame:frame client)))
        80)))

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
        (when (pine.path:pathp d) d)))))

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
          (pine.ns:write
           (fset:map ((pine.buf:at name :selection) (selection-path name))
                     ((pine.buf:at name :text) (%rows-text rows))
                     ((pine.buf:at name :point) (fset:seq line 0))))))))
  nil)

(defun %thunk (node)
  "The thunk NODE activates to: an action's callback, a selectable whose data
is a function, or such a node anywhere below."
  (typecase node
    (null nil)
    (pine.ui.node:action (pine.ui.node:callback node))
    (pine.ui.node:selectable
     (let ((d (pine.ui.node:data node)))
       (if (functionp d) d (some #'%thunk (pine.ui.layout:nodes-of node)))))
    (t (some #'%thunk (pine.ui.layout:nodes-of node)))))

(defun node-at-point (&optional (name (pine.text.buffer:name-of
                                       (pine.editor.frame:current-buffer))))
  "The node under point in NAME's rendered view, or NIL."
  (multiple-value-bind (rows tree) (rendered name)
    (declare (ignore rows))
    (when tree
      (let ((point (pine.ns:read (pine.buf:at name :point))))
        (pine.ui.layout:node-at tree
                                (or (fset:lookup point 0) 0)
                                (or (fset:lookup point 1) 0))))))

(defun %activate (name)
  "Run what the row under point activates, else the selected row's."
  (let ((thunk (or (%thunk (node-at-point name))
                   (%thunk (nth (%index name) (%selectables name))))))
    (if thunk
        (funcall thunk)
        (pine.editor.echo:message "nothing to activate here")))
  nil)

(defun %rows-text (rows)
  "ROWS as text: what a tool buffer reads as, so point, motion and search work
in one like anywhere else."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (row) (string-right-trim " " (car row))) rows)))

(defun %text (name)
  (%rows-text (rendered name)))

(defun install ()
  "Give a buffer the verbs a view needs, and the minor mode whose keys move
between rows. A mode that claims :activate answers before the built-in one,
which is what makes a tool buffer's Return its own."
  (pine.ns:write /minor/view {:precedence 15})
  (setf pine.buf:*verbs*
        (fset:map (:activate (lambda (name) (%activate name)))
                  (:select (lambda (name delta) (%select name delta)))))
  ;; the view moved, so the buffer reads as its rows. Derived from the view,
  ;; so it is not stored and comes back by being rendered again.
  (pine.ns:watch /buf/*/view
                 (pine.data:fn [v]
                   (let ((name (pine.path:leaf (pine.path:parent (pine.ns:here)))))
                     (if v
                         (fset:map ((pine.buf:at name :text) (%text name)))
                         {})))
                 :as :view-text))

(defun show (name view &key (mode :text))
  "Open NAME as a tool buffer showing VIEW, a function of the buffer name, and
switch to it.

A mode with a :view needs none of this -- writing the mode is enough -- so this
is for a view nothing declared, which is what pine's own tool buffers are until
each has a mode of its own."
  (let ((buf (pine.editor.frame:make-buffer name)))
    (pine.editor.frame:set-buffer-mode buf mode)
    (pine.ns:write (pine.buf:at name :view) (funcall view name) :keep nil)
    (pine.ns:write (pine.buf:at name :minor) [:conj :view])
    (setf (pine.buf:asked name :selection) 0)
    (pine.ns:write (pine.buf:at name :selection) (selection-path name))
    (pine.editor.frame:switch-buffer name)
    (let* ((client (pine.editor.frame:current-client))
           (r (ignore-errors (pine.editor.frame:renderer client))))
      (when r (sento.actor:tell r (list :switch-buffer :buffer buf :name name))))
    buf))
