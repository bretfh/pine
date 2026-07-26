(defpackage #:pine.editor.layout
  (:use #:cl)
  (:export #:layout-activate #:layout-node-at-point #:layout-select #:show-layout))

(in-package #:pine.editor.layout)

;;;; Interaction on layout buffers. A layout buffer's snapshot carries the
;;;; arranged node tree (:layout-tree) and the selection index
;;;; (:layout-selection), so navigation is data + re-render -- the published
;;;; tree is never mutated -- and activation resolves point (or the selection)
;;;; to a node and runs its thunk: an action's callback, or a selectable whose
;;;; :data is a function.

(defun %layout-buffer ()
  (pine.editor.frame:current-buffer (pine.editor.frame:current-client)))

(defun %layout-snap (&optional (buf (%layout-buffer)))
  (and buf (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)))

(defun layout-tree (&optional (snap (%layout-snap)))
  (and snap (pine.text.buffer:buffer-local snap :layout-tree)))

(defun layout-node-at-point ()
  "The node under point on the current buffer's layout tree, or nil."
  (let* ((snap (%layout-snap))
         (tree (layout-tree snap)))
    (when tree
      (pine.ui.layout:node-at tree (pine.text.buffer:point-line snap)
                           (pine.text.buffer:point-col snap)))))

(defun layout-select (delta)
  "Move the layout selection by DELTA (wrapping), reproject, and land point on
the selected row. The reproject and the snapshot read serialize in the buffer's
mailbox, so the tree we read is the reprojected one."
  (let* ((buf (%layout-buffer))
         (snap (%layout-snap buf))
         (tree (layout-tree snap)))
    (when tree
      (let* ((n (length (pine.ui.layout:collect-selectables tree)))
             (cur (pine.text.buffer:buffer-local snap :layout-selection 0))
             (new (if (plusp n) (mod (+ cur delta) n) 0)))
        (sento.actor:tell buf (list :reproject :selection new))
        (let* ((snap2 (%layout-snap buf))
               (tree2 (layout-tree snap2))
               (sel (and tree2 (nth new (pine.ui.layout:collect-selectables tree2)))))
          (when sel
            (sento.actor:tell buf (list :move-point
                                        :line (pine.ui.node:start-line sel)
                                        :col 0))))))))

(defun %node-activation (node)
  "The thunk NODE activates to: an action's callback, a selectable whose data is
a function, or such a node anywhere below."
  (typecase node
    (null nil)
    (pine.ui.node:action (pine.ui.node:callback node))
    (pine.ui.node:selectable
     (let ((d (pine.ui.node:data node)))
       (if (functionp d) d (some #'%node-activation (pine.ui.layout:nodes-of node)))))
    (t (some #'%node-activation (pine.ui.layout:nodes-of node)))))

(defun layout-activate ()
  "Run the activation under point, else the selected row's."
  (let* ((snap (%layout-snap))
         (tree (layout-tree snap)))
    (when tree
      (let* ((at (pine.ui.layout:node-at tree (pine.text.buffer:point-line snap)
                                      (pine.text.buffer:point-col snap)))
             (sel (nth (pine.text.buffer:buffer-local snap :layout-selection 0)
                       (pine.ui.layout:collect-selectables tree)))
             (thunk (or (%node-activation at) (%node-activation sel))))
        (if thunk
            (funcall thunk)
            (pine.editor.echo:message "nothing to activate here"))))))

(defun show-layout (name builder &key (mode :base-mode) (selection 0))
  "Open buffer NAME as a layout buffer showing BUILDER (state -> node tree),
switch to it, and enable layout-mode on it. Returns the buffer."
  (let* ((client (pine.editor.frame:current-client))
         (cols (pine.text.window:frame-cols (pine.editor.frame:frame client)))
         (buf (pine.editor.frame:make-buffer name)))
    (pine.editor.frame:set-buffer-mode buf mode)
    (pine.editor.ask:tell buf :set-layout :builder builder :width cols
                          :selection selection)
    (pine.ui.render:subscribe-to-buffer buf)
    (pine.editor.frame:switch-buffer name)
    (let ((r (ignore-errors (pine.editor.frame:renderer client))))
      (when r (sento.actor:tell r (list :switch-buffer :buffer buf :name name))))
    (ignore-errors (pine.editor.frame:enable-minor-mode client :layout-mode))
    buf))
