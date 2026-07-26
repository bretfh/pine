(defpackage #:pine.editor.window
  (:use #:cl)
  (:export #:%split-window #:delete-other-windows-cmd #:delete-window-cmd #:other-window-cmd #:scroll-window))

(in-package #:pine.editor.window)

;;;; Window commands over the live editor tree. The arrangement is layout
;;;; nodes; a split wraps the focused window leaf in a column/row with a new
;;;; window on the same buffer, delete prunes, other-window cycles the leaves.

(defun %editor-leaves (&optional (client (pine.editor.frame:current-client)))
  "The window leaves of CLIENT's live tree, in tree order."
  (let ((tree (pine.editor.frame:arrangement client)) (acc nil))
    (when tree
      (labels ((walk (n)
                 (when (and (typep n 'pine.ui.node:window-node)
                            (eq (pine.ui.node:window-kind n) :window)
                            (pine.ui.node:window-of n))
                   (push n acc))
                 (dolist (c (pine.ui.node:nodes-of n)) (walk c))))
        (walk tree)))
    (nreverse acc)))

(defun %focused-leaf (&optional (client (pine.editor.frame:current-client)))
  (find (pine.editor.frame:focused-window client) (%editor-leaves client)
        :key #'pine.ui.node:window-of))

(defun %focus-leaf (leaf)
  "Focus LEAF's backing window and follow with the current buffer, so typing
lands in it."
  (let ((client (pine.editor.frame:current-client))
        (w (pine.ui.node:window-of leaf)))
    (pine.editor.frame:focus-window w)
    (setf (pine.editor.frame:current-buffer client) (pine.text.buffer:buffer-ref w))
    (pine.state.world:save-world :arrangement)))

(defun %split-window (orient)
  "Split the focused window along ORIENT (:column below, :row beside): a new
window on the same buffer joins the parent as a flat sibling when the parent
already stacks that way, else the leaf wraps in a fresh stack. A divider sits
between; sizes stay even because siblings share one weight."
  (let* ((client (pine.editor.frame:current-client))
         (tree (pine.editor.frame:arrangement client))
         (leaf (%focused-leaf client)))
    (unless leaf
      (pine.editor.echo:message "no window to split")
      (return-from %split-window))
    (let* ((w (pine.ui.node:window-of leaf))
           (buf (pine.text.buffer:buffer-ref w))
           (weight (max 1 (pine.ui.node:expand-of leaf)))
           (nw (pine.editor.frame:make-window buf (pine.text.buffer:window-name w)))
           (nn (pine.ui.node:window nil :of nw :kind :window :expand weight
                                   :font-px (pine.ui.node:font-px leaf)
                                   :opacity (pine.ui.node:window-opacity leaf)))
           (div (pine.ui.node:rule :vertical (eq orient :row)
                                  :face :border-inactive))
           (root (pine.ui.node:split-node tree leaf nn orient :divider div)))
      (setf (pine.text.buffer:snap nw) (pine.text.buffer:snap w)
            (pine.text.buffer:scroll-top nw) (pine.text.buffer:scroll-top w))
      (unless root
        (pine.editor.echo:message "cannot split here")
        (return-from %split-window))
      (setf (pine.editor.frame:arrangement client) root)
      (%focus-leaf leaf)
      (pine.ui.render:relayout))))

(defun %delete-leaf (leaf)
  "Remove LEAF and its divider from the live tree, splicing out a container
left with one child, and dropping its backing window."
  (let* ((client (pine.editor.frame:current-client))
         (root (pine.ui.node:remove-node (pine.editor.frame:arrangement client) leaf)))
    (when root
      (setf (pine.editor.frame:arrangement client) root)
      (pine.editor.frame:remove-window (pine.ui.node:window-of leaf))
      t)))

(defun delete-window-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (leaves (%editor-leaves client))
         (leaf (%focused-leaf client)))
    (cond
      ((null (rest leaves)) (pine.editor.echo:message "only one window"))
      ((and leaf (%delete-leaf leaf))
       (let ((next (first (%editor-leaves client))))
         (when next (%focus-leaf next)))
       (pine.ui.render:relayout))
      (t (pine.editor.echo:message "cannot delete this window")))))

(defun delete-other-windows-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (leaf (%focused-leaf client)))
    (when leaf
      (dolist (other (remove leaf (%editor-leaves client) :test #'eq))
        (%delete-leaf other))
      (%focus-leaf leaf)
      (pine.ui.render:relayout))))

(defun other-window-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (leaves (%editor-leaves client))
         (pos (position (%focused-leaf client) leaves :test #'eq)))
    (when (and leaves (rest leaves))
      (%focus-leaf (nth (mod (1+ (or pos 0)) (length leaves)) leaves)))))

(defun scroll-window (delta)
  (let* ((client (pine.editor.frame:current-client))
         (w (pine.editor.frame:focused-window client))
         (buf (pine.editor.frame:current-buffer client)))
    (when (and w buf)
      (let ((snap (pine.text.buffer:snap w)))
        (when (and snap (typep snap 'pine.text.buffer:snapshot))
          (let* ((max-scroll (max 0 (- (pine.text.buffer:line-count snap)
                                       (pine.text.buffer:win-height w))))
                 (new-scroll (max 0 (min max-scroll
                                         (+ (pine.text.buffer:scroll-top w) delta))))
                 (h (pine.text.buffer:win-height w))
                 (pl (pine.text.buffer:point-line snap))
                 (pc (pine.text.buffer:point-col snap)))
            (setf (pine.text.buffer:scroll-top w) new-scroll)
            (cond
              ((< pl new-scroll)
               (sento.actor:tell buf (list :move-point :line new-scroll
                 :col (min pc (length (fset:@ (pine.text.buffer:lines snap) new-scroll))))))
              ((>= pl (+ new-scroll h))
               (let ((target (+ new-scroll h -1)))
                 (sento.actor:tell buf (list :move-point :line target
                   :col (min pc (length (fset:@ (pine.text.buffer:lines snap) target))))))))
            (sento.actor:tell (pine.editor.frame:renderer client) '(:force-render))))))))
