(defpackage #:pine.editor.win
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path)
                    (#:frame #:pine.editor.frame) (#:pane #:pine.pane))
  (:export #:split-window #:delete-other-windows-cmd #:delete-window-cmd
           #:other-window-cmd #:scroll-window))

(in-package #:pine.editor.win)
(named-readtables:in-readtable pine.path:syntax)

;;;; Window commands, as writes to /win. The arrangement is the path, so a
;;;; split is a verb on the focused window and the tree follows it: nothing
;;;; here rebuilds anything, because the watch on /win does.

(defun split-window (orient)
  "Split the focused window: :column puts the new one below, :row beside. It
shows the same buffer, and the two share the weight the one had."
  (ns:write /win/focused (fset:seq :split (if (eq orient :row) :beside :below))))

(defun delete-window-cmd ()
  (if (null (rest (pine.win:windows)))
      (pine.echo:message "only one window")
      (ns:write /win/focused [:close])))

(defun delete-other-windows-cmd ()
  (ns:write /win/focused [:only]))

(defun other-window-cmd ()
  (let* ((windows (pine.win:windows))
         (at (position (pine.win:focused) windows :test #'fset:equal?)))
    (when (rest windows)
      (pine.win:focus (nth (mod (1+ (or at 0)) (length windows)) windows)))))

(defun %pane ()
  "The pane the keyboard is in: the focused window, or the current buffer when
nothing is arranged."
  (or (pine.win:focused) /buf/current))

(defun %height (at)
  "How many lines the pane at AT is showing. Its own leaf when the renderer has
said, which it does every frame it arranges one."
  (or (ns:read (p:path at "height")) 24))

(defun scroll-window (delta)
  "Scroll the pane the keyboard is in by DELTA lines, taking point with it when
it would otherwise leave the pane.

Every part of this is a place: where the pane is scrolled, how tall the renderer
last arranged it, and where point is. So a scroll is a write, and the pane a
config named scrolls the same as one in the arrangement."
  (let* ((at (%pane))
         (buffer (pane:subject at))
         (name (p:leaf buffer))
         (lines (or (ns:held (pine.buf:at name :text)) (fset:seq "")))
         (height (%height at))
         (highest (max 0 (- (fset:size lines) height)))
         (top (max 0 (min highest (+ (pane:scroll at) delta)))))
    (setf (pane:scroll at) top)
    (multiple-value-bind (line col) (pine.buf:point name)
      (let ((target (cond ((< line top) top)
                          ((>= line (+ top height)) (+ top height -1))
                          (t nil))))
        (when target
          (pine.buf:put-point name target
                              (min col (length (fset:@ lines target)))))))
    (let ((client (ignore-errors (frame:current-client))))
      (when client
        (sento.actor:tell (frame:renderer client) '(:force-render))))))
