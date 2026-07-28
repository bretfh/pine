(defpackage #:pine.editor.window
  (:use #:cl)
  (:export #:split-window #:delete-other-windows-cmd #:delete-window-cmd
           #:other-window-cmd #:scroll-window #:reproject))

(in-package #:pine.editor.window)
(named-readtables:in-readtable pine.path:syntax)

;;;; Window commands, as writes to /win. The arrangement is the path, so a
;;;; split is a verb on the focused window and the live tree is built again
;;;; from what the path now says.

(defun reproject ()
  "Build the live tree again from /win and lay it out."
  (let ((client (pine.editor.frame:current-client)))
    (setf (pine.editor.frame:arrangement client)
          (pine.editor.session:editor-tree client))
    (let ((w (pine.editor.frame:focused-window)))
      (when w
        (setf (pine.editor.frame:current-buffer client)
              (pine.text.window:buffer-ref w))))
    (pine.ui.render:relayout)))

(defun split-window (orient)
  "Split the focused window: :column puts the new one below, :row beside. It
shows the same buffer, and the two share the weight the one had."
  (pine.ns:write /win/focused
                 (fset:seq :split (if (eq orient :row) :beside :below)))
  (reproject))

(defun delete-window-cmd ()
  (if (null (rest (pine.win:windows)))
      (pine.editor.echo:message "only one window")
      (progn (pine.ns:write /win/focused [:close])
             (reproject))))

(defun delete-other-windows-cmd ()
  (pine.ns:write /win/focused [:only])
  (reproject))

(defun other-window-cmd ()
  (let* ((windows (pine.win:windows))
         (at (position (pine.win:focused) windows :test #'fset:equal?)))
    (when (rest windows)
      (pine.win:focus (nth (mod (1+ (or at 0)) (length windows)) windows))
      (reproject))))

(defun scroll-window (delta)
  "Scroll the focused window by DELTA lines, taking point with it when it
would otherwise leave the window."
  (let* ((client (pine.editor.frame:current-client))
         (path (pine.win:focused))
         (w (pine.editor.frame:focused-window))
         (buf (pine.editor.frame:current-buffer client)))
    (when (and path w buf)
      (let ((snap (pine.text.window:snap w)))
        (when (and snap (typep snap 'pine.text.buffer:snapshot))
          (let* ((max-scroll (max 0 (- (pine.text.buffer:line-count snap)
                                       (pine.text.window:win-height w))))
                 (new-scroll (max 0 (min max-scroll
                                         (+ (pine.win:scroll-of path) delta))))
                 (h (pine.text.window:win-height w))
                 (pl (pine.text.buffer:point-line snap))
                 (pc (pine.text.buffer:point-col snap)))
            (pine.ns:write (pine.path:child path "scroll") new-scroll)
            (setf (pine.text.window:scroll-top w) new-scroll)
            (cond
              ((< pl new-scroll)
               (pine.text.buffer:put-point
                buf new-scroll
                (min pc (length (fset:@ (pine.text.buffer:lines snap) new-scroll)))))
              ((>= pl (+ new-scroll h))
               (let ((target (+ new-scroll h -1)))
                 (pine.text.buffer:put-point
                  buf target
                  (min pc (length (fset:@ (pine.text.buffer:lines snap) target)))))))
            (sento.actor:tell (pine.editor.frame:renderer client)
                              '(:force-render))))))))
