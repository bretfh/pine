(defpackage #:pine.editor.render
  (:use #:cl)
  (:export
   #:start-renderer
   #:watch-buffers
   #:unwatch-buffers
   #:modeline-rows
   #:echo-rows
   #:arrange-editor-tree
   #:refresh-editor-tree
   #:view-leaves
   #:relayout))

(in-package #:pine.editor.render)
(named-readtables:in-readtable pine.path:syntax)

;;;; One frame: arrange the client's live tree through the layout engine, then
;;;; ask each pane what it shows. What a pane shows is the content type's own
;;;; answer, so nothing here knows what a buffer is.

(defun rs-update (&rest pairs)
  (let* ((client (pine.editor.frame:current-client))
         (rs (pine.editor.frame:render-state client)))
    (loop for (k v) on pairs by #'cddr
          do (setf rs (fset:with rs k v)))
    (setf (pine.editor.frame:render-state client) rs)))

;;;; Renderer actor (per client). The renderer reads what changed rather than
;;;; being sent it, so nothing has to remember to subscribe and a buffer nobody
;;;; is watching costs nothing.

(defun watch-buffers (client)
  "Repaint because a buffer moved.

Told each commit rather than each path: one edit moves the text, the point, the
tick and the modified flag in one transaction, and a watch over /buf hears about
each of them. A frame is what the tree says now, so a commit is one frame
however many leaves it carried."
  (setf (pine.ns:on-commit (list :renderer client))
        (lambda (moved)
          (when (some (lambda (change) (pine.path:prefixp /buf (first change)))
                      moved)
            (let ((renderer (pine.editor.frame:renderer client)))
              (when renderer (sento.actor:tell renderer '(:force-render))))))))

(defun unwatch-buffers (client)
  (setf (pine.ns:on-commit (list :renderer client)) nil))

(defun paint-frame (client)
  "A frame is due: fire the client's paint sink, which is the seam to the
attached frontend."
  (let ((sink (pine.editor.frame:paint-sink client)))
    (when sink (funcall sink))))

(defun start-renderer (client)
  (let* ((sys (pine.core.server:actor-system (pine.editor.frame:server-of client)))
         (renderer
           (sento.actor-context:actor-of sys
             :name (format nil "renderer-~a" (gensym "R"))
             ;; the frame path owns its thread: nothing else's receive can be in
             ;; front of a repaint
             :dispatcher :pinned
             :state nil
             :receive
             (lambda (msg)
               (let ((pine.editor.frame:*client* client))
                 (handler-case
                     (case (first msg)
                       (:resize
                        (destructuring-bind (&key cols rows width height cell-w cell-h)
                            (rest msg)
                          (setf (pine.editor.frame:cols client) cols
                                (pine.editor.frame:rows client) rows
                                (pine.editor.frame:px-width client) width
                                (pine.editor.frame:px-height client) height
                                (pine.editor.frame:cell-w client) cell-w
                                (pine.editor.frame:cell-h client) cell-h)
                          (relayout)
                          (pine.term:resize-active-terminal cols rows)
                          (paint-frame client)))
                       (:switch-buffer
                        ;; what a pane shows is its own path, already written;
                        ;; this is the repaint that follows
                        (rs-update :dirty t)
                        (paint-frame client))
                       (:term-tick
                        (when (pine.term:drain-terminals client)
                          (paint-frame client)))
                       ((:snapshot :force-render)
                        (paint-frame client)))
                   ;; the renderer runs with its client bound and must not block,
                   ;; since it draws the debugger, so a failure goes out through
                   ;; the same command-error path: the echo line, or the
                   ;; *debugger* under debug-on-error
                   (error (e)
                     (ignore-errors (pine.key:command-error e)))))))))
    (setf (pine.editor.frame:renderer client) renderer)
    (watch-buffers client)
    renderer))

;;;; Arranging

(defun view-leaves (tree)
  "The pane leaves under TREE, in tree order."
  (let (acc)
    (labels ((walk (n)
               (when (and (typep n 'pine.ui.node:view-node)
                          (not (eq (class-of n)
                                   (find-class 'pine.ui.node:view-node))))
                 (push n acc))
               (dolist (c (pine.ui.layout:nodes-of n)) (walk c))))
      (walk tree))
    (nreverse acc)))

(defun %leaf-width (n)
  (max 1 (- (pine.ui.node:end-col n) (pine.ui.node:start-col n))))

(defun %leaf-height (n)
  (max 1 (1+ (- (pine.ui.node:end-line n) (pine.ui.node:start-line n)))))

(defun %px-metrics (client)
  "The client's reported cell metrics, when the frontend gave its pixel
geometry with :resize; (values cell-w cell-h px-w px-h) or nil."
  (let ((cw (pine.editor.frame:cell-w client)) (ch (pine.editor.frame:cell-h client))
        (pw (pine.editor.frame:px-width client)) (ph (pine.editor.frame:px-height client)))
    (when (and cw ch pw ph (plusp cw) (plusp ch))
      (values cw ch pw ph))))

(defun %cell-metric (cw ch)
  "A *text-size* measurer for a uniform monospace cell grid."
  (lambda (text pine.ui.node:font-px)
    (declare (ignore pine.ui.node:font-px))
    (values (* (length text) cw) ch)))

(defun %leaf-cols (client n)
  "N's arranged width in cells, whichever unit the tree was arranged in."
  (let ((cw (pine.editor.frame:cell-w client)))
    (if cw (max 1 (floor (%leaf-width n) cw)) (%leaf-width n))))

(defun %leaf-lines (client n)
  "N's arranged height in cells."
  (let ((ch (pine.editor.frame:cell-h client)))
    (if ch (max 1 (floor (%leaf-height n) ch)) (%leaf-height n))))

(defun %publish-panes (client leaves)
  "Say how tall each pane was arranged, and tell each buffer the union of the
line ranges the panes showing it display.

The height is a place because everything else that needs it -- a scroll command,
a page down, the range a buffer bounds its parse by -- is not the renderer and
should not have to ask it. A write of a height that did not change moves
nothing, so this settles after the first frame at a size."
  (let ((ranges (make-hash-table :test 'equal)))
    (dolist (n leaves)
      (let ((at (pine.ui.node:of n)))
        (when (and at (pine.path:pathp at)
                   (typep n 'pine.ui.node:buffer-view))
          (let* ((name (pine.path:leaf (pine.pane:subject at)))
                 (top (pine.pane:scroll at))
                 (height (%leaf-lines client n))
                 (held (gethash name ranges)))
            (pine.ns:write (pine.path:path at "height") height)
            (pine.ns:write (pine.path:path at "width") (%leaf-cols client n))
            (setf (gethash name ranges)
                  (if held
                      (cons (min (car held) top) (max (cdr held) (+ top height)))
                      (cons top (+ top height))))))))
    (maphash (lambda (name range)
               (pine.buf:showing name (fset:seq (car range) (cdr range))))
             ranges)))

(defun arrange-editor-tree (client)
  "Arrange the client's live tree once: in pixels at the frontend's reported
geometry when it gave one, so the rects cross the wire and are painted as they
are, else in cells at the frame size. Answers the pane leaves."
  (let ((tree (pine.editor.frame:tree client)))
    (when tree
      (multiple-value-bind (cw ch pw ph) (%px-metrics client)
        (let ((aw (if cw pw (pine.editor.frame:cols client)))
              (ah (if cw ph (pine.editor.frame:rows client)))
              (leaves (view-leaves tree)))
          (dolist (n leaves)
            (setf (pine.ui.node:view-rows n) nil
                  (pine.ui.node:view-crow n) -1
                  (pine.ui.node:view-ccol n) -1))
          (let ((pine.ui.layout:*text-size* (when cw (%cell-metric cw ch))))
            (pine.ui.layout:measure tree aw ah)
            (pine.ui.layout:arrange tree 0 0 aw ah))
          (%publish-panes client leaves)
          leaves)))))

;;;; Filling a pane

(defun %pad (str width)
  (let ((s (if (> (length str) width) (subseq str 0 width) str)))
    (concatenate 'string s (make-string (max 0 (- width (length s)))
                                        :initial-element #\Space))))

(defun %one-row (text cols face-name)
  "TEXT as a single wire row COLS wide, in FACE-NAME."
  (let ((r (pine.ui.raster:make-raster cols 1))
        (fg (pine.ui.face:face-fg face-name))
        (bg (pine.ui.face:face-bg face-name))
        (padded (%pad text cols)))
    (loop :for i :from 0 :below (min (length padded) cols)
          :do (pine.ui.raster:raster-put-rgb
               r 0 i (char padded i) (first fg) (second fg) (third fg)
               (if bg (first bg) -1) (if bg (second bg) -1) (if bg (third bg) -1)
               0))
    (pine.ui.cells:rows-of r)))

(defun modeline-rows (at cols)
  "The mode line of the pane at AT: what it shows, the mode that buffer is in,
and where point is. Read off the paths, so a mode line beside a pane a config
named needs no object either."
  (let* ((name (pine.path:leaf (pine.pane:subject at)))
         (line (multiple-value-bind (l c) (pine.buf:point name)
                 (format nil " ~a   ~a   L~d C~d" name
                         (or (pine.mode:setting
                              (pine.editor.frame:buffer-mode name) :indicator)
                             "")
                         (1+ l) c))))
    (%one-row line cols :modeline)))

(defun echo-rows (client cols)
  "The echo block at COLS: the completion popup rows, while a prompt is up,
above the echo line. Answers (values rows crow ccol), the minibuffer caret
within the block, or -1 -1 when no prompt is up."
  (declare (ignore client))
  (pine.ns:write /echo/cols cols)
  (let* ((prompt (pine.echo:prompt-text))
         (prows (and (pine.echo:prompt-p) (pine.echo:popup-rows)))
         (mb-snap (and prompt (pine.echo:snap)))
         (input (if (and mb-snap (plusp (pine.text:line-count mb-snap)))
                    (fset:@ (pine.text:lines mb-snap) 0)
                    ""))
         (text (if prompt
                   (concatenate 'string prompt input)
                   (pine.echo:current-message))))
    (values (append prows (%one-row text cols (if prompt :prompt :echo)))
            (if prompt 0 -1)
            (if prompt
                (+ (length prompt)
                   (if mb-snap (pine.text:point-col mb-snap) (length input)))
                -1))))

(defgeneric render-leaf (node client focused prompt)
  (:documentation "Put NODE's rows into it for this frame.")
  (:method (node client focused prompt)
    (declare (ignore node client focused prompt))
    nil))

(defun %has-the-keyboard-p (at focused)
  "Whether the caret belongs in the pane at AT.

The arrangement says which pane has the keyboard. A config that names a buffer
directly never touches /win, and then the pane showing the current buffer is the
one being typed into."
  (if focused
      (and (pine.path:pathp at) (fset:equal? at focused))
      (fset:equal? (pine.pane:subject at) (pine.pane:subject /buf/current))))

(defmethod render-leaf ((n pine.ui.node:buffer-view) client focused prompt)
  "What the pane shows, asked of the layer: the node names its content and the
content type answers."
  (let ((at (pine.ui.node:of n)))
    (when at
      (multiple-value-bind (rows crow ccol)
          (pine.pane:rows at (%leaf-cols client n) (%leaf-lines client n))
        (setf (pine.ui.node:view-rows n) rows)
        (when (and (not prompt) (%has-the-keyboard-p at focused))
          (setf (pine.ui.node:view-crow n) crow
                (pine.ui.node:view-ccol n) ccol))))))

(defmethod render-leaf ((n pine.ui.node:modeline-view) client focused prompt)
  (declare (ignore prompt))
  (let ((at (or (pine.ui.node:of n) focused /buf/current)))
    (setf (pine.ui.node:view-rows n)
          (modeline-rows at (%leaf-cols client n)))))

(defmethod render-leaf ((n pine.ui.node:echo-view) client focused prompt)
  (declare (ignore focused prompt))
  (multiple-value-bind (rows crow ccol) (echo-rows client (%leaf-cols client n))
    (setf (pine.ui.node:view-rows n) rows
          (pine.ui.node:view-crow n) crow
          (pine.ui.node:view-ccol n) ccol)))

(defun refresh-editor-tree (client)
  "One frame: arrange through the engine, then fill every pane. Answers the
tree, or nil when the client has none."
  (let ((tree (pine.editor.frame:tree client))
        (leaves (arrange-editor-tree client)))
    (when tree
      ;; the focused pane is a path, the same as every other pane: the caret is
      ;; drawn in the one the arrangement says has the keyboard
      (let ((focused (pine.win:focused))
            (prompt (pine.echo:prompt-p)))
        (dolist (n leaves)
          (render-leaf n client focused prompt)))
      tree)))

(defun relayout ()
  "Re-arrange the current client's tree. Nothing is saved: the split shape is
/win and a held path is the store's business, so a crash cannot lose it."
  (arrange-editor-tree (pine.editor.frame:current-client)))
