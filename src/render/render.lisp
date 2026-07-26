(in-package :pine.render)

(defun rs@ (key)
  (fset:@ (pine.client:render-state (pine.client:current-client)) key))

(defun rs-update (&rest pairs)
  (let* ((client (pine.client:current-client))
         (rs (pine.client:render-state client)))
    (loop for (k v) on pairs by #'cddr
          do (setf rs (fset:with rs k v)))
    (setf (pine.client:render-state client) rs)))


;;;; Frame wire encoding

(defun frame->rows (frame)
  "Encode FRAME's cells as wire rows (text . runs), each run
(col fr fg fb br bg bb attr) extending to the next run's col. Cells carry
their own (row col); scatter by those."
  (let ((cells (pine.buffer:frame-cells frame))
        (cols (pine.buffer:frame-cols frame))
        (rows (pine.buffer:frame-rows frame))
        (count (pine.buffer:frame-cell-count frame)))
    (when (and cells (plusp rows))
      (let ((chars  (make-array rows))
            (styles (make-array rows)))
        (dotimes (r rows)
          (setf (aref chars r) (make-string cols :initial-element #\space)
                (aref styles r) (make-array cols :initial-element nil)))
        (loop for i from 0 below count by 10
              for row = (svref cells i)
              for col = (svref cells (+ i 1))
              when (and (integerp row) (< -1 row rows) (integerp col) (< -1 col cols))
                do (setf (char (aref chars row) col) (code-char (svref cells (+ i 2)))
                         (aref (aref styles row) col)
                         (list (svref cells (+ i 3)) (svref cells (+ i 4)) (svref cells (+ i 5))
                               (svref cells (+ i 6)) (svref cells (+ i 7)) (svref cells (+ i 8))
                               (svref cells (+ i 9)))))
        (loop for r from 0 below rows collect
          (let ((text (aref chars r)) (row-styles (aref styles r)) (runs nil) (prev nil))
            (dotimes (c cols)
              (let ((style (or (aref row-styles c)
                               (append (pine.buffer:face-fg :default) '(-1 -1 -1 0)))))
                (unless (equal style prev)
                  (push (list* c style) runs)
                  (setf prev style))))
            (cons text (nreverse runs))))))))

;;;; Renderer actor (per-client)

(defun paint-frame (client)
  "A frame is due: fire the client's paint sink. The sink is the seam to the
attached frontend -- the editor session's sink refreshes the live tree
(refresh-editor-tree) and pushes it to the app as widgets."
  (let ((sink (pine.client:paint-sink client)))
    (when sink (funcall sink))))

(defun start-renderer (client)
  (let* ((sys (pine.core.server:actor-system (pine.client:server-of client)))
         (renderer
           (sento.actor-context:actor-of sys
             :name (format nil "renderer-~a" (gensym "R"))
             :state nil
             :receive
             (lambda (msg)
               (let ((pine.client:*client* client))
                 (handler-case
                     (case (first msg)
                       (:snapshot
                        (destructuring-bind (&key snapshot) (rest msg)
                          (apply-snapshot snapshot)
                          (paint-frame client)))
                       (:resize
                        (destructuring-bind (&key cols rows width height cell-w cell-h)
                            (rest msg)
                          (let ((f (pine.client:frame client)))
                            (setf (pine.buffer:frame-cols f) cols
                                  (pine.buffer:frame-rows f) rows
                                  (pine.client:px-width client) width
                                  (pine.client:px-height client) height
                                  (pine.client:cell-w client) cell-w
                                  (pine.client:cell-h client) cell-h))
                          (relayout)
                          (pine.term:resize-active-terminal cols rows)
                          ;; a layout buffer laid out for an old width
                          ;; reprojects at its window's new one
                          (dolist (w (pine.client:windows client))
                            (let ((s (pine.buffer:snap w)))
                              (when (and s (pine.buffer:buffer-local s :layout-builder)
                                         (/= (pine.buffer:win-width w)
                                             (pine.buffer:buffer-local s :layout-width 0)))
                                (sento.actor:tell (pine.buffer:buffer-ref w)
                                                  (list :reproject
                                                        :width (pine.buffer:win-width w))))))
                          (paint-frame client)))
                       (:switch-buffer
                        (destructuring-bind (&key buffer name) (rest msg)
                          (switch-window-buffer buffer name)
                          (paint-frame client)))
                       (:term-tick
                        (when (pine.term:drain-terminals client)
                          (paint-frame client)))
                       (:force-render
                        (paint-frame client)))
                   ;; the renderer runs with its client bound and must not block
                   ;; (it draws the debugger), so surface non-blocking through the
                   ;; same command-error path: echo, or the *debugger* under
                   ;; debug-on-error. never a silent stderr drop.
                   (error (e)
                     (ignore-errors (pine.editor.command:command-error e)))))))))
    (setf (pine.client:renderer client) renderer)
    renderer))


;;;; Window management

(defun apply-snapshot (snap)
  (when (typep snap 'pine.buffer:snapshot)
    (let ((snap-name (pine.buffer:name snap)))
      (dolist (w (pine.client:windows (pine.client:current-client)))
        (when (string= (pine.buffer:window-name w) snap-name)
          (setf (pine.buffer:snap w) snap))))))

(defun switch-window-buffer (buf name)
  (let ((w (pine.client:focused-window (pine.client:current-client))))
    (when w
      (setf (pine.buffer:buffer-ref w) buf
            (pine.buffer:window-name w) name
            (pine.buffer:scroll-top w) 0
            (pine.buffer:col w) 0
            (pine.buffer:snap w) nil
            (pine.buffer:win-display w) nil)))
  (rs-update :dirty t))

(defun %window-leaves (tree)
  "The view leaves under TREE (window nodes carrying a kind), in tree order."
  (let (acc)
    (labels ((walk (n)
               (when (and (typep n 'pine.layout:window-node)
                          (pine.layout:window-kind n))
                 (push n acc))
               (dolist (c (pine.layout:nodes-of n)) (walk c))))
      (walk tree))
    (nreverse acc)))

(defun %leaf-width (n)
  "N's arranged width in cells."
  (max 1 (- (pine.layout:end-col n) (pine.layout:start-col n))))

(defun %leaf-height (n)
  "N's arranged height in cells."
  (max 1 (1+ (- (pine.layout:end-line n) (pine.layout:start-line n)))))

(defun %px-metrics (client)
  "The client's reported cell metrics, when the frontend gave its pixel
geometry with :resize; (values cell-w cell-h px-w px-h) or nil."
  (let ((cw (pine.client:cell-w client)) (ch (pine.client:cell-h client))
        (pw (pine.client:px-width client)) (ph (pine.client:px-height client)))
    (when (and cw ch pw ph (plusp cw) (plusp ch))
      (values cw ch pw ph))))

(defun %cell-metric (cw ch)
  "A *text-size* measurer for a uniform monospace cell grid."
  (lambda (text font-px)
    (declare (ignore font-px))
    (values (* (length text) cw) ch)))

(defun %leaf-cols (client n)
  "N's arranged width in cells, whichever unit the tree was arranged in."
  (let ((cw (pine.client:cell-w client)))
    (if cw (max 1 (floor (%leaf-width n) cw)) (%leaf-width n))))

(defun arrange-editor-tree (client)
  "Arrange the client's live editor tree ONCE, on the daemon: in pixels at the
frontend's reported geometry when it gave one (the rects cross the wire and
the frontend paints them as-is), else in cells at the frame size. Each view
leaf's rect sizes its backing window. Returns the leaves."
  (let ((tree (pine.client:arrangement client))
        (f (pine.client:frame client)))
    (when tree
      (multiple-value-bind (cw ch pw ph) (%px-metrics client)
        (let ((aw (if cw pw (pine.buffer:frame-cols f)))
              (ah (if cw ph (pine.buffer:frame-rows f)))
              (leaves (%window-leaves tree)))
          (dolist (n leaves)
            (setf (pine.layout:window-rows n) nil
                  (pine.layout:window-crow n) -1
                  (pine.layout:window-ccol n) -1))
          (let ((pine.layout:*text-size* (when cw (%cell-metric cw ch))))
            (pine.layout:measure tree aw ah)
            (pine.layout:arrange tree 0 0 aw ah))
          (dolist (n leaves)
            (let ((w (pine.layout:window-of n)))
              (when (and w (eq (pine.layout:window-kind n) :window))
                (setf (pine.buffer:win-width w)
                      (if cw (max 1 (floor (%leaf-width n) cw)) (%leaf-width n))
                      (pine.buffer:win-height w)
                      (if ch (max 1 (floor (%leaf-height n) ch)) (%leaf-height n))))))
          leaves)))))

(defun refresh-editor-tree (client)
  "One frame of the live editor tree: arrange through the engine, fit each
backing window to its arranged rect, render every view leaf's rows. Returns
the tree, or nil when the client has none."
  (let ((tree (pine.client:arrangement client))
        (leaves (arrange-editor-tree client)))
    (when tree
      (let ((focused (pine.client:focused-window client))
            (prompt (pine.echo:prompt-active-p)))
        (dolist (n leaves)
          (ecase (pine.layout:window-kind n)
            (:window
             (let ((w (pine.layout:window-of n)))
               (when w
                 (multiple-value-bind (rows crow ccol) (render-window-rows w)
                   (setf (pine.layout:window-rows n) rows)
                   (when (and (eq w focused) (not prompt))
                     (setf (pine.layout:window-crow n) crow
                           (pine.layout:window-ccol n) ccol))))))
            (:modeline
             (let ((w (or (pine.layout:window-of n) focused)))
               (when w
                 (setf (pine.layout:window-rows n)
                       (modeline-rows w (%leaf-cols client n))))))
            (:echo
             (multiple-value-bind (rows crow ccol)
                 (echo-rows client (%leaf-cols client n))
               (setf (pine.layout:window-rows n) rows
                     (pine.layout:window-crow n) crow
                     (pine.layout:window-ccol n) ccol))))))
      tree)))

(defun relayout ()
  "Re-arrange the current client's live editor tree at the frame size, and
save the arrangement to the world -- every structural mutation ends here, so
a crash never loses the split shape."
  (prog1 (arrange-editor-tree (pine.client:current-client))
    (pine.state.world:save-world :arrangement)))


;;;; Cell emission

(defun face-rgb (face-name)
  (pine.buffer:face-fg face-name))

(defun face-attrs (face-name)
  "The packed bold/italic/underline bits for FACE-NAME (0 when unset)."
  (pine.buffer:face-attr-bits (when face-name (pine.buffer:find-face face-name))))

(defun build-highlight-table (highlights scroll-top visible-rows)
  (let ((table (make-hash-table)))
    (dolist (h highlights)
      (let ((line (first h)) (sc (second h)) (ec (third h)) (face (fourth h)))
        (when (and (>= line scroll-top) (< line (+ scroll-top visible-rows)))
          (push (list sc ec face) (gethash (- line scroll-top) table)))))
    (maphash (lambda (k v)
               (setf (gethash k table) (nreverse v)))
             table)
    table))

(defun face-priority (face)
  (case face
    (:keyword        10)
    (:function-name   8)
    (:function-call   8)
    (:builtin         7)
    (:constant        6)
    (:number          6)
    (:escape          6)
    (:string          5)
    (:comment         5)
    (:variable-param  9)
    (:type            4)
    (:variable        1)
    (t                0)))

(defun build-face-slots (line-hl line-length)
  (let ((slots (make-array line-length :initial-element nil))
        (prios (make-array line-length :initial-element -1)))
    (dolist (h line-hl)
      (let* ((sc (first h))
             (ec (min (second h) line-length))
             (face (third h))
             (p (face-priority face)))
        (when face
          (loop for c from (max 0 sc) below ec
                when (> p (aref prios c))
                  do (setf (aref slots c) face
                           (aref prios c) p)))))
    slots))

(defun emit-string (f off row str fg &optional bg (attr 0) (col0 0))
  "Write STR at ROW starting COL0 into F's cells with FG (list r g b),
optional BG, and packed text ATTR bits. Returns the new cell offset."
  (let ((cells (pine.buffer:frame-cells f)))
    (loop for i from 0 below (length str)
          for ch = (char-code (char str i))
          do (setf (svref cells (+ off 0)) row
                   (svref cells (+ off 1)) (+ col0 i)
                   (svref cells (+ off 2)) ch
                   (svref cells (+ off 3)) (first fg)
                   (svref cells (+ off 4)) (second fg)
                   (svref cells (+ off 5)) (third fg)
                   (svref cells (+ off 6)) (if bg (first bg) -1)
                   (svref cells (+ off 7)) (if bg (second bg) -1)
                   (svref cells (+ off 8)) (if bg (third bg) -1)
                   (svref cells (+ off 9)) attr)
             (incf off 10))
    off))

(defun %pad (str width)
  (let ((s (if (> (length str) width) (subseq str 0 width) str)))
    (concatenate 'string s (make-string (max 0 (- width (length s)))
                                        :initial-element #\Space))))

(defun emit-row (f off row text runs cols)
  "Blit one rendered (TEXT . RUNS) row -- the pine.layout:render format, which
is also the frame wire format -- into F's cells at ROW, clipped to COLS.
Returns the new cell offset."
  (let ((cells (pine.buffer:frame-cells f)))
    (loop for (run . more) on runs do
      (destructuring-bind (col fr fg fb br bg bb attr) run
        (let ((end (min (if more (car (first more)) (length text)) cols)))
          (loop for c from col below end
                do (setf (svref cells (+ off 0)) row
                         (svref cells (+ off 1)) c
                         (svref cells (+ off 2)) (char-code (char text c))
                         (svref cells (+ off 3)) fr
                         (svref cells (+ off 4)) fg
                         (svref cells (+ off 5)) fb
                         (svref cells (+ off 6)) br
                         (svref cells (+ off 7)) bg
                         (svref cells (+ off 8)) bb
                         (svref cells (+ off 9)) attr)
                   (incf off 10))))))
  off)

(defun %scratch-frame (cols rows)
  "A fresh frame sized COLS x ROWS for one render pass."
  (let ((f (make-instance 'pine.buffer:frame)))
    (setf (pine.buffer:frame-cols f) cols
          (pine.buffer:frame-rows f) rows)
    (pine.buffer:ensure-frame-cells f)
    f))

(defun %frame-rows (f off)
  (setf (pine.buffer:frame-cell-count f) off)
  (frame->rows f))

(defun modeline-rows (w cols)
  "Window W's mode line as one wire row at COLS: buffer name, mode indicator,
point position."
  (let* ((f (%scratch-frame cols 1))
         (s (pine.buffer:snap w))
         (line (if (and s (typep s 'pine.buffer:snapshot))
                   (format nil " ~a   ~a   L~d C~d"
                           (pine.buffer:window-name w)
                           (pine.mode:mode-indicator (pine.client:buffer-mode s))
                           (1+ (pine.buffer:point-line s))
                           (pine.buffer:point-col s))
                   (format nil " ~a" (pine.buffer:window-name w)))))
    (%frame-rows f (emit-string f 0 0 (%pad line cols)
                                (pine.buffer:face-fg :modeline)
                                (pine.buffer:face-bg :modeline)))))

(defun echo-rows (client cols)
  "The echo block at COLS: the completion popup rows (while a prompt is active)
above the echo/minibuffer line. Returns (values rows crow ccol) with the
minibuffer caret within the block, or -1 -1 when no prompt is active."
  (let* ((prompt (pine.echo:prompt-text))
         (prows (and (pine.echo:prompt-active-p)
                     (pine.client:popup-rows (pine.client:completion-state client))))
         (mb-snap (and prompt (pine.client:minibuffer-snap client)))
         (input (if (and mb-snap (plusp (pine.buffer:line-count mb-snap)))
                    (fset:@ (pine.buffer:lines mb-snap) 0)
                    ""))
         (text (if prompt
                   (concatenate 'string prompt input)
                   (pine.echo:current-message)))
         (f (%scratch-frame cols 1))
         (line (%frame-rows f (emit-string f 0 0 (%pad text cols)
                                           (pine.buffer:face-fg
                                            (if prompt :prompt :echo))))))
    (values (append prows line)
            (if prompt 0 -1)
            (if prompt
                (+ (length prompt)
                   (if mb-snap (pine.buffer:point-col mb-snap) (length input)))
                -1))))

(defun %term-rgb (plist key default)
  "Resolve a terminal cell color to an (r g b) list. color-index-to-rgb
returns a vector, SGR truecolor a 3-list."
  (let ((c (getf plist key)))
    (cond ((null c) default)
          ((integerp c) (coerce (pine.vt:color-index-to-rgb c) 'list))
          ((and (vectorp c) (= 3 (length c))) (coerce c 'list))
          ((and (listp c) (= 3 (length c))) c)
          (t default))))

(defun render-terminal-rows (w tobj)
  "Window W rendered from its terminal's emulator grid at W's size.
Returns (values rows crow ccol)."
  (let* ((term (pine.term:terminal-term tobj))
         (f (%scratch-frame (pine.buffer:win-width w) (pine.buffer:win-height w)))
         (cells (pine.buffer:frame-cells f))
         (rows (min (pine.vt:term-height term) (pine.buffer:win-height w)))
         (cols (min (pine.vt:term-width term) (pine.buffer:win-width w)))
         (off 0))
    (dotimes (y rows)
      (multiple-value-bind (chars faces) (pine.vt:term-render-line term y)
        (let ((cur nil))
          (dotimes (x cols)
            (let ((change (assoc x faces)))
              (when change (setf cur (second change))))
            (let ((fg (%term-rgb cur :fg (pine.buffer:face-fg :default)))
                  (bg (%term-rgb cur :bg nil)))
              (setf (svref cells (+ off 0)) y
                    (svref cells (+ off 1)) x
                    (svref cells (+ off 2)) (char-code (char chars x))
                    (svref cells (+ off 3)) (first fg)
                    (svref cells (+ off 4)) (second fg)
                    (svref cells (+ off 5)) (third fg)
                    (svref cells (+ off 6)) (if bg (first bg) -1)
                    (svref cells (+ off 7)) (if bg (second bg) -1)
                    (svref cells (+ off 8)) (if bg (third bg) -1)
                    (svref cells (+ off 9)) 0)
              (incf off 10))))))
    (values (%frame-rows f off)
            (max 0 (min (pine.vt:term-cursor-y term) (1- (max 1 rows))))
            (max 0 (min (pine.vt:term-cursor-x term) (1- (max 1 cols)))))))

(defun %snapshot-region (s)
  "Normalized region (values start-line start-col end-line end-col) from the
mark (buffer meta) and point, or nil when no mark is set."
  (let ((ml (fset:@ (pine.buffer:meta s) :mark-line))
        (mc (fset:@ (pine.buffer:meta s) :mark-col)))
    (when (and ml mc)
      (let ((pl (pine.buffer:point-line s)) (pc (pine.buffer:point-col s)))
        (if (or (< ml pl) (and (= ml pl) (<= mc pc)))
            (values ml mc pl pc)
            (values pl pc ml mc))))))

(defun %in-region-p (region line col)
  (destructuring-bind (sl sc el ec) region
    (cond ((or (< line sl) (> line el)) nil)
          ((= sl el) (and (>= col sc) (< col ec)))
          ((= line sl) (>= col sc))
          ((= line el) (< col ec))
          (t t))))

(defun selection-bg () (pine.buffer:face-bg :selection))

(defun %overlay-cell-style (class)
  "(values fg-rgb attr) for an overlay CLASS through the stylesheet, falling
back to the comment face."
  (let* ((st (pine.style:resolve (list (pine.layout:class-names class))))
         (fg (pine.style:st-fg st)))
    (values (if fg
                (mapcar (lambda (c) (round (* 255 c))) fg)
                (pine.buffer:face-fg :comment))
            (if (pine.style:st-bold st) 1 0))))

(defun render-window-rows (w)
  "Window W's buffer rendered to wire rows at W's size: visible lines with
highlights and region (text), the emulator grid (terminal buffer), or the
buffer's layout rows (layout buffer). Overlays draw after their line's text.
Returns (values rows crow ccol), the point position within the rows or -1 -1."
  (when (pine.buffer:snap w)
    (pine.buffer:ensure-point-visible w)
    (pine.buffer:ensure-col-visible w)
    (setf (pine.buffer:win-display w) (pine.buffer:window-display-lines w)))
  (let ((tobj (pine.term:terminal-for-buffer (pine.buffer:buffer-ref w))))
    (when tobj (return-from render-window-rows (render-terminal-rows w tobj))))
  ;; SNAP is read once: a buffer switch can null it from the renderer thread
  ;; mid-paint, so treat a missing snapshot as an empty buffer rather than
  ;; dereferencing nil.
  (let* ((f (%scratch-frame (pine.buffer:win-width w) (pine.buffer:win-height w)))
         (s (pine.buffer:snap w))
         (dl (and s (pine.buffer:win-display w)))
         (wid (pine.buffer:win-width w))
         (cells (pine.buffer:frame-cells f))
         (left (pine.buffer:col w))
         (hl (and s (pine.buffer:highlights s)))
         (hl-table (when hl
                     (build-highlight-table hl (pine.buffer:scroll-top w) (length dl))))
         (region (when s (multiple-value-bind (sl sc el ec) (%snapshot-region s)
                           (when sl (list sl sc el ec)))))
         (off 0))
    (cond
      ;; a layout buffer: blit its rendered rows (the render carries the
      ;; styling; no face-name table)
      ((and s (pine.buffer:buffer-local s :layout-rows))
       (loop for row-cells in (nthcdr (pine.buffer:scroll-top w)
                                      (pine.buffer:buffer-local s :layout-rows))
             for row from 0 below (pine.buffer:win-height w)
             do (setf off (emit-row f off row (car row-cells) (cdr row-cells) wid))))
      (s
       (loop with overlays = (pine.buffer:buffer-local s :overlays)
             for d in dl
             for row from 0
             for text = (pine.buffer:display-text d)
             for buf-line-idx = (+ row (pine.buffer:scroll-top w))
             for overlay = (and overlays (fset:@ overlays buf-line-idx))
             do (when overlay
                  (destructuring-bind (otext oclass) overlay
                    (multiple-value-bind (orgb oattr) (%overlay-cell-style oclass)
                      (let* ((col0 (+ (length text) 2))
                             (room (- wid col0)))
                        (when (plusp room)
                          (setf off (emit-string
                                     f off row
                                     (subseq otext 0 (min (length otext) room))
                                     orgb nil oattr col0)))))))
                (let* ((line-hl (when hl-table (gethash row hl-table)))
                       (buf-line-len (if (< buf-line-idx (pine.buffer:line-count s))
                                         (length (fset:@ (pine.buffer:lines s) buf-line-idx))
                                         0))
                       (face-slots (when line-hl
                                     (build-face-slots line-hl buf-line-len))))
                  (loop for i from 0 below (min (length text) wid)
                        for ch = (char-code (char text i))
                        for buf-col = (+ i left)
                        for face = (when (and face-slots (< buf-col (length face-slots)))
                                     (aref face-slots buf-col))
                        for rgb = (face-rgb face)
                        for attr = (face-attrs face)
                        for bg = (when (and region (%in-region-p region buf-line-idx buf-col))
                                   (selection-bg))
                        do (setf (svref cells (+ off 0)) row
                                 (svref cells (+ off 1)) i
                                 (svref cells (+ off 2)) ch
                                 (svref cells (+ off 3)) (first rgb)
                                 (svref cells (+ off 4)) (second rgb)
                                 (svref cells (+ off 5)) (third rgb)
                                 (svref cells (+ off 6)) (if bg (first bg) -1)
                                 (svref cells (+ off 7)) (if bg (second bg) -1)
                                 (svref cells (+ off 8)) (if bg (third bg) -1)
                                 (svref cells (+ off 9)) attr)
                           (incf off 10))))))
    (values (%frame-rows f off)
            (if s
                (max 0 (min (- (pine.buffer:point-line s) (pine.buffer:scroll-top w))
                            (1- (pine.buffer:win-height w))))
                -1)
            (if s (- (pine.buffer:point-col s) (pine.buffer:col w)) -1))))


;;;; Subscription
;;;; Subscribers are identified by the client's renderer actor ref.
;;;; Buffers `tell` snapshots to the ref; unsubscribe matches by eq.

(defun subscribe-to-buffer (buffer-actor)
  (let ((renderer (pine.client:renderer (pine.client:current-client))))
    (when (and buffer-actor renderer)
      (sento.actor:tell buffer-actor
                        (list :subscribe :renderer renderer)))))

(defun unsubscribe-from-buffer (buffer-actor)
  (let ((renderer (pine.client:renderer (pine.client:current-client))))
    (when (and buffer-actor renderer)
      (sento.actor:tell buffer-actor
                        (list :unsubscribe :renderer renderer)))))
