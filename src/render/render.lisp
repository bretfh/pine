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

(defun paint-focused (client)
  (let ((sink (pine.client:paint-sink client))
        (w (pine.client:focused-window client)))
    (when (and sink w)
      (when (pine.buffer:snap w)
        (pine.buffer:ensure-point-visible w)
        (pine.buffer:ensure-col-visible w)
        (setf (pine.buffer:win-display w) (pine.buffer:window-display-lines w)))
      (render-buffer-to-frame w)
      (let ((f (pine.client:frame client)))
        (funcall sink (frame->rows f)
                 (pine.buffer:frame-cursor-row f)
                 (pine.buffer:frame-cursor-col f))))))

(defun render-window (client)
  "The focused window's rendered rows + point from CLIENT's frame -- the buffer
laid out (visible lines, highlights, point, scroll, selection), plus the mode
line and echo line as the last two rows. The editor surface's window / modeline /
echo nodes read through this; there is no separate frame wire message."
  (let ((f (pine.client:frame client)))
    (values (frame->rows f)
            (pine.buffer:frame-cursor-row f)
            (pine.buffer:frame-cursor-col f))))

(defun start-renderer (client)
  (let* ((sys (pine.server:actor-system (pine.client:server-of client)))
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
                          (paint-focused client)))
                       (:resize
                        (destructuring-bind (&key cols rows) (rest msg)
                          (let ((f (pine.client:frame client)))
                            (setf (pine.buffer:frame-cols f) cols
                                  (pine.buffer:frame-rows f) rows))
                          (relayout)
                          (pine.buffer:ensure-frame-cells (pine.client:frame client))
                          (pine.term:resize-active-terminal cols rows)
                          (paint-focused client)))
                       (:switch-buffer
                        (destructuring-bind (&key buffer name) (rest msg)
                          (switch-window-buffer buffer name)
                          (paint-focused client)))
                       (:term-tick
                        (when (pine.term:drain-terminals client)
                          (paint-focused client)))
                       (:force-render
                        (paint-focused client)))
                   (error (e)
                     (ignore-errors
                      (format *error-output* "~&renderer error on ~s: ~a~%"
                              (first msg) e)))))))))
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

(defun relayout ()
  (let* ((client (pine.client:current-client))
         (f (pine.client:frame client))
         (cols (pine.buffer:frame-cols f))
         (rows (pine.buffer:frame-rows f)))
    ;; reserve the bottom two rows for the modeline and the echo/minibuffer.
    (dolist (w (pine.client:windows client))
      (setf (pine.buffer:row w) 0
            (pine.buffer:col w) 0
            (pine.buffer:win-width w) cols
            (pine.buffer:win-height w) (max 1 (- rows 2))))))


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

(defun emit-string (f off row str fg &optional bg (attr 0))
  "Write STR at ROW (col 0..) into F's cells with FG (list r g b), optional BG,
and packed text ATTR bits. Returns the new cell offset."
  (let ((cells (pine.buffer:frame-cells f)))
    (loop for i from 0 below (length str)
          for ch = (char-code (char str i))
          do (setf (svref cells (+ off 0)) row
                   (svref cells (+ off 1)) i
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

(defun render-chrome (f off w)
  "Paint the modeline, echo/minibuffer line, and any completion candidates.
Returns the new cell offset."
  (let* ((cols (pine.buffer:frame-cols f))
         (rows (pine.buffer:frame-rows f))
         (mode-row (- rows 2))
         (echo-row (- rows 1))
         (s (pine.buffer:snap w)))
    ;; modeline
    (let* ((mode (pine.mode:current-buffer-mode))
           (line (if (and s (typep s 'pine.buffer:snapshot))
                     (format nil " ~a   ~a   L~d C~d"
                             (pine.buffer:window-name w)
                             (pine.mode:mode-indicator mode)
                             (1+ (pine.buffer:point-line s))
                             (pine.buffer:point-col s))
                     (format nil " ~a" (pine.buffer:window-name w)))))
      (setf off (emit-string f off mode-row (%pad line cols)
                             (pine.buffer:face-fg :modeline)
                             (pine.buffer:face-bg :modeline))))
    ;; completions (just above the echo line)
    (let ((ct (pine.echo:completions-text)))
      (when (and ct (pine.echo:input-active-p))
        (let* ((clines (let ((acc '()) (start 0))
                         (loop for nl = (position #\Newline ct :start start)
                               do (push (subseq ct start (or nl (length ct))) acc)
                               while nl do (setf start (1+ nl)))
                         (nreverse acc)))
               (n (length clines))
               (top (max 0 (- mode-row n))))
          (loop for cl in clines
                for r from top below mode-row
                for selp = (and (plusp (length cl)) (char= (char cl 0) #\>))
                do (setf off (emit-string
                              f off r (%pad cl cols)
                              (pine.buffer:face-fg
                               (if selp :completion-selected :completion))
                              (pine.buffer:face-bg
                               (if selp :completion-selected :completion))))))))
    ;; echo / minibuffer line
    (let* ((prompt (pine.echo:input-prompt))
           (text (if prompt
                     (concatenate 'string prompt (pine.echo:input-text))
                     (pine.echo:current-message))))
      (setf off (emit-string f off echo-row (%pad text cols)
                             (pine.buffer:face-fg (if prompt :prompt :echo))))
      ;; cursor sits in the minibuffer while a prompt is active
      (when prompt
        (setf (pine.buffer:frame-cursor-row f) echo-row
              (pine.buffer:frame-cursor-col f) (length text))))
    off))

(defun %term-rgb (plist key default)
  "Resolve a terminal cell color to an (r g b) list. color-index-to-rgb
returns a vector, SGR truecolor a 3-list."
  (let ((c (getf plist key)))
    (cond ((null c) default)
          ((integerp c) (coerce (pine.vt:color-index-to-rgb c) 'list))
          ((and (vectorp c) (= 3 (length c))) (coerce c 'list))
          ((and (listp c) (= 3 (length c))) c)
          (t default))))

(defun render-terminal-cells (f w tobj)
  "Fill the buffer area of F from the terminal's emulator grid. Returns off."
  (declare (ignore w))
  (let* ((term (pine.term:terminal-term tobj))
         (cells (pine.buffer:frame-cells f))
         (rows (min (pine.vt:term-height term)
                    (max 1 (- (pine.buffer:frame-rows f) 2))))
         (cols (min (pine.vt:term-width term) (pine.buffer:frame-cols f)))
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
    (setf (pine.buffer:frame-cursor-row f)
          (max 0 (min (pine.vt:term-cursor-y term) (1- rows)))
          (pine.buffer:frame-cursor-col f)
          (max 0 (min (pine.vt:term-cursor-x term) (1- cols))))
    off))

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

(defun render-buffer-to-frame (w)
  (let ((f (pine.client:frame (pine.client:current-client))))
    (pine.buffer:ensure-frame-cells f)
    (let ((tobj (pine.term:terminal-for-buffer (pine.buffer:buffer-ref w))))
      (when tobj
        (let ((off (render-chrome f (render-terminal-cells f w tobj) w)))
          (setf (pine.buffer:frame-cell-count f) off
                (pine.buffer:frame-dirtyp f) t))
        (return-from render-buffer-to-frame)))
    ;; SNAP is read once: a buffer switch can null it from the renderer thread
    ;; mid-paint, so treat a missing snapshot as an empty buffer (chrome still
    ;; paints) rather than dereferencing nil.
    (let* ((s (pine.buffer:snap w))
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
      (when s
        (loop for d in dl
              for display-row from 0
              for row = (+ (pine.buffer:row w) display-row)
              for text = (pine.buffer:display-text d)
              for buf-line-idx = (+ display-row (pine.buffer:scroll-top w))
              do (let* ((line-hl (when hl-table (gethash display-row hl-table)))
                        (buf-line-len (if (< buf-line-idx (pine.buffer:line-count s))
                                          (length (fset:@ (pine.buffer:lines s) buf-line-idx))
                                          0))
                        (face-slots (when line-hl
                                      (build-face-slots line-hl buf-line-len))))
                   (loop for i from 0 below (min (length text) wid)
                         for ch = (char-code (char text i))
                         for col = i
                         for buf-col = (+ i left)
                         for face = (when (and face-slots (< buf-col (length face-slots)))
                                      (aref face-slots buf-col))
                         for rgb = (face-rgb face)
                         for attr = (face-attrs face)
                         for bg = (when (and region (%in-region-p region buf-line-idx buf-col))
                                    (selection-bg))
                         do (setf (svref cells (+ off 0)) row
                                  (svref cells (+ off 1)) col
                                  (svref cells (+ off 2)) ch
                                  (svref cells (+ off 3)) (first rgb)
                                  (svref cells (+ off 4)) (second rgb)
                                  (svref cells (+ off 5)) (third rgb)
                                  (svref cells (+ off 6)) (if bg (first bg) -1)
                                  (svref cells (+ off 7)) (if bg (second bg) -1)
                                  (svref cells (+ off 8)) (if bg (third bg) -1)
                                  (svref cells (+ off 9)) attr)
                            (incf off 10))))
        (setf (pine.buffer:frame-cursor-row f)
              (max 0 (min (- (pine.buffer:point-line s) (pine.buffer:scroll-top w))
                          (1- (pine.buffer:win-height w))))
              (pine.buffer:frame-cursor-col f)
              (- (pine.buffer:point-col s) (pine.buffer:col w))
              (pine.buffer:frame-scroll-pixel f)
              (coerce (pine.buffer:scroll-top w) 'double-float)))
      ;; modeline + echo/minibuffer (may move the cursor into the minibuffer)
      (setf off (render-chrome f off w))
      (setf (pine.buffer:frame-cell-count f) off
            (pine.buffer:frame-dirtyp f) t))))


;;;; The paint loop lives in the surface (pine.gtk pump), not here.


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
