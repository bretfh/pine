(in-package :pine.layout)

;;;; The widget engine. A widget tree is laid out in three passes over a cell
;;;; raster: MEASURE reports each node's natural size bottom-up, ARRANGE assigns
;;;; absolute rects top-down (distributing slack to expanders), and PAINT writes
;;;; styled cells into each rect. Separating the passes is what gives the box
;;;; model -- alignment, expansion, and 2D placement -- that a linear emit model
;;;; cannot. Faces cascade as a background fill plus a foreground colour; after
;;;; arrange every node has an absolute rect, so hit-testing is exact.


;;;; Nodes. The base carries the arranged rect (start/end line+col), a style
;;;; FACE, and an EXPAND weight for main-axis slack distribution.

(defclass node ()
  ((key-of  :initarg :key    :accessor key-of  :initform nil)
   (parent  :initarg :parent :accessor parent  :initform nil)
   (face    :initarg :face   :accessor face    :initform nil)
   (css-class :initarg :class :accessor css-class :initform nil)
   (hint    :initarg :hint   :accessor hint    :initform nil)
   (hovered :initform nil    :accessor hovered)
   ;; cairo chrome drawn behind the content: a rounded/gradient background.
   ;; radius in px; fill and grad are #rrggbb (grad = a linear gradient to it).
   (radius  :initarg :radius :accessor radius  :initform 0)
   (fill-of :initarg :fill   :accessor fill-of :initform nil)
   (grad    :initarg :grad   :accessor grad    :initform nil)
   ;; font size in px for the pixel/cairo render; nil = the default size
   (font-px :initarg :font-px :accessor font-px :initform nil)
   ;; padding inside the node's rect (px), and a minimum size, as in CSS.
   ;; :pad sets both axes; :pad-x / :pad-y set one.
   (pad     :initarg :pad    :initform nil)
   (pad-x   :initarg :pad-x  :accessor pad-x   :initform 0)
   (pad-y   :initarg :pad-y  :accessor pad-y   :initform 0)
   (min-w   :initarg :min-w  :accessor min-w   :initform 0)
   (min-h   :initarg :min-h  :accessor min-h   :initform 0)
   ;; outer margin (top right bottom left) in px, or nil; set only by the cairo
   ;; style pass, so cell-grid nodes never carry it and layout is unchanged there.
   (margin  :initarg :margin :accessor node-margin :initform nil)
   (expand  :initarg :expand :accessor expand-of :initform 0)
   (start-line :initform 0 :accessor start-line)
   (start-col  :initform 0 :accessor start-col)
   (end-line   :initform 0 :accessor end-line)
   (end-col    :initform 0 :accessor end-col)))

(defclass text-node (node)
  ((content :initarg :content :accessor content :initform "")))

(defclass separator (node)
  ((sep-char :initarg :char :accessor sep-char :initform (code-char #x2500))
   (vertical :initarg :vertical :accessor sep-vertical :initform nil)))

(defmethod initialize-instance :after ((n separator) &key)
  (when (and (sep-vertical n) (char= (sep-char n) (code-char #x2500)))
    (setf (sep-char n) (code-char #x2502))))

(defclass spacer (node) ())            ; flexible empty space; default expand 1

(defclass vstack (node)
  ((nodes :initarg :nodes :accessor nodes :initform nil)
   (spacing  :initarg :spacing  :accessor spacing  :initform 0)
   (align    :initarg :align    :accessor align    :initform :start)))

(defclass hstack (node)
  ((nodes :initarg :nodes :accessor nodes :initform nil)
   (spacing  :initarg :spacing  :accessor spacing  :initform 1)
   (align    :initarg :align    :accessor align    :initform :start)))

(defclass box (node)
  ((node    :initarg :node    :accessor node    :initform nil)
   (width-of :initarg :width    :accessor width-of :initform 0)
   (align    :initarg :align    :accessor align    :initform :left)
   (pad-char :initarg :pad      :accessor pad-char :initform #\space)))

(defclass center (node)
  ((node :initarg :node :accessor node :initform nil)))

(defclass scroll (node)
  ((node   :initarg :node  :accessor node         :initform nil)
   (offset  :initarg :offset :accessor scroll-offset :initform 0)
   (vheight :initarg :height :accessor vheight        :initform 10)))

(defclass selectable (node)
  ((node             :initarg :node    :accessor node             :initform nil)
   (data              :initarg :data     :accessor data              :initform nil)
   (selectedp         :initarg :selected :accessor selectedp         :initform nil)
   (prefix-selected   :initarg :prefix-selected   :accessor prefix-selected   :initform "> ")
   (prefix-unselected :initarg :prefix-unselected :accessor prefix-unselected :initform "  ")))

(defclass action (node)
  ((node    :initarg :node    :accessor node    :initform nil)
   (callback :initarg :callback :accessor callback :initform nil)))

(defclass list-node (node)
  ((items       :initarg :items       :accessor items       :initform nil)
   (item-fn     :initarg :item-fn     :accessor item-fn     :initform nil)
   (max-visible :initarg :max-visible :accessor max-visible :initform nil)
   ;; the item nodes built at the last measure, kept so arrange/paint and
   ;; hit-testing can reach them (item-fn builds them transiently otherwise).
   (rendered    :accessor rendered    :initform nil)))

(defclass grid (node)
  ((cells      :initarg :cells      :accessor cells      :initform nil)
   (col-widths :initarg :col-widths :accessor col-widths :initform nil)))

(defclass slider (node)
  ((value       :initarg :value       :accessor value       :initform 0)
   (min-of      :initarg :min         :accessor min-of      :initform 0)
   (max-of      :initarg :max         :accessor max-of      :initform 100)
   (track       :initarg :track       :accessor track       :initform 16)
   ;; on-change is a function of the new value; clicking/dragging at a column
   ;; maps to a value and calls it.
   (on-change   :initarg :on-change   :accessor on-change   :initform nil)
   (filled-face :initarg :filled-face :accessor filled-face :initform :function-name)
   (empty-face  :initarg :empty-face  :accessor empty-face  :initform :comment)))

(defclass ring (node)
  ((value      :initarg :value      :accessor value      :initform 0)
   (min-of     :initarg :min        :accessor min-of     :initform 0)
   (max-of     :initarg :max        :accessor max-of     :initform 100)
   (thickness  :initarg :thickness  :accessor thickness  :initform 5)
   (diameter   :initarg :diameter   :accessor diameter   :initform 56)
   (arc-face   :initarg :arc-face   :accessor arc-face   :initform :function-name)
   (track-face :initarg :track-face :accessor track-face :initform :comment)
   (node       :initarg :node       :accessor node       :initform nil)))

(defclass calendar (node)
  ((cal-year  :initarg :year  :accessor cal-year  :initform 2000)
   (cal-month :initarg :month :accessor cal-month :initform 1)
   (cal-day   :initarg :day   :accessor cal-day   :initform 1)))

(defclass picture (node)
  ((path :initarg :path :accessor pic-path :initform "")))

;; A leaf holding rendered cell rows (a buffer's or a terminal's view, each row
;; a (text . runs) pair). One node, not a tree of characters: measure and
;; arrange are O(1), and paint blits the rows. This is how a buffer or a
;; terminal composes into a widget tree without the layout engine ever touching
;; the text cell by cell. The class is WINDOW-NODE (node convention); the
;; constructor and the wire tag stay `window' -- the user language reads
;; (window "scratch") and pine.buffer's window (the scroll/focus view) keeps
;; its own name in its own package. OF backs a live view with that
;; pine.buffer:window; KIND (:window :modeline :echo) marks what the editor's
;; render walk refreshes; neither crosses the wire.
(defclass window-node (node)
  ((rows :initarg :rows :accessor window-rows :initform nil)
   (crow :initarg :crow :accessor window-crow :initform -1)
   (ccol :initarg :ccol :accessor window-ccol :initform -1)
   (opacity :initarg :opacity :accessor window-opacity :initform 1.0)
   ;; BASE bounds the in-flow rows; anything before them is an overlay drawn
   ;; upward from the arranged rect, outside measure -- how the completion
   ;; popup floats above the echo line wherever the echo sits, without
   ;; reflowing the tree as the candidate count changes. nil = all rows flow.
   (base :initarg :base :accessor window-base :initform nil)
   (of   :initarg :of   :accessor window-of   :initform nil)
   (kind :initarg :kind :accessor window-kind :initform nil)))

(defclass centerbox (node)
  ((orient    :initarg :orient :accessor cb-orient :initform :v)
   (cb-start  :initarg :start  :accessor cb-start  :initform nil)
   (cb-center :initarg :center :accessor cb-center :initform nil)
   (cb-end    :initarg :end    :accessor cb-end    :initform nil)))

(defmethod initialize-instance :after ((n node) &key pad)
  (when pad (setf (pad-x n) pad (pad-y n) pad)))

(defmethod initialize-instance :after ((n spacer) &key)
  (when (zerop (expand-of n)) (setf (expand-of n) 1)))

(defmethod initialize-instance :after ((n slider) &key)
  ;; a slider reads live cell values, which may be nil before their source has
  ;; run; coerce to numbers so measure/paint never do arithmetic on nil.
  (unless (numberp (value n))  (setf (value n) 0))
  (unless (numberp (min-of n)) (setf (min-of n) 0))
  (unless (numberp (max-of n)) (setf (max-of n) 100)))

(defun slider-fraction (n)
  "The filled fraction 0..1 of slider N, clamped and nil-safe."
  (let* ((span (max 1 (- (max-of n) (min-of n))))
         (v (max 0 (min span (- (value n) (min-of n))))))
    (/ v span)))

(defmethod initialize-instance :after ((n ring) &key)
  (unless (numberp (value n))  (setf (value n) 0))
  (unless (numberp (min-of n)) (setf (min-of n) 0))
  (unless (numberp (max-of n)) (setf (max-of n) 100)))

(defun ring-fraction (n)
  (let* ((span (max 1 (- (max-of n) (min-of n))))
         (v (max 0 (min span (- (value n) (min-of n))))))
    (/ v span)))


;;;; Declarative constructor DSL. Each widget is a terse function: leading
;;;; :keyword value pairs are props, the remaining arguments are nodes (a
;;;; node that is itself a list is spliced, so (mapcar ...) works like eww's
;;;; `for'). Trees read like markup -- (column :spacing 1 (label "a") (row ...))
;;;; -- and defwidget names a reusable component. This is the eww analog.

(defun %parse-args (args)
  "(values plist nodes): peel leading keyword/value prop pairs, then treat the
rest as nodes, dropping nils and splicing lists."
  (let ((props nil) (rest args))
    (loop while (and rest (keywordp (car rest)) (cdr rest))
          do (push (pop rest) props) (push (pop rest) props))
    (values (nreverse props)
            (loop for c in rest when c append (if (listp c) c (list c))))))

(defun label (text &rest props)
  "A text run. Props are any node style: :face :font-px :pad :min-w :radius ..."
  (apply #'make-instance 'text-node :content (or text "") props))

(defun icon (glyph &rest props)
  "A glyph (a codepoint or string). With :on-click it becomes a clickable cell:
:face/:font-px style the glyph, the rest (:min-w :pad :radius :hint) style the
clickable node, which centres the glyph.
(icon #xF120 :on-click thunk :hint \"Term\" :min-w 28 :pad-y 8 :radius 8 :font-px 15)"
  (let ((g (if (integerp glyph) (string (code-char glyph)) (string glyph)))
        (click (getf props :on-click)))
    (if click
        (let ((lbl (make-instance 'text-node :content g :class (getf props :glyph-class)
                                  :face (getf props :face) :font-px (getf props :font-px)))
              (p (copy-list props)))
          (remf p :face) (remf p :font-px) (remf p :on-click) (remf p :glyph-class)
          (apply #'make-instance 'action :callback click :node lbl p))
        (make-instance 'text-node :content g :class (getf props :class)
                       :face (getf props :face) :font-px (getf props :font-px)))))

(defun column (&rest args)
  "A vertical box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'vstack :nodes items props)))

(defun row (&rest args)
  "A horizontal box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'hstack :nodes items props)))

(defun centerbox (&key orient class hint expand start center end)
  "eww centerbox: three slots pinned start / centre / end along ORIENT (:v or :h).
The centre floats in the slack; the ends stay anchored, so an oversize start
never pushes the end past the surface. (centerbox :orient :v :start .. :end ..)"
  (make-instance 'centerbox :orient (or orient :v) :class class :hint hint
                 :expand (or expand 0) :start start :center center :end end))

(defun button (&rest args)
  "A clickable wrapper carrying any node style. It centres its one node.
(button :on-click thunk :hint \"...\" :pad-x 12 :radius 8 (label \"go\"))"
  (multiple-value-bind (props items) (%parse-args args)
    (let ((click (getf props :on-click)))
      (remf props :on-click)
      (apply #'make-instance 'action :callback click :node (first items) props))))

(defun boxed (&rest args)
  "A fixed-width cell. Props :width :align :pad :face; one node."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'box :node (first items) props)))

(defun centered (&rest args)
  "Centre one node in the space it is given."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'center :node (first items) props)))

(defun viewport (&rest args)
  "A clipped, scrollable window onto a taller node. Props :height :offset."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'scroll :node (first items) props)))

(defun gap (&rest props)
  "Flexible empty space. (gap :expand 2)"
  (apply #'make-instance 'spacer props))

(defun rule (&rest props)
  "A separator line. (rule :char #\\= :face :comment)"
  (apply #'make-instance 'separator props))

(defun meter (&rest props)
  "A slider/gauge. (meter :value v :min 0 :max 100 :track 12 :on-change fn)"
  (apply #'make-instance 'slider props))

(defun ring (&rest args)
  "A circular-progress gauge. Props :value :min :max :thickness :diameter
:arc-face :track-face; the optional one node is centred inside the ring."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'ring :node (first items) props)))

(defun cal (&rest props)
  "A month calendar. Props :year :month :day."
  (apply #'make-instance 'calendar props))

(defun pic (path &rest props)
  "An image loaded from file PATH."
  (apply #'make-instance 'picture :path (or path "") props))

(defun window (rows &rest props)
  "A leaf rendering already-laid-out cell ROWS (each (text . runs)) -- an Emacs
window onto a buffer or a terminal. Props may set :crow / :ccol for the point,
plus any node style. Measure/arrange are O(1); paint blits the rows."
  (apply #'make-instance 'window-node :rows rows props))

(defun rows (items item-fn &rest props)
  "A vertical list built by mapping ITEM-FN over ITEMS. (rows nets #'net-row)"
  (apply #'make-instance 'list-node :items items :item-fn item-fn props))

(defun choice (&rest args)
  "A selectable row (keyboard-navigable). (choice :data d (label ...))"
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'selectable :node (first items) props)))


;;;; Faces -> cell colours

(defun %face-cell-rgb (designator)
  "(values fr fg fb br bg bb attr) for a face DESIGNATOR; bg -1 means none, attr
is the packed bold/italic/underline bits. DESIGNATOR is a face name resolved
through the active theme, or a precomputed (FG BG ATTR) tuple -- FG/BG (r g b)
lists or nil -- installed by RESOLVE-STYLES! for the cell render."
  (if (consp designator)
      (destructuring-bind (fg bg attr) designator
        (let ((f (or fg (pine.buffer:face-fg :default))))
          (values (first f) (second f) (third f)
                  (if bg (first bg) -1) (if bg (second bg) -1) (if bg (third bg) -1)
                  (or attr 0))))
      (let ((fg (pine.buffer:face-fg designator))
            (bg (pine.buffer:face-bg designator))
            (attr (let ((f (ignore-errors (pine.buffer:find-face designator))))
                    (if f (pine.buffer:face-attr-bits f) 0))))
        (values (first fg) (second fg) (third fg)
                (if bg (first bg) -1) (if bg (second bg) -1) (if bg (third bg) -1)
                attr))))


;;;; Raster -- a cell buffer widgets paint into. Cells are the flat 10-slot
;;;; [row col code fr fg fb br bg bb bold] format the surface painter draws.

(defstruct (raster (:constructor %make-raster)) cols rows cells (clip nil))

(defun make-raster (cols rows)
  (let* ((n (* cols rows)) (v (make-array (* 10 n))))
    (dotimes (i n)
      (let ((off (* 10 i)))
        (setf (svref v off) (floor i cols)
              (svref v (+ off 1)) (mod i cols)
              (svref v (+ off 2)) 32
              (svref v (+ off 3)) 205 (svref v (+ off 4)) 214 (svref v (+ off 5)) 244
              (svref v (+ off 6)) -1 (svref v (+ off 7)) -1 (svref v (+ off 8)) -1
              (svref v (+ off 9)) 0)))
    (%make-raster :cols cols :rows rows :cells v)))

(declaim (inline %cell-off %in-raster))
(defun %cell-off (r row col) (* 10 (+ (* row (raster-cols r)) col)))
(defun %in-raster (r row col)
  (and (>= row 0) (< row (raster-rows r)) (>= col 0) (< col (raster-cols r))
       (let ((c (raster-clip r)))
         (or (null c)
             (and (>= col (first c)) (< col (third c))
                  (>= row (second c)) (< row (fourth c)))))))

(defmacro %with-clip ((r x0 y0 x1 y1) &body body)
  "Restrict raster writes to the rect [X0 X1) x [Y0 Y1) within BODY."
  (let ((rr (gensym)) (old (gensym)))
    `(let* ((,rr ,r) (,old (raster-clip ,rr)))
       (setf (raster-clip ,rr) (list ,x0 ,y0 ,x1 ,y1))
       (unwind-protect (progn ,@body) (setf (raster-clip ,rr) ,old)))))

(defun raster-put (r row col ch face)
  (when (%in-raster r row col)
    (multiple-value-bind (fr fg fb br bg bb attr) (%face-cell-rgb face)
      (let ((off (%cell-off r row col)) (v (raster-cells r)))
        (setf (svref v (+ off 2)) (char-code ch)
              (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
              (svref v (+ off 9)) attr)
        (when (>= br 0)
          (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))))

(defun raster-put-bg (r row col br bg bb)
  (when (%in-raster r row col)
    (let ((off (%cell-off r row col)) (v (raster-cells r)))
      (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))

(defun raster-put-rgb (r row col ch fr fg fb br bg bb attr)
  "Write a cell with explicit colours (not a face), for blitting cell rows."
  (when (%in-raster r row col)
    (let ((off (%cell-off r row col)) (v (raster-cells r)))
      (setf (svref v (+ off 2)) (char-code ch)
            (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
            (svref v (+ off 9)) (or attr 0))
      (when (and (integerp br) (>= br 0))
        (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb)))))

(defun blit-row (r row col0 text runs)
  "Blit one (TEXT . RUNS) row into raster R at ROW, starting COL0. Each run is
(col fr fg fb br bg bb attr), its colours extending to the next run's col."
  (loop for (run . more) on runs do
    (destructuring-bind (col fr fg fb br bg bb attr) run
      (let ((end (if more (car (first more)) (length text))))
        (loop for c from col below (min end (length text))
              do (raster-put-rgb r row (+ col0 c) (char text c) fr fg fb br bg bb attr))))))



;;;; Layout protocol

(defvar *text-size* nil
  "When bound to a function of (text font-px) -> (values w h), text leaves
measure through it (pixel/cairo mode); otherwise a character is one cell.")
(defvar *default-font-px* 13)

(defun %text-size (text font-px)
  (if *text-size*
      (funcall *text-size* text (or font-px *default-font-px*))
      (values (length text) 1)))

(defun %line-h (font-px)
  (if *text-size* (nth-value 1 (%text-size "M" font-px)) 1))

(defgeneric measure (node avail-w avail-h)
  (:documentation "The node's natural (values w h) given the available space,
in cells (default) or pixels (when *text-size* is bound). Bottom-up."))

(defgeneric arrange (node x y w h)
  (:documentation "Assign the node the rect X Y W H and place its nodes."))

(defgeneric paint (node raster)
  (:documentation "Draw the node into its arranged rect on RASTER."))

(defun %node-width (n) (- (end-col n) (start-col n)))

(defmethod measure ((n node) aw ah) (declare (ignore aw ah)) (values 0 1))

;;; Margin: outer space around a node's border-box (CSS margin). NIL means none,
;;; the common case, and every helper collapses to zero -- so a node without a
;;; margin (every cell-grid node) measures and arranges exactly as before.

(defun %margin-x (n) (let ((m (node-margin n))) (if m (+ (fourth m) (second m)) 0)))
(defun %margin-y (n) (let ((m (node-margin n))) (if m (+ (first m) (third m)) 0)))
(defun %margin-l (n) (let ((m (node-margin n))) (if m (fourth m) 0)))
(defun %margin-t (n) (let ((m (node-margin n))) (if m (first m) 0)))

(defmethod measure :around ((n node) aw ah)
  "Wrap the intrinsic measure with the CSS box model: content is measured in the
space left after margin and padding; the result adds padding back (floored at
min-w/min-h for the border-box) and then adds margin for the outer size."
  (multiple-value-bind (w h)
      (call-next-method n (max 0 (- aw (* 2 (pad-x n)) (%margin-x n)))
                          (max 0 (- ah (* 2 (pad-y n)) (%margin-y n))))
    (values (+ (max (min-w n) (+ w (* 2 (pad-x n)))) (%margin-x n))
            (+ (max (min-h n) (+ h (* 2 (pad-y n)))) (%margin-y n)))))

(defmethod arrange :around ((n node) x y w h)
  "Inset the allocated rect by the node's margin before the primary arrange, so
the node's border-box (and everything it lays out inside) sits within its margin."
  (if (node-margin n)
      (call-next-method n (+ x (%margin-l n)) (+ y (%margin-t n))
                        (max 0 (- w (%margin-x n))) (max 0 (- h (%margin-y n))))
      (call-next-method)))

(defun %inner (n x y w h)
  "The content rect of N inside its padding."
  (values (+ x (pad-x n)) (+ y (pad-y n))
          (max 0 (- w (* 2 (pad-x n)))) (max 0 (- h (* 2 (pad-y n))))))

(defmethod arrange ((n node) x y w h)
  (setf (start-col n) x (start-line n) y
        (end-col n) (+ x w) (end-line n) (+ y (max 0 (1- h)))))

(defmethod paint ((n node) r) (declare (ignore r)) nil)

(defparameter *hover-face* nil
  "A face designator painted as the background of the hovered node, or nil.")

(defun %fill-bg (n r)
  "Fill the node's rect background: the hover face when hovered, else its own."
  (let ((f (if (and (hovered n) *hover-face*) *hover-face* (face n))))
    (when f
      (multiple-value-bind (fr fg fb br bg bb attr) (%face-cell-rgb f)
        (declare (ignore fr fg fb attr))
        (when (>= br 0)
          (loop for row from (start-line n) to (end-line n) do
            (loop for col from (start-col n) below (end-col n)
                  do (raster-put-bg r row col br bg bb))))))))

(defmethod paint :before ((n node) r) (%fill-bg n r))

;;; Leaves

(defmethod measure ((n text-node) aw ah)
  (declare (ignore aw ah)) (%text-size (content n) (font-px n)))
(defmethod paint ((n text-node) r)
  (let ((s (content n)) (w (%node-width n)))
    (loop for i from 0 below (min (length s) w)
          do (raster-put r (start-line n) (+ (start-col n) i) (char s i) (face n)))))

(defun %window-overlay-count (n)
  "How many of N's leading rows are overlay (drawn above the arranged rect)."
  (let ((base (window-base n)))
    (if base (max 0 (- (length (window-rows n)) base)) 0)))

(defmethod measure ((n window-node) aw ah)
  (declare (ignore aw ah))
  (let* ((rows (window-rows n))
         (cols (reduce #'max rows :initial-value 1 :key (lambda (row) (length (car row)))))
         (nrows (max 1 (- (length rows) (%window-overlay-count n)))))
    (if *text-size*
        (multiple-value-bind (cw ch) (%text-size "M" (font-px n))
          (values (* cols cw) (* nrows ch)))
        (values cols nrows))))
(defmethod paint ((n window-node) r)
  (let ((over (%window-overlay-count n)))
    (loop for row in (window-rows n)
          for y from (- (start-line n) over)
          do (blit-row r y (start-col n) (car row) (cdr row)))))

(defmethod measure ((n separator) aw ah)
  "A separator's natural size is its thickness; its length comes from the
container's arrange (:align :stretch), never from the available space --
reporting AW/AH here would eat the whole axis as natural size."
  (declare (ignore aw ah))
  (let ((px (max 1 (pine.buffer:metric :border 2))))
    (if (sep-vertical n)
        (values (if *text-size* px 1) 1)
        (values 1 (if *text-size* px 1)))))
(defmethod paint ((n separator) r)
  (if (sep-vertical n)
      (loop for row from (start-line n) to (end-line n)
            do (raster-put r row (start-col n) (sep-char n) (face n)))
      (loop for col from (start-col n) below (end-col n)
            do (raster-put r (start-line n) col (sep-char n) (face n)))))

(defmethod measure ((n spacer) aw ah) (declare (ignore aw ah)) (values 0 0))

(defparameter +slider-filled+ (code-char #x2588)) ; full block
(defparameter +slider-empty+  (code-char #x2500)) ; light horizontal

(defmethod measure ((n slider) aw ah)
  (declare (ignore aw ah))
  (if *text-size* (values (* 8 (track n)) (%line-h (font-px n))) (values (track n) 1)))
(defmethod paint ((n slider) r)
  (let* ((w (track n))
         (span (max 1 (- (max-of n) (min-of n))))
         (v (max 0 (min span (- (value n) (min-of n)))))
         (fill (round (* (/ v span) w))))
    (loop for i from 0 below w
          do (raster-put r (start-line n) (+ (start-col n) i)
                         (if (< i fill) +slider-filled+ +slider-empty+)
                         (if (< i fill) (filled-face n) (empty-face n))))))

(defun slider-value-at (n col)
  "The value a click at absolute COL maps to for arranged slider N, using its
arranged width (start/end-col hold pixels or cells, whichever it was laid out in)
-- not the dynamic *text-size*, which is only bound during a draw."
  (let* ((w (max 1 (- (end-col n) (start-col n))))
         (rel (max 0 (min w (- col (start-col n)))))
         (span (- (max-of n) (min-of n))))
    (+ (min-of n) (round (* (/ rel w) span)))))

(defmethod measure ((n ring) aw ah)
  (declare (ignore aw ah))
  (let ((d (max (diameter n) (min-w n) (min-h n))))
    (values d d)))
(defmethod arrange ((n ring) x y w h)
  (call-next-method)
  (when (node n)
    (multiple-value-bind (ix iy iw ih) (%inner n x y w h)
      (multiple-value-bind (cw ch) (measure (node n) iw ih)
        (arrange (node n) (+ ix (floor (- iw cw) 2)) (+ iy (floor (- ih ch) 2)) cw ch)))))

;;; Containers -- the flex box algebra

(defmethod measure ((n vstack) aw ah)
  (let ((w 0) (h 0) (items (nodes n)))
    (dolist (c items)
      (multiple-value-bind (cw ch) (measure c aw ah)
        (setf w (max w cw)) (incf h ch)))
    (values w (+ h (* (spacing n) (max 0 (1- (length items))))))))

(defmethod arrange ((n vstack) x y w h)
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (let* ((items (nodes n))
           (nats (mapcar (lambda (c) (multiple-value-list (measure c w h))) items))
           (natsum (+ (reduce #'+ (mapcar #'second nats) :initial-value 0)
                      (* (spacing n) (max 0 (1- (length items))))))
           (slack (max 0 (- h natsum)))
           (tw (reduce #'+ (mapcar #'expand-of items) :initial-value 0))
           (acc 0) (given 0)
           (cy y))
      ;; slack shares round cumulatively so the extras sum to exactly SLACK --
      ;; independent rounding can overshoot and push tail children off the rect
      (loop for c in items for (cw ch) in nats
            for extra = (if (plusp tw)
                            (progn
                              (incf acc (* slack (/ (expand-of c) tw)))
                              (prog1 (- (round acc) given)
                                (setf given (round acc))))
                            0)
            for fh = (+ ch extra)
            for cx = (ecase (align n)
                       ((:start :stretch) x)
                       (:center (+ x (floor (- w cw) 2)))
                       (:end (+ x (- w cw))))
            for fw = (if (eq (align n) :stretch) w cw)
            do (arrange c cx cy fw fh)
               (setf cy (+ cy fh (spacing n)))))))

(defmethod paint ((n vstack) r) (dolist (c (nodes n)) (paint c r)))

(defmethod measure ((n hstack) aw ah)
  (let ((w 0) (h 0) (items (nodes n)))
    (dolist (c items)
      (multiple-value-bind (cw ch) (measure c aw ah)
        (incf w cw) (setf h (max h ch))))
    (values (+ w (* (spacing n) (max 0 (1- (length items))))) h)))

(defmethod arrange ((n hstack) x y w h)
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (let* ((items (nodes n))
           (nats (mapcar (lambda (c) (multiple-value-list (measure c w h))) items))
           (natsum (+ (reduce #'+ (mapcar #'first nats) :initial-value 0)
                      (* (spacing n) (max 0 (1- (length items))))))
           (slack (max 0 (- w natsum)))
           (tw (reduce #'+ (mapcar #'expand-of items) :initial-value 0))
           (acc 0) (given 0)
           (cx x))
      (loop for c in items for (cw ch) in nats
            for extra = (if (plusp tw)
                            (progn
                              (incf acc (* slack (/ (expand-of c) tw)))
                              (prog1 (- (round acc) given)
                                (setf given (round acc))))
                            0)
            for fw = (+ cw extra)
            for cy = (ecase (align n)
                       ((:start :stretch) y)
                       (:center (+ y (floor (- h ch) 2)))
                       (:end (+ y (- h ch))))
            for fh = (if (eq (align n) :stretch) h ch)
            do (arrange c cx cy fw fh)
               (setf cx (+ cx fw (spacing n)))))))

(defmethod paint ((n hstack) r) (dolist (c (nodes n)) (paint c r)))

(defmethod measure ((n box) aw ah)
  (declare (ignore aw))
  (let ((c (node n)))
    (values (width-of n) (if c (nth-value 1 (measure c (width-of n) ah)) 1))))
(defmethod arrange ((n box) x y w h)
  (declare (ignore w))
  (call-next-method n x y (width-of n) h)
  (let ((c (node n)))
    (when c
      (multiple-value-bind (cw ch) (measure c (width-of n) h)
        (let ((cx (ecase (align n)
                    (:left x)
                    (:right (+ x (- (width-of n) cw)))
                    (:center (+ x (floor (- (width-of n) cw) 2))))))
          (arrange c cx y cw (max 1 ch)))))))
(defmethod paint ((n box) r)
  (unless (char= (pad-char n) #\space)
    (loop for col from (start-col n) below (end-col n)
          do (raster-put r (start-line n) col (pad-char n) (face n))))
  (when (node n) (paint (node n) r)))

(defmethod measure ((n center) aw ah)
  (if (node n) (measure (node n) aw ah) (values 0 0)))
(defmethod arrange ((n center) x y w h)
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (let ((c (node n)))
      (when c
        (multiple-value-bind (cw ch) (measure c w h)
          (arrange c (+ x (floor (- w cw) 2)) (+ y (floor (- h ch) 2)) cw ch))))))
(defmethod paint ((n center) r) (when (node n) (paint (node n) r)))

(defun %cb-parts (n) (remove nil (list (cb-start n) (cb-center n) (cb-end n))))
(defmethod measure ((n centerbox) aw ah)
  (let ((w 0) (h 0))
    (dolist (c (%cb-parts n))
      (multiple-value-bind (cw ch) (measure c aw ah) (setf w (max w cw)) (incf h ch)))
    (values w h)))
(defmethod arrange ((n centerbox) x y w h)
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (let ((s (cb-start n)) (c (cb-center n)) (e (cb-end n)))
      (when s (multiple-value-bind (sw sh) (measure s w h) (declare (ignore sw))
                (arrange s x y w sh)))
      (when e (multiple-value-bind (ew eh) (measure e w h) (declare (ignore ew))
                (arrange e x (+ y (- h eh)) w eh)))
      (when c (multiple-value-bind (cw ch) (measure c w h) (declare (ignore cw))
                (arrange c x (+ y (max 0 (floor (- h ch) 2))) w ch))))))
(defmethod paint ((n centerbox) r) (dolist (c (%cb-parts n)) (paint c r)))

(defmethod measure ((n scroll) aw ah)
  (declare (ignore ah))
  (values (if (node n) (nth-value 0 (measure (node n) aw 100000)) 0)
          (vheight n)))
(defmethod arrange ((n scroll) x y w h)
  (call-next-method n x y w (vheight n))
  (when (node n)
    (let ((ch (nth-value 1 (measure (node n) w 100000))))
      ;; the node is laid out at full height, shifted up by the scroll offset;
      ;; paint clips it to the viewport.
      (arrange (node n) x (- y (scroll-offset n)) w ch))))
(defmethod paint ((n scroll) r)
  (when (node n)
    (%with-clip (r (start-col n) (start-line n) (end-col n) (+ (start-line n) (vheight n)))
      (paint (node n) r))))

(defmethod measure ((n selectable) aw ah)
  (let ((pfx (if (selectedp n) (prefix-selected n) (prefix-unselected n))))
    (multiple-value-bind (cw ch) (if (node n) (measure (node n) aw ah) (values 0 1))
      (values (+ (length pfx) cw) ch))))
(defmethod arrange ((n selectable) x y w h)
  (call-next-method)
  (let ((pfx (if (selectedp n) (prefix-selected n) (prefix-unselected n))))
    (when (node n) (arrange (node n) (+ x (length pfx)) y (- w (length pfx)) h))))
(defmethod paint ((n selectable) r)
  (let ((pfx (if (selectedp n) (prefix-selected n) (prefix-unselected n))))
    (loop for i from 0 below (length pfx)
          do (raster-put r (start-line n) (+ (start-col n) i) (char pfx i) (face n)))
    (when (node n) (paint (node n) r))))

(defmethod measure ((n action) aw ah)
  (if (node n) (measure (node n) aw ah) (values 0 1)))
(defmethod arrange ((n action) x y w h)
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (when (node n)
      (multiple-value-bind (cw ch) (measure (node n) w h)
        (arrange (node n) (+ x (floor (- w cw) 2)) (+ y (floor (- h ch) 2)) cw ch)))))
(defmethod paint ((n action) r) (when (node n) (paint (node n) r)))

(defun %list-items (n)
  (let* ((is (items n)) (mx (max-visible n))
         (vis (if mx (subseq is 0 (min mx (length is))) is)))
    (setf (rendered n)
          (loop for item in vis for i from 0 collect (funcall (item-fn n) item i)))))

(defmethod measure ((n list-node) aw ah)
  (let ((w 0) (h 0))
    (dolist (c (%list-items n))
      (multiple-value-bind (cw ch) (measure c aw ah) (setf w (max w cw)) (incf h ch)))
    (values w h)))
(defmethod arrange ((n list-node) x y w h)
  (call-next-method)
  (let ((cy y))
    (dolist (c (rendered n))
      (let ((ch (nth-value 1 (measure c w h))))
        (arrange c x cy w ch)
        (setf cy (+ cy ch))))))
(defmethod paint ((n list-node) r) (dolist (c (rendered n)) (paint c r)))

(defmethod measure ((n grid) aw ah)
  (declare (ignore aw ah))
  (values (reduce #'+ (col-widths n) :initial-value 0) (length (cells n))))
(defmethod arrange ((n grid) x y w h)
  (call-next-method)
  (loop for row in (cells n) for ry from y do
    (loop with cx = x for cell in row for cwidth in (col-widths n)
          do (arrange cell cx ry cwidth 1) (incf cx cwidth))))
(defmethod paint ((n grid) r)
  (dolist (row (cells n)) (dolist (cell row) (paint cell r))))


;;;; The cell render: the same node tree the desktop paints in pixels,
;;;; rendered to styled cells for a buffer or the chrome. Styling is the ONE
;;;; resolution: a node's CSS classes through pine.style (the desktop's
;;;; theme-rules), else its face name through the theme faces -- both terminate
;;;; in the same (fg bg attr) run values. The arranged tree rides along so any
;;;; rendered (line col) maps back to its node with no side table.

(defun class-names (c)
  "Normalize a :class value to a list of class-name strings: a keyword or
symbol is its downcased name, a list is the class set, a string splits on
spaces."
  (etypecase c
    (null nil)
    (symbol (list (string-downcase (symbol-name c))))
    (string (remove "" (uiop:split-string c :separator '(#\space)) :test #'string=))
    (list (mapcan #'class-names c))))

(defun node-classes (n)
  (class-names (css-class n)))

(defun %scale-rgb (c)
  "pine.style colours are 0..1; cells carry 0..255."
  (list (round (* 255 (first c))) (round (* 255 (second c))) (round (* 255 (third c)))))

(defun %node-cell-style (n full)
  "The (FG BG ATTR) cell tuple for N from CSS matched on the class chain FULL,
falling back per-part to N's face name; nil when CSS contributes nothing (the
face name, if any, then resolves as usual at paint)."
  (let* ((st (pine.style:resolve full))
         (css-fg (pine.style:st-fg st))
         (css-bg (pine.style:st-bg st))
         (bold (pine.style:st-bold st))
         (name (and (keywordp (face n)) (face n))))
    (when (or css-fg css-bg bold)
      (list (if css-fg (%scale-rgb css-fg) (and name (pine.buffer:face-fg name)))
            (if css-bg (%scale-rgb css-bg) (and name (pine.buffer:face-bg name)))
            (logior (if bold 1 0)
                    (if name
                        (pine.buffer:face-attr-bits (pine.buffer:find-face name))
                        0))))))

(defun resolve-styles! (root &optional chain)
  "Resolve every node's cell colours before a cell render: CSS classes win,
the node's face name fills the gaps, and the result lands in the face slot as a
precomputed (FG BG ATTR) tuple. Colour/weight only -- CSS box properties are
pixel values and do not apply to cell layout. A selected selectable resolves
with a \"sel\" class appended, so \".foo.sel\" rules style the selection. Runs
at build time; the rendered tree is read-only afterwards."
  (labels ((walk (n chain)
             (let* ((classes (append (node-classes n)
                                     (and (typep n 'selectable) (selectedp n)
                                          (list "sel"))))
                    (full (append chain (list classes))))
               (when classes
                 (let ((tuple (%node-cell-style n full)))
                   (when tuple (setf (face n) tuple))))
               (dolist (c (nodes-of n)) (walk c full)))))
    (walk root chain))
  root)

(defun raster->rows (r)
  "Scan raster R into rows (TEXT . RUNS), run = (col fr fg fb br bg bb attr) --
the same run format frame->rows ships on the wire, so one format serves the
frame, the chrome, and layout buffer rows."
  (let ((cols (raster-cols r)) (v (raster-cells r)))
    (loop for row from 0 below (raster-rows r) collect
      (let ((text (make-string cols :initial-element #\space)) (runs nil) (prev nil))
        (dotimes (c cols)
          (let* ((off (%cell-off r row c))
                 (style (list (svref v (+ off 3)) (svref v (+ off 4)) (svref v (+ off 5))
                              (svref v (+ off 6)) (svref v (+ off 7)) (svref v (+ off 8))
                              (svref v (+ off 9)))))
            (setf (char text c) (code-char (svref v (+ off 2))))
            (unless (equal style prev)
              (push (list* c style) runs)
              (setf prev style))))
        (cons text (nreverse runs))))))

(defun render (root width &key height selection)
  "Render ROOT to cells: flag SELECTION (the nth selectable), resolve styles,
lay out at WIDTH, paint, and return (values rows tree) -- rows in the wire row
format, TREE the arranged root whose rects make node-at a position->node map.
The returned tree is read-only: selection is an input here, never a mutation of
a published tree. Layout buffers use static children (vstack), not list-node
item functions, which build only at measure."
  (let ((*text-size* nil))
    (when selection
      (loop for s in (collect-selectables root) for i from 0
            do (setf (selectedp s) (= i selection))))
    (resolve-styles! root)
    (multiple-value-bind (mw mh) (measure root width (or height 1000))
      (declare (ignore mw))
      (let* ((h (max 1 (or height mh)))
             (r (make-raster width h)))
        (arrange root 0 0 width h)
        (paint root r)
        (values (raster->rows r) root)))))


;;;; Widgets -- a widget is a function of its arguments returning a node tree.
;;;; Reading reactive cells (pine.ref:deref) in the body makes it re-render
;;;; when those cells change, when rendered inside a reactive view. Widgets
;;;; compose by calling one another.

(defmacro defwidget (name (&rest args) &body body)
  `(defun ,name (,@args) ,@body))


;;;; Wire codec: a widget tree <-> plain data, so a declarative UI can cross the
;;;; attach wire. CLOS nodes and closure click-handlers do not serialize, so a
;;;; node becomes (TYPE PLIST . CHILDREN) and any handler is replaced by an
;;;; :action id -- the producer keeps the closure keyed by id, the renderer sends
;;;; the id back on interaction. Only plain data (keywords, numbers, strings,
;;;; lists) is emitted.

(defun %wire-base (n)
  "The non-default style props of node N as a plist, plus its arranged rect
when one was assigned (so an arranged tree's geometry crosses the wire)."
  (let (p)
    (flet ((put (k v &optional default) (unless (equal v default) (setf (getf p k) v))))
      (put :class (css-class n)) (put :face (face n)) (put :hint (hint n)) (put :font-px (font-px n))
      (put :radius (radius n) 0) (put :fill (fill-of n)) (put :grad (grad n))
      (put :pad-x (pad-x n) 0) (put :pad-y (pad-y n) 0)
      (put :min-w (min-w n) 0) (put :min-h (min-h n) 0)
      (put :expand (expand-of n) 0)
      (when (or (plusp (end-col n)) (plusp (end-line n)))
        (put :rect (list (start-line n) (start-col n) (end-line n) (end-col n)))))
    p))

(defun node->wire (n &key on-action)
  "Serialize node N to plain data. ON-ACTION, given a handler closure, returns an
id to embed."
  (when n
    (flet ((kids (list) (mapcar (lambda (c) (node->wire c :on-action on-action)) list))
           (act (cb) (and cb on-action (funcall on-action cb))))
      (etypecase n
        (text-node (list :label (list* :content (content n) (%wire-base n))))
        (separator (list :rule (list* :char (char-code (sep-char n))
                                      :vertical (sep-vertical n)
                                      (%wire-base n))))
        (spacer    (list :gap (%wire-base n)))
        (slider    (list :meter (list* :value (value n) :min (min-of n) :max (max-of n)
                                       :action (act (on-change n)) (%wire-base n))))
        (ring      (list* :ring (list* :value (value n) :min (min-of n) :max (max-of n)
                                       :thickness (thickness n) :diameter (diameter n)
                                       :arc-face (arc-face n) :track-face (track-face n)
                                       (%wire-base n))
                          (kids (and (node n) (list (node n))))))
        (calendar  (list :calendar (list* :year (cal-year n) :month (cal-month n)
                                          :day (cal-day n) (%wire-base n))))
        (picture   (list :picture (list* :path (pic-path n) (%wire-base n))))
        (window-node (list :window (list* :rows (window-rows n) :crow (window-crow n)
                                        :ccol (window-ccol n) :opacity (window-opacity n)
                                        :base (window-base n)
                                        (%wire-base n))))
        (centerbox (list* :centerbox (list* :orient (cb-orient n) (%wire-base n))
                          (kids (list (cb-start n) (cb-center n) (cb-end n)))))
        (action    (list* :action (list* :action (act (callback n)) (%wire-base n))
                          (kids (list (node n)))))
        (selectable (list* :choice (list* :selected (selectedp n)
                                          :prefix-selected (prefix-selected n)
                                          :prefix-unselected (prefix-unselected n)
                                          (%wire-base n))
                           (kids (list (node n)))))
        (box       (list* :box (list* :width (width-of n) :align (align n) (%wire-base n))
                          (kids (list (node n)))))
        (scroll    (list* :viewport (list* :height (vheight n) (%wire-base n))
                          (kids (list (node n)))))
        (center    (list* :centered (%wire-base n) (kids (list (node n)))))
        (list-node (list* :column (list* :spacing 0 :align :start (%wire-base n))
                          (kids (%list-items n))))
        (grid      (list* :grid (list* :col-widths (col-widths n)
                                       :ncols (length (col-widths n)) (%wire-base n))
                          (kids (apply #'append (cells n)))))
        (vstack    (list* :column (list* :spacing (spacing n) :align (align n) (%wire-base n))
                          (kids (nodes n))))
        (hstack    (list* :row (list* :spacing (spacing n) :align (align n) (%wire-base n))
                          (kids (nodes n))))))))

(defun %wire-clean (props &rest drop)
  (loop for (k v) on props by #'cddr unless (member k drop) append (list k v)))

(defun arranged-p (n)
  "True when N carries an arranged rect (from its own arrange, or the wire)."
  (or (plusp (end-col n)) (plusp (end-line n))))

(defun wire->node (form &key on-action)
  "Rebuild a node from wire FORM, restoring its arranged rect when the wire
carries one. ON-ACTION, given an id, returns a handler (a function of any
interaction args) -- the renderer's 'send this id back'."
  (when form
    (destructuring-bind (type props &rest children) form
      (let ((rect (getf props :rect)))
        (setf props (%wire-clean props :rect))
        (let ((n (%wire->node type props children on-action)))
          (when (and n rect)
            (destructuring-bind (sl sc el ec) rect
              (setf (start-line n) sl (start-col n) sc
                    (end-line n) el (end-col n) ec)))
          n)))))

(defun %wire->node (type props children on-action)
  (flet ((kids () (mapcar (lambda (c) (wire->node c :on-action on-action)) children))
         (handler (id) (and id on-action (funcall on-action id))))
    (ecase type
          (:label    (apply #'label (getf props :content "") (%wire-clean props :content)))
          (:rule     (apply #'rule :char (code-char (getf props :char #x2500))
                            :vertical (getf props :vertical)
                            (%wire-clean props :char :vertical)))
          (:gap      (apply #'gap (%wire-clean props)))
          (:meter    (apply #'meter :value (getf props :value 0) :min (getf props :min 0)
                            :max (getf props :max 100)
                            :on-change (handler (getf props :action))
                            (%wire-clean props :value :min :max :action)))
          (:ring     (apply #'ring :value (getf props :value 0) :min (getf props :min 0)
                            :max (getf props :max 100) :thickness (getf props :thickness 5)
                            :diameter (getf props :diameter 56)
                            :arc-face (getf props :arc-face :function-name)
                            :track-face (getf props :track-face :comment)
                            (append (%wire-clean props :value :min :max :thickness :diameter
                                                  :arc-face :track-face)
                                    (kids))))
          (:calendar (apply #'cal :year (getf props :year 2000) :month (getf props :month 1)
                            :day (getf props :day 1) (%wire-clean props :year :month :day)))
          (:picture  (apply #'pic (getf props :path "") (%wire-clean props :path)))
          (:window   (apply #'window (getf props :rows)
                            :crow (getf props :crow -1) :ccol (getf props :ccol -1)
                            :opacity (getf props :opacity 1.0)
                            :base (getf props :base)
                            (%wire-clean props :rows :crow :ccol :opacity :base)))
          (:centerbox (centerbox :orient (getf props :orient :v)
                                 :class (getf props :class) :hint (getf props :hint)
                                 :expand (getf props :expand 0)
                                 :start  (wire->node (first children) :on-action on-action)
                                 :center (wire->node (second children) :on-action on-action)
                                 :end    (wire->node (third children) :on-action on-action)))
          (:action   (apply #'make-instance 'action
                            :callback (handler (getf props :action))
                            :node (first (kids)) (%wire-clean props :action)))
          (:choice   (apply #'make-instance 'selectable
                            :selected (getf props :selected)
                            :prefix-selected (getf props :prefix-selected "> ")
                            :prefix-unselected (getf props :prefix-unselected "  ")
                            :node (first (kids))
                            (%wire-clean props :selected :prefix-selected :prefix-unselected)))
          (:box      (apply #'boxed :width (getf props :width 0) :align (getf props :align :left)
                            (append (%wire-clean props :width :align) (kids))))
          (:grid     (let* ((ncols (max 1 (getf props :ncols 1)))
                            (flat (kids)))
                       (make-instance 'grid :col-widths (getf props :col-widths)
                         :cells (loop for i from 0 below (length flat) by ncols
                                      collect (subseq flat i (min (+ i ncols) (length flat)))))))
          (:viewport (apply #'viewport :height (getf props :height 10)
                            (append (%wire-clean props :height) (kids))))
          (:centered (apply #'centered (append (%wire-clean props) (kids))))
          (:column   (apply #'column (append (%wire-clean props) (kids))))
          (:row      (apply #'row (append (%wire-clean props) (kids)))))))


;;;; Scroll helper

(defun scroll-to-selection (sel offset max-vis)
  (when (minusp sel)
    (return-from scroll-to-selection (max 0 offset)))
  (let ((o offset))
    (when (>= sel (+ o max-vis)) (setf o (1+ (- sel max-vis))))
    (when (< sel o)              (setf o sel))
    (max 0 o)))


;;;; Hit-testing -- map an arranged (line col) to the node under it.

(defun %node-contains (n line col)
  (and (<= (start-line n) line) (<= line (end-line n))
       (<= (start-col n) col) (< col (end-col n))))

(defun node-at (node line col)
  "The deepest action, selectable, or slider whose rect contains (LINE COL)."
  (labels ((walk (n)
             (when n
               (typecase n
                 (action     (when (%node-contains n line col) (or (walk (node n)) n)))
                 (selectable (when (%node-contains n line col) (or (walk (node n)) n)))
                 (slider     (when (%node-contains n line col) n))
                 (hstack (some #'walk (nodes n)))
                 (vstack (some #'walk (nodes n)))
                 (centerbox (some #'walk (%cb-parts n)))
                 (box    (walk (node n)))
                 (center (walk (node n)))
                 (scroll (when (%node-contains n line col) (walk (node n))))
                 (grid   (some (lambda (row) (some #'walk row)) (cells n)))
                 (list-node (some #'walk (rendered n)))
                 (t nil)))))
    (walk node)))

(defun action-at (node line col)
  "The callback of the action under (LINE COL), or nil."
  (let ((hit (node-at node line col)))
    (when (typep hit 'action) (callback hit))))

(defun hint-at (root line col)
  "The hover hint of the node under (LINE COL), or nil."
  (let ((n (node-at root line col)))
    (and n (hint n))))

(defun nodes-of (n)
  "N's node nodes, for tree walks (e.g. the cairo chrome pass)."
  (typecase n
    ((or vstack hstack) (nodes n))
    ((or box center action selectable scroll ring) (and (node n) (list (node n))))
    (centerbox (%cb-parts n))
    (grid (apply #'append (cells n)))
    (list-node (rendered n))
    (t nil)))

(defun click-thunk (root line col)
  "A nullary thunk for a click at (LINE COL) on arranged ROOT: an action's
callback, or a slider's on-change applied to the column-mapped value. Nil if the
point is over neither."
  (let ((hit (node-at root line col)))
    (typecase hit
      (action (callback hit))
      (slider (let ((fn (on-change hit)) (v (slider-value-at hit col)))
                (when fn (lambda () (funcall fn v)))))
      (t nil))))


;;;; Selection navigation

(defun collect-selectables (n)
  (let ((result nil))
    (labels ((walk (x)
               (when x
                 (typecase x
                   (selectable (push x result))
                   (vstack (mapc #'walk (nodes x)))
                   (hstack (mapc #'walk (nodes x)))
                   (centerbox (mapc #'walk (%cb-parts x)))
                   (box (walk (node x)))
                   (center (walk (node x)))
                   (scroll (walk (node x)))
                   (grid (dolist (row (cells x)) (mapc #'walk row)))
                   (action (walk (node x)))
                   (list-node (mapc #'walk (rendered x)))
                   (t nil)))))
      (walk n))
    (nreverse result)))
