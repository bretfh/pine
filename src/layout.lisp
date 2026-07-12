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
   (expand  :initarg :expand :accessor expand-of :initform 0)
   (start-line :initform 0 :accessor start-line)
   (start-col  :initform 0 :accessor start-col)
   (end-line   :initform 0 :accessor end-line)
   (end-col    :initform 0 :accessor end-col)))

(defclass text-node (node)
  ((content :initarg :content :accessor content :initform "")))

(defclass separator (node)
  ((sep-char :initarg :char :accessor sep-char :initform (code-char #x2500))))

(defclass spacer (node) ())            ; flexible empty space; default expand 1

(defclass field (node)
  ((content       :initarg :content       :accessor content       :initform "")
   (prefix-length :initarg :prefix-length :accessor prefix-length :initform 0)
   (input-start-line :initform 0 :accessor input-start-line)
   (input-start-col  :initform 0 :accessor input-start-col)
   (input-end-line   :initform 0 :accessor input-end-line)
   (input-end-col    :initform 0 :accessor input-end-col)))

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
  (let* ((g (if (integerp glyph) (string (code-char glyph)) (string glyph)))
         (lbl (make-instance 'text-node :content g
                             :face (getf props :face) :font-px (getf props :font-px))))
    (if (getf props :on-click)
        (let ((p (copy-list props)))
          (remf p :face) (remf p :font-px)
          (let ((click (getf p :on-click)))
            (remf p :on-click)
            (apply #'make-instance 'action :callback click :node lbl p)))
        lbl)))

(defun column (&rest args)
  "A vertical box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'vstack :nodes items props)))

(defun row (&rest args)
  "A horizontal box. Props :spacing :align :expand :face; rest are nodes."
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'hstack :nodes items props)))

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

(defun rows (items item-fn &rest props)
  "A vertical list built by mapping ITEM-FN over ITEMS. (rows nets #'net-row)"
  (apply #'make-instance 'list-node :items items :item-fn item-fn props))

(defun choice (&rest args)
  "A selectable row (keyboard-navigable). (choice :data d (label ...))"
  (multiple-value-bind (props items) (%parse-args args)
    (apply #'make-instance 'selectable :node (first items) props)))


;;;; Faces -> cell colours

(defun %hex-rgb (hex)
  "The (values r g b) of a #rrggbb face colour, or a light default."
  (if (and hex (>= (length hex) 7) (char= (char hex 0) #\#))
      (values (parse-integer hex :start 1 :end 3 :radix 16)
              (parse-integer hex :start 3 :end 5 :radix 16)
              (parse-integer hex :start 5 :end 7 :radix 16))
      (values 205 214 244)))

(defun %face-cell-rgb (designator)
  "(values fr fg fb br bg bb bold) for a face DESIGNATOR; bg -1 means none.
Falls back to the default when no face (or no client to resolve it) exists."
  (let ((f (and designator (ignore-errors (pine.buffer:find-face designator)))))
    (if f
        (multiple-value-bind (fr fg fb) (%hex-rgb (or (pine.buffer:fg f) "#cdd6f4"))
          (if (pine.buffer:bg f)
              (multiple-value-bind (br bg bb) (%hex-rgb (pine.buffer:bg f))
                (values fr fg fb br bg bb (if (pine.buffer:bold f) 1 0)))
              (values fr fg fb -1 -1 -1 (if (pine.buffer:bold f) 1 0))))
        (values 205 214 244 -1 -1 -1 0))))


;;;; Raster — a cell buffer widgets paint into. Cells are the flat 10-slot
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
    (multiple-value-bind (fr fg fb br bg bb bold) (%face-cell-rgb face)
      (let ((off (%cell-off r row col)) (v (raster-cells r)))
        (setf (svref v (+ off 2)) (char-code ch)
              (svref v (+ off 3)) fr (svref v (+ off 4)) fg (svref v (+ off 5)) fb
              (svref v (+ off 9)) bold)
        (when (>= br 0)
          (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))))

(defun raster-put-bg (r row col br bg bb)
  (when (%in-raster r row col)
    (let ((off (%cell-off r row col)) (v (raster-cells r)))
      (setf (svref v (+ off 6)) br (svref v (+ off 7)) bg (svref v (+ off 8)) bb))))

(defun raster-lines (r)
  (loop for row from 0 below (raster-rows r)
        collect (let ((s (make-string (raster-cols r))))
                  (dotimes (col (raster-cols r))
                    (setf (char s col)
                          (code-char (svref (raster-cells r) (+ 2 (%cell-off r row col))))))
                  (string-right-trim '(#\space) s))))


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

(defmethod measure :around ((n node) aw ah)
  "Wrap the intrinsic measure with padding and the minimum size (CSS box model):
content is measured in the space left after padding; the result adds padding
back and is floored at min-w/min-h."
  (multiple-value-bind (w h)
      (call-next-method n (max 0 (- aw (* 2 (pad-x n)))) (max 0 (- ah (* 2 (pad-y n)))))
    (values (max (min-w n) (+ w (* 2 (pad-x n))))
            (max (min-h n) (+ h (* 2 (pad-y n)))))))

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
      (multiple-value-bind (fr fg fb br bg bb bold) (%face-cell-rgb f)
        (declare (ignore fr fg fb bold))
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

(defmethod measure ((n separator) aw ah)
  (declare (ignore ah)) (values aw (%line-h (font-px n))))
(defmethod paint ((n separator) r)
  (loop for col from (start-col n) below (end-col n)
        do (raster-put r (start-line n) col (sep-char n) (face n))))

(defmethod measure ((n spacer) aw ah) (declare (ignore aw ah)) (values 0 0))

(defmethod measure ((n field) aw ah)
  (declare (ignore aw ah)) (%text-size (content n) (font-px n)))
(defmethod arrange ((n field) x y w h)
  (call-next-method)
  (setf (input-start-line n) y (input-start-col n) (+ x (prefix-length n))
        (input-end-line n) y (input-end-col n) (+ x (length (content n)))))
(defmethod paint ((n field) r)
  (let ((s (content n)) (w (%node-width n)))
    (loop for i from 0 below (min (length s) w)
          do (raster-put r (start-line n) (+ (start-col n) i) (char s i) (face n)))))

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
  "The value a click at absolute COL maps to for arranged slider N. The track
width is the arranged pixel width in pixel mode, else the cell track."
  (let* ((w (if *text-size* (max 1 (- (end-col n) (start-col n))) (track n)))
         (rel (max 0 (min w (- col (start-col n)))))
         (span (- (max-of n) (min-of n))))
    (+ (min-of n) (round (* (/ rel w) span)))))

;;; Containers — the flex box algebra

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
           (cy y))
      (loop for c in items for (cw ch) in nats
            for extra = (if (plusp tw) (round (* slack (/ (expand-of c) tw))) 0)
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
           (cx x))
      (loop for c in items for (cw ch) in nats
            for extra = (if (plusp tw) (round (* slack (/ (expand-of c) tw))) 0)
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


;;;; Render entry points

(defun render-layout-grid (layout &key hover)
  "Render LAYOUT to (values lines cell-vector slot-count). CELL-VECTOR is the
flat 10-slot format the surface painter draws; SLOT-COUNT is 10 * cols * rows,
matching frame-cell-count so paint-cell-grid iterates it with `below n by 10'.
HOVER, a (line . col) cons, marks the node under it hovered before painting."
  (let* ((root (layout-root layout))
         (w (layout-width layout)))
    (if (null root)
        (values (list "") (make-array 0) 0)
        (multiple-value-bind (mw mh) (measure root w (or (layout-height layout) 1000))
          (declare (ignore mw))
          (let* ((h (max 1 (or (layout-height layout) mh)))
                 (r (make-raster w h)))
            (arrange root 0 0 w h)
            (when hover
              (let ((n (node-at root (car hover) (cdr hover))))
                (when n (setf (hovered n) t))))
            (paint root r)
            (values (raster-lines r) (raster-cells r) (* 10 w h)))))))

(defun render-layout (layout)
  (nth-value 0 (render-layout-grid layout)))

(defun layout-lines (layout)
  (reduce (lambda (seq line) (fset:with-last seq line))
          (render-layout layout)
          :initial-value (fset:empty-seq)))

(defun node-to-string (n width)
  "Render node N at WIDTH to a single string."
  (multiple-value-bind (mw mh) (measure n width 1)
    (declare (ignore mw mh))
    (let ((r (make-raster width 1)))
      (arrange n 0 0 width 1)
      (paint n r)
      (first (raster-lines r)))))


;;;; Widgets — a widget is a function of its arguments returning a node tree.
;;;; Reading reactive cells (pine.cell:cell-ref) in the body makes it re-render
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
  "The non-default style props of node N as a plist."
  (let (p)
    (flet ((put (k v &optional default) (unless (equal v default) (setf (getf p k) v))))
      (put :face (face n)) (put :hint (hint n)) (put :font-px (font-px n))
      (put :radius (radius n) 0) (put :fill (fill-of n)) (put :grad (grad n))
      (put :pad-x (pad-x n) 0) (put :pad-y (pad-y n) 0)
      (put :min-w (min-w n) 0) (put :min-h (min-h n) 0)
      (put :expand (expand-of n) 0))
    p))

(defun node->wire (n &key on-action)
  "Serialize node N to plain data. ON-ACTION, given a handler closure, returns an
id to embed."
  (when n
    (flet ((kids (list) (mapcar (lambda (c) (node->wire c :on-action on-action)) list))
           (act (cb) (and cb on-action (funcall on-action cb))))
      (etypecase n
        (text-node (list :label (list* :content (content n) (%wire-base n))))
        (separator (list :rule (%wire-base n)))
        (spacer    (list :gap (%wire-base n)))
        (slider    (list :meter (list* :value (value n) :min (min-of n) :max (max-of n)
                                       :action (act (on-change n)) (%wire-base n))))
        (action    (list* :action (list* :action (act (callback n)) (%wire-base n))
                          (kids (list (node n)))))
        (scroll    (list* :viewport (list* :height (vheight n) (%wire-base n))
                          (kids (list (node n)))))
        (center    (list* :centered (%wire-base n) (kids (list (node n)))))
        (vstack    (list* :column (list* :spacing (spacing n) :align (align n) (%wire-base n))
                          (kids (nodes n))))
        (hstack    (list* :row (list* :spacing (spacing n) :align (align n) (%wire-base n))
                          (kids (nodes n))))))))

(defun %wire-clean (props &rest drop)
  (loop for (k v) on props by #'cddr unless (member k drop) append (list k v)))

(defun wire->node (form &key on-action)
  "Rebuild a node from wire FORM. ON-ACTION, given an id, returns a handler (a
function of any interaction args) -- the renderer's 'send this id back'."
  (when form
    (destructuring-bind (type props &rest children) form
      (flet ((kids () (mapcar (lambda (c) (wire->node c :on-action on-action)) children))
             (handler (id) (and id on-action (funcall on-action id))))
        (ecase type
          (:label    (apply #'label (getf props :content "") (%wire-clean props :content)))
          (:rule     (apply #'rule (%wire-clean props)))
          (:gap      (apply #'gap (%wire-clean props)))
          (:meter    (apply #'meter :value (getf props :value 0) :min (getf props :min 0)
                            :max (getf props :max 100)
                            :on-change (handler (getf props :action))
                            (%wire-clean props :value :min :max :action)))
          (:action   (apply #'make-instance 'action
                            :callback (handler (getf props :action))
                            :node (first (kids)) (%wire-clean props :action)))
          (:viewport (apply #'viewport :height (getf props :height 10)
                            (append (%wire-clean props :height) (kids))))
          (:centered (apply #'centered (append (%wire-clean props) (kids))))
          (:column   (apply #'column (append (%wire-clean props) (kids))))
          (:row      (apply #'row (append (%wire-clean props) (kids)))))))))


;;;; Scroll helper

(defun scroll-to-selection (sel offset max-vis)
  (when (minusp sel)
    (return-from scroll-to-selection (max 0 offset)))
  (let ((o offset))
    (when (>= sel (+ o max-vis)) (setf o (1+ (- sel max-vis))))
    (when (< sel o)              (setf o sel))
    (max 0 o)))


;;;; Layout container + registry (keyed by buffer name on the server)

(defclass layout ()
  ((root        :initarg :root   :accessor layout-root        :initform nil)
   (buffer-name :initarg :buffer :accessor layout-buffer-name :initform nil)
   (state       :initarg :state  :accessor layout-state       :initform nil)
   (width       :initarg :width  :accessor layout-width       :initform 80)
   ;; when set, the root is arranged into this many rows (a fixed-size surface
   ;; like the sidebar), so expanders distribute the extra space; otherwise the
   ;; natural height is used.
   (height      :initarg :height :accessor layout-height       :initform nil)))

(defun layouts-table ()
  (let ((srv (pine.client:server-of (pine.client:current-client))))
    (or (pine.server:layouts srv)
        (setf (pine.server:layouts srv) (make-hash-table :test #'equal)))))

(defun install-layout (layout)
  (setf (gethash (layout-buffer-name layout) (layouts-table)) layout))

(defun uninstall-layout (layout)
  (remhash (layout-buffer-name layout) (layouts-table)))

(defun buffer-layout (name)
  (gethash name (layouts-table)))

(defun layout-get (layout key &optional default)
  (getf (layout-state layout) key default))

(defun (setf layout-get) (value layout key)
  (setf (getf (layout-state layout) key) value)
  value)


;;;; Input string operations (for fields / completing-read)

(defun input-string (layout)
  (getf (layout-state layout) :input ""))

(defun (setf input-string) (val layout)
  (setf (getf (layout-state layout) :input) val))

(defun cursor-offset (layout)
  (let ((input (input-string layout)))
    (min (getf (layout-state layout) :cursor-offset (length input))
         (length input))))

(defun (setf cursor-offset) (val layout)
  (setf (getf (layout-state layout) :cursor-offset) val))

(defun type-char-at-cursor (layout char)
  (let* ((input (input-string layout))
         (off (cursor-offset layout)))
    (setf (input-string layout)
          (concatenate 'string (subseq input 0 off) (string char) (subseq input off)))
    (setf (cursor-offset layout) (1+ off))))

(defun delete-char-before-cursor (layout)
  (let* ((input (input-string layout))
         (off (cursor-offset layout)))
    (when (plusp off)
      (setf (input-string layout)
            (concatenate 'string (subseq input 0 (1- off)) (subseq input off)))
      (setf (cursor-offset layout) (1- off))
      t)))

(defun kill-input (layout)
  (setf (input-string layout) ""
        (cursor-offset layout) 0))

(defun kill-to-end (layout)
  (let ((off (cursor-offset layout)))
    (setf (input-string layout) (subseq (input-string layout) 0 off))))

(defun find-word-start (input offset &optional (sep #\space))
  (let* ((pos (loop for i from (1- offset) downto 0
                    while (eql (char input i) sep)
                    finally (return (1+ i)))))
    (loop for i from (1- pos) downto 0
          while (not (eql (char input i) sep))
          finally (return (1+ i)))))

(defun kill-word-before-cursor (layout &optional (sep #\space))
  (let* ((input (input-string layout))
         (off (cursor-offset layout))
         (ws (find-word-start input off sep)))
    (setf (input-string layout)
          (concatenate 'string (subseq input 0 ws) (subseq input off)))
    (setf (cursor-offset layout) ws)))

(defun set-input (layout text)
  (setf (input-string layout) (or text "")
        (cursor-offset layout) (length (or text ""))))

(defun move-cursor (layout delta)
  (let* ((input (input-string layout))
         (off (cursor-offset layout))
         (new (max 0 (min (length input) (+ off delta)))))
    (setf (cursor-offset layout) new)))

(defun cursor-to-start (layout)
  (setf (cursor-offset layout) 0))

(defun cursor-to-end (layout)
  (setf (cursor-offset layout) (length (input-string layout))))

(defun confirm-input (layout)
  (let ((sel (getf (layout-state layout) :selection -1))
        (filtered (getf (layout-state layout) :filtered)))
    (if (and (>= sel 0) (< sel (length filtered)))
        (nth sel filtered)
      (input-string layout))))


;;;; Hit-testing — map an arranged (line col) to the node under it.

(defun %node-contains (n line col)
  (and (<= (start-line n) line (end-line n))
       (cond ((= (start-line n) (end-line n))
              (and (<= (start-col n) col) (< col (end-col n))))
             ((= line (start-line n)) (<= (start-col n) col))
             ((= line (end-line n))   (< col (end-col n)))
             (t t))))

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
    ((or box center action selectable scroll) (and (node n) (list (node n))))
    (grid (apply #'append (cells n)))
    (list-node (rendered n))
    (t nil)))

(defun hex-rgb (hex)
  "The (values r g b) 0-255 of a #rrggbb string."
  (%hex-rgb hex))

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
                   (box (walk (node x)))
                   (center (walk (node x)))
                   (scroll (walk (node x)))
                   (grid (dolist (row (cells x)) (mapc #'walk row)))
                   (action (walk (node x)))
                   (list-node (mapc #'walk (rendered x)))
                   (t nil)))))
      (walk n))
    (nreverse result)))

(defun update-selection (layout index)
  (let* ((sels (collect-selectables (layout-root layout)))
         (n (length sels)))
    (when (zerop n) (return-from update-selection nil))
    (setf index (mod index n))
    (setf (layout-get layout :selection-index) index)
    (loop for s in sels for i from 0
          do (setf (selectedp s) (= i index)))
    (nth index sels)))

(defun selected-node (layout)
  (let ((idx (layout-get layout :selection-index 0)))
    (let ((sels (collect-selectables (layout-root layout))))
      (when sels (nth (mod idx (length sels)) sels)))))

(defun selection-move (layout delta)
  (let ((idx (layout-get layout :selection-index 0)))
    (update-selection layout (+ idx delta))))
