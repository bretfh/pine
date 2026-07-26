(defpackage #:pine.ui.node
  (:use #:cl)
  (:export #:action #:align #:arc-face #:box #:cal-day #:cal-month #:cal-year #:calendar #:callback #:cb-center #:cb-end #:cb-orient #:cb-start #:cells #:center #:centerbox #:col-widths #:content #:css-class #:data #:diameter #:empty-face #:end-col #:end-line #:expand-of #:face #:fill-of #:filled-face #:font-px #:grad #:grid #:hint #:hovered #:hstack #:item-fn #:items #:key-of #:list-node #:max-of #:max-visible #:min-h #:min-of #:min-w #:node #:node-margin #:nodes #:on-change #:pad-char #:pad-x #:pad-y #:parent #:pic-path #:picture #:prefix-selected #:prefix-unselected #:radius #:rendered #:ring #:ring-fraction #:scroll #:scroll-offset #:selectable #:selectedp #:sep-char #:sep-vertical #:separator #:slider #:slider-fraction #:spacer #:spacing #:start-col #:start-line #:text-node #:thickness #:track #:track-face #:value #:vheight #:vstack #:width-of #:window-base #:window-ccol #:window-crow #:window-kind #:window-node #:window-of #:window-opacity #:window-rows))

(in-package #:pine.ui.node)

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
;; (window "scratch") and pine.text.buffer's window (the scroll/focus view) keeps
;; its own name in its own package. OF backs a live view with that
;; pine.text.window:window; KIND (:window :modeline :echo) marks what the editor's
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
