(defpackage #:pine.ui.node
  (:use #:cl)
  (:export

   #:node #:key-of #:of #:face #:css-class #:hint #:hovered
   #:radius #:fill-of #:grad #:font-px #:pad-x #:pad-y #:min-w #:min-h
   #:node-margin #:expand-of #:start-line #:start-col #:end-line #:end-col

   #:text-node #:content #:on-change
   #:separator #:sep-char #:sep-vertical
   #:spacer #:vstack #:hstack #:stack #:nodes #:spacing #:align
   #:box #:width-of #:pad-char #:center #:centerbox
   #:cb-orient #:cb-start #:cb-center #:cb-end
   #:scroll #:scroll-offset #:vheight
   #:selectable #:data #:click #:selectedp #:prefix-selected #:prefix-unselected
   #:action #:callback
   #:list-node #:items #:item-fn #:max-visible #:rendered
   #:slider #:slider-fraction #:filled-face #:empty-face #:track
   #:ring #:ring-fraction #:thickness #:diameter #:arc-face #:track-face
   #:value #:min-of #:max-of
   #:calendar #:cal-year #:cal-month #:cal-day
   #:picture #:pic-path

   #:view-node #:view-rows #:view-crow #:view-ccol #:view-opacity
   #:view-base
   #:buffer-view #:modeline-view #:echo-view #:os-window-view))

(in-package #:pine.ui.node)

(defclass node ()
  ((key-of  :initarg :key    :accessor key-of  :initform nil)

   (of      :initarg :of     :accessor of      :initform nil)

   (selectedp :initarg :selected :accessor selectedp :initform nil)
   (face    :initarg :face   :accessor face    :initform nil)
   (css-class :initarg :class :accessor css-class :initform nil)
   (hint    :initarg :hint   :accessor hint    :initform nil)
   (hovered :initform nil    :accessor hovered)

   (radius  :initarg :radius :accessor radius  :initform 0)
   (fill-of :initarg :fill   :accessor fill-of :initform nil)
   (grad    :initarg :grad   :accessor grad    :initform nil)

   (font-px :initarg :font-px :accessor font-px :initform nil)

   (pad     :initarg :pad    :initform nil)
   (pad-x   :initarg :pad-x  :accessor pad-x   :initform 0)
   (pad-y   :initarg :pad-y  :accessor pad-y   :initform 0)
   (min-w   :initarg :min-w  :accessor min-w   :initform 0)
   (min-h   :initarg :min-h  :accessor min-h   :initform 0)

   (margin  :initarg :margin :accessor node-margin :initform nil)
   (expand  :initarg :expand :accessor expand-of :initform 0)
   (start-line :initform 0 :accessor start-line)
   (start-col  :initform 0 :accessor start-col)
   (end-line   :initform 0 :accessor end-line)
   (end-col    :initform 0 :accessor end-col)))

(defclass text-node (node)
  ((content :initarg :content :accessor content :initform "")

   (on-change :initarg :on-change :accessor on-change :initform nil)))

(defclass separator (node)
  ((sep-char :initarg :char :accessor sep-char :initform (code-char #x2500))
   (vertical :initarg :vertical :accessor sep-vertical :initform nil)))

(defclass spacer (node) ())

(defclass vstack (node)
  ((nodes :initarg :nodes :accessor nodes :initform nil)
   (spacing  :initarg :spacing  :accessor spacing  :initform 0)
   (align    :initarg :align    :accessor align    :initform :start)))

(defclass hstack (node)
  ((nodes :initarg :nodes :accessor nodes :initform nil)
   (spacing  :initarg :spacing  :accessor spacing  :initform 1)
   (align    :initarg :align    :accessor align    :initform :start)))

(defclass stack (node)
  ((nodes :initarg :nodes :accessor nodes :initform nil))
  (:documentation "Nodes in one place: each is given the whole rect and they are
painted in the order they were written, so the last one is on top."))

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
   (click             :initarg :click    :accessor click             :initform nil)
   (prefix-selected   :initarg :prefix-selected   :accessor prefix-selected   :initform "> ")
   (prefix-unselected :initarg :prefix-unselected :accessor prefix-unselected :initform "  ")))

(defclass action (node)
  ((node    :initarg :node    :accessor node    :initform nil)
   (callback :initarg :callback :accessor callback :initform nil)))

(defclass list-node (node)
  ((items       :initarg :items       :accessor items       :initform nil)
   (item-fn     :initarg :item-fn     :accessor item-fn     :initform nil)
   (max-visible :initarg :max-visible :accessor max-visible :initform nil)

   (rendered    :accessor rendered    :initform nil)))

(defclass slider (node)
  ((value       :initarg :value       :accessor value       :initform 0)
   (min-of      :initarg :min         :accessor min-of      :initform 0)
   (max-of      :initarg :max         :accessor max-of      :initform 100)
   (track       :initarg :track       :accessor track       :initform 16)

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

(defclass view-node (node)
  ((rows :initarg :rows :accessor view-rows :initform nil)
   (crow :initarg :crow :accessor view-crow :initform -1)
   (ccol :initarg :ccol :accessor view-ccol :initform -1)
   (opacity :initarg :opacity :accessor view-opacity :initform 1.0)

   (base :initarg :base :accessor view-base :initform nil)))

(defclass buffer-view (view-node) ()
  (:documentation "A window onto a buffer."))

(defclass modeline-view (view-node) ()
  (:documentation "The mode line of a window."))

(defclass echo-view (view-node) ()
  (:documentation "The echo line."))

(defclass os-window-view (view-node) ()
  (:documentation "A window the compositor holds, laid out by /wm."))

(defclass centerbox (node)
  ((orient    :initarg :orient :accessor cb-orient :initform :v)
   (cb-start  :initarg :start  :accessor cb-start  :initform nil)
   (cb-center :initarg :center :accessor cb-center :initform nil)
   (cb-end    :initarg :end    :accessor cb-end    :initform nil)))

(defmethod initialize-instance :after ((n node) &key pad)
  (when pad (setf (pad-x n) pad (pad-y n) pad)))

(defmethod initialize-instance :after ((n separator) &key)
  (when (and (sep-vertical n) (char= (sep-char n) (code-char #x2500)))
    (setf (sep-char n) (code-char #x2502))))

(defmethod initialize-instance :after ((n spacer) &key)
  (when (zerop (expand-of n)) (setf (expand-of n) 1)))

(defmethod initialize-instance :after ((n slider) &key)

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
