(defpackage #:pine/ui/layout
  (:use #:cl #:pine/ui/node #:pine/ui/raster)
  (:export

   #:measure #:arrange #:paint #:nodes-of

   #:*text-size* #:*default-font-px* #:text-size #:line-height

   #:split-node #:remove-node

   #:node-at #:clicked #:click-thunk #:slider-value-at #:collect-selectables
   #:placep

   #:centerbox-parts #:list-items #:view-overlay-count))
(in-package #:pine/ui/layout)

(defvar *text-size* nil
  "When bound to a function of (text font-px) -> (values w h), text leaves
measure through it (pixel/cairo mode); otherwise a character is one cell.")
(defvar *default-font-px* 13)

(defparameter *hover-face* nil
  "A face designator painted as the background of the hovered node, or nil.")

(defparameter +slider-filled+ (code-char #x2588))
(defparameter +slider-empty+  (code-char #x2500))

(defun text-size (text font-px)
  (if *text-size*
      (funcall *text-size* text (or font-px *default-font-px*))
      (values (length text) 1)))

(defun line-height (font-px)
  (if *text-size* (nth-value 1 (text-size "M" font-px)) 1))

(defgeneric nodes-of (node)
  (:documentation "NODE's nodes, in the order they are laid out and painted.

The one place a class says what it contains. Every walk over the tree is
written on this, so a new node kind states its structure once.")
  (:method ((n node)) (declare (ignore n)) nil))

(defgeneric measure (node avail-w avail-h)
  (:documentation "The node's natural (values w h) given the available space,
in cells (default) or pixels (when *text-size* is bound). Bottom-up."))

(defgeneric arrange (node x y w h)
  (:documentation "Assign the node the rect X Y W H and place its nodes."))

(defgeneric paint (node raster)
  (:documentation "Draw the node into its arranged rect on RASTER."))

(defgeneric node-at (node line col)
  (:documentation "The deepest node at (LINE COL) that answers interaction: an
action, a selectable, or a slider. Nil when nothing there does.

The default descends into the nodes, so a container needs no method and an
interactive node says only what it does with a hit.")
  (:method ((n node) line col)
    (some (lambda (node) (node-at node line col)) (nodes-of n))))

(defun %node-width (n) (- (end-col n) (start-col n)))

(defmethod measure ((n node) aw ah) (declare (ignore aw ah)) (values 0 1))

(defun %margin-x (n) (let ((m (node-margin n))) (if m (+ (fourth m) (second m)) 0)))
(defun %margin-y (n) (let ((m (node-margin n))) (if m (+ (first m) (third m)) 0)))

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
  (let ((m (node-margin n)))
    (if m
        (call-next-method n (+ x (fourth m)) (+ y (first m))
                          (max 0 (- w (%margin-x n))) (max 0 (- h (%margin-y n))))
        (call-next-method))))

(defun %inner (n x y w h)
  "The content rect of N inside its padding."
  (values (+ x (pad-x n)) (+ y (pad-y n))
          (max 0 (- w (* 2 (pad-x n)))) (max 0 (- h (* 2 (pad-y n))))))

(defmethod arrange ((n node) x y w h)
  (setf (start-col n) x (start-line n) y
        (end-col n) (+ x w) (end-line n) (+ y (max 0 (1- h)))))

(defmethod paint ((n node) r) (declare (ignore r)) nil)

(defun %fill-bg (n r)
  "Fill the node's rect background: the hover face when hovered, else its own."
  (let ((f (if (and (hovered n) *hover-face*) *hover-face* (face n))))
    (when f
      (multiple-value-bind (fr fg fb br bg bb attr) (face-cell-rgb f)
        (declare (ignore fr fg fb attr))
        (when (>= br 0)
          (loop for row from (start-line n) to (end-line n) do
            (loop for col from (start-col n) below (end-col n)
                  do (raster-put-bg r row col br bg bb))))))))

(defmethod paint :before ((n node) r) (%fill-bg n r))

(defmethod measure ((n text-node) aw ah)
  (declare (ignore aw ah)) (text-size (content n) (font-px n)))
(defmethod paint ((n text-node) r)
  (let ((s (content n)) (w (%node-width n)))
    (loop for i from 0 below (min (length s) w)
          do (raster-put r (start-line n) (+ (start-col n) i) (char s i) (face n)))))

(defun view-overlay-count (n)
  "How many of N's leading rows are overlay (drawn above the arranged rect)."
  (let ((base (view-base n)))
    (if base (max 0 (- (length (view-rows n)) base)) 0)))

(defmethod measure ((n view-node) aw ah)
  (declare (ignore aw ah))
  (let* ((rows (view-rows n))
         (cols (reduce #'max rows :initial-value 1 :key (lambda (row) (length (car row)))))
         (nrows (max 1 (- (length rows) (view-overlay-count n)))))
    (if *text-size*
        (multiple-value-bind (cw ch) (text-size "M" (font-px n))
          (values (* cols cw) (* nrows ch)))
        (values cols nrows))))
(defmethod paint ((n view-node) r)
  (let ((over (view-overlay-count n)))
    (loop for row in (view-rows n)
          for y from (- (start-line n) over)
          do (blit-row r y (start-col n) (car row) (cdr row)))))

(defmethod measure ((n separator) aw ah)
  "A separator's natural size is its thickness; its length comes from the
container's arrange (:align :stretch), never from the available space --
reporting AW/AH here would eat the whole axis as natural size."
  (declare (ignore aw ah))
  (let ((px (max 1 (pine/ui/face:metric :border 2))))
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

(defmethod measure ((n slider) aw ah)
  (declare (ignore aw ah))
  (if *text-size* (values (* 8 (track n)) (line-height (font-px n))) (values (track n) 1)))
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

(defmethod measure ((n stack) aw ah)
  (let ((w 0) (h 0))
    (dolist (c (nodes n) (values w h))
      (multiple-value-bind (cw ch) (measure c aw ah)
        (setf w (max w cw) h (max h ch))))))

(defmethod arrange ((n stack) x y w h)
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (dolist (c (nodes n)) (arrange c x y w h))))

(defmethod paint ((n stack) r) (dolist (c (nodes n)) (paint c r)))

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

(defun centerbox-parts (n) (remove nil (list (cb-start n) (cb-center n) (cb-end n))))
(defmethod measure ((n centerbox) aw ah)
  (let ((w 0) (h 0))
    (dolist (c (centerbox-parts n))
      (multiple-value-bind (cw ch) (measure c aw ah) (setf w (max w cw)) (incf h ch)))
    (values w h)))
(defmethod arrange ((n centerbox) x y w h)
  "Start at one end, end at the other, and the middle centred in what is left
between them. Centring it in the whole box instead is what puts a bar's apps
on top of its workspaces once there are enough workspaces."
  (call-next-method)
  (multiple-value-bind (x y w h) (%inner n x y w h)
    (let* ((s (cb-start n)) (c (cb-center n)) (e (cb-end n))
           (sh (if s (nth-value 1 (measure s w h)) 0))
           (eh (if e (nth-value 1 (measure e w h)) 0)))
      (when s (arrange s x y w sh))
      (when e (arrange e x (+ y (- h eh)) w eh))
      (when c
        (let* ((ch (nth-value 1 (measure c w h)))
               (room (max 0 (- h sh eh))))
          (arrange c x (+ y sh (max 0 (floor (- room ch) 2))) w ch))))))
(defmethod paint ((n centerbox) r) (dolist (c (centerbox-parts n)) (paint c r)))

(defmethod measure ((n scroll) aw ah)
  (declare (ignore ah))
  (values (if (node n) (nth-value 0 (measure (node n) aw 100000)) 0)
          (vheight n)))
(defmethod arrange ((n scroll) x y w h)
  (call-next-method n x y w (vheight n))
  (when (node n)
    (let ((ch (nth-value 1 (measure (node n) w 100000))))

      (arrange (node n) x (- y (scroll-offset n)) w ch))))
(defmethod paint ((n scroll) r)
  (when (node n)
    (with-clip (r (start-col n) (start-line n) (end-col n) (+ (start-line n) (vheight n)))
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

(defun list-items (n)
  (let* ((is (items n)) (mx (max-visible n))
         (vis (if mx (subseq is 0 (min mx (length is))) is)))
    (setf (rendered n)
          (loop for item in vis for i from 0 collect (funcall (item-fn n) item i)))))

(defmethod measure ((n list-node) aw ah)
  (let ((w 0) (h 0))
    (dolist (c (list-items n))
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
