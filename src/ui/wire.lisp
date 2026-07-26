(defpackage #:pine.ui.wire
  (:use #:cl #:pine.ui.node #:pine.ui.raster)
  (:export #:apply-rows-patch #:arranged-p #:node->wire #:rows-patch #:scroll-to-selection #:wire->node #:wire-windows))

(in-package #:pine.ui.wire)

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
        (pine.ui.node:ring      (list* :ring (list* :value (value n) :min (min-of n) :max (max-of n)
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
        (pine.ui.node:centerbox (list* :centerbox (list* :orient (cb-orient n) (%wire-base n))
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
                          (kids (pine.ui.layout:%list-items n))))
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

;;;; Sending only what moved.
;;;;
;;;; Nearly all of an editor frame is the rendered lines inside its pine.ui.build:window
;;;; nodes, and a keystroke changes one of them. These compare two wire forms
;;;; and produce the lines that differ, so a push can carry those instead of
;;;; the screen. Everything here is plain data, the same as the wire itself.

(defun wire-windows (form)
  "Every :window form in FORM, in tree order."
  (let (acc)
    (labels ((walk (f)
               (when (and (consp f) (keywordp (first f)))
                 (when (eq (first f) :window) (push f acc))
                 (dolist (child (cddr f)) (walk child)))))
      (walk form))
    (nreverse acc)))

(defun wire-shape (form)
  "FORM with everything a patch can carry removed: the pine.ui.build:rows and the cursor.

Two forms with the same shape differ only in what a patch can express, which
is the test for whether one may be sent."
  (if (and (consp form) (keywordp (first form)))
      (let ((props (copy-list (second form))))
        (when (eq (first form) :window)
          (remf props :rows) (remf props :crow) (remf props :ccol))
        (list* (first form) props (mapcar #'wire-shape (cddr form))))
      form))

(defun rows-patch (old new)
  "What changed between two wire forms, or NIL when a patch cannot say it.

The patch is one entry per pine.ui.build:window, (INDEX CROW CCOL (LINE . ROW)...), carrying
only the lines that differ. NIL means the caller must send FORM whole: a
different tree, a different number of lines, or no previous form at all."
  (when (and old new (equal (wire-shape old) (wire-shape new)))
    (let ((olds (wire-windows old))
          (news (wire-windows new)))
      (when (= (length olds) (length news))
        (loop :for o :in olds
              :for n :in news
              :for index :from 0
              :for o-rows := (getf (second o) :rows)
              :for n-rows := (getf (second n) :rows)
              :unless (= (length o-rows) (length n-rows))
                :do (return-from rows-patch nil)
              :collect (list index
                             (getf (second n) :crow)
                             (getf (second n) :ccol)
                             (loop :for a :in o-rows
                                   :for b :in n-rows
                                   :for line :from 0
                                   :unless (equal a b)
                                     :collect (cons line b))))))))

(defun apply-rows-patch (form patch)
  "FORM with PATCH applied: a fresh wire form carrying the patched lines."
  (let ((windows (wire-windows form))
        (index -1))
    (labels ((patched (f)
               (if (and (consp f) (keywordp (first f)))
                   (if (eq (first f) :window)
                       (let* ((entry (assoc (incf index) patch))
                              (props (copy-list (second f))))
                         (cond
                           ((null entry) f)
                           (t (destructuring-bind (crow ccol lines) (rest entry)
                                (setf (getf props :crow) crow
                                      (getf props :ccol) ccol
                                      (getf props :rows)
                                      (let ((pine.ui.build:rows (copy-list (getf props :rows))))
                                        (dolist (line lines pine.ui.build:rows)
                                          (setf (nth (car line) pine.ui.build:rows) (cdr line)))))
                                (list* :window props (cddr f))))))
                       (list* (first f) (second f) (mapcar #'patched (cddr f))))
                   f)))
      (declare (ignorable windows))
      (patched form))))

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
          (:label    (apply #'pine.ui.build:label (getf props :content "") (%wire-clean props :content)))
          (:rule     (apply #'pine.ui.build:rule :char (code-char (getf props :char #x2500))
                            :vertical (getf props :vertical)
                            (%wire-clean props :char :vertical)))
          (:gap      (apply #'pine.ui.build:gap (%wire-clean props)))
          (:meter    (apply #'pine.ui.build:meter :value (getf props :value 0) :min (getf props :min 0)
                            :max (getf props :max 100)
                            :on-change (handler (getf props :action))
                            (%wire-clean props :value :min :max :action)))
          (:ring     (apply #'pine.ui.build:ring :value (getf props :value 0) :min (getf props :min 0)
                            :max (getf props :max 100) :thickness (getf props :thickness 5)
                            :diameter (getf props :diameter 56)
                            :arc-face (getf props :arc-face :function-name)
                            :track-face (getf props :track-face :comment)
                            (append (%wire-clean props :value :min :max :thickness :diameter
                                                  :arc-face :track-face)
                                    (kids))))
          (:calendar (apply #'pine.ui.build:cal :year (getf props :year 2000) :month (getf props :month 1)
                            :day (getf props :day 1) (%wire-clean props :year :month :day)))
          (:picture  (apply #'pine.ui.build:pic (getf props :path "") (%wire-clean props :path)))
          (:window   (apply #'pine.ui.build:window (getf props :rows)
                            :crow (getf props :crow -1) :ccol (getf props :ccol -1)
                            :opacity (getf props :opacity 1.0)
                            :base (getf props :base)
                            (%wire-clean props :rows :crow :ccol :opacity :base)))
          (:centerbox (pine.ui.build:centerbox :orient (getf props :orient :v)
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
          (:box      (apply #'pine.ui.build:boxed :width (getf props :width 0) :align (getf props :align :left)
                            (append (%wire-clean props :width :align) (kids))))
          (:grid     (let* ((ncols (max 1 (getf props :ncols 1)))
                            (flat (kids)))
                       (make-instance 'grid :col-widths (getf props :col-widths)
                         :cells (loop for i from 0 below (length flat) by ncols
                                      collect (subseq flat i (min (+ i ncols) (length flat)))))))
          (:viewport (apply #'pine.ui.build:viewport :height (getf props :height 10)
                            (append (%wire-clean props :height) (kids))))
          (:centered (apply #'pine.ui.build:centered (append (%wire-clean props) (kids))))
          (:column   (apply #'pine.ui.build:column (append (%wire-clean props) (kids))))
          (:row      (apply #'pine.ui.build:row (append (%wire-clean props) (kids)))))))


;;;; Scroll helper

(defun scroll-to-selection (sel offset max-vis)
  (when (minusp sel)
    (return-from scroll-to-selection (max 0 offset)))
  (let ((o offset))
    (when (>= sel (+ o max-vis)) (setf o (1+ (- sel max-vis))))
    (when (< sel o)              (setf o sel))
    (max 0 o)))
