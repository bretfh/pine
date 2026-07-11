(in-package :pine.layout)


;;;; Nodes

(defclass node ()
  ((key-of  :initarg :key    :accessor key-of  :initform nil)
   (parent  :initarg :parent :accessor parent  :initform nil)
   (start-line :initform 0 :accessor start-line)
   (start-col  :initform 0 :accessor start-col)
   (end-line   :initform 0 :accessor end-line)
   (end-col    :initform 0 :accessor end-col)))

(defclass text-node (node)
  ((content :initarg :content :accessor content :initform "")
   (face    :initarg :face    :accessor face    :initform nil)))

(defclass separator (node)
  ((sep-char :initarg :char :accessor sep-char :initform #\─)))

(defclass field (node)
  ((content       :initarg :content       :accessor content       :initform "")
   (prefix-length :initarg :prefix-length :accessor prefix-length :initform 0)
   (face          :initarg :face          :accessor face          :initform nil)
   (input-start-line :initform 0 :accessor input-start-line)
   (input-start-col  :initform 0 :accessor input-start-col)
   (input-end-line   :initform 0 :accessor input-end-line)
   (input-end-col    :initform 0 :accessor input-end-col)))

(defclass vstack (node)
  ((children :initarg :children :accessor children :initform nil)
   (spacing  :initarg :spacing  :accessor spacing  :initform 0)))

(defclass hstack (node)
  ((children :initarg :children :accessor children :initform nil)
   (spacing  :initarg :spacing  :accessor spacing  :initform 1)))

(defclass box (node)
  ((child    :initarg :child    :accessor child    :initform nil)
   (width-of :initarg :width    :accessor width-of :initform 0)
   (align    :initarg :align    :accessor align    :initform :left)
   (pad-char :initarg :pad      :accessor pad-char :initform #\space)))

(defclass selectable (node)
  ((child             :initarg :child    :accessor child             :initform nil)
   (data              :initarg :data     :accessor data              :initform nil)
   (selectedp         :initarg :selected :accessor selectedp         :initform nil)
   (prefix-selected   :initarg :prefix-selected   :accessor prefix-selected   :initform "> ")
   (prefix-unselected :initarg :prefix-unselected :accessor prefix-unselected :initform "  ")))

(defclass action (node)
  ((child    :initarg :child    :accessor child    :initform nil)
   (callback :initarg :callback :accessor callback :initform nil)))

(defclass list-node (node)
  ((items       :initarg :items       :accessor items       :initform nil)
   (item-fn     :initarg :item-fn     :accessor item-fn     :initform nil)
   (max-visible :initarg :max-visible :accessor max-visible :initform nil)))

(defclass grid (node)
  ((cells      :initarg :cells      :accessor cells      :initform nil)
   (col-widths :initarg :col-widths :accessor col-widths :initform nil)))


;;;; Scroll helper

(defun scroll-to-selection (sel offset max-vis)
  (when (minusp sel)
    (return-from scroll-to-selection (max 0 offset)))
  (let ((o offset))
    (when (>= sel (+ o max-vis)) (setf o (1+ (- sel max-vis))))
    (when (< sel o)              (setf o sel))
    (max 0 o)))


;;;; Layout container

(defclass layout ()
  ((root        :initarg :root   :accessor layout-root        :initform nil)
   (buffer-name :initarg :buffer :accessor layout-buffer-name :initform nil)
   (state       :initarg :state  :accessor layout-state       :initform nil)
   (width       :initarg :width  :accessor layout-width       :initform 80)))


;;;; Rendering

(defclass render-ctx ()
  ((ctx-lines :initform nil :accessor ctx-lines)
   (current   :initform ""  :accessor current-line)
   (line-idx  :initform 0   :accessor line-idx)
   (col       :initform 0   :accessor col)))

(defun ctx-emit (ctx text)
  (setf (current-line ctx) (concatenate 'string (current-line ctx) text))
  (incf (col ctx) (length text)))

(defun ctx-emit-char (ctx char)
  (ctx-emit ctx (string char)))

(defun ctx-newline (ctx)
  (push (current-line ctx) (ctx-lines ctx))
  (setf (current-line ctx) "")
  (incf (line-idx ctx))
  (setf (col ctx) 0))

(defun ctx-position (ctx)
  (values (line-idx ctx) (col ctx)))

(defun ctx-finalize (ctx)
  (nreverse (cons (current-line ctx) (ctx-lines ctx))))

(defun render-layout (layout)
  (let ((ctx (make-instance 'render-ctx))
        (w (layout-width layout)))
    (render-node (layout-root layout) ctx w)
    (ctx-finalize ctx)))

(defun layout-lines (layout)
  (reduce (lambda (seq line) (fset:with-last seq line))
          (render-layout layout)
          :initial-value (fset:empty-seq)))

(defgeneric render-node (node ctx width))

(defmacro with-node-bounds (node ctx &body body)
  `(progn
     (multiple-value-bind (l c) (ctx-position ,ctx)
                          (setf (start-line ,node) l (start-col ,node) c))
     ,@body
     (multiple-value-bind (l c) (ctx-position ,ctx)
                          (setf (end-line ,node) l (end-col ,node) c))))

(defmethod render-node ((n text-node) ctx width)
           (declare (ignore width))
           (with-node-bounds n ctx
                             (when (plusp (length (content n)))
                               (ctx-emit ctx (content n)))))

(defmethod render-node ((n separator) ctx width)
           (with-node-bounds n ctx
                             (ctx-emit ctx (make-string width :initial-element (sep-char n)))))

(defmethod render-node ((n field) ctx width)
           (declare (ignore width))
           (with-node-bounds n ctx
                             (let* ((c (content n))
                                    (plen (prefix-length n)))
                               (when (plusp plen)
                                 (ctx-emit ctx (subseq c 0 (min plen (length c)))))
                               (multiple-value-bind (l col) (ctx-position ctx)
                                                    (setf (input-start-line n) l (input-start-col n) col))
                               (when (< plen (length c))
                                 (ctx-emit ctx (subseq c plen)))
                               (multiple-value-bind (l col) (ctx-position ctx)
                                                    (setf (input-end-line n) l (input-end-col n) col)))))

(defmethod render-node ((n vstack) ctx width)
           (with-node-bounds n ctx
                             (let ((sp (spacing n)))
                               (loop for (ch . rest) on (children n)
                                     do (setf (parent ch) n)
                                     (render-node ch ctx width)
                                     (when rest
                                       (ctx-newline ctx)
                                       (dotimes (_ sp) (ctx-newline ctx)))))))

(defmethod render-node ((n hstack) ctx width)
           (with-node-bounds n ctx
                             (let ((sp (spacing n))
                                   (used 0))
                               (loop for (ch . rest) on (children n)
                                     do (setf (parent ch) n)
                                     (let ((before (col ctx)))
                                       (render-node ch ctx (- width used))
                                       (incf used (- (col ctx) before)))
                                     (when rest
                                       (dotimes (_ sp)
                                         (ctx-emit-char ctx #\space)
                                         (incf used)))))))

(defmethod render-node ((n box) ctx width)
           (declare (ignore width))
           (with-node-bounds n ctx
                             (let* ((bw (width-of n))
                                    (ch (child n))
                                    (al (align n))
                                    (pc (pad-char n)))
                               (if ch
                                   (let* ((str (node-to-string ch bw))
                                          (len (length str))
                                          (padded (if (>= len bw)
                                                      (subseq str 0 bw)
                                                    (let ((pt (- bw len)))
                                                      (ecase al
                                                             (:left (concatenate 'string str (make-string pt :initial-element pc)))
                                                             (:right (concatenate 'string (make-string pt :initial-element pc) str))
                                                             (:center (let ((l (floor pt 2)) (r (ceiling pt 2)))
                                                                        (concatenate 'string
                                                                                     (make-string l :initial-element pc) str
                                                                                     (make-string r :initial-element pc)))))))))
                                     (ctx-emit ctx padded))
                                 (ctx-emit ctx (make-string bw :initial-element pc))))))

(defmethod render-node ((n selectable) ctx width)
           (with-node-bounds n ctx
                             (let* ((sel (selectedp n))
                                    (prefix (if sel (prefix-selected n) (prefix-unselected n)))
                                    (ch (child n)))
                               (ctx-emit ctx prefix)
                               (when ch
                                 (setf (parent ch) n)
                                 (render-node ch ctx (- width (length prefix)))))))

(defmethod render-node ((n action) ctx width)
           (with-node-bounds n ctx
                             (when (child n)
                               (setf (parent (child n)) n)
                               (render-node (child n) ctx width))))

(defmethod render-node ((n list-node) ctx width)
           (with-node-bounds n ctx
                             (let* ((is (items n))
                                    (mx (max-visible n))
                                    (fn (item-fn n))
                                    (vis (if mx (subseq is 0 (min mx (length is))) is)))
                               (loop for (item . rest) on vis
                                     for idx from 0
                                     for ch = (funcall fn item idx)
                                     do (setf (parent ch) n)
                                     (render-node ch ctx width)
                                     (when rest (ctx-newline ctx))))))

(defmethod render-node ((n grid) ctx width)
           (declare (ignore width))
           (with-node-bounds n ctx
                             (let ((cw (col-widths n)))
                               (loop for (row . more) on (cells n)
                                     do (loop for cell in row
                                              for w in cw
                                              do (setf (parent cell) n)
                                              (let ((c0 (col ctx)))
                                                (render-node cell ctx w)
                                                (let ((written (- (col ctx) c0)))
                                                  (cond
                                                   ((< written w)
                                                    (ctx-emit ctx (make-string (- w written) :initial-element #\space)))
                                                   ((> written w)
                                                    (let* ((ln (current-line ctx))
                                                           (keep (+ c0 w)))
                                                      (setf (current-line ctx) (subseq ln 0 (min keep (length ln))))
                                                      (setf (col ctx) keep)))))))
                                     (when more (ctx-newline ctx))))))

(defun node-to-string (n width)
  (when (and (typep n 'text-node) (null (face n)))
    (let ((s (content n)))
      (return-from node-to-string
                   (if (<= (length s) width) s (subseq s 0 width)))))
  (let ((ctx (make-instance 'render-ctx)))
    (render-node n ctx width)
    (current-line ctx)))


;;;; Registry — keyed by buffer name on the server.

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


;;;; Selection navigation

(defun collect-selectables (n)
  (let ((result nil))
    (labels ((walk (x)
                   (when x
                     (typecase x
                               (selectable (push x result))
                               (vstack (mapc #'walk (children x)))
                               (hstack (mapc #'walk (children x)))
                               (box (walk (child x)))
                               (grid (dolist (row (cells x)) (mapc #'walk row)))
                               (action (walk (child x)))
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
