(in-package #:pine/edit)

(defparameter +candidates+ 12)
(defvar *cols* 80)
(defvar *lines* 24)
(defvar *font* nil)
(defun drawn-line (text width)
  (let ((out (make-string-output-stream))
        (at 0)
        (where (make-array (length text) :element-type 'fixnum)))
    (loop :for ch :across text
          :for i :from 0
          :do (setf (aref where i) at)
              (cond ((char= ch #\Tab)
                     (let ((to (* width (1+ (floor at width)))))
                       (dotimes (n (- to at)) (write-char #\Space out))
                       (setf at to)))
                    (t (write-char ch out) (incf at))))
    (values (get-output-stream-string out) where)))

(defun drawn-col (where col)
  (cond ((zerop (length where)) col)
        ((< col (length where)) (aref where col))
        (t (1+ (aref where (1- (length where)))))))

(defun %buffer-of (win)
  (let ((it (shows win)))
    (or (and (stringp it) (fs:at "/text" it))
        (and (typep it 'text:buffer) it)
        (text:current))))

(defun caret-col (buffer)
  (multiple-value-bind (drawn where)
      (drawn-line (text:line buffer (text:at-line buffer))
                  (max 1 (mode:says buffer :tab-width 8)))
    (declare (ignore drawn))
    (drawn-col where (text:at-col buffer))))

(defun %sideways (win)
  (let* ((buffer (%buffer-of win))
         (col (caret-col buffer))
         (left (sideways win))
         (width (max 1 (width win))))
    (setf (sideways win)
          (cond ((< col left) col)
                ((>= col (+ left width)) (1+ (- col width)))
                (t left)))))

(defun scroll-to-point (win)
  (let* ((buffer (%buffer-of win))
         (line (text:at-line buffer))
         (from (scrolled win))
         (height (max 1 (height win))))
    (setf (scrolled win)
          (cond ((< line from) line)
                ((>= line (+ from height)) (1+ (- line height)))
                (t from)))
    (%sideways win)))

(defmethod text:band ((buffer text:buffer))
  (let ((win (find buffer (panes) :key #'%buffer-of)))
    (when win
      (let* ((from (scrolled win))
             (height (max 1 (height win))))
        (cons (max 0 (- from height)) (+ from (* 2 height)))))))

(defun %overlays (buffer line)
  (remove line (text:overlays buffer) :key #'first :test-not #'eql))

(defun %by-line (buffer)
  (let ((found (make-hash-table :test 'eql)))
    (dolist (run (text:highlights buffer))
      (destructuring-bind (line from to face) run
        (push (list from to face) (gethash line found))))
    (dolist (run (text:spans buffer) found)
      (destructuring-bind (line from to face) run
        (push (list from to face) (gethash line found))))))

(defun %face-at (by-line line col)
  (loop :for (from to face) :in (gethash line by-line)
        :when (and (>= col from) (< col to))
          :do (return face)))

(defun %region (buffer)
  (let ((mark (text:mark buffer)))
    (when mark
      (destructuring-bind (line col) mark
        (let ((at-line (text:at-line buffer)) (at-col (text:at-col buffer)))
          (if (or (< line at-line) (and (= line at-line) (<= col at-col)))
              (list line col at-line at-col)
              (list at-line at-col line col)))))))

(defun %paint-region (g buffer from height width left)
  (let ((span (%region buffer))
        (bg (ui:unhex (ui:bg (ui:in-force :selection)))))
    (when (and span bg)
      (destructuring-bind (start-line start-col end-line end-col) span
        (destructuring-bind (br bg bb) bg
          (loop :for line :from (max start-line from)
                  :to (min end-line (1- (+ from height)))
                :do (loop :for col :from (if (= line start-line) start-col 0)
                            :below (if (= line end-line) end-col (+ left width))
                          :when (>= col left)
                            :do (ui:put-bg g (- line from) (- col left)
                                             br bg bb))))))))

(defun %at-col (where drawn)
  (or (position drawn where) (max 0 (1- (length where)))))

(defun %caretp (win)
  (and (eq win (focused)) (not (askingp))))

(defgeneric drawn (content win)
  (:method (content win)
    (declare (ignore content))
    (ui:cells (ui:by-row (ui:make-cell-grid (max 1 (width win))
                                            (max 1 (height win))))
                 :class "editor-view" :expand 1 :font *font*)))

(defmethod drawn ((content string) win)
  (let ((buffer (fs:at "/text" content)))
    (if buffer (drawn buffer win) (call-next-method))))

(defmethod drawn ((content ui:widget) win)
  (let* ((width (max 1 (width win)))
         (rows (max 1 (height win)))
         (g (ui:make-cell-grid width rows)))
    (ui:with-pass
      (ui:dress content)
      (ui:measure content g width rows)
      (ui:lay content g 0 0 width rows)
      (ui:paint content g))
    (ui:cells (ui:by-row g) :class "editor-view" :expand 1 :font *font*)))

(defmethod drawn ((buffer text:buffer) win)
  (fs:depend-on buffer)
  (let* ((from (scrolled win))
         (left (sideways win))
         (width (max 1 (width win)))
         (height (max 1 (height win)))
         (by-line (%by-line buffer))
         (g (ui:make-cell-grid width height))
         (caret (%caretp win)))
    (loop :with tab := (max 1 (mode:says buffer :tab-width 8))
          :for line :from from :below (min (text:line-count buffer) (+ from height))
          :for row :from 0
          :do (multiple-value-bind (drawn where)
                  (drawn-line (text:line buffer line) tab)
                (loop :for col :from left :below (min (+ left width) (length drawn))
                      :do (ui:put g row (- col left) (char drawn col)
                                    (or (%face-at by-line line (%at-col where col))
                                        :default)))
                (let ((said (%overlays buffer line)))
                  (when said
                    (let ((at (min (1- width)
                                   (max 0 (- (+ 2 (length drawn)) left)))))
                      (loop :for (nil text face) :in said
                            :do (loop :for i :from 0
                                        :below (min (- width at) (length text))
                                      :do (ui:put g row (+ at i) (char text i)
                                                    (or face :comment)))))))))
    (%paint-region g buffer from height width left)
    (ui:cells (ui:by-row g)
                 :class "editor-view" :expand 1 :font *font*
                 :caret (when caret
                          (cons (min (1- height)
                                     (max 0 (- (text:at-line buffer) from)))
                                (min (1- width)
                                     (max 0 (- (caret-col buffer) left))))))))

(defun buffer-tree (win)
  (drawn (or (shows win) (text:current)) win))

(defun modelinep (win)
  (let ((it (shows win)))
    (or (null it) (typep it 'text:buffer)
        (and (stringp it) (fs:at "/text" it)))))

(defun modeline (win)
  (let* ((buffer (%buffer-of win))
         (width (max 1 (width win)))
         (text (format nil " ~:[  ~;**~] ~a  ~a  L~d C~d"
                       (text:modified buffer)
                       (fs:name buffer)
                       (fs:name (text:mode-of buffer))
                       (1+ (text:at-line buffer))
                       (text:at-col buffer)))
         (g (ui:make-cell-grid width 1)))
    (loop :for col :from 0 :below width
          :do (ui:put g 0 col
                        (if (< col (length text)) (char text col) #\space)
                        :modeline))
    (ui:cells (ui:by-row g) :class "modeline" :font *font*)))

(defun pane-tree (win)
  (scroll-to-point win)
  (if (modelinep win)
      (ui:column :align :stretch :class "pane" :expand 1
                    (buffer-tree win)
                    (modeline win))
      (ui:column :align :stretch :class "pane" :expand 1
                    (buffer-tree win))))

(defun %candidates ()
  (let ((p (asking)))
    (when p
      (let* ((found (matching p))
             (n (length found))
             (chosen (chosen p))
             (from (max 0 (min (- n +candidates+)
                               (- chosen (floor +candidates+ 2))))))
        (values (subseq found (min from n) (min (+ from +candidates+) n)) from)))))

(defun %candidate-rows (found from width)
  (let ((chosen (chosen (asking)))
        (g (ui:make-cell-grid (max 1 width) (max 1 (length found)))))
    (loop :for each :in found
          :for row :from 0
          :for text := (as-row each width)
          :for face := (if (= (+ row from) chosen)
                           :completion-selected
                           :completion)
          :do (loop :for col :from 0 :below width
                    :do (ui:put g row col
                                  (if (< col (length text)) (char text col) #\space)
                                  face)))
    (ui:by-row g)))

(defun echo (width &optional said)
  (let* ((p (and (null said) (asking)))
         (question (if p (or (asked) (question p)) ""))
         (text (or said (showing)))
         (g (ui:make-cell-grid (max 1 width) 1)))
    (loop :for col :from 0 :below (min width (length text))
          :do (ui:put g 0 col (char text col)
                        (if (< col (length question)) :prompt :echo)))
    (multiple-value-bind (found from) (and p (%candidates))
      (let ((rows (ui:by-row g)))
        (ui:cells (if found
                         (append (%candidate-rows found from width) rows)
                         rows)
                     :class "echo" :font *font*
                     :over (if found (length found) 0)
                     :caret (when p
                              (cons (if found (length found) 0)
                                    (min (1- width)
                                         (+ (length question)
                                            (text:at-col (answering)))))))))))

(defun %frame (width lines said)
  (fs:depend-on (root))
  (fs:depend-on (%asking-node))
  (fs:depend-on (fs:at "/log"))
  (let* ((wins (panes))
         (weight (reduce #'+ wins :key #'weight :initial-value 0))
         (room (max 2 (1- lines))))
    (dolist (win wins) (fs:depend-on win))
    (dolist (win wins)
      (setf (width win) (max 1 width)
            (height win)
            (let ((share (max 2 (floor (* room (weight win))
                                       (max 1 weight)))))
              (max 1 (if (modelinep win) (1- share) share)))))
    (apply #'ui:column :align :stretch :class "editor"
           (append (loop :for win :in wins
                         :for first := t :then nil
                         :append (if first
                                     (list (pane-tree win))
                                     (list (ui:rule :face :border-inactive)
                                           (pane-tree win))))
                   (list (echo width said))))))

(defun frame (&key (cols *cols*) (lines *lines*) said)
  (meter:timing (:frame) (%frame cols lines said)))

(defun rows (&key (cols 80) (lines 24) said)
  (let ((tree (frame :cols cols :lines lines :said said))
        (g (ui:make-cell-grid cols lines)))
    (ui:with-pass
      (ui:dress tree)
      (ui:measure tree g cols lines)
      (ui:lay tree g 0 0 cols lines)
      (ui:paint tree g))
    (ui:by-row g)))

(defun %without-a-parse (buffer line)
  (or (mode:indent (text:mode-of buffer) buffer line)
      (and (plusp line) (text:indent-of buffer (1- line)))
      0))

(defun indenting (buffer from to then)
  (let ((width (mode:says (text:mode-of buffer) :indent 2)))
    (or (text:indent buffer from to :width width :then then)
        (funcall then (loop :for line :from from :to to
                            :collect (cons line (%without-a-parse buffer line)))))))

