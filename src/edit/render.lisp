(defpackage #:pine/edit/render
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:meter #:pine/run/meter)
                    (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:build #:pine/ui/build)
                    (#:grid #:pine/ui/grid) (#:face #:pine/ui/face)
                    (#:w #:pine/ui/widget) (#:layout #:pine/ui/layout)
                    (#:mode #:pine/text/mode) (#:doc #:pine/text/document)
                    (#:parser #:pine/text/ts/parser)
                    (#:window #:pine/edit/window) (#:prompt #:pine/edit/prompt)
                    (#:match #:pine/edit/matching))
  (:export #:shows #:frame #:window-tree #:document-tree #:modeline #:echo
           #:rows #:drawn-line #:drawn-col #:caret-col #:showing
           #:scroll-to-point #:indenting #:modelinep
           #:*cols* #:*lines* #:*font*))
(in-package #:pine/edit/render)

(defparameter +candidates+ 12)
(defvar *cols* 80)
(defvar *lines* 24)
(defvar *font* nil)
(defun drawn-line (text width)
  "TEXT as it is drawn: a tab takes you to the next stop rather than one cell.
Answers the text and the column each character landed in."
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

(defun %document-of (win)
  (let ((it (window:shows win)))
    (or (and (stringp it) (doc:named it))
        (and (typep it 'doc:document) it)
        (doc:current))))

(defun caret-col (document)
  "The column point is drawn in: where it lands once tabs are expanded."
  (multiple-value-bind (drawn where)
      (drawn-line (doc:line document (doc:at-line document))
                  (max 1 (or (doc:setting document :tab-width) 8)))
    (declare (ignore drawn))
    (drawn-col where (doc:at-col document))))

(defun %sideways (win)
  "Keep point in the window sideways too: a long line scrolls under it rather than
putting the caret where the text is not."
  (let* ((document (%document-of win))
         (col (caret-col document))
         (left (window:sideways win))
         (width (max 1 (window:cols win))))
    (setf (window:sideways win)
          (cond ((< col left) col)
                ((>= col (+ left width)) (1+ (- col width)))
                (t left)))))

(defun scroll-to-point (win)
  (let* ((document (%document-of win))
         (line (doc:at-line document))
         (from (window:scroll win))
         (height (max 1 (window:lines win))))
    (setf (window:scroll win)
          (cond ((< line from) line)
                ((>= line (+ from height)) (1+ (- line height)))
                (t from)))
    (%sideways win)))

(defun showing (document)
  "The band of lines a window is showing of DOCUMENT, a screen either side, so
paging lands on lines that were walked already."
  (let ((win (find document (window:windows) :key #'%document-of)))
    (when win
      (let* ((from (window:scroll win))
             (height (max 1 (window:lines win))))
        (cons (max 0 (- from height)) (+ from (* 2 height)))))))

(defun %overlays (document line)
  (remove line (doc:overlays document) :key #'first :test-not #'eql))

(defun %by-line (document)
  (let ((found (make-hash-table :test 'eql)))
    (dolist (run (parser:highlights document))
      (destructuring-bind (line from to face) run
        (push (list from to face) (gethash line found))))
    (dolist (run (doc:spans document) found)
      (destructuring-bind (line from to face) run
        (push (list from to face) (gethash line found))))))

(defun %face-at (by-line line col)
  (loop :for (from to face) :in (gethash line by-line)
        :when (and (>= col from) (< col to))
          :do (return face)))

(defun %region (document)
  (let ((mark (doc:mark document)))
    (when mark
      (destructuring-bind (line col) mark
        (let ((at-line (doc:at-line document)) (at-col (doc:at-col document)))
          (if (or (< line at-line) (and (= line at-line) (<= col at-col)))
              (list line col at-line at-col)
              (list at-line at-col line col)))))))

(defun %paint-region (g document from height width left)
  (let ((span (%region document))
        (bg (face:rgb (face:bg (face:named :selection)))))
    (when (and span bg)
      (destructuring-bind (start-line start-col end-line end-col) span
        (destructuring-bind (br bg bb) bg
          (loop :for line :from (max start-line from)
                  :to (min end-line (1- (+ from height)))
                :do (loop :for col :from (if (= line start-line) start-col 0)
                            :below (if (= line end-line) end-col (+ left width))
                          :when (>= col left)
                            :do (grid:put-bg g (- line from) (- col left)
                                             br bg bb))))))))

(defun %at-col (where drawn)
  (or (position drawn where) (max 0 (1- (length where)))))

(defun %caretp (win)
  (and (eq win (window:focused)) (not (prompt:askingp))))

(defgeneric shows (content win)
  (:documentation "The cells a window shows of what it holds. A document, a widget
tree and the name of a document are all things a window can hold; another kind is a
method somebody else writes, and nothing here has to know about it.")
  (:method (content win)
    (declare (ignore content))
    (build:cells (grid:rows (grid:make-grid (max 1 (window:cols win))
                                            (max 1 (window:lines win))))
                 :class "editor-view" :expand 1 :font *font*)))

(defmethod shows ((content string) win)
  (let ((document (doc:named content)))
    (if document (shows document win) (call-next-method))))

(defmethod shows ((content w:widget) win)
  "A widget tree draws to cells like a surface does, so a config can put one in a
window beside a document."
  (let* ((cols (max 1 (window:cols win)))
         (rows (max 1 (window:lines win)))
         (g (grid:make-grid cols rows)))
    (layout:with-pass
      (layout:dress content)
      (layout:measure content g cols rows)
      (layout:arrange content g 0 0 cols rows)
      (layout:paint content g))
    (build:cells (grid:rows g) :class "editor-view" :expand 1 :font *font*)))

(defmethod shows ((document doc:document) win)
  (node:reading document)
  (let* ((from (window:scroll win))
         (left (window:sideways win))
         (width (max 1 (window:cols win)))
         (height (max 1 (window:lines win)))
         (by-line (%by-line document))
         (g (grid:make-grid width height))
         (caret (%caretp win)))
    (loop :with tab := (max 1 (or (doc:setting document :tab-width) 8))
          :for line :from from :below (min (doc:line-count document) (+ from height))
          :for row :from 0
          :do (multiple-value-bind (drawn where)
                  (drawn-line (doc:line document line) tab)
                (loop :for col :from left :below (min (+ left width) (length drawn))
                      :do (grid:put g row (- col left) (char drawn col)
                                    (or (%face-at by-line line (%at-col where col))
                                        :default)))
                (let ((said (%overlays document line)))
                  (when said
                    (let ((at (min (1- width)
                                   (max 0 (- (+ 2 (length drawn)) left)))))
                      (loop :for (nil text face) :in said
                            :do (loop :for i :from 0
                                        :below (min (- width at) (length text))
                                      :do (grid:put g row (+ at i) (char text i)
                                                    (or face :comment)))))))))
    (%paint-region g document from height width left)
    (build:cells (grid:rows g)
                 :class "editor-view" :expand 1 :font *font*
                 :caret (when caret
                          (cons (min (1- height)
                                     (max 0 (- (doc:at-line document) from)))
                                (min (1- width)
                                     (max 0 (- (caret-col document) left))))))))

(defun document-tree (win)
  (shows (or (window:shows win) (doc:current)) win))

(defun modelinep (win)
  "Whether what this window holds has a modeline: a document says what line you are
on, and a widget tree has nothing of the sort to say."
  (let ((it (window:shows win)))
    (or (null it) (typep it 'doc:document)
        (and (stringp it) (doc:named it)))))

(defun modeline (win)
  (let* ((document (%document-of win))
         (width (max 1 (window:cols win)))
         (text (format nil " ~:[  ~;**~] ~a  ~a  L~d C~d"
                       (doc:modified document)
                       (node:name document)
                       (mode:type (doc:mode-of document))
                       (1+ (doc:at-line document))
                       (doc:at-col document)))
         (g (grid:make-grid width 1)))
    (loop :for col :from 0 :below width
          :do (grid:put g 0 col
                        (if (< col (length text)) (char text col) #\space)
                        :modeline))
    (build:cells (grid:rows g) :class "modeline" :font *font*)))

(defun window-tree (win)
  (scroll-to-point win)
  (if (modelinep win)
      (build:column :align :stretch :class "window" :expand 1
                    (document-tree win)
                    (modeline win))
      (build:column :align :stretch :class "window" :expand 1
                    (document-tree win))))

(defun %candidates ()
  (let ((p (prompt:asking)))
    (when p
      (let* ((found (prompt:matching p))
             (n (length found))
             (chosen (prompt:chosen p))
             (from (max 0 (min (- n +candidates+)
                               (- chosen (floor +candidates+ 2))))))
        (values (subseq found (min from n) (min (+ from +candidates+) n)) from)))))

(defun %candidate-rows (found from width)
  (let ((chosen (prompt:chosen (prompt:asking)))
        (g (grid:make-grid (max 1 width) (max 1 (length found)))))
    (loop :for each :in found
          :for row :from 0
          :for text := (match:shows each width)
          :for face := (if (= (+ row from) chosen)
                           :completion-selected
                           :completion)
          :do (loop :for col :from 0 :below width
                    :do (grid:put g row col
                                  (if (< col (length text)) (char text col) #\space)
                                  face)))
    (grid:rows g)))

(defun echo (width &optional said)
  (let* ((p (and (null said) (prompt:asking)))
         (question (if p (or (prompt:asked) (prompt:question p)) ""))
         (text (or said (prompt:showing)))
         (g (grid:make-grid (max 1 width) 1)))
    (loop :for col :from 0 :below (min width (length text))
          :do (grid:put g 0 col (char text col)
                        (if (< col (length question)) :prompt :echo)))
    (multiple-value-bind (found from) (and p (%candidates))
      (let ((rows (grid:rows g)))
        (build:cells (if found
                         (append (%candidate-rows found from width) rows)
                         rows)
                     :class "echo" :font *font*
                     :over (if found (length found) 0)
                     :caret (when p
                              (cons (if found (length found) 0)
                                    (min (1- width)
                                         (+ (length question)
                                            (doc:at-col (prompt:answering)))))))))))

(defun %frame (cols lines said)
  "The frame, and what it read: every window, the question standing and what was
last said. A surface follows what it read, so this is where the editor says what
moving means."
  (node:reading (window:root))
  (node:reading (tree:ensure nil "prompt"))
  (node:reading (tree:ensure nil "log"))
  (let* ((wins (window:windows))
         (weight (reduce #'+ wins :key #'window:weight :initial-value 0))
         (room (max 2 (1- lines))))
    (dolist (win wins) (node:reading win))
    (dolist (win wins)
      (setf (window:cols win) (max 1 cols)
            (window:lines win)
            (let ((share (max 2 (floor (* room (window:weight win))
                                       (max 1 weight)))))
              (max 1 (if (modelinep win) (1- share) share)))))
    (apply #'build:column :align :stretch :class "editor"
           (append (loop :for win :in wins
                         :for first := t :then nil
                         :append (if first
                                     (list (window-tree win))
                                     (list (build:rule :face :border-inactive)
                                           (window-tree win))))
                   (list (echo cols said))))))

(defun frame (&key (cols *cols*) (lines *lines*) said)
  (meter:timing (:frame) (%frame cols lines said)))

(defun rows (&key (cols 80) (lines 24) said)
  "The frame as rows of cells: what the screen blits, and what a test reads."
  (let ((tree (frame :cols cols :lines lines :said said))
        (g (grid:make-grid cols lines)))
    (layout:with-pass
      (layout:dress tree)
      (layout:measure tree g cols lines)
      (layout:arrange tree g 0 0 cols lines)
      (layout:paint tree g))
    (grid:rows g)))

(defun %without-a-parse (document line)
  (or (mode:indent (doc:mode-of document) document line)
      (and (plusp line) (doc:indent-of document (1- line)))
      0))

(defun indenting (document from to then)
  (let ((width (or (mode:setting (doc:mode-of document) :indent) 2)))
    (or (parser:indent document from to :width width :then then)
        (funcall then (loop :for line :from from :to to
                            :collect (cons line (%without-a-parse document line)))))))
