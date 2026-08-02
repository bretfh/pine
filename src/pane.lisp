(defpackage #:pine.pane
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text)
                    (#:face #:pine.ui.face))
  (:export #:pane #:panep #:content #:contentp
           #:rows #:subject #:scroll #:hscroll #:showing
           #:window #:terminal #:modeline #:echo #:arrangement))

(in-package #:pine.pane)
(named-readtables:in-readtable pine.path:syntax)

;;;; A pane holds content, and this is what a content type says it looks like:
;;;; cell rows, at the size the pane was given, and where the caret sits in
;;;; them. Every pane in pine goes through here -- the editor's arrangement, a
;;;; config's (window /buf/scratch), a bar's inset view -- so nothing has to be
;;;; an editor to show a buffer.
;;;;
;;;; A pane is a path, and what it shows is read off that path: which buffer,
;;;; where it is scrolled, where point is, what the parser said about its
;;;; colours. Nothing is carried in an object, so a pane a config named before
;;;; any frontend existed renders the moment one asks.

(defun panep (x)
  "Whether X is a pane: a path, so its scroll and its size are places."
  (p:pathp x))

(deftype pane ()
  "A pane: a place on a surface that holds content. Its subject, its scroll and
the height it was last arranged at are leaves under it, so a pane a config named
and a pane in the arrangement are the same thing."
  '(satisfies panep))

(defun contentp (x)
  "Whether X is something a pane can hold."
  (or (pine.buf:bufp x) (typep x 'pine.ui.node:node)))

(deftype content ()
  "What a pane may hold: a buffer, by path or by name, or a widget tree that is
its own content. A content type pine does not ship answers ROWS and needs
nothing else."
  '(satisfies contentp))

(defun subject (at)
  "The buffer the pane at AT shows.

A window in the arrangement names one at its own BUF leaf. A path that holds
another path names what it holds, which is what /buf/current is. A buffer names
itself, and a bare name is the buffer it names."
  (let* ((at (if (stringp at) (pine.buf:at at) at))
         (named (ns:read (p:child at "buf")))
         (held (ns:held at)))
    (cond ((p:pathp named) named)
          ((stringp named) (pine.buf:at named))
          ((p:pathp held) held)
          (t at))))

(defun scroll (at)
  "The first line the pane at AT shows. A place, so where a pane is scrolled
outlives the image that scrolled it and a second frontend sees the same."
  (or (ns:read (p:child at "scroll")) 0))

(defun (setf scroll) (line at)
  (ns:write (p:child at "scroll") line)
  line)

(defun hscroll (at)
  (or (ns:read (p:child at "hscroll")) 0))

(defun (setf hscroll) (col at)
  (ns:write (p:child at "hscroll") col)
  col)

(defun showing (at height)
  "The line range the pane at AT shows, as [from to]. What a buffer bounds its
parse and its highlighting by."
  (let ((top (scroll at)))
    (fset:seq top (+ top (max 1 height)))))

;;;; Following point. A pane scrolls so point is visible, and says so at its own
;;;; path rather than only in the image that drew it.

(defun %follow (at line col height width)
  "Scroll the pane at AT so LINE and COL are visible, and answer (values top
left)."
  (let* ((top (scroll at))
         (left (hscroll at))
         (next-top (cond ((< line top) line)
                         ((>= line (+ top height)) (1+ (- line height)))
                         (t top)))
         (next-left (cond ((< col left) col)
                          ((>= col (+ left width)) (1+ (- col width)))
                          (t left))))
    (unless (= next-top top) (setf (scroll at) next-top))
    (unless (= next-left left) (setf (hscroll at) next-left))
    (values next-top next-left)))

;;;; Colour. The parser writes what it found at /buf/?name/face, so that is
;;;; where it is read: a run is (LINE START-COL END-COL FACE).

(defun %priority (face)
  (case face
    (:keyword        10)
    (:variable-param  9)
    (:function-name   8)
    (:function-call   8)
    (:builtin         7)
    (:constant        6)
    (:number          6)
    (:escape          6)
    (:string          5)
    (:comment         5)
    (:type            4)
    (:variable        1)
    (t                0)))

(defun %runs-by-row (runs top height)
  "RUNS as a table of pane row to the runs on it."
  (let ((table (make-hash-table)))
    (dolist (run runs table)
      (destructuring-bind (line start end name) run
        (when (and (>= line top) (< line (+ top height)))
          (push (list start end name) (gethash (- line top) table)))))))

(defun %faces-for-line (runs width forward)
  "A vector of WIDTH faces, the highest-priority one winning each column.

FORWARD maps a column of the line as it is held to the column it is shown at,
which is where a run's own columns have to be read: the parser counts the tab,
and the screen counts what the tab ran out to."
  (let ((faces (make-array width :initial-element nil))
        (won (make-array width :initial-element -1))
        (last (1- (length forward))))
    (dolist (run runs faces)
      (destructuring-bind (start end name) run
        (when name
          (let ((p (%priority name))
                (from (aref forward (max 0 (min start last))))
                (to (aref forward (max 0 (min end last)))))
            (loop :for c :from from :below (min to width)
                  :when (> p (aref won c))
                    :do (setf (aref faces c) name (aref won c) p))))))))

;;;; Tabs. A tab is one character and some number of columns, so a line is shown
;;;; as what its tabs ran out to, and everything that counts columns -- point,
;;;; the region, a highlight run -- is read through the same two maps.

(defun %tab-width (name)
  (max 1 (or (ns:read (pine.buf:at name :tab-width)) 8)))

(defun %shown-line (text tab)
  "TEXT with its tabs run out to the next stop, and the two column maps:
(values SHOWN FORWARD BACK), FORWARD from a held column to a shown one and BACK
the other way."
  (let ((shown (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (forward (make-array (1+ (length text)) :element-type 'fixnum
                                                :initial-element 0))
        (back (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)))
    (dotimes (i (length text))
      (setf (aref forward i) (fill-pointer shown))
      (let ((ch (char text i)))
        (if (char= ch #\Tab)
            (dotimes (n (- tab (mod (fill-pointer shown) tab)))
              (vector-push-extend #\Space shown)
              (vector-push-extend i back))
            (progn (vector-push-extend ch shown)
                   (vector-push-extend i back)))))
    (setf (aref forward (length text)) (fill-pointer shown))
    (values (coerce shown 'simple-string) forward back)))

(defun %shown-col (text tab col)
  "Where COL of TEXT is shown."
  (multiple-value-bind (shown forward) (%shown-line text tab)
    (declare (ignore shown))
    (aref forward (max 0 (min col (1- (length forward)))))))

;;;; The region, which is a place like everything else.

(defun %region (name line col)
  "The region between the mark and point, normalized, or NIL."
  (let* ((mark (ns:read (pine.buf:at name :mark)))
         (ml (and (fset:seq? mark) (fset:lookup mark 0)))
         (mc (and (fset:seq? mark) (fset:lookup mark 1))))
    (when (and ml mc)
      (if (or (< ml line) (and (= ml line) (<= mc col)))
          (list ml mc line col)
          (list line col ml mc)))))

(defun %in-region (region line col)
  (destructuring-bind (sl sc el ec) region
    (cond ((or (< line sl) (> line el)) nil)
          ((= sl el) (and (>= col sc) (< col ec)))
          ((= line sl) (>= col sc))
          ((= line el) (< col ec))
          (t t))))

(defun %overlay-style (class)
  "(values fg attr) for an overlay CLASS through the stylesheet."
  (let* ((st (pine.ui.style:resolve (list (pine.ui.cells:class-names class))))
         (fg (pine.ui.style:st-fg st)))
    (values (if fg
                (mapcar (lambda (c) (round (* 255 c))) fg)
                (face:face-fg :comment))
            (if (pine.ui.style:st-bold st) 1 0))))

;;;; Painting a buffer's text.

(defun %put (r row col ch fg bg attr)
  (pine.ui.raster:raster-put-rgb r row col ch
                                 (first fg) (second fg) (third fg)
                                 (if bg (first bg) -1)
                                 (if bg (second bg) -1)
                                 (if bg (third bg) -1)
                                 attr))

(defun %put-string (r row col text fg &optional bg (attr 0))
  (loop :for i :from 0 :below (length text)
        :do (%put r row (+ col i) (char text i) fg bg attr)))

(defun %text-rows (name at width height)
  "NAME's text as the pane at AT shows it: the visible lines, coloured by what
the parser said, the region behind them, and any overlay after its line."
  (multiple-value-bind (line col) (pine.buf:point name)
    (let* ((lines (or (ns:held (pine.buf:at name :text)) (fset:seq "")))
           (count (fset:size lines))
           (tab (%tab-width name))
           (caret (%shown-col (if (< line count) (fset:@ lines line) "") tab col)))
      (multiple-value-bind (top left) (%follow at line caret height width)
        (let* ((runs (ns:read (pine.buf:at name :face)))
               (by-row (%runs-by-row (if (listp runs) runs nil) top height))
               (region (%region name line col))
               (overlays (ns:read (pine.buf:at name :overlays)))
               (default (face:face-fg :default))
               (selection (face:face-bg :selection))
               (r (pine.ui.raster:make-raster width height)))
          (loop :for row :from 0 :below height
                :for at-line := (+ top row)
                :while (< at-line count)
                :do (multiple-value-bind (text forward back)
                        (%shown-line (fset:@ lines at-line) tab)
                      (let ((faces (%faces-for-line (gethash row by-row) width
                                                    forward)))
                        (loop :for i :from 0 :below width
                              :for source := (+ i left)
                              :while (< source (length text))
                              :for name* := (aref faces (min source (1- width)))
                              :for fg := (if name* (face:face-fg name*) default)
                              :for attr := (if name*
                                               (face:face-attr-bits
                                                (face:find-face name*))
                                               0)
                              :for bg := (when (and region
                                                    (%in-region region at-line
                                                                (aref back source)))
                                           selection)
                              :do (%put r row i (char text source) fg bg attr)))
                      (let ((overlay (and (fset:map? overlays)
                                          (fset:lookup overlays at-line))))
                        (when overlay
                          (destructuring-bind (text* class) overlay
                            (multiple-value-bind (fg attr) (%overlay-style class)
                              (let* ((col0 (+ (max 0 (- (length text) left)) 2))
                                     (room (- width col0)))
                                (when (plusp room)
                                  (%put-string r row col0
                                               (subseq text* 0 (min (length text*) room))
                                               fg nil attr)))))))))
          (values (pine.ui.cells:rows-of r)
                  (max 0 (min (- line top) (1- height)))
                  (max 0 (- caret left))))))))

;;;; A terminal's grid, which is the emulator's and not the tree's.

(defun %term-rgb (plist key default)
  (let ((c (getf plist key)))
    (cond ((null c) default)
          ((integerp c) (coerce (pine.vt:color-index-to-rgb c) 'list))
          ((and (vectorp c) (= 3 (length c))) (coerce c 'list))
          ((and (listp c) (= 3 (length c))) c)
          (t default))))

(defun %terminal-rows (tobj width height)
  (let* ((term (pine.term:terminal-term tobj))
         (r (pine.ui.raster:make-raster width height))
         (lines (min (pine.vt:term-height term) height))
         (cols (min (pine.vt:term-width term) width))
         (default (face:face-fg :default)))
    (dotimes (y lines)
      (multiple-value-bind (chars faces) (pine.vt:term-render-line term y)
        (let ((cur nil))
          (dotimes (x cols)
            (let ((change (assoc x faces)))
              (when change (setf cur (second change))))
            (%put r y x (char chars x)
                  (%term-rgb cur :fg default) (%term-rgb cur :bg nil) 0)))))
    (values (pine.ui.cells:rows-of r)
            (max 0 (min (pine.vt:term-cursor-y term) (1- (max 1 lines))))
            (max 0 (min (pine.vt:term-cursor-x term) (1- (max 1 cols)))))))

;;;; What a pane shows, by what it holds.

(defgeneric rows (content width height)
  (:documentation "CONTENT as (values ROWS CROW CCOL): the cell rows a pane
WIDTH by HEIGHT shows of it, and where the caret sits in them, or -1 -1.

One method per kind of content there is. A content type pine does not ship is a
method someone else writes, and a pane holding it needs nothing else.")
  (:method (content width height)
    (declare (ignore content width height))
    nil))

(defmethod rows ((content null) width height)
  (declare (ignore width height))
  nil)

(defmethod rows ((content string) width height)
  "A name is the buffer it names."
  (rows (pine.buf:at content) width height))

(defmethod rows ((content pine.ui.node:node) width height)
  "A widget tree is its own content: it renders to cells."
  (values (pine.ui.cells:render content width :height height) -1 -1))

;;;; Building panes. A pane is a leaf that names its content; the arrangement is
;;;; the panes /win says there are, stacked the way it says, with a divider
;;;; between them. Both are here because both are panes.

(defun font-px ()
  "The cell font size every pane is laid out and painted at.

One theme metric, read by both sides: the daemon arranges a pane in cells this
size, and the frontend measures its own cell grid from the same number, so rows
laid out for N columns land in N columns."
  (face:metric :font-px 15))

(defun %leaf (at &rest props)
  (apply #'pine.ui.build:cells nil :as 'pine.ui.node:buffer-view :of at
         :font-px (font-px) props))

(defun %node (at &rest props)
  "The pane at AT, or the stack its parts make."
  (if (pine.win:stack-p at)
      (let* ((row (eq :row (pine.win:runs-of at)))
             (parts (mapcar #'%node (pine.win:parts at)))
             (kids (loop :for part :in parts
                         :for first := t :then nil
                         :append (if first
                                     (list part)
                                     (list (pine.ui.build:rule
                                            :vertical row :face :border-inactive)
                                           part)))))
        (apply (if row #'pine.ui.build:row #'pine.ui.build:column)
               :align :stretch :expand 1 (append props kids)))
      (apply #'%leaf at :expand (max 1 (pine.win:weight-of at)) props)))

(defun arrangement (&optional on &rest props)
  "Every pane /win says there is, as one node. ON is what the first pane starts
on when there is no arrangement to come back to.

The panes are read off /win, so this is an expression over it: a split, a close
or another buffer shown moves the path, and what read it is built again."
  (let ((parts (pine.win:parts /win)))
    (cond ((null parts)
           (apply #'%node
                  (pine.win:seed (if (stringp on) (pine.buf:at on)
                                     (or on (pine.buf:at "scratch"))))
                  props))
          ((null (rest parts)) (apply #'%node (first parts) props))
          (t (apply #'pine.ui.build:column :align :stretch :expand 1
                    (mapcar #'%node parts))))))

(defun window (&optional at &rest props)
  "A pane. AT is the pane when it is one, and otherwise the content the
arrangement starts on.

A pane is a place, not a name it was built with: which buffer it shows is the
BUF leaf under it, so opening a file moves that leaf and the pane follows. A
buffer is content, so naming one says where the arrangement begins rather than
what it is stuck on. A pane of its own, beside the arrangement, is a path with
a BUF written under it."
  (if (and (p:pathp at) (not (p:prefixp /buf at)))
      (apply #'%leaf at props)
      (apply #'arrangement at props)))

(defun terminal (at &rest props)
  "A pane on a terminal buffer. A terminal is a buffer holding one, so this is
a pane like any other; the name is here because a config reads better for it."
  (apply #'window at props))

(defun modeline (&optional at)
  "The mode line of the pane AT, or of the one with the keyboard."
  (pine.ui.build:cells nil :as 'pine.ui.node:modeline-view :of at
                            :font-px (font-px)))

(defun echo ()
  "The echo line."
  (pine.ui.build:cells nil :as 'pine.ui.node:echo-view :base 1
                            :font-px (font-px)))

(defmethod rows ((content p:path) width height)
  "A path names a buffer, or a pane that names one. What the buffer holds says
how it shows: a terminal is its emulator's grid, a widget tree is that tree,
and anything else is its text."
  (let* ((at content)
         (buffer (subject at))
         (name (p:leaf buffer))
         (width (max 1 width))
         (height (max 1 height)))
    (let ((tobj (pine.term:terminal-for-buffer name)))
      (cond
        (tobj (%terminal-rows tobj width height))
        ((ns:read (pine.buf:at name :view))
         (let ((built (pine.view:rendered name)))
           (values (nthcdr (scroll at) built) -1 -1)))
        (t
         (pine.buf:showing name (showing at height))
         (%text-rows name at width height))))))
