(in-package #:pine/edit)

(defvar *counter* 0)

(defclass window (node:node)
  ((shows    :initarg :shows    :accessor shows    :initform nil)
   (scrolled   :initarg :scroll   :accessor scrolled   :initform 0)
   (sideways :initarg :sideways :accessor sideways :initform 0)
   (across     :initarg :cols     :accessor across     :initform 80)
   (down    :initarg :lines    :accessor down    :initform 24)
   (runs     :initarg :runs     :accessor runs     :initform nil)
   (weight   :initarg :weight   :accessor weight   :initform 1))
  (:documentation "A view onto something. What a window holds is only what the
render has a method for, so a widget tree beside a document costs nothing here.

RUNS says a window was split: :row or :column, and its parts are the windows it
was split into."))

(defmethod print-object ((w window) stream)
  (print-unreadable-object (w stream :type t)
    (format stream "~a~@[ ~a~]" (node:name w)
            (let ((it (shows w)))
              (if (node:nodep it) (node:name it) it)))))

(defmethod node:contents ((w window))
  (let ((it (shows w)))
    (if (node:nodep it) (node:name it) it)))

(defmethod (setf node:contents) (value (w window))
  (show w value)
  value)

(defun root () (tree:ensure "/window"))

(defun make-window (&key shows (into (root)) name)
  (let ((w (make-instance 'window
                          :name (or name (format nil "~d" (d:swap *counter* #'1+)))
                          :shows shows
                          :describes "what one window is showing")))
    (node:attach w into)
    w))

(defun parts (of)
  (remove-if-not (lambda (n) (typep n 'window)) (node:nodes of)))

(defun splitp (w) (and (runs w) t))

(defun windows (&optional (of (root)))
  (if (and (typep of 'window) (not (splitp of)))
      (list of)
      (loop :for part :in (parts of) :append (windows part))))

(defun named (name &optional (of (root)))
  (find (princ-to-string name) (windows of) :key #'node:name :test #'equal))

(defun focused (&optional (of (root)))
  (let ((said (node:contents (tree:ensure of "focused"))))
    (or (and said (named said of))
        (first (windows of)))))

(defun focus (w &optional (of (root)))
  (setf (node:contents (tree:ensure of "focused")) (node:name w))
  w)

(defun show (w it)
  "Put something in a window: a document, the name of one, or a widget tree."
  (let ((content (if (stringp it) (or (text:named it) it) it)))
    (setf (shows w) content)
    (when (typep content 'text:document) (setf (text:current) content))
    (node:moved w)
    w))

(defun follow (document)
  "The focused window follows what became current, unless it is showing something
else or the document keeps to itself."
  (let ((w (focused)))
    (when (and w (typep (shows w) '(or null text:document))
               (not (text:asidep document))
               (not (eq document (shows w))))
      (setf (shows w) document)
      (node:moved w))
    w))

(defmethod text:showing :after ((document text:document))
  "What became current is what the focused window shows. Said here, where the
windows are, rather than by whoever sets the current document."
  (follow document))

(defun split (w side)
  (let* ((runs (if (member side '(:beside :right :left)) :row :column))
         (a (make-window :shows (shows w) :into w))
         (b (make-window :shows (shows w) :into w)))
    (setf (runs w) runs (shows w) nil)
    (focus (if (member side '(:above :left)) a b))
    (values a b)))

(defun close-window (w &optional (of (root)))
  "Close W, and let what is left take its place.

What the one left over holds comes up with it, and so does what is under it. Only
the flag saying it was split came up before, so closing the sibling of a split
left a window claiming to run in a column with nothing in it -- and everything
that had been in that column was still hanging off the one just taken away, where
nothing could reach it."
  (let ((up (node:parent w)))
    (when (and up (typep up 'window))
      (node:detach up (node:name w))
      (let ((left (parts up)))
        (when (= 1 (length left))
          (let ((only (first left)))
            (setf (shows up) (shows only)
                  (runs up) (runs only))
            (dolist (each (parts only))
              (node:detach only (node:name each))
              (node:attach each up))
            (node:detach up (node:name only)))))
      (focus (or (first (windows of)) up) of))
    w))

(defun only (w &optional (of (root)))
  (dolist (part (parts of)) (node:detach of (node:name part)))
  (let ((fresh (make-window :shows (shows w) :into of)))
    (focus fresh of)
    fresh))

(defun seed (document &optional (of (root)))
  (or (first (windows of))
      (let ((w (make-window :shows document :into of)))
        (focus w of)
        w)))

(command:defcommand "split-window-below" ()
    (:describes "two windows, one above the other" :on '(text "C-x 2"))
  (split (focused) :below)
  (mapcar #'node:name (windows)))

(command:defcommand "split-window-right" ()
    (:describes "two windows, side by side" :on '(text "C-x 3"))
  (split (focused) :beside)
  (mapcar #'node:name (windows)))

(command:defcommand "delete-window" ()
    (:describes "close this window" :on '(text "C-x 0"))
  (close-window (focused))
  (mapcar #'node:name (windows)))

(command:defcommand "delete-other-windows" ()
    (:describes "this window alone" :on '(text "C-x 1"))
  (only (focused))
  (mapcar #'node:name (windows)))

(command:defcommand "other-window" ()
    (:describes "move the keyboard to the next window" :on '(text "C-x o"))
  (let* ((all (windows))
         (at (or (position (focused) all) 0)))
    (focus (nth (mod (1+ at) (length all)) all))
    (node:name (focused))))

(defun %weighed (win to)
  "Give WIN this much of the room, and say it moved. What lays the windows out is
worked out from what it read, and how big this one is is one of the things it
read: set without a word, the room changed and nothing drew it again."
  (setf (weight win) to)
  (node:moved win)
  to)

(command:defcommand "enlarge-window" ()
    (:describes "give this window more of the room" :on '(text "C-x ^"))
  (let ((win (focused)))
    (%weighed win (min 16 (1+ (weight win))))))

(command:defcommand "shrink-window" ()
    (:describes "give this window less of the room" :on '(text "C-x -"))
  (let ((win (focused)))
    (%weighed win (max 1 (1- (weight win))))))

(command:defcommand "balance-windows" ()
    (:describes "every window the same size" :on '(text "C-x +"))
  (dolist (win (windows) t) (%weighed win 1)))

