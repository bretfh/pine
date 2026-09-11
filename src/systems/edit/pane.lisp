(in-package #:pine/edit)

(defvar *counter* 0)

(defclass pane (fs:mount)
  ((shows    :initarg :shows    :accessor shows    :initform nil)
   (scrolled   :initarg :scroll   :accessor scrolled   :initform 0)
   (sideways :initarg :sideways :accessor sideways :initform 0)
   (width     :initarg :cols     :accessor width     :initform 80)
   (height    :initarg :lines    :accessor height    :initform 24)
   (runs     :initarg :body     :accessor runs     :initform nil)
   (weight   :initarg :weight   :accessor weight   :initform 1)))

(defmethod print-object ((w pane) stream)
  (print-unreadable-object (w stream :type t)
    (format stream "~a~@[ ~a~]" (fs:name w)
            (let ((it (shows w)))
              (if (fs:nodep it) (fs:name it) it)))))

(defmethod fs:names ((w pane))
  '((:shows . "what it is showing, by name")))

(defmethod fs:read ((w pane) (name (eql :shows)))
  (let ((it (shows w)))
    (if (fs:nodep it) (fs:name it) it)))

(defmethod fs:write ((w pane) (name (eql :shows)) value)
  (show w value))

(fs:mount (lambda () (make-instance 'fs:mount :describes "the editor")) "/edit")
(fs:mount (lambda () (make-instance 'fs:mount :describes "every pane, as the screen is split"))
          "/edit/pane")

(defun root () (fs:at "/edit/pane"))

(defun make-pane (&key shows (into (root)) name)
  (let ((w (make-instance 'pane
                          :name (or name (format nil "~d" (sb-ext:atomic-update *counter* (lambda (old) (1+ old)))))
                          :shows shows
                          :describes "what one pane is showing")))
    (fs:mount w into)
    w))

(defun parts (of)
  (remove-if-not (lambda (n) (typep n 'pane)) (fs:children of)))

(defun splitp (w) (and (runs w) t))

(defun panes (&optional (of (root)))
  (if (and (typep of 'pane) (not (splitp of)))
      (list of)
      (loop :for part :in (parts of) :append (panes part))))

(defun focused (&optional (of (root)))
  (let ((said (let ((n (fs:at of "focused"))) (and n (fs:contents n)))))
    (or (and said (fs:at of said))
        (first (panes of)))))

(defun focus (w &optional (of (root)))
  (setf (fs:contents (fs:mount (make-instance 'fs:value :name "focused") of)) (fs:name w))
  w)

(defun show (w it)
  (let ((content (if (stringp it) (or (fs:at "/text" it) it) it)))
    (setf (shows w) content)
    (when (typep content 'text:buffer) (setf (text:current) content))
    (fs:touch w)
    w))

(defun follow (buffer)
  (let ((w (focused)))
    (when (and w (typep (shows w) '(or null text:buffer))
               (not (text:asidep buffer))
               (not (eq buffer (shows w))))
      (setf (shows w) buffer)
      (fs:touch w))
    w))

(defmethod text:showing :after ((buffer text:buffer))
  (follow buffer))

(defun split (w side)
  (let* ((runs (if (member side '(:beside :right :left)) :row :column))
         (a (make-pane :shows (shows w) :into w))
         (b (make-pane :shows (shows w) :into w)))
    (setf (runs w) runs (shows w) nil)
    (focus (if (member side '(:above :left)) a b))
    (values a b)))

(defun close-pane (w &optional (of (root)))
  (let ((up (fs:parent w)))
    (when (and up (typep up 'pane))
      (fs:detach up (fs:name w))
      (let ((left (parts up)))
        (when (= 1 (length left))
          (let ((only (first left)))
            (setf (shows up) (shows only)
                  (runs up) (runs only))
            (dolist (each (parts only))
              (fs:detach only (fs:name each))
              (fs:mount each up))
            (fs:detach up (fs:name only)))))
      (focus (or (first (panes of)) up) of))
    w))

(defun only (w &optional (of (root)))
  (dolist (part (parts of)) (fs:detach of (fs:name part)))
  (let ((fresh (make-pane :shows (shows w) :into of)))
    (focus fresh of)
    fresh))

(defun seed (buffer &optional (of (root)))
  (or (first (panes of))
      (let ((w (make-pane :shows buffer :into of)))
        (focus w of)
        w)))

(command:defcommand "split-pane-below" ()
    (:describes "two panes, one above the other" :on '(text "C-x 2"))
  (split (focused) :below)
  (mapcar #'fs:name (panes)))

(command:defcommand "split-pane-right" ()
    (:describes "two panes, side by side" :on '(text "C-x 3"))
  (split (focused) :beside)
  (mapcar #'fs:name (panes)))

(command:defcommand "delete-pane" ()
    (:describes "close this pane" :on '(text "C-x 0"))
  (close-pane (focused))
  (mapcar #'fs:name (panes)))

(command:defcommand "delete-other-panes" ()
    (:describes "this pane alone" :on '(text "C-x 1"))
  (only (focused))
  (mapcar #'fs:name (panes)))

(command:defcommand "other-pane" ()
    (:describes "move the keyboard to the next pane" :on '(text "C-x o"))
  (let* ((all (panes))
         (at (or (position (focused) all) 0)))
    (focus (nth (mod (1+ at) (length all)) all))
    (fs:name (focused))))

(defun %weighed (win to)
  (setf (weight win) to)
  (fs:touch win)
  to)

(command:defcommand "enlarge-pane" ()
    (:describes "give this pane more of the room" :on '(text "C-x ^"))
  (let ((win (focused)))
    (%weighed win (min 16 (1+ (weight win))))))

(command:defcommand "shrink-pane" ()
    (:describes "give this pane less of the room" :on '(text "C-x -"))
  (let ((win (focused)))
    (%weighed win (max 1 (1- (weight win))))))

(command:defcommand "balance-panes" ()
    (:describes "every pane the same size" :on '(text "C-x +"))
  (dolist (win (panes) t) (%weighed win 1)))

