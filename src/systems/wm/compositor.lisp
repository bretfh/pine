(defpackage #:pine/wm/compositor
  (:use #:cl)
  (:local-nicknames (#:fs #:pine/fs))
  (:export #:compositor #:parts #:workspaces #:windows #:titled #:focused #:focus
           #:outputs #:ids #:rect #:hidden #:hide #:show
           #:step-window #:close-window #:overview #:leave #:split #:act #:verbs))
(in-package #:pine/wm/compositor)

(defclass compositor (fs:mount)
  ((parts :initform nil :accessor parts))
  (:documentation "The compositor this session is under. PARTS are what it answers
for, put under /wm: its outputs, its windows, and what it will take.

A class, so pine being the compositor and pine talking to one are the same protocol
with two subclasses under it. What is under here is the machine's window resources
and nothing about how they are arranged: that is a system you load."))

(defgeneric workspaces (compositor)
  (:documentation "Every workspace, as maps. Grouping is policy: a compositor that
has none, and a pine with no system that keeps them, answers nothing.")
  (:method ((c null)) nil)
  (:method ((c compositor)) nil))

(defgeneric outputs (compositor)
  (:documentation "Every output, as plists: :name, :position, :size, and the :area
left after the bars have taken their strip.")
  (:method ((c null)) nil)
  (:method ((c compositor)) nil))

(defgeneric windows (compositor)
  (:documentation "Every window, as maps.")
  (:method ((c null)) nil))

(defgeneric ids (compositor)
  (:documentation "Every window's id, in order, as the compositor names them. A
placement is written in these, so what goes in comes back out unchanged.")
  (:method ((c null)) nil)
  (:method ((c compositor))
    (mapcar (lambda (w) (gethash "id" w)) (windows c))))

(defgeneric titled (compositor id)
  (:documentation "What a window is called.")
  (:method ((c null) id) (declare (ignore id)) nil))

(defgeneric rect (compositor id)
  (:documentation "Where a window is now, as (X Y WIDTH HEIGHT).")
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric hidden (compositor id)
  (:documentation "Whether a window is off the screen rather than on it.")
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric hide (compositor id)
  (:documentation "Take a window off the screen without closing it.")
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric show (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric focused (compositor)
  (:documentation "Which window has the keyboard.")
  (:method ((c null)) nil))

(defgeneric focus (compositor id)
  (:documentation "Give the keyboard to a window.")
  (:method ((c null) id) (declare (ignore id)) nil))

(defgeneric act (compositor verb &rest arguments)
  (:documentation "Do one of the things this compositor takes.")
  (:method ((c null) verb &rest arguments)
    (declare (ignore verb arguments))
    nil))

(defgeneric verbs (compositor)
  (:documentation "What this compositor will take.")
  (:method ((c null)) nil))

(defgeneric step-window (compositor by)
  (:documentation "The window after this one, or before it.")
  (:method ((c null) by) (declare (ignore by)) nil)
  (:method (c by)
    (let* ((all (mapcar (lambda (w) (princ-to-string (gethash "id" w ""))) (windows c)))
           (at (position (focused c) all :test #'equal)))
      (when (and all at)
        (focus c (nth (mod (+ at by) (length all)) all))))))

(defgeneric close-window (compositor)
  (:method ((c null)) nil)
  (:method (c) (act c "close")))

(defgeneric overview (compositor)
  (:method ((c null)) nil)
  (:method (c) (act c "overview")))

(defgeneric leave (compositor)
  (:documentation "End the session.")
  (:method ((c null)) nil)
  (:method (c) (act c "exit")))

(defgeneric split (compositor side)
  (:method ((c null) side) (declare (ignore side)) nil)
  (:method (c side)
    (act c (if (member side '(:beside :right :row)) "expel" "consume"))))

(defun %named (things key name)
  (find-if (lambda (thing)
             (equal (princ-to-string name)
                    (princ-to-string (gethash key thing ""))))
           things))

(defun %field (said name)
  (getf said (intern (string-upcase name) :keyword)))

(defun %output-said (c name)
  (find (princ-to-string name) (outputs c)
        :key (lambda (o) (princ-to-string (getf o :name)))
        :test #'equal))

(defun %window-said (c id)
  (let ((w (%named (windows c) "id" id)))
    (when w
      (list :title (gethash "title" w)
            :app (gethash "app_id" w)
            :rect (rect c id)
            :hidden (hidden c id)
            :focused (and (gethash "is_focused" w) t)))))

(defclass window (fs:mount) ()
  (:documentation "One window at /wm/windows/<id>: what it says about itself.
Writing HIDDEN takes it off the screen or puts it back, and writing FOCUSED gives
it the keyboard; the rest is what the compositor says, and says alone."))

(defmethod fs:livep ((w window) &optional name) (declare (ignore name)) t)

(defun %own (w) (%window-said (fs:of w) (fs:name w)))

(defmethod fs:read ((w window) (name (eql :title))) (%field (%own w) "title"))
(defmethod fs:read ((w window) (name (eql :app))) (%field (%own w) "app"))
(defmethod fs:read ((w window) (name (eql :rect))) (%field (%own w) "rect"))
(defmethod fs:read ((w window) (name (eql :hidden))) (%field (%own w) "hidden"))
(defmethod fs:read ((w window) (name (eql :focused))) (%field (%own w) "focused"))

(defmethod fs:write ((w window) (name (eql :hidden)) value)
  (if value (hide (fs:of w) (fs:name w)) (show (fs:of w) (fs:name w))))

(defmethod fs:write ((w window) (name (eql :focused)) value)
  (when value (focus (fs:of w) (fs:name w))))

(defclass windows (fs:mount) ()
  (:documentation "/wm/windows: every window there is."))

(defmethod fs:livep ((d windows) &optional name) (declare (ignore name)) t)

(defmethod fs:entry ((d windows) id)
  (let ((id (princ-to-string id)))
    (when (%named (windows (fs:of d)) "id" id)
      (fs:child d id (lambda () (make-instance 'window :name id :of (fs:of d) :parent d))))))

(defmethod fs:entries ((d windows))
  (remove nil (mapcar (lambda (id) (fs:entry d id)) (ids (fs:of d)))))

(defclass output (fs:mount) ()
  (:documentation "One screen at /wm/outputs/<name>: where it is, how big, and
the area left after the bars have taken their strip."))

(defmethod fs:livep ((o output) &optional name) (declare (ignore name)) t)

(defmethod fs:read ((o output) (name (eql :position)))
  (%field (%output-said (fs:of o) (fs:name o)) "position"))
(defmethod fs:read ((o output) (name (eql :size)))
  (%field (%output-said (fs:of o) (fs:name o)) "size"))
(defmethod fs:read ((o output) (name (eql :area)))
  (%field (%output-said (fs:of o) (fs:name o)) "area"))

(defclass outputs (fs:mount) ()
  (:documentation "/wm/outputs: every screen, and what is left after the bars."))

(defmethod fs:livep ((d outputs) &optional name) (declare (ignore name)) t)

(defmethod fs:entry ((d outputs) name)
  (let ((name (princ-to-string name)))
    (when (%output-said (fs:of d) name)
      (fs:child d name (lambda () (make-instance 'output :name name :of (fs:of d) :parent d))))))

(defmethod fs:entries ((d outputs))
  (remove nil (mapcar (lambda (o) (fs:entry d (getf o :name))) (outputs (fs:of d)))))

(defclass workspace (fs:derived) ()
  (:documentation "One workspace at /wm/workspaces/<idx>: whether it is focused or
urgent, and its window. Writing anything here goes to it."))

(defmethod fs:livep ((n workspace) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n workspace))
  (let ((w (%named (workspaces (fs:of n)) "idx" (fs:name n))))
    (when w
      (list :focused (and (gethash "is_focused" w) t)
            :urgent (and (gethash "is_urgent" w) t)
            :windows (gethash "active_window_id" w)))))

(defmethod fs:takes ((n workspace) value)
  (when value (act (fs:of n) "workspace" (fs:name n))))

(defclass workspaces (fs:mount) ()
  (:documentation "/wm/workspaces: how this compositor groups its windows, where
it does."))

(defmethod fs:livep ((d workspaces) &optional name) (declare (ignore name)) t)

(defmethod fs:entry ((d workspaces) idx)
  (let ((idx (princ-to-string idx)))
    (when (%named (workspaces (fs:of d)) "idx" idx)
      (fs:child d idx (lambda () (make-instance 'workspace :name idx :of (fs:of d) :parent d))))))

(defmethod fs:entries ((d workspaces))
  (remove nil (mapcar (lambda (w) (fs:entry d (gethash "idx" w))) (workspaces (fs:of d)))))

(defclass focus (fs:derived) ()
  (:documentation "/wm/focused: which window has the keyboard. Writing an id gives
it the keyboard."))

(defmethod fs:livep ((n focus) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n focus)) (focused (fs:of n)))

(defmethod fs:takes ((n focus) value) (when value (focus (fs:of n) value)))

(defclass verb (fs:derived) ()
  (:documentation "One thing the compositor will do, at /wm/<verb>: writing
anything here does it."))

(defmethod fs:livep ((n verb) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n verb)) (fs:name n))

(defmethod fs:takes ((n verb) value)
  (declare (ignore value))
  (act (fs:of n) (fs:name n)))

(defmethod initialize-instance :after ((c compositor) &key)
  (setf (parts c)
        (list* (make-instance 'outputs :name "outputs" :of c
                              :describes "every screen, and what is left after the bars")
               (make-instance 'workspaces :name "workspaces" :of c
                              :describes "how this compositor groups them, where it does")
               (make-instance 'windows :name "windows" :of c
                              :describes "every window there is")
               (make-instance 'focus :name "focused" :of c
                              :describes "which window has the keyboard")
               (mapcar (lambda (name) (make-instance 'verb :name name :of c)) (verbs c)))))


