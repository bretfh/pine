(defpackage #:pine/wm
  (:use #:cl #:pine)
  (:shadowing-import-from #:pine #:read #:write #:map #:set)
  (:shadow #:leave)
  (:local-nicknames (#:fs #:pine/fs) (#:d #:pine/data)
                    (#:job #:pine/run/job) (#:module #:pine/run/module)
                    (#:command #:pine/run/command) (#:sh #:pine/host/shell)
                    (#:fault #:pine/run/fault) (#:wkeys #:pine/wm/keys))
  (:export
   #:current
   #:compositor #:workspaces #:windows #:titled #:focused #:focus
   #:outputs #:ids #:rect #:hidden #:hide #:show
   #:step-window #:close-window #:overview #:split #:act #:verbs
   #:managed #:niri
   #:layout #:tall #:wide #:full #:stacked #:arrange #:layouts
   #:area #:placed #:id-of #:x-of #:y-of #:width-of #:height-of #:clip-of #:stack-of))
(in-package #:pine/wm)

(defclass compositor (fs:mount)
  ((parts :initform nil :accessor parts)))

(defgeneric workspaces (compositor)
  (:method ((c null)) nil)
  (:method ((c compositor)) nil))

(defgeneric outputs (compositor)
  (:method ((c null)) nil)
  (:method ((c compositor)) nil))

(defgeneric windows (compositor)
  (:method ((c null)) nil))

(defgeneric ids (compositor)
  (:method ((c null)) nil)
  (:method ((c compositor))
    (mapcar (lambda (w) (gethash "id" w)) (windows c))))

(defgeneric titled (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil))

(defgeneric rect (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric hidden (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric hide (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric show (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil)
  (:method ((c compositor) id) (declare (ignore id)) nil))

(defgeneric focused (compositor)
  (:method ((c null)) nil))

(defgeneric focus (compositor id)
  (:method ((c null) id) (declare (ignore id)) nil))

(defgeneric act (compositor verb &rest arguments)
  (:method ((c null) verb &rest arguments)
    (declare (ignore verb arguments))
    nil))

(defgeneric verbs (compositor)
  (:method ((c null)) nil))

(defgeneric step-window (compositor by)
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

(defclass window (fs:mount) ())

(defmethod fs:volatile-p ((w window) &optional name) (declare (ignore name)) t)

(defun %own (w) (%window-said (fs:of w) (fs:name w)))

(defmethod fs:names ((w window))
  '((:title   . "what the window is called")
    (:app     . "which program it belongs to")
    (:rect    . "where it is now")
    (:hidden  . "whether it is off the screen")
    (:focused . "whether it has the keyboard")))

(defmethod fs:read ((w window) (name (eql :title))) (%field (%own w) "title"))
(defmethod fs:read ((w window) (name (eql :app))) (%field (%own w) "app"))
(defmethod fs:read ((w window) (name (eql :rect))) (%field (%own w) "rect"))
(defmethod fs:read ((w window) (name (eql :hidden))) (%field (%own w) "hidden"))
(defmethod fs:read ((w window) (name (eql :focused))) (%field (%own w) "focused"))

(defmethod fs:write ((w window) (name (eql :hidden)) value)
  (if value (hide (fs:of w) (fs:name w)) (show (fs:of w) (fs:name w))))

(defmethod fs:write ((w window) (name (eql :focused)) value)
  (when value (focus (fs:of w) (fs:name w))))

(defclass windows (fs:mount) ())

(defmethod fs:volatile-p ((d windows) &optional name) (declare (ignore name)) t)

(defmethod fs:child ((d windows) id)
  (let ((id (princ-to-string id)))
    (when (%named (windows (fs:of d)) "id" id)
      (fs:ensure-child d id (lambda () (make-instance 'window :name id :of (fs:of d) :parent d))))))

(defmethod fs:children ((d windows))
  (remove nil (mapcar (lambda (id) (fs:child d id)) (ids (fs:of d)))))

(defclass output (fs:mount) ())

(defmethod fs:volatile-p ((o output) &optional name) (declare (ignore name)) t)

(defmethod fs:names ((o output))
  '((:position . "where the screen starts")
    (:size     . "how big it is")
    (:area     . "what is left after the bars took their strip")))

(defmethod fs:read ((o output) (name (eql :position)))
  (%field (%output-said (fs:of o) (fs:name o)) "position"))
(defmethod fs:read ((o output) (name (eql :size)))
  (%field (%output-said (fs:of o) (fs:name o)) "size"))
(defmethod fs:read ((o output) (name (eql :area)))
  (%field (%output-said (fs:of o) (fs:name o)) "area"))

(defclass outputs (fs:mount) ())

(defmethod fs:volatile-p ((d outputs) &optional name) (declare (ignore name)) t)

(defmethod fs:child ((d outputs) name)
  (let ((name (princ-to-string name)))
    (when (%output-said (fs:of d) name)
      (fs:ensure-child d name (lambda () (make-instance 'output :name name :of (fs:of d) :parent d))))))

(defmethod fs:children ((d outputs))
  (remove nil (mapcar (lambda (o) (fs:child d (getf o :name))) (outputs (fs:of d)))))

(defclass workspace (fs:derived) ())

(defmethod fs:volatile-p ((n workspace) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n workspace))
  (let ((w (%named (workspaces (fs:of n)) "idx" (fs:name n))))
    (when w
      (list :focused (and (gethash "is_focused" w) t)
            :urgent (and (gethash "is_urgent" w) t)
            :windows (gethash "active_window_id" w)))))

(defmethod fs:takes ((n workspace) value)
  (when value (act (fs:of n) "workspace" (fs:name n))))

(defclass workspaces (fs:mount) ())

(defmethod fs:volatile-p ((d workspaces) &optional name) (declare (ignore name)) t)

(defmethod fs:child ((d workspaces) idx)
  (let ((idx (princ-to-string idx)))
    (when (%named (workspaces (fs:of d)) "idx" idx)
      (fs:ensure-child d idx (lambda () (make-instance 'workspace :name idx :of (fs:of d) :parent d))))))

(defmethod fs:children ((d workspaces))
  (remove nil (mapcar (lambda (w) (fs:child d (gethash "idx" w))) (workspaces (fs:of d)))))

(defclass focus (fs:derived) ())

(defmethod fs:volatile-p ((n focus) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n focus)) (focused (fs:of n)))

(defmethod fs:takes ((n focus) value) (when value (focus (fs:of n) value)))

(defclass verb (fs:derived) ())

(defmethod fs:volatile-p ((n verb) &optional name) (declare (ignore name)) t)

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

