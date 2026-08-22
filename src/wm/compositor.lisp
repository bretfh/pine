(defpackage #:pine/wm/compositor
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export #:compositor #:workspaces #:windows #:titled #:focused #:focus
           #:outputs #:ids #:rect #:hidden #:hide #:show
           #:step-window #:close-window #:overview #:leave #:split #:act
           #:workspaces-node #:workspace-node #:windows-node #:window-node
           #:outputs-node #:output-node #:field-node
           #:focused-node #:verb-node #:verbs))
(in-package #:pine/wm/compositor)

(defparameter +output-fields+ '("position" "size" "area"))

(defparameter +window-fields+ '("title" "app" "rect" "hidden" "focused"))

(defclass compositor (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "The compositor this session is under, in the namespace: its
outputs, its windows, and what it will take.

A class, so pine being the compositor and pine talking to one are the same protocol
with two subclasses under it. What is under here is the machine's window resources
and nothing about how they are arranged: that is a system you load."))

(defmacro %live (name)
  `(defclass ,name (node:node)
     ((livep  :allocation :class :initform t   :reader node:livep)
      (savedp :allocation :class :initform nil :reader node:savedp))))

(%live workspaces-node)
(%live workspace-node)
(%live windows-node)
(%live window-node)
(%live outputs-node)
(%live output-node)
(%live focused-node)
(%live verb-node)

(defclass field-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "One thing a window or an output says about itself, at a path of
its own, so it can be read and watched on its own."))

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

(defun %kid (n name class)
  (node:child n name
              (lambda () (make-instance class :name (princ-to-string name)
                                              :over n))))

(defun %field (n name)
  (node:child n name
              (lambda () (make-instance 'field-node :name (princ-to-string name)
                                                    :over n))))

(defun %named (things key name)
  (find-if (lambda (thing)
             (equal (princ-to-string name)
                    (princ-to-string (gethash key thing ""))))
           things))

(defmethod node:nodes ((c compositor))
  "What the compositor answers for, and whatever was hung here besides: a window
manager attaches its own places under /wm and they are found like any other."
  (append (list (%kid c "outputs" 'outputs-node)
                (%kid c "workspaces" 'workspaces-node)
                (%kid c "windows" 'windows-node)
                (%kid c "focused" 'focused-node))
          (call-next-method)))

(defmethod node:resolve ((c compositor) name)
  (let ((name (princ-to-string name)))
    (cond ((equal name "outputs") (%kid c name 'outputs-node))
          ((equal name "workspaces") (%kid c name 'workspaces-node))
          ((equal name "windows") (%kid c name 'windows-node))
          ((equal name "focused") (%kid c name 'focused-node))
          ((member name (verbs c) :test #'equal) (%kid c name 'verb-node))
          (t (call-next-method)))))

(defmethod node:contents ((c compositor))
  (list :outputs (length (outputs c))
        :windows (length (windows c))
        :focused (focused c)))

(defun %compositor (n)
  (loop :for at := n :then (node:over at)
        :while at
        :when (typep at 'compositor) :do (return at)))

(defun %output (n name)
  (find (princ-to-string name) (outputs (%compositor n))
        :key (lambda (o) (princ-to-string (getf o :name)))
        :test #'equal))

(defmethod node:nodes ((n outputs-node))
  (loop :for o :in (outputs (%compositor n))
        :collect (%kid n (princ-to-string (getf o :name)) 'output-node)))

(defmethod node:resolve ((n outputs-node) name)
  (when (%output n name) (%kid n name 'output-node)))

(defmethod node:contents ((n outputs-node))
  (mapcar (lambda (o) (princ-to-string (getf o :name))) (outputs (%compositor n))))

(defmethod node:nodes ((n output-node))
  (loop :for field :in +output-fields+ :collect (%field n field)))

(defmethod node:contents ((n output-node)) (%output (node:over n) (node:name n)))

(defmethod node:nodes ((n windows-node))
  (loop :for w :in (windows (%compositor n))
        :collect (%kid n (princ-to-string (gethash "id" w)) 'window-node)))

(defmethod node:resolve ((n windows-node) name)
  (when (%named (windows (%compositor n)) "id" name)
    (%kid n name 'window-node)))

(defmethod node:contents ((n windows-node))
  (mapcar (lambda (w) (princ-to-string (gethash "id" w)))
          (windows (%compositor n))))

(defmethod node:nodes ((n window-node))
  (loop :for field :in +window-fields+ :collect (%field n field)))

(defmethod node:contents ((n window-node))
  (let* ((c (%compositor n))
         (w (%named (windows c) "id" (node:name n))))
    (when w
      (list :title (gethash "title" w)
            :app (gethash "app_id" w)
            :rect (rect c (node:name n))
            :hidden (hidden c (node:name n))
            :focused (and (gethash "is_focused" w) t)))))

(defmethod (setf node:contents) (value (n window-node))
  (when value (focus (%compositor n) (node:name n)))
  value)

(defmethod node:contents ((n field-node))
  (let* ((over (node:over n))
         (name (node:name n))
         (said (node:contents over)))
    (cond ((typep over 'output-node)
           (getf said (intern (string-upcase name) :keyword)))
          ((typep over 'window-node)
           (getf said (intern (string-upcase name) :keyword))))))

(defmethod (setf node:contents) (value (n field-node))
  "Writing a window's HIDDEN takes it off the screen or puts it back; writing its
FOCUSED gives it the keyboard. The rest is what the compositor says, and says
alone."
  (let ((over (node:over n))
        (name (node:name n)))
    (when (typep over 'window-node)
      (let ((c (%compositor over))
            (id (node:name over)))
        (cond ((equal name "hidden") (if value (hide c id) (show c id)))
              ((equal name "focused") (when value (focus c id))))))
    value))

(defmethod node:contents ((n focused-node)) (focused (%compositor n)))

(defmethod (setf node:contents) (value (n focused-node))
  (when value (focus (%compositor n) value))
  value)

(defmethod node:contents ((n verb-node)) (node:name n))

(defmethod (setf node:contents) (value (n verb-node))
  (declare (ignore value))
  (act (%compositor n) (node:name n)))

(defmethod node:nodes ((n workspaces-node))
  (loop :for w :in (workspaces (%compositor n))
        :collect (%kid n (princ-to-string (gethash "idx" w)) 'workspace-node)))

(defmethod node:resolve ((n workspaces-node) name)
  (when (%named (workspaces (%compositor n)) "idx" name)
    (%kid n name 'workspace-node)))

(defmethod node:contents ((n workspaces-node))
  (mapcar (lambda (w) (princ-to-string (gethash "idx" w)))
          (workspaces (%compositor n))))

(defmethod node:contents ((n workspace-node))
  (let ((w (%named (workspaces (%compositor n)) "idx" (node:name n))))
    (when w
      (list :focused (and (gethash "is_focused" w) t)
            :urgent (and (gethash "is_urgent" w) t)
            :windows (gethash "active_window_id" w)))))

(defmethod (setf node:contents) (value (n workspace-node))
  (when value (act (%compositor n) "workspace" (node:name n)))
  value)
