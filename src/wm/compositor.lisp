(defpackage #:pine/wm/compositor
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export #:compositor #:workspaces #:windows #:titled #:focused #:focus
           #:step-window #:close-window #:overview #:leave #:split #:act
           #:workspaces-node #:workspace-node #:windows-node #:window-node
           #:focused-node #:verb-node #:verbs))
(in-package #:pine/wm/compositor)

(defclass compositor (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "The compositor this session is under, in the namespace: its
workspaces, its windows, and what it will take.

A class, so pine being the compositor and pine talking to one are the same protocol
with two subclasses under it."))

(defclass workspaces-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))
(defclass workspace-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))
(defclass windows-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))
(defclass window-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))
(defclass focused-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))
(defclass verb-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defgeneric workspaces (compositor)
  (:documentation "Every workspace, as maps.")
  (:method ((c null)) nil))

(defgeneric windows (compositor)
  (:documentation "Every window, as maps.")
  (:method ((c null)) nil))

(defgeneric titled (compositor id)
  (:documentation "What a window is called.")
  (:method ((c null) id) (declare (ignore id)) nil))

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

(defun %named (things key name)
  (find-if (lambda (thing)
             (equal (princ-to-string name)
                    (princ-to-string (gethash key thing ""))))
           things))

(defmethod node:nodes ((c compositor))
  (list (%kid c "workspaces" 'workspaces-node)
        (%kid c "windows" 'windows-node)
        (%kid c "focused" 'focused-node)))

(defmethod node:resolve ((c compositor) name)
  (let ((name (princ-to-string name)))
    (cond ((equal name "workspaces") (%kid c name 'workspaces-node))
          ((equal name "windows") (%kid c name 'windows-node))
          ((equal name "focused") (%kid c name 'focused-node))
          ((member name (verbs c) :test #'equal) (%kid c name 'verb-node)))))

(defmethod node:contents ((c compositor))
  (list :workspaces (length (workspaces c))
        :windows (length (windows c))
        :focused (focused c)))

(defun %compositor (n)
  (loop :for at := n :then (node:over at)
        :while at
        :when (typep at 'compositor) :do (return at)))

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

(defmethod node:nodes ((n windows-node))
  (loop :for w :in (windows (%compositor n))
        :collect (%kid n (princ-to-string (gethash "id" w)) 'window-node)))

(defmethod node:resolve ((n windows-node) name)
  (when (%named (windows (%compositor n)) "id" name)
    (%kid n name 'window-node)))

(defmethod node:contents ((n windows-node))
  (mapcar (lambda (w) (princ-to-string (gethash "id" w)))
          (windows (%compositor n))))

(defmethod node:contents ((n window-node))
  (let ((w (%named (windows (%compositor n)) "id" (node:name n))))
    (when w
      (list :title (gethash "title" w)
            :app (gethash "app_id" w)
            :focused (and (gethash "is_focused" w) t)))))

(defmethod (setf node:contents) (value (n window-node))
  (when value (focus (%compositor n) (node:name n)))
  value)

(defmethod node:contents ((n focused-node)) (focused (%compositor n)))

(defmethod (setf node:contents) (value (n focused-node))
  (when value (focus (%compositor n) value))
  value)

(defmethod node:contents ((n verb-node)) (node:name n))

(defmethod (setf node:contents) (value (n verb-node))
  (declare (ignore value))
  (act (%compositor n) (node:name n)))
