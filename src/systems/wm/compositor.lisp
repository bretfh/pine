(defpackage #:pine/wm/compositor
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export #:compositor #:workspaces #:windows #:titled #:focused #:focus
           #:outputs #:ids #:rect #:hidden #:hide #:show
           #:step-window #:close-window #:overview #:leave #:split #:act #:verbs))
(in-package #:pine/wm/compositor)

(defparameter +output-fields+ '("position" "size" "area"))

(defparameter +window-fields+ '("title" "app" "rect" "hidden" "focused"))

(defclass compositor (node:live)
  ((places :initform nil :accessor places))
  (:documentation "The compositor this session is under, in the namespace: its
outputs, its windows, and what it will take.

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

(defun %window-field (c id name)
  "One thing a window says about itself, at a path of its own. Writing HIDDEN takes
it off the screen or puts it back and writing FOCUSED gives it the keyboard; the
rest is what the compositor says, and says alone."
  (when (member name +window-fields+ :test #'equal)
    (make-instance 'node:place :name name
                :reads (lambda () (%field (%window-said c id) name))
                :writes (lambda (value)
                          (cond ((equal name "hidden")
                                 (if value (hide c id) (show c id)))
                                ((equal name "focused")
                                 (when value (focus c id))))))))

(defun %window (c id)
  (when (%named (windows c) "id" id)
    (make-instance 'node:place :name id
                :names (constantly +window-fields+)
                :each (lambda (name) (%window-field c id name))
                :reads (lambda () (%window-said c id))
                :writes (lambda (value) (when value (focus c id))))))

(defun %windows (c)
  (make-instance 'node:place :name "windows"
              :names (lambda () (ids c))
              :each (lambda (id) (%window c id))
              :describes "every window there is"))

(defun %output (c name)
  (when (%output-said c name)
    (make-instance 'node:place :name name
                :names (constantly +output-fields+)
                :each (lambda (field)
                        (when (member field +output-fields+ :test #'equal)
                          (make-instance 'node:place :name field
                                      :reads (lambda ()
                                               (%field (%output-said c name)
                                                       field)))))
                :reads (lambda () (%output-said c name)))))

(defun %outputs (c)
  (make-instance 'node:place :name "outputs"
              :names (lambda () (mapcar (lambda (o) (getf o :name)) (outputs c)))
              :each (lambda (name) (%output c name))
              :describes "every screen, and what is left after the bars"))

(defun %workspace (c idx)
  (when (%named (workspaces c) "idx" idx)
    (make-instance 'node:place :name idx
                :reads (lambda ()
                         (let ((w (%named (workspaces c) "idx" idx)))
                           (when w
                             (list :focused (and (gethash "is_focused" w) t)
                                   :urgent (and (gethash "is_urgent" w) t)
                                   :windows (gethash "active_window_id" w)))))
                :writes (lambda (value) (when value (act c "workspace" idx))))))

(defun %workspaces (c)
  (make-instance 'node:place :name "workspaces"
              :names (lambda () (mapcar (lambda (w) (gethash "idx" w))
                                        (workspaces c)))
              :each (lambda (idx) (%workspace c idx))
              :describes "how this compositor groups them, where it does"))

(defun %focused (c)
  (make-instance 'node:place :name "focused"
              :reads (lambda () (focused c))
              :writes (lambda (value) (when value (focus c value)))
              :describes "which window has the keyboard"))

(defun %verb (c name)
  (make-instance 'node:place :name name
              :reads (constantly name)
              :writes (lambda (value)
                        (declare (ignore value))
                        (act c name))))

(defmethod initialize-instance :after ((c compositor) &key)
  (let ((all (list* (%outputs c) (%workspaces c) (%windows c) (%focused c)
                    (mapcar (lambda (name) (%verb c name)) (verbs c)))))
    (dolist (each all) (setf (node:parent each) c))
    (setf (places c) all)))

(defmethod node:nodes ((c compositor))
  "What the compositor answers for, and whatever was hung here besides: a window
manager attaches its own places under /wm and they are found like any other."
  (append (places c) (call-next-method)))

(defmethod node:resolve ((c compositor) name)
  (let ((name (princ-to-string name)))
    (or (find name (places c) :key #'node:name :test #'equal)
        (call-next-method))))

(defmethod node:contents ((c compositor))
  (list :outputs (length (outputs c))
        :windows (length (windows c))
        :focused (focused c)))

