(defpackage #:pine.provider.wm
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:wm-node #:install #:niri #:workspaces #:windows #:focused
           #:act #:json))

(in-package #:pine.provider.wm)

(defparameter +verbs+ '("overview" "close" "expel" "consume" "exit"))
(defparameter +actions+
  '(("overview" . "toggle-overview")
    ("close"    . "close-window")
    ("expel"    . "consume-or-expel-window-right")
    ("consume"  . "consume-or-expel-window-left")
    ("exit"     . "quit --skip-confirmation")))

(defclass wm-node (node:node) ())
(defclass workspaces-node (node:node) ())
(defclass workspace-node (node:node) ())
(defclass windows-node (node:node) ())
(defclass window-node (node:node) ())
(defclass focused-node (node:node) ())
(defclass verb-node (node:node) ())

(defun json (text)
  (when (and text (plusp (length text)))
    (ignore-errors (com.inuoe.jzon:parse text))))

(defun %msg (format &rest arguments)
  (json (apply #'out:sh (concatenate 'string "niri msg --json " format) arguments)))

(defun %list (command)
  (let ((value (%msg command)))
    (when (vectorp value) (coerce value 'list))))

(defun workspaces () (%list "workspaces"))
(defun windows () (%list "windows"))

(defun %named (things key name)
  (find-if (lambda (thing)
             (equal (princ-to-string name)
                    (princ-to-string (gethash key thing ""))))
           things))

(defun focused ()
  (let ((found (find-if (lambda (w) (gethash "is_focused" w)) (windows))))
    (when found (princ-to-string (gethash "id" found)))))

(defun act (verb &rest arguments)
  (let ((action (cdr (assoc (princ-to-string verb) +actions+ :test #'equal))))
    (when action
      (apply #'out:sh (concatenate 'string "niri msg action " action) arguments)
      t)))

(defmethod node:nodes ((n wm-node))
  (list (make-instance 'workspaces-node :name "workspaces" :parent n)
        (make-instance 'windows-node :name "windows" :parent n)
        (make-instance 'focused-node :name "focused" :parent n)))

(defmethod node:resolve ((n wm-node) name)
  (cond ((equal name "workspaces") (make-instance 'workspaces-node :name name :parent n))
        ((equal name "windows") (make-instance 'windows-node :name name :parent n))
        ((equal name "focused") (make-instance 'focused-node :name name :parent n))
        ((member name +verbs+ :test #'equal)
         (make-instance 'verb-node :name name :parent n))))

(defmethod node:contents ((n wm-node))
  (list :workspaces (length (workspaces))
        :windows (length (windows))
        :focused (focused)))

(defmethod node:nodes ((n workspaces-node))
  (loop :for w :in (workspaces)
        :collect (make-instance 'workspace-node :parent n
                                :name (princ-to-string (gethash "idx" w)))))

(defmethod node:resolve ((n workspaces-node) name)
  (when (%named (workspaces) "idx" name)
    (make-instance 'workspace-node :name name :parent n)))

(defmethod node:contents ((n workspaces-node))
  (mapcar (lambda (w) (princ-to-string (gethash "idx" w))) (workspaces)))

(defmethod node:contents ((n workspace-node))
  (let ((w (%named (workspaces) "idx" (node:name n))))
    (when w
      (list :focused (and (gethash "is_focused" w) t)
            :urgent (and (gethash "is_urgent" w) t)
            :windows (gethash "active_window_id" w)))))

(defmethod (setf node:contents) (value (n workspace-node))
  (when value (out:sh "niri msg action focus-workspace ~a" (node:name n)))
  value)

(defmethod node:nodes ((n windows-node))
  (loop :for w :in (windows)
        :collect (make-instance 'window-node :parent n
                                :name (princ-to-string (gethash "id" w)))))

(defmethod node:resolve ((n windows-node) name)
  (when (%named (windows) "id" name)
    (make-instance 'window-node :name name :parent n)))

(defmethod node:contents ((n windows-node))
  (mapcar (lambda (w) (princ-to-string (gethash "id" w))) (windows)))

(defmethod node:contents ((n window-node))
  (let ((w (%named (windows) "id" (node:name n))))
    (when w
      (list :title (gethash "title" w)
            :app (gethash "app_id" w)
            :focused (and (gethash "is_focused" w) t)))))

(defmethod (setf node:contents) (value (n window-node))
  (when value (out:sh "niri msg action focus-window --id ~a" (node:name n)))
  value)

(defmethod node:contents ((n focused-node)) (focused))

(defmethod (setf node:contents) (value (n focused-node))
  (when value (out:sh "niri msg action focus-window --id ~a" value))
  value)

(defmethod node:contents ((n verb-node)) (node:name n))

(defmethod (setf node:contents) (value (n verb-node))
  (declare (ignore value))
  (act (node:name n)))

(defmethod node:leafp ((n window-node)) t)
(defmethod node:leafp ((n workspace-node)) t)
(defmethod node:leafp ((n focused-node)) t)
(defmethod node:leafp ((n verb-node)) t)

(defmethod node:livep ((n wm-node)) t)
(defmethod node:livep ((n workspaces-node)) t)
(defmethod node:livep ((n workspace-node)) t)
(defmethod node:livep ((n windows-node)) t)
(defmethod node:livep ((n window-node)) t)
(defmethod node:livep ((n focused-node)) t)
(defmethod node:livep ((n verb-node)) t)

(defun install (root &optional (name "wm"))
  (node:attach (make-instance 'wm-node :name name
                                       :describes "the compositor: its workspaces, its windows, and what it takes")
               root))

(defun niri (&optional (name "wm"))
  (install (pine.world.world:root pine.world.world:*world*) name))
