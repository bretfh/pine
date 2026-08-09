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

(defun %kid (n name class)
  (node:child n name
              (lambda () (make-instance class :name (princ-to-string name) :parent n))))

(defmethod node:announces ((n wm-node)) (list "niri msg --json event-stream"))

(defmethod node:nodes ((n wm-node))
  (list (%kid n "workspaces" 'workspaces-node)
        (%kid n "windows" 'windows-node)
        (%kid n "focused" 'focused-node)))

(defmethod node:resolve ((n wm-node) name)
  (cond ((equal name "workspaces") (%kid n name 'workspaces-node))
        ((equal name "windows") (%kid n name 'windows-node))
        ((equal name "focused") (%kid n name 'focused-node))
        ((member name +verbs+ :test #'equal) (%kid n name 'verb-node))))

(defmethod node:contents ((n wm-node))
  (list :workspaces (length (workspaces))
        :windows (length (windows))
        :focused (focused)))

(defmethod node:nodes ((n workspaces-node))
  (loop :for w :in (workspaces)
        :collect (%kid n (princ-to-string (gethash "idx" w)) 'workspace-node)))

(defmethod node:resolve ((n workspaces-node) name)
  (when (%named (workspaces) "idx" name) (%kid n name 'workspace-node)))

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
        :collect (%kid n (princ-to-string (gethash "id" w)) 'window-node)))

(defmethod node:resolve ((n windows-node) name)
  (when (%named (windows) "id" name) (%kid n name 'window-node)))

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
