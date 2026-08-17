(defpackage #:pine/wm
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:command #:pine/run/command) (#:sh #:pine/host/shell)
                    (#:compositor #:pine/wm/compositor) (#:niri #:pine/wm/niri)
                    (#:tiles #:pine/wm/tiles) (#:managed #:pine/wm/managed))
  (:export #:wm #:current #:terminal #:layout #:*terminal* #:*manage*))
(in-package #:pine/wm)

(defvar *terminal* "alacritty")
(defvar *manage* nil
  "Whether pine is the window manager rather than a client of one. /wm-manage says
so too: the screen, finding a compositor that asks for a manager, writes it there
before it asks for this system, because it cannot name this package yet.")

(defclass wm (system:system) ()
  (:documentation "The compositor, in the namespace, and the commands that act on
it. Which compositor it is is one class under COMPOSITOR."))

(system:offers 'wm)

(defun current () (tree:at nil "wm"))

(defun terminal ()
  (or (node:contents (tree:ensure nil "wm-terminal")) *terminal*))

(defun %under ()
  "Which compositor this session is under, as a class. Pine managing one and pine
talking to one are the same protocol with two subclasses under it, and this is
where a third is added."
  (cond ((or *manage* (node:contents (tree:ensure nil "wm-manage")))
         'managed:managed)
        ((uiop:getenv "NIRI_SOCKET") 'niri:niri)
        ((sh:has "niri") 'niri:niri)))

(defun layout (&optional name)
  "The layout in force, by name, or put one there. It is a node, so a config and
the cli change it the same way: pine write /wm/layout wide."
  (let* ((c (current))
         (n (and c (node:resolve c "layout"))))
    (when n
      (when name (setf (node:contents n) (princ-to-string name)))
      (node:contents n))))

(defmethod job:start ((s wm))
  (let ((class (%under)))
    (unless class (error "no compositor here that pine knows how to talk to."))
    (node:attach (make-instance class :name "wm"
                                :describes "the compositor: its workspaces, its
windows, and what it takes")
                 (tree:root)))
  (command:defcommand "wm-focus-next" ()
      (:describes "the keyboard to the next window")
    (compositor:step-window (current) 1))
  (command:defcommand "wm-focus-previous" ()
      (:describes "the keyboard to the window before")
    (compositor:step-window (current) -1))
  (command:defcommand "wm-close-window" () (:describes "close the focused window")
    (compositor:close-window (current)))
  (command:defcommand "wm-overview" ()
      (:describes "the compositor's overview, either way")
    (compositor:overview (current)))
  (command:defcommand "wm-split" (side)
      (:describes "put the focused window beside the others, or back among them")
    (compositor:split (current)
                      (if (member (princ-to-string side) '("right" "beside")
                                  :test #'equal)
                          :beside
                          :column)))
  (command:defcommand "wm-exit" () (:describes "end the session")
    (compositor:leave (current)))
  (command:defcommand "wm-terminal" ()
      (:describes "a terminal, the one this machine uses")
    (sh:launch (list (terminal)))
    (terminal))
  (command:defcommand "wm-windows" () (:describes "every window, with its title")
    (let ((c (current)))
      (loop :for w :in (compositor:windows c)
            :collect (list (princ-to-string (gethash "id" w ""))
                           (gethash "title" w "")))))
  (command:defcommand "wm-title" ()
      (:describes "what the focused window is called")
    (let ((c (current)))
      (compositor:titled c (compositor:focused c))))
  (command:defcommand "wm-layout" (&optional name)
      (:describes "how windows are laid out, where pine is the one laying them out")
    (layout name))
  (command:defcommand "wm-layouts" () (:describes "every layout there is")
    (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
            (tiles:layouts)))
  s)

(defmethod job:stop ((s wm))
  (dolist (name '("wm-focus-next" "wm-focus-previous" "wm-close-window"
                  "wm-overview" "wm-split" "wm-exit" "wm-terminal"
                  "wm-windows" "wm-title" "wm-layout" "wm-layouts"))
    (command:forget name))
  (tree:erase nil "wm")
  s)
