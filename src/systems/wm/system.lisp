(defpackage #:pine/wm
  (:use #:cl)
  (:local-nicknames (#:fs #:pine/fs)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:command #:pine/run/command) (#:sh #:pine/host/shell)
                    (#:compositor #:pine/wm/compositor) (#:niri #:pine/wm/niri)
                    (#:wkeys #:pine/wm/keys) (#:managed #:pine/wm/managed))
  (:export
   #:current))
(in-package #:pine/wm)

(defvar *terminal* "alacritty")

(defclass wm (system:system)
  ((compositor :initform nil :accessor compositor-of))
  (:documentation "The compositor, in the namespace at /wm, and the commands that
act on it. Which compositor it is is one class under COMPOSITOR."))

(defun current ()
  (let ((s (system:named "wm"))) (and s (compositor-of s))))

(defun terminal ()
  (or (fs:contents (fs:leaf "/wm/terminal")) *terminal*))

(defun places ()
  "The name of the system that says where the windows go, or nothing. Core does
not know what is behind the name: it is a system, and it is used the way any of
them is. A config writes it because /wm cannot exist until the compositor has
handed the windows over, which is after the config was read."
  (fs:contents (fs:leaf "/wm/places")))

(defun %under ()
  "Which compositor this session is under, as a class. Pine managing one and pine
talking to one are the same protocol with two subclasses under it, and this is
where a third is added.

Who manages the windows is written at /wm/manages rather than held here: the screen
finds a compositor asking for a manager before this system exists, and a path is
what it can reach that a package it cannot name is not.

:PINE or :COMPOSITOR, and not a yes and a no. ENSURE makes the node to read it, so
a place written NIL and a place nobody has written are the same node holding the
same thing, and the answer to which one it was decided who lays out the screen."
  (cond ((eq :pine (fs:contents (fs:leaf "/wm/manages")))
         'managed:managed)
        ((uiop:getenv "NIRI_SOCKET") 'niri:niri)
        ((sh:has "niri") 'niri:niri)))

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

(command:defcommand "wm-outputs" ()
    (:describes "every screen, and what is left of it after the bars")
  (compositor:outputs (current)))

(command:defcommand "wm-hide" (id) (:describes "take a window off the screen")
  (and (compositor:hide (current) (princ-to-string id)) t))

(command:defcommand "wm-show" (id) (:describes "put one back")
  (and (compositor:show (current) (princ-to-string id)) t))

(command:defcommand "switch-to-window" (which)
    (:describes "go to a window by name"
     :asks '((:prompt "Window: " :category :window :must-match t)))
  (let* ((said (princ-to-string which))
         (at (position #\Space said))
         (id (if at (subseq said 0 at) said)))
    (compositor:focus (current) id)
    id))

(defmethod job:start ((s wm))
  (let ((class (%under)) (under (fs:ensure "/wm")))
    (unless class (error "no compositor here that pine knows how to talk to."))
    (let ((c (make-instance class :name "compositor")))
      (setf (compositor-of s) c)
      (dolist (each (compositor:parts c)) (system:puts each under)))
    (system:puts (wkeys:keys-node) under))
  (let ((places (places)))
    (when places
      (when (system:named places) (system:drop places))
      (system:use places)))
  s)

(defmethod job:stop ((s wm))
  "What places the windows goes first."
  (let ((places (places)))
    (when (and places (system:named places)) (system:drop places)))
  s)
