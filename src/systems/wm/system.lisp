(in-package #:pine/wm)

(defvar *terminal* "alacritty")

(defclass wm (module:module)
  ((compositor :initform nil :accessor compositor-of)))

(defun current ()
  (let ((s (module:named "wm"))) (and s (compositor-of s))))

(fs:mount (lambda () (make-instance 'fs:mount :describes "the compositor, and what places its windows"))
          "/wm")

(defun %said (name)
  (let ((n (fs:at "/wm" name))) (and n (fs:contents n))))

(defun terminal ()
  (or (%said "terminal") *terminal*))

(defun places ()
  (%said "places"))

(defun %under ()
  (cond ((eq :pine (%said "manages"))
         'managed)
        ((uiop:getenv "NIRI_SOCKET") 'niri)
        ((sh:has "niri") 'niri)))

(command:defcommand "wm-focus-next" ()
    (:describes "the keyboard to the next window")
  (step-window (current) 1))

(command:defcommand "wm-focus-previous" ()
    (:describes "the keyboard to the window before")
  (step-window (current) -1))

(command:defcommand "wm-close-window" () (:describes "close the focused window")
  (close-window (current)))

(command:defcommand "wm-overview" ()
    (:describes "the compositor's overview, either way")
  (overview (current)))

(command:defcommand "wm-split" (side)
    (:describes "put the focused window beside the others, or back among them")
  (split (current)
                    (if (member (princ-to-string side) '("right" "beside")
                                :test #'equal)
                        :beside
                        :column)))

(command:defcommand "wm-exit" () (:describes "end the session")
  (leave (current)))

(command:defcommand "wm-terminal" ()
    (:describes "a terminal, the one this machine uses")
  (sh:launch (list (terminal)))
  (terminal))

(command:defcommand "wm-windows" () (:describes "every window, with its title")
  (let ((c (current)))
    (loop :for w :in (windows c)
          :collect (list (princ-to-string (gethash "id" w ""))
                         (gethash "title" w "")))))

(command:defcommand "wm-title" ()
    (:describes "what the focused window is called")
  (let ((c (current)))
    (titled c (focused c))))

(command:defcommand "wm-outputs" ()
    (:describes "every screen, and what is left of it after the bars")
  (outputs (current)))

(command:defcommand "wm-hide" (id) (:describes "take a window off the screen")
  (and (hide (current) (princ-to-string id)) t))

(command:defcommand "wm-show" (id) (:describes "put one back")
  (and (show (current) (princ-to-string id)) t))

(command:defcommand "switch-to-window" (which)
    (:describes "go to a window by name"
     :asks '((:prompt "Window: " :category :window :must-match t)))
  (let* ((said (princ-to-string which))
         (at (position #\Space said))
         (id (if at (subseq said 0 at) said)))
    (focus (current) id)
    id))

(defmethod job:start ((s wm))
  (let ((class (%under)) (under (fs:at "/wm")))
    (unless class (error "no compositor here that pine knows how to talk to."))
    (let ((c (make-instance class :name "compositor")))
      (setf (compositor-of s) c)
      (dolist (each (parts c)) (fs:mount each under)))
    (fs:mount (wkeys:keys-node) under))
  (let ((places (places)))
    (when places
      (when (module:named places) (module:drop places))
      (module:use places)))
  s)

(defmethod job:stop ((s wm))
  (let ((places (places)))
    (when (and places (module:named places)) (module:drop places)))
  s)
