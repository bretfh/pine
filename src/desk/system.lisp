(defpackage #:pine/desk
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:path #:pine/fs/path) (#:job #:pine/run/job)
                    (#:system #:pine/run/system) (#:command #:pine/run/command)
                    (#:log #:pine/run/log)
                    (#:surface #:pine/ui/surface) (#:build #:pine/ui/build))
  (:export #:desk #:shown #:show #:hide #:toggle))
(in-package #:pine/desk)

(defclass desk (system:system) ()
  (:documentation "The desktop: a bar along the top, and the panels it opens.

Nothing here is privileged. Every surface is declared the way a config declares
one, and taking this system away takes them with it."))

(system:offers 'desk)

(defun shown (name)
  (let ((n (tree:at nil (format nil "surface/~a/shown" name))))
    (and n (node:contents n))))

(defun show (name &optional (value t))
  (let ((n (tree:at nil (format nil "surface/~a/shown" name))))
    (when n (setf (node:contents n) value))))

(defun hide (name) (show name nil))

(defun toggle (name) (show name (not (shown name))))

(defun %clock ()
  (build:row :class "clock"
             (build:label (path:parse "/dev/clock/hour"))
             (build:label ":")
             (build:label (path:parse "/dev/clock/minute"))))

(defun %workspaces ()
  (build:rows (path:parse "/wm/workspaces")
              (lambda ()
                (let ((here (build:here)))
                  (build:button :class "workspace" :click here
                                (build:label (path:leaf here)))))))

(defun %title ()
  (build:label (path:parse "/wm/focused") :class "title"))

(defun %sound ()
  (build:button :class "sound" :click (lambda () (toggle "sound"))
                (build:row (build:label "vol ")
                           (build:label (path:parse "/dev/audio/volume")))))

(defun %battery ()
  (build:button :class "battery" :click (lambda () (toggle "power"))
                (build:row (build:label (path:parse "/dev/power/battery"))
                           (build:label "%"))))

(defun %bar ()
  (surface:builds
   "bar"
   (lambda ()
     (build:centerbox
      :class "bar"
      :start (build:row :class "left" (%workspaces))
      :center (build:row :class "middle" (%title))
      :end (build:row :class "right" (%sound) (%battery)
                      (build:button :class "clock-button"
                                    :click (lambda () (toggle "calendar"))
                                    (%clock)))))
   :as 'surface:bar :shown t))

(defun %sound-panel ()
  (surface:builds
   "sound"
   (lambda ()
     (build:column :class "panel sound-panel"
                   (build:label "sound")
                   (build:slider (path:parse "/dev/audio/volume"))
                   (build:button :class "mute"
                                 :click (path:parse "/dev/audio/muted")
                                 (build:label "mute"))))
   :as 'surface:panel))

(defun %power-panel ()
  (surface:builds
   "power"
   (lambda ()
     (build:column :class "panel power-panel"
                   (build:label "power")
                   (build:row (build:label (path:parse "/dev/power/battery"))
                              (build:label "%  ")
                              (build:label (path:parse "/dev/power/state")))
                   (build:rule)
                   (build:rows '("lock" "suspend" "reboot" "poweroff" "logout")
                               (lambda (verb i)
                                 (declare (ignore i))
                                 (build:button
                                  :class "verb"
                                  :click (path:parse
                                          (format nil "/dev/power/~a" verb))
                                  :confirm (format nil "~a?" verb)
                                  (build:label verb))))))
   :as 'surface:panel))

(defun %calendar ()
  (surface:builds
   "calendar"
   (lambda ()
     (build:column :class "panel calendar-panel"
                   (build:calendar
                    :year (or (build:held (path:parse "/dev/clock/year")) 2000)
                    :month (or (build:held (path:parse "/dev/clock/month")) 1)
                    :day (or (build:held (path:parse "/dev/clock/day")) 1))))
   :as 'surface:panel))

(command:defcommand "show-surface" (name) (:describes "put a surface up")
  (and (show (princ-to-string name)) t))

(command:defcommand "hide-surface" (name) (:describes "take a surface down")
  (and (hide (princ-to-string name)) t))

(command:defcommand "toggle-surface" (name) (:describes "the panel, either way")
  (toggle (princ-to-string name))
  (shown (princ-to-string name)))

(command:defcommand "surfaces" () (:describes "every surface there is")
  (loop :for each :in (surface:surfaces)
        :collect (list (node:name each)
                       (string-downcase
                        (class-name (class-of (surface:role each))))
                       (and (surface:shown each) t))))

(command:defcommand "surface" (name) (:describes "what one surface is")
  (let ((s (surface:named name)))
    (when s
      (list :role (string-downcase (class-name (class-of (surface:role s))))
            :shown (and (surface:shown s) t)
            :size (surface:size s)))))

(defmethod job:start ((s desk))
  (%bar)
  (%sound-panel)
  (%power-panel)
  (%calendar)
  (log:note "~d surface~:p" (length (surface:surfaces)))
  s)

(defmethod job:stop ((s desk))
  (dolist (name '("bar" "sound" "power" "calendar"))
    (tree:erase nil (format nil "surface/~a" name)))
  s)
