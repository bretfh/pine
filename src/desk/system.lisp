(defpackage #:pine/desk
  (:use #:pine/user)
  (:export #:desk))
(in-package #:pine/desk)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defclass desk (system) ()
  (:documentation "The desktop: a bar along the top, and the panels it opens.

Nothing here is privileged, and nothing here is written in a language a config is
not. It names no package of pine's: what it reads and writes, it says by path, and
what it draws, it says in the words pine lends."))

(offers 'desk)

(defun %at (name) (at nil (format nil "surface/~a/shown" name)))

(defun %shown (name)
  (let ((n (%at name))) (and n (contents n))))

(defun %show (name &optional (value t))
  (let ((n (%at name))) (when n (setf (contents n) value))))

(defun %hide (name) (%show name nil))

(defun %toggle (name) (%show name (not (%shown name))))

(defun %clock ()
  (row :class "clock"
       (label /dev/clock/hour) (label ":") (label /dev/clock/minute)))

(defun %workspaces ()
  (rows /wm/workspaces
        (lambda ()
          (button :class "workspace" :click (here)
                  (label (leaf (here)))))))

(defun %title () (label /wm/focused :class "title"))

(defun %sound ()
  (button :class "sound" :click (lambda () (%toggle "sound"))
          (row (label "vol ") (label /dev/audio/volume))))

(defun %battery ()
  (button :class "battery" :click (lambda () (%toggle "power"))
          (row (label /dev/power/battery) (label "%"))))

(defun %bar ()
  (builds "bar"
          (lambda ()
            (centerbox
             :class "bar"
             :start (row :class "left" (%workspaces))
             :center (row :class "middle" (%title))
             :end (row :class "right" (%sound) (%battery)
                       (button :class "clock-button"
                               :click (lambda () (%toggle "calendar"))
                               (%clock)))))
          :as 'bar :shown t))

(defun %sound-panel ()
  (builds "sound"
          (lambda ()
            (column :class "panel sound-panel"
                    (label "sound")
                    (slider /dev/audio/volume)
                    (button :class "mute" :click /dev/audio/muted
                            (label "mute"))))
          :as 'panel))

(defun %power-panel ()
  (builds "power"
          (lambda ()
            (column :class "panel power-panel"
                    (label "power")
                    (row (label /dev/power/battery) (label "%  ")
                         (label /dev/power/state))
                    (rule)
                    (rows '("lock" "suspend" "reboot" "poweroff" "logout")
                          (lambda (verb i)
                            (declare (ignore i))
                            (button :class "verb"
                                    :click (path (format nil "/dev/power/~a"
                                                         verb))
                                    :confirm (format nil "~a?" verb)
                                    (label verb))))))
          :as 'panel))

(defun %calendar ()
  (builds "calendar"
          (lambda ()
            (column :class "panel calendar-panel"
                    (calendar :year (or (read /dev/clock/year) 2000)
                              :month (or (read /dev/clock/month) 1)
                              :day (or (read /dev/clock/day) 1))))
          :as 'panel))

(defcommand "show-surface" (name) (:describes "put a surface up")
  (and (%show (princ-to-string name)) t))

(defcommand "hide-surface" (name) (:describes "take a surface down")
  (and (%hide (princ-to-string name)) t))

(defcommand "toggle-surface" (name) (:describes "the panel, either way")
  (%toggle (princ-to-string name))
  (%shown (princ-to-string name)))

(defcommand "surfaces" () (:describes "every surface there is")
  (loop :for each :in (surfaces)
        :collect (list (name each)
                       (string-downcase (class-name (class-of (role each))))
                       (and (shown each) t))))

(defcommand "surface" (said) (:describes "what one surface is")
  (let ((s (at nil (format nil "surface/~a" said))))
    (when s
      (list :role (string-downcase (class-name (class-of (role s))))
            :shown (and (shown s) t)
            :size (read (at s "size"))))))

(defmethod start ((s desk))
  (%bar)
  (%sound-panel)
  (%power-panel)
  (%calendar)
  (note "~d surface~:p" (length (surfaces)))
  s)

(defmethod stop ((s desk))
  (dolist (each '("bar" "sound" "power" "calendar"))
    (erase nil (format nil "surface/~a" each)))
  s)
