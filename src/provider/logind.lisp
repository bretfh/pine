(defpackage #:pine.provider.logind
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:out #:pine.provider.out))
  (:export #:logind))

(in-package #:pine.provider.logind)
(named-readtables:in-readtable pine.path:syntax)

;;;; Power: what the battery says, and the five things a session can do to
;;;; itself.
;;;;
;;;;   (write /power (logind))
;;;;
;;;; The battery is sysfs; locking, logging out and going down are loginctl and
;;;; systemctl, which is what a desktop session actually has.

(defun %battery-dir ()
  (or (first (directory "/sys/class/power_supply/BAT*/"))
      (first (directory "/sys/class/power_supply/*/"))))

(defun %battery ()
  (let ((dir (%battery-dir)))
    (when dir (out:int-file (merge-pathnames "capacity" dir)))))

(defun %state ()
  (let ((dir (%battery-dir)))
    (when dir
      (let ((text (ignore-errors
                    (string-trim '(#\newline #\space)
                                 (uiop:read-file-string
                                  (merge-pathnames "status" dir))))))
        (cond ((null text) nil)
              ((string-equal text "Charging") :charging)
              ((string-equal text "Discharging") :discharging)
              ((string-equal text "Full") :full)
              ((string-equal text "Not charging") :idle)
              (t :unknown))))))

(defun logind ()
  "Power: the battery, and what a session can do to itself."
  (ns:provider
   {:doc "the battery through sysfs, the actions through loginctl and systemctl"}

   (/power/battery {:read (pine.data:fn [] (%battery)) :doc "0..100"})
   (/power/state {:read (pine.data:fn [] (%state))
                  :doc ":charging :discharging :full :idle"})
   (/power
    {:verbs {:lock (pine.data:fn [] (out:sh "loginctl lock-session") t)
             :logout (pine.data:fn [] (out:sh "loginctl terminate-session ''") t)
             :suspend (pine.data:fn [] (out:sh "systemctl suspend") t)
             :reboot (pine.data:fn [] (out:sh "systemctl reboot") t)
             :poweroff (pine.data:fn [] (out:sh "systemctl poweroff") t)}
     :doc "battery, and [:lock] [:logout] [:suspend] [:reboot] [:poweroff]"})))
