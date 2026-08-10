(defpackage #:pine.provider.power
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:power-node #:install #:battery #:charging #:state #:act))

(in-package #:pine.provider.power)

(defparameter +verbs+
  '(("lock" . "loginctl lock-session")
    ("suspend" . "systemctl suspend")
    ("reboot" . "systemctl reboot")
    ("poweroff" . "systemctl poweroff")
    ("logout" . "loginctl terminate-session $XDG_SESSION_ID")))

(defparameter +readings+ '("battery" "charging" "state"))

(defclass power-node (node:node) ())
(defclass reading-node (node:node) ())
(defclass verb-node (node:node) ())

(defun %supply ()
  (or (first (directory "/sys/class/power_supply/BAT*/"))
      (first (remove-if-not
              (lambda (each) (probe-file (merge-pathnames "capacity" each)))
              (directory "/sys/class/power_supply/*/")))))

(defun battery ()
  (let ((where (%supply)))
    (when where
      (out:number-in (out:sh "cat ~acapacity 2>/dev/null" (namestring where))))))

(defun state ()
  "What the battery is doing: charging, discharging, full, idle, or unknown."
  (let ((where (%supply)))
    (when where
      (let ((said (out:sh "cat ~astatus 2>/dev/null" (namestring where))))
        (cond ((search "Charging" said) :charging)
              ((search "Discharging" said) :discharging)
              ((search "Full" said) :full)
              ((search "Not charging" said) :idle)
              ((plusp (length said)) :unknown))))))

(defun charging () (eq :charging (state)))

(defun act (what)
  (let ((line (cdr (assoc what +verbs+ :test #'equal))))
    (when line (out:sh "~a" line) t)))

(defun %kid (n name class &rest initargs)
  (node:child n name
              (lambda ()
                (apply #'make-instance class :name (princ-to-string name)
                       :parent n initargs))))

(defmethod node:every-seconds ((n power-node)) 10)

(defmethod node:nodes ((n power-node))
  (append (loop :for name :in +readings+ :collect (%kid n name 'reading-node))
          (loop :for (name . nil) :in +verbs+ :collect (%kid n name 'verb-node))))

(defmethod node:resolve ((n power-node) name)
  (cond ((member name +readings+ :test #'equal) (%kid n name 'reading-node))
        ((assoc name +verbs+ :test #'equal) (%kid n name 'verb-node))))

(defmethod node:contents ((n power-node)) (battery))

(defmethod node:contents ((n reading-node))
  (let ((name (node:name n)))
    (cond ((equal "battery" name) (battery))
          ((equal "state" name) (state))
          (t (charging)))))

(defmethod node:contents ((n verb-node)) (node:name n))

(defmethod (setf node:contents) (value (n verb-node))
  (declare (ignore value))
  (act (node:name n)))

(defmethod node:leafp ((n reading-node)) t)
(defmethod node:leafp ((n verb-node)) t)
(defmethod node:livep ((n power-node)) t)
(defmethod node:livep ((n reading-node)) t)
(defmethod node:livep ((n verb-node)) t)

(defun install (root &optional (name "power"))
  (node:attach (make-instance 'power-node :name name
                                          :describes "the battery, and lock suspend reboot poweroff logout")
               root))
