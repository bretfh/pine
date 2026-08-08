(defpackage #:pine.provider.power
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:power-node #:install #:battery #:charging #:act))

(in-package #:pine.provider.power)

(defparameter +verbs+
  '(("lock" . "loginctl lock-session")
    ("suspend" . "systemctl suspend")
    ("reboot" . "systemctl reboot")
    ("poweroff" . "systemctl poweroff")
    ("logout" . "loginctl terminate-session $XDG_SESSION_ID")))

(defparameter +readings+ '("battery" "charging"))

(defclass power-node (node:node) ())
(defclass reading-node (node:node) ())
(defclass verb-node (node:node) ())

(defun %supply ()
  (first (directory "/sys/class/power_supply/BAT*/")))

(defun battery ()
  (let ((where (%supply)))
    (when where
      (out:number-in (out:sh "cat ~acapacity 2>/dev/null" (namestring where))))))

(defun charging ()
  (let ((where (%supply)))
    (when where
      (let ((state (out:sh "cat ~astatus 2>/dev/null" (namestring where))))
        (and (search "Charging" state) t)))))

(defun act (what)
  (let ((line (cdr (assoc what +verbs+ :test #'equal))))
    (when line (out:sh "~a" line) t)))

(defmethod node:nodes ((n power-node))
  (append (loop :for name :in +readings+
                :collect (make-instance 'reading-node :name name :parent n))
          (loop :for (name . nil) :in +verbs+
                :collect (make-instance 'verb-node :name name :parent n))))

(defmethod node:resolve ((n power-node) name)
  (cond ((member name +readings+ :test #'equal)
         (make-instance 'reading-node :name name :parent n))
        ((assoc name +verbs+ :test #'equal)
         (make-instance 'verb-node :name name :parent n))))

(defmethod node:contents ((n power-node)) (battery))

(defmethod node:contents ((n reading-node))
  (if (equal "battery" (node:name n)) (battery) (charging)))

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
