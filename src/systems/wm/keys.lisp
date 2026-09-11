(defpackage #:pine/wm/keys
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:fs #:pine/fs) (#:mode #:pine/mode))
  (:export
   #:wm #:chords #:keys-node))
(in-package #:pine/wm/keys)

(defvar *pending* nil)

(defclass wm (mode:mode) ())

(defun pending () *pending*)

(defun chords ()
  (mapcar #'car (mode:bindings (make-instance 'wm))))

(defun dispatch (said)
  (let ((m (make-instance 'wm)))
    (loop :for k :in (ui:chord (princ-to-string said))
          :do (multiple-value-bind (answer typed) (mode:dispatch m nil k
                                                                  (pending))
                (setf *pending* (if (eq answer :pending) typed nil))
                (unless (eq answer :pending) (return answer))))))

(defclass pressed (fs:derived) ())

(defmethod fs:volatile-p ((n pressed) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n pressed)) (ui:spelled (pending)))

(defmethod fs:takes ((n pressed) value) (dispatch value))

(defun keys-node ()
  (make-instance 'pressed :name "key" :describes "write a chord here to press it"))

