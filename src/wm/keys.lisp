(defpackage #:pine/wm/keys
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:key #:pine/ui/key) (#:mode #:pine/mode))
  (:export #:wm #:dispatch #:pending #:chords #:keys-node))
(in-package #:pine/wm/keys)

(defvar *pending* (d:box nil)
  "The chord standing so far, the window manager's own. The editor keeps its in
KEY:PENDING; a chord the compositor handed over must not finish one somebody was
half way through typing into a document.")

(defclass wm (mode:mode) ()
  (:documentation "The chords the window manager owns: what a key means when it
was not typed at anything, because the compositor took it before whatever has
focus could see it.

A mode like any other, so a config binds one the way it binds a chord in text, and
a chord inherits down a class the same way."))

(defun pending () (d:held *pending*))

(defun chords ()
  "Every chord bound here. What the compositor has to be asked for is worked out
from this: a key it was never told about is one it will not hand over."
  (mapcar #'car (mode:bindings (make-instance 'wm))))

(defun dispatch (said)
  "Take a chord the compositor handed over."
  (let ((m (make-instance 'wm)))
    (loop :for k :in (key:chord (princ-to-string said))
          :do (multiple-value-bind (answer so-far) (mode:dispatch m nil k
                                                                  (pending))
                (d:put! *pending* (if (eq answer :pending) so-far nil))
                (unless (eq answer :pending) (return answer))))))

(defclass keys-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Where a chord the compositor took arrives. Writing one here is
pressing it, so a keyboard, a test and another pine all press the same way."))

(defmethod node:contents ((n keys-node)) (key:text (pending)))

(defmethod (setf node:contents) (value (n keys-node))
  (dispatch value)
  value)
