(defpackage #:pine/wm/keys
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:node #:pine/fs/node) (#:mode #:pine/mode))
  (:export
   #:wm #:chords #:keys-node))
(in-package #:pine/wm/keys)

(defvar *pending* nil
  "The chord standing so far, the window manager's own. The editor keeps its in
KEY:PENDING; a chord the compositor handed over must not finish one somebody was
half way through typing into a document.")

(defclass wm (mode:mode) ()
  (:documentation "The chords the window manager owns: what a key means when it
was not typed at anything, because the compositor took it before whatever has
focus could see it.

A mode like any other, so a config binds one the way it binds a chord in text, and
a chord inherits down a class the same way."))

(defun pending () *pending*)

(defun chords ()
  "Every chord bound here. What the compositor has to be asked for is worked out
from this: a key it was never told about is one it will not hand over."
  (mapcar #'car (mode:bindings (make-instance 'wm))))

(defun dispatch (said)
  "Take a chord the compositor handed over."
  (let ((m (make-instance 'wm)))
    (loop :for k :in (ui:chord (princ-to-string said))
          :do (multiple-value-bind (answer so-far) (mode:dispatch m nil k
                                                                  (pending))
                (setf *pending* (if (eq answer :pending) so-far nil))
                (unless (eq answer :pending) (return answer))))))

(defun keys-node ()
  "Where a chord the compositor took arrives. Writing one here is pressing it, so a
keyboard, a test and another pine all press the same way."
  (node:answers "key"
              :reads (lambda () (ui:spelled (pending)))
              :writes #'dispatch
              :describes "write a chord here to press it"))

