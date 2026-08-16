(defpackage #:pine/wm/niri
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:sh #:pine/host/shell)
                    (#:compositor #:pine/wm/compositor))
  (:export #:niri #:json))
(in-package #:pine/wm/niri)

(defparameter +actions+
  '(("overview"  . "toggle-overview")
    ("close"     . "close-window")
    ("expel"     . "consume-or-expel-window-right")
    ("consume"   . "consume-or-expel-window-left")
    ("workspace" . "focus-workspace")
    ("window"    . "focus-window --id")
    ("exit"      . "quit --skip-confirmation")))

(defclass niri (compositor:compositor) ()
  (:documentation "niri, over its own json protocol."))

(defmethod node:announces ((c niri)) (list "niri msg --json event-stream"))

(defun json (text)
  (when (and text (plusp (length text)))
    (ignore-errors (com.inuoe.jzon:parse text))))

(defun %list (command)
  (let ((value (json (sh:sh "niri msg --json ~a" command))))
    (when (vectorp value) (coerce value 'list))))

(defmethod compositor:workspaces ((c niri)) (%list "workspaces"))

(defmethod compositor:windows ((c niri)) (%list "windows"))

(defmethod compositor:focused ((c niri))
  (let ((found (find-if (lambda (w) (gethash "is_focused" w))
                        (compositor:windows c))))
    (when found (princ-to-string (gethash "id" found)))))

(defmethod compositor:titled ((c niri) id)
  (let ((found (find-if (lambda (w)
                          (equal (princ-to-string id)
                                 (princ-to-string (gethash "id" w ""))))
                        (compositor:windows c))))
    (when found (gethash "title" found))))

(defmethod compositor:focus ((c niri) id)
  (compositor:act c "window" id))

(defmethod compositor:verbs ((c niri)) (mapcar #'car +actions+))

(defmethod compositor:act ((c niri) verb &rest arguments)
  (let ((action (cdr (assoc (princ-to-string verb) +actions+ :test #'equal))))
    (when action
      (sh:sh "niri msg action ~a~{ ~a~}" action arguments)
      t)))
