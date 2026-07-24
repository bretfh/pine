(in-package #:pine.echo)

(defvar *message* "")
(defvar *prompt* nil)

(defun message (text)
  (setf *message* (or text ""))
  nil)

(defun current-message () *message*)

(defun show-prompt (prompt)
  (setf *prompt* (or prompt "")
        *message* "")
  nil)

(defun hide-prompt ()
  (setf *prompt* nil)
  nil)

(defun prompt-active-p () (and *prompt* t))
(defun prompt-text () *prompt*)
