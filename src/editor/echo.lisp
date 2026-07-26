(defpackage #:pine.editor.echo
  (:use :cl)
  (:export #:message #:current-message
           #:show-prompt #:hide-prompt
           #:prompt-active-p #:prompt-text))

(in-package #:pine.editor.echo)

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
