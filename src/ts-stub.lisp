(in-package #:pine.ts)

(defstruct (ts-runtime (:constructor %make-ts-runtime)))

(defun make-ts-runtime () (%make-ts-runtime))
(defun ts-loaded-p (rt) (declare (ignore rt)) nil)
(defun ensure-ts (rt) (declare (ignore rt)) nil)
(defun ensure-language (rt language) (declare (ignore rt language)) nil)
(defun compute-highlights (rt language text)
  (declare (ignore rt language text))
  nil)
(defun capture-name-to-face (name) (declare (ignore name)) nil)
