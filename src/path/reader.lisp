(defpackage #:pine.path.reader
  (:use #:cl)
  (:local-nicknames (#:path #:pine.path.path))
  (:export #:syntax #:read-path))

(in-package #:pine.path.reader)

(defvar +stops+
  '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\' #\` #\, #\;))

(defun %token (stream)
  (with-output-to-string (out)
    (loop :for ch := (peek-char nil stream nil nil)
          :while (and ch (not (member ch +stops+)))
          :do (write-char (read-char stream) out))))

(defun read-path (stream char)
  (declare (ignore char))
  (let ((text (%token stream)))
    (if (zerop (length text))
        (list 'function '/)
        (list 'path:parse (concatenate 'string "/" text)))))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char #\/ #'read-path t))
