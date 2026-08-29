(defpackage #:pine/fs/reader
  (:use #:cl)
  (:local-nicknames (#:path #:pine/fs/path))
  (:export
   #:syntax))
(in-package #:pine/fs/reader)

(defvar +stops+
  '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\' #\` #\, #\;)
  "What ends a path.")

(defun %token (stream)
  (with-output-to-string (out)
    (loop :for ch := (peek-char nil stream nil nil)
          :while (and ch (not (member ch +stops+)))
          :do (write-char (read-char stream) out))))

(defun read-path (stream char)
  (declare (ignore char))
  (let ((text (%token stream)))
    (if (zerop (length text))
        '/
        (list 'path:path (concatenate 'string "/" text)))))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char #\/ #'read-path t))
