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

(defun %until (stream close)
  (loop :for ch := (peek-char t stream t nil t)
        :until (char= ch close)
        :collect (read stream t nil t)
        :finally (read-char stream t nil t)))

(defun read-map (stream char)
  (declare (ignore char))
  (cons 'pine.data:map (%until stream #\})))

(defun read-seq (stream char)
  (declare (ignore char))
  (cons 'pine.data:seq (%until stream #\])))

(defun read-set (stream char arg)
  (declare (ignore char arg))
  (cons 'pine.data:set (%until stream #\})))

(defun read-close (stream char)
  (declare (ignore stream))
  (error "unmatched ~a" char))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char #\/ #'read-path t)
  (:macro-char #\{ #'read-map nil)
  (:macro-char #\} #'read-close nil)
  (:macro-char #\[ #'read-seq nil)
  (:macro-char #\] #'read-close nil)
  (:dispatch-macro-char #\# #\{ #'read-set))
