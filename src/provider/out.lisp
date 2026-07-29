(defpackage #:pine.provider.out
  (:use #:cl)
  (:shadow #:number)
  (:export #:sh #:lines #:split #:starts-with #:number #:percent #:json
           #:field #:int-file))

(in-package #:pine.provider.out)

;;;; What a command said, taken apart.
;;;;
;;;; A device provider is a shell command and a way of reading its output, and
;;;; the reading is the same handful of moves every time: the first number in a
;;;; line, the field after a colon, a percentage, some JSON. They are here so a
;;;; provider is the paths it serves and not a page of string handling.

(defun sh (control &rest arguments)
  "Run the command CONTROL formats and answer what it said, trimmed. A command
that is not there or fails says nothing, because a device that is absent is a
path with no value rather than an error."
  (let ((command (if arguments (apply #'format nil control arguments) control)))
    (or (ignore-errors
          (string-trim '(#\newline #\space)
                       (uiop:run-program (list "sh" "-c" command)
                                         :output :string
                                         :error-output nil
                                         :ignore-error-status t)))
        "")))

(defun split (text ch &optional limit)
  "TEXT split on CH into at most LIMIT parts; the last keeps any remaining CH."
  (let ((parts nil) (start 0) (n 0))
    (dotimes (i (length text))
      (when (and (char= (char text i) ch) (or (null limit) (< (1+ n) limit)))
        (push (subseq text start i) parts)
        (setf start (1+ i))
        (incf n)))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun lines (text)
  "TEXT's non-empty lines."
  (remove "" (split text #\newline) :test #'string=))

(defun starts-with (text prefix)
  (and (>= (length text) (length prefix))
       (string= text prefix :end1 (length prefix))))

(defun number (text)
  "The first number in TEXT, integer or decimal, or NIL."
  (let ((start (position-if (lambda (c) (or (digit-char-p c) (char= c #\.)))
                            text)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c)
                                                      (char= c #\.)))
                                      text :start start)
                     (length text))))
        (ignore-errors (read-from-string (subseq text start end)))))))

(defun percent (value)
  "VALUE as 0..100, whether it came back as a fraction or as a percentage."
  (let ((n (if (stringp value) (number value) value)))
    (when n
      (max 0 (min 100 (round (if (<= n 1) (* 100 n) n)))))))

(defun field (text name &optional (separator #\:))
  "The value after NAME on the line that starts with it."
  (loop :for line :in (lines text)
        :when (starts-with (string-left-trim " " line) name)
          :return (string-trim " " (second (split line separator 2)))))

(defun json (text)
  "TEXT as JSON, or NIL when it is not."
  (ignore-errors (com.inuoe.jzon:parse text)))

(defun int-file (path)
  "The integer a sysfs file holds, or NIL."
  (when (probe-file path)
    (ignore-errors (parse-integer (string-trim '(#\newline #\space)
                                               (uiop:read-file-string path))))))
