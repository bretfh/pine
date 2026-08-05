(defpackage #:pine.provider.out
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:shadow #:number)
  (:export #:sh #:run #:file #:int-file #:lines #:split #:starts-with
           #:number #:percent #:json #:field))

(in-package #:pine.provider.out)
(named-readtables:in-readtable pine.path:syntax)

(defun sh (control &rest arguments)
  (let ((command (if arguments (apply #'format nil control arguments) control)))
    (or (ns:read (p:path /sh command)) "")))

(defun run (control &rest arguments)
  (let ((command (if arguments (apply #'format nil control arguments) control)))
    (ns:write (p:path /sh command) t)))

(defun file (path)
  (ns:read (p:path /file (p:spliced path))))

(defun split (text ch &optional limit)
  (pine.data:split text :on ch :limit (and limit (1- limit))))

(defun lines (text)
  (pine.data:split text :on #\newline :empties nil))

(defun starts-with (text prefix)
  (and (>= (length text) (length prefix))
       (string= text prefix :end1 (length prefix))))

(defun number (text)
  (let ((start (position-if (lambda (c) (or (digit-char-p c) (char= c #\.)))
                            text)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c)
                                                      (char= c #\.)))
                                      text :start start)
                     (length text))))
        (ignore-errors (read-from-string (subseq text start end)))))))

(defun int-file (path)
  (let ((text (file path)))
    (when (stringp text)
      (let ((n (number text))) (and n (round n))))))

(defun percent (value)
  (let ((n (if (stringp value) (number value) value)))
    (when n
      (max 0 (min 100 (round (if (<= n 1) (* 100 n) n)))))))

(defun field (text name &optional (separator #\:))
  (loop :for line :in (lines text)
        :when (starts-with (string-left-trim " " line) name)
          :return (string-trim " " (second (split line separator 2)))))

(defun json (text)
  (ignore-errors (com.inuoe.jzon:parse text)))
