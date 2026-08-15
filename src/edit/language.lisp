(defpackage #:pine/edit/language
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:buffer #:pine/edit/buffer))
  (:export #:package-of #:readtable-of #:reading #:said #:forget))
(in-package #:pine/edit/language)

(defvar *said* (d:table))

(defun %named-after (b word)
  "The name the buffer gives after the last WORD in it, as it is written. What
is in force at the end of a file is what the last one says, so this reads the
lines backwards and stops at the first one that says anything."
  (let ((lines (d:held (buffer:lines b))))
    (loop :for n :downfrom (1- (d:size lines)) :to 0
          :for line := (d:at lines n)
          :for at := (search word line :test #'char-equal)
          :when at
            :do (let* ((from (position-if-not
                              (lambda (ch) (member ch '(#\Space #\Tab)))
                              line :start (+ at (length word))))
                       (to (and from (or (position-if
                                          (lambda (ch)
                                            (member ch '(#\Space #\Tab #\( #\))))
                                          line :start from)
                                         (length line)))))
                  (when (and from to (< from to))
                    (return (subseq line from to)))))))

(defun %readtable-named (said)
  (when said
    (or (ignore-errors
         (named-readtables:find-readtable
          (let ((*package* (find-package :cl-user)) (*read-eval* nil))
            (read-from-string said))))
        (ignore-errors
         (named-readtables:find-readtable
          (intern (string-upcase (string-left-trim "#:" said)) :keyword))))))

(defun said (b)
  "What this buffer says it is written in: (PACKAGE . READTABLE), worked out
once per edit. Every frame asks, and reading a hundred thousand lines to answer
is the frame."
  (let ((had (d:at (d:all *said*) (node:name b)))
        (tick (buffer:tick b)))
    (if (and had (eql (car had) tick))
        (cdr had)
        (let* ((package (let ((name (%named-after b "in-package")))
                          (or (and name
                                   (find-package
                                    (string-upcase (string-left-trim "#:" name))))
                              (find-package :pine/user)
                              (find-package :cl-user))))
               (readtable (%readtable-named (%named-after b "in-readtable")))
               (answer (cons package readtable)))
          (d:keep! *said* (node:name b) (cons tick answer))
          answer))))

(defun forget (b)
  (d:drop! *said* (node:name b))
  b)

(defun package-of (b)
  "The package this buffer's forms are read in: the one its own (in-package)
names. A config's macros live there, not in CL, so this is what tells the
highlighter and the evaluator what a name in it means."
  (car (said b)))

(defun readtable-of (b)
  "The readtable this buffer is written in, from its own (in-readtable), or nil
where it is written in the standard one."
  (cdr (said b)))

(defun reading (b)
  "What reading a form out of B means: its package and its readtable."
  (let ((it (said b)))
    (values (car it) (or (cdr it) *readtable*))))
