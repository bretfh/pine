(defpackage #:pine/edit/language
  (:use #:cl)
  (:local-nicknames (#:buffer #:pine/edit/buffer))
  (:export #:package-of #:readtable-of #:reading))
(in-package #:pine/edit/language)

(defun %named-after (b word)
  "The name the buffer gives after the last WORD in it, as it is written. What
is in force at the end of a file is what the last one says."
  (let* ((text (buffer:text-of b))
         (at (search word text :from-end t :test #'char-equal)))
    (when at
      (let* ((from (position-if-not
                    (lambda (ch) (member ch '(#\Space #\Tab #\Newline)))
                    text :start (+ at (length word))))
             (to (and from (or (position-if
                                (lambda (ch)
                                  (member ch '(#\Space #\Tab #\Newline #\( #\))))
                                text :start from)
                               (length text)))))
        (when (and from to (< from to)) (subseq text from to))))))

(defun package-of (b)
  "The package this buffer's forms are read in: the one its own (in-package)
names. A config's macros live there, not in CL, so this is what tells the
highlighter and the evaluator what a name in it means."
  (let ((said (%named-after b "in-package")))
    (or (and said (find-package (string-upcase (string-left-trim "#:" said))))
        (find-package :pine.user)
        (find-package :cl-user))))

(defun readtable-of (b)
  "The readtable this buffer is written in, from its own (in-readtable), or nil
where it is written in the standard one."
  (let ((said (%named-after b "in-readtable")))
    (when said
      (or (ignore-errors
           (named-readtables:find-readtable
            (let ((*package* (find-package :cl-user)) (*read-eval* nil))
              (read-from-string said))))
          (ignore-errors
           (named-readtables:find-readtable
            (intern (string-upcase (string-left-trim "#:" said)) :keyword)))))))

(defun reading (b)
  "What reading a form out of B means: its package and its readtable."
  (values (package-of b) (or (readtable-of b) *readtable*)))
