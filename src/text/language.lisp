(defpackage #:pine/text/language
  (:use #:cl)
  (:local-nicknames (#:doc #:pine/text/document))
  (:export #:package-of #:readtable-of #:reading))
(in-package #:pine/text/language)

(defun %named-after (document word)
  "The name written after the last WORD in this document. What is in force at the
end of a file is what the last one says."
  (let* ((text (doc:text document))
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

(defun package-of (document)
  "The package this document's forms are read in: the one its own (in-package)
names. What somebody's config defines lives there, not in CL, so this is what tells
the walk and the evaluator what a name in it means."
  (let ((said (%named-after document "in-package")))
    (or (and said (find-package (string-upcase (string-left-trim "#:" said))))
        (find-package :pine/user)
        (find-package :cl-user))))

(defun readtable-of (document)
  "The readtable this document is written in, from its own (in-readtable), or
nothing where it is written in the standard one."
  (let ((said (%named-after document "in-readtable")))
    (when said
      (or (ignore-errors
           (named-readtables:find-readtable
            (let ((*package* (find-package :cl-user)) (*read-eval* nil))
              (read-from-string said))))
          (ignore-errors
           (named-readtables:find-readtable
            (intern (string-upcase (string-left-trim "#:" said)) :keyword)))))))

(defun reading (document)
  "What reading a form out of DOCUMENT means: its package and its readtable."
  (values (package-of document) (or (readtable-of document) *readtable*)))
