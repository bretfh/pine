(defpackage #:pine/word
            (:use #:cl)
            (:export #:lends #:lent #:user #:*name*))
(in-package #:pine/word)

(defparameter *name* :pine/user)
(defvar *lent* nil
  "Every word offered to the language somebody writes their own package in, in the
order it was offered: (HOME WORD FROM). Newest last, so a word offered twice is
the later one, and a package loaded later brings its own words with it.")

(defun lends (&rest names)
  "Say this package lends NAMES to whoever writes their own.

Said in the file the code is in, because that is where it is known: what pine's
language has is the sum of what pine loaded, not a list somewhere else that has to
be kept up with it.

A name is a string, and it is the name the symbol already has: a word is lent as
itself. Lending one under another name used to be possible, and it copied a
FDEFINITION, so redefining the original at the repl left the language holding the
old one.

Once the language stands, lending a word puts it there at once: a system loaded
after it can write in the words a system loaded before it lent."
  (let ((home (package-name *package*)))
    (dolist (each names)
      (let ((row (list home (string-upcase each))))
        (setf *lent* (append (remove row *lent* :test #'equal) (list row)))))
    (when (find-package *name*) (user))
    names))

(defun lent () *lent*)

(defun user ()
  "Build the package somebody writes their own in, and answer it and whatever two
packages both claimed a word for.

It is a language, not a grab bag: it carries Common Lisp and every word pine
lends, so a package that uses this one and nothing else can say everything the
editor can say.
"
  (let ((p (or (find-package *name*) (make-package *name* :use '(:cl))))
        (claimed (make-hash-table :test 'equal))
        (clashes nil)
        (names nil))
    (loop :for (home word) :in (lent)
          :for package := (find-package home)
          :for symbol := (and package (find-symbol word package))
          :when symbol
          :do (let ((had (gethash word claimed)))
                (when (and had (not (equal had home)))
                  (pushnew (format nil "~a is ~a's and ~a's" word had home)
                           clashes :test #'equal))
                (setf (gethash word claimed) home)
                (shadowing-import symbol p)
                (pushnew word names :test #'equal)))
    (do-external-symbols (symbol (find-package :cl))
                         (pushnew (symbol-name symbol) names :test #'equal))
    (dolist (word names)
      (multiple-value-bind (symbol status) (find-symbol word p)
                           (when status
                             (handler-case (export (list symbol) p)
                               (sb-ext:name-conflict ()
                                                     (pushnew
                                                      (format nil "~a is already something else where the language
is used" word)
                                                      clashes :test #'equal))))))
    (values p (reverse clashes))))
