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

A name is a string. It may be (WORD FROM) to lend FROM under another word, which
is how PINE:WRITE-AT is READ and WRITE there."
  (let ((home (package-name *package*)))
    (dolist (each names names)
      (destructuring-bind (word &optional (from word))
          (if (consp each) each (list each))
        (let ((row (list home (string-upcase word) (string-upcase from))))
          (setf *lent* (append (remove row *lent* :test #'equal) (list row))))))))

(defun lent () *lent*)

(defun %renamed (word symbol into)
  "WORD in INTO, standing for SYMBOL under a different name. Only what it does
carries over: a word lent under another name is a verb, and a verb is what it is
fbound to."
  (shadow (list word) into)
  (let ((it (intern word into)))
    (when (fboundp symbol) (setf (fdefinition it) (fdefinition symbol)))
    it))

(defun user ()
  "Build the package somebody writes their own in, and answer it and whatever two
packages both claimed a word for.

It is a language, not a grab bag: it carries Common Lisp and every word pine
lends, so a package that uses this one and nothing else can say everything the
editor can say.

  (defpackage #:notes (:use #:pine/user))"
  (let ((p (or (find-package *name*) (make-package *name* :use '(:cl))))
        (claimed (make-hash-table :test 'equal))
        (clashes nil)
        (names nil))
    (loop :for (home word from) :in (lent)
          :for package := (find-package home)
          :for symbol := (and package (find-symbol from package))
          :when symbol
            :do (let ((had (gethash word claimed)))
                  (when (and had (not (equal had home)))
                    (pushnew (format nil "~a is ~a's and ~a's" word had home)
                             clashes :test #'equal))
                  (setf (gethash word claimed) home)
                  (if (string= word from)
                      (shadowing-import symbol p)
                      (%renamed word symbol p))
                  (pushnew word names :test #'equal)))
    (do-external-symbols (symbol (find-package :cl))
      (pushnew (symbol-name symbol) names :test #'equal))
    (dolist (word names)
      (multiple-value-bind (symbol status) (find-symbol word p)
        (when status
          (handler-case (export (list symbol) p)
            (sb-ext:name-conflict ()
              (pushnew (format nil "~a is already something else where the language
is used" word)
                       clashes :test #'equal))))))
    (values p (reverse clashes))))
