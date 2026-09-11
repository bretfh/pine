(defpackage #:pine/run/libs
  (:use #:cl)
  (:local-nicknames (#:fault #:pine/run/fault))
  (:export
   #:attend))
(in-package #:pine/run/libs)

(defparameter +built-in+ (sb-ext:posix-getenv "GUIX_ENVIRONMENT"))

(defparameter +under+ '("lib" "lib/tree-sitter"))

(defun built-in () +built-in+)

(defun dirs ()
  (let ((roots (remove nil (list (sb-ext:posix-getenv "PINE_LIB")
                                 (sb-ext:posix-getenv "GUIX_ENVIRONMENT")
                                 +built-in+
                                 (fault:or-nothing
                                     "a saved image has no source tree"
                                   (handler-bind ((warning #'muffle-warning))
                                     (namestring
                                      (asdf:system-source-directory :pine))))))))
    (remove-duplicates
     (loop :for root :in roots
           :append (loop :for under :in +under+
                         :collect (format nil "~a/~a/" (string-right-trim "/" root)
                                          under)))
     :test #'equal)))

(defun attend ()
  (dolist (dir (dirs) cffi:*foreign-library-directories*)
    (let ((path (pathname dir)))
      (pushnew path cffi:*foreign-library-directories* :test #'equal))))
