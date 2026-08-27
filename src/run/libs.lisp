(defpackage #:pine/run/libs
  (:use #:cl)
  (:local-nicknames (#:fault #:pine/run/fault))
  (:export #:built-in #:dirs #:attend))
(in-package #:pine/run/libs)

(defparameter +built-in+ (sb-ext:posix-getenv "GUIX_ENVIRONMENT")
  "The profile pine was built against. Read as this file loads, which is inside
the build environment, and kept by the saved image: a binary run from a
terminal that never entered that environment still knows where its C libraries
are.")

(defparameter +under+ '("lib" "lib/tree-sitter")
  "Where a shared library sits under a profile.")

(defun built-in () +built-in+)

(defun dirs ()
  "Every directory a foreign library of pine's may be in: what PINE_LIB says,
the profile this image is running under, the one it was built against, and
pine's own tree."
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
  "Say where pine's C libraries are before anything asks for one."
  (dolist (dir (dirs) cffi:*foreign-library-directories*)
    (let ((path (pathname dir)))
      (pushnew path cffi:*foreign-library-directories* :test #'equal))))
