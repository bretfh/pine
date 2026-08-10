(in-package :pine.test)

(def-suite* :pine.style :in :pine)

(defparameter +modules+ '("run" "fs" "world" "proc" "repl" "path" "ui" "ts" "edit"
                          "provider" "net" "app" "wayland" "wayland/app" "cairo")
  "The v2 modules. Each step adds its own; what is not here has not been ported.")

(defparameter +line-limit+ 400)

(defparameter +definers+
  '("defun" "defmacro" "defclass" "defmethod" "defgeneric" "define-condition"
    "defstruct" "deftype" "defsetf"))

(defun %module-root ()
  (merge-pathnames "src/" (asdf:system-source-directory :pine)))

(defun %lang-files ()
  (append (directory (merge-pathnames "ts/lang/*.lisp" (%module-root)))
          (directory (merge-pathnames "wayland/protocol/*.lisp" (%module-root)))))

(defun %files ()
  (remove-if (lambda (f) (char= #\. (char (file-namestring f) 0)))
             (append (list (merge-pathnames "boot.lisp" (%module-root))
                           (merge-pathnames "frontend.lisp" (%module-root)))
                     (loop :for module :in +modules+
                           :append (directory
                                    (merge-pathnames (format nil "~a/*.lisp" module)
                                                     (%module-root))))
                     (%lang-files))))

(defun %lines (file)
  (with-open-file (in file)
    (loop :for line = (read-line in nil nil) :while line :collect line)))

(defun %commented-lines (file)
  (let ((in-string nil) (found nil) (n 0))
    (dolist (line (%lines file) (nreverse found))
      (incf n)
      (loop :with i := 0
            :while (< i (length line))
            :for ch := (char line i)
            :do (cond ((char= ch #\\) (incf i 2))
                      ((and (char= ch #\#) (< (1+ i) (length line))
                            (char= (char line (1+ i)) #\\))
                       (incf i 3))
                      ((char= ch #\") (setf in-string (not in-string)) (incf i))
                      ((and (char= ch #\;) (not in-string))
                       (push n found)
                       (return))
                      (t (incf i)))))))

(defun %starts-with-any (line words)
  (let ((trimmed (string-left-trim " " line)))
    (and (plusp (length trimmed))
         (char= #\( (char trimmed 0))
         (some (lambda (w)
                 (let ((head (concatenate 'string "(" w " ")))
                   (and (>= (length trimmed) (length head))
                        (string-equal head trimmed :end2 (length head)))))
               words))))

(test v2-code-carries-no-commentary
  (let ((found nil))
    (dolist (file (%files))
      (dolist (n (%commented-lines file))
        (push (format nil "~a:~d" (file-namestring file) n) found)))
    (is (null found) "~d commented line~:p:~{~%  ~a~}" (length found) (reverse found))))

(test every-global-is-in-the-block-at-the-top
  (let ((loose nil))
    (dolist (file (%files))
      (let ((lines (%lines file))
            (first-definition nil)
            (last-global nil))
        (loop :for line :in lines
              :for n :from 1
              :do (when (and (null first-definition) (%starts-with-any line +definers+))
                    (setf first-definition n))
                  (when (%starts-with-any line '("defvar" "defparameter" "defconstant"))
                    (setf last-global n)))
        (when (and first-definition last-global (> last-global first-definition))
          (push (format nil "~a: a global at line ~d, below a definition at ~d"
                        (file-namestring file) last-global first-definition)
                loose))))
    (is (null loose) "~{~%  ~a~}" (reverse loose))))

(test no-file-is-longer-than-one-idea
  (let ((long nil))
    (dolist (file (%files))
      (let ((n (length (%lines file))))
        (when (> n +line-limit+)
          (push (format nil "~a: ~d lines" (file-namestring file) n) long))))
    (is (null long) "~d file~:p over ~d lines:~{~%  ~a~}"
        (length long) +line-limit+ (reverse long))))

(defparameter +declarations+ '("syntax.lisp" "commonlisp.lisp" "scheme.lisp"
                               "pine.lisp" "reader.lisp")
  "Where the sugar belongs: the language declarations, and the module that is it.")

(test the-operating-system-does-not-read-in-its-own-sugar
  (let ((using nil))
    (dolist (file (%files))
      (when (and (find-if (lambda (line) (search "named-readtables:in-readtable" line)) (%lines file))
                 (not (member (file-namestring file) +declarations+ :test #'equal)))
        (push (file-namestring file) using)))
    (is (null using) "~{~%  ~a declares a readtable~}" (reverse using))))
