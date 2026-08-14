(defpackage #:pine.provider.out
  (:use #:cl)
  (:export #:sh #:lines #:words #:number-in #:firstp #:has #:each-line
           #:*through*))

(in-package #:pine.provider.out)

(defvar *through* nil)

(defun sh (format &rest arguments)
  "Run a line and answer what it said. Through /sh when there is one, so a
provider's shell-outs are visible there and two providers asking the same
question in the same breath ask it once."
  (let ((line (apply #'format nil format arguments)))
    (when *through*
      (return-from sh (funcall *through* line)))
    (multiple-value-bind (out err code)
        (ignore-errors
         (uiop:run-program (list "sh" "-c" line)
                           :output '(:string :stripped t)
                           :error-output nil
                           :ignore-error-status t))
      (declare (ignore err code))
      (or out ""))))

(defun lines (text)
  (remove "" (uiop:split-string (or text "") :separator '(#\Newline))
          :test #'string=))

(defun words (text &optional (on #\Space))
  (remove "" (uiop:split-string (or text "") :separator (list on))
          :test #'string=))

(defun number-in (text)
  (let* ((text (or text ""))
         (start (position-if (lambda (c) (or (digit-char-p c) (char= c #\-))) text)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                      text :start (1+ start))
                     (length text))))
        (ignore-errors (read-from-string (subseq text start end)))))))

(defun has (command)
  (plusp (length (sh "command -v ~a 2>/dev/null" command))))

(defun firstp (text) (first (lines text)))

(defun each-line (text function)
  (mapcar function (lines text)))
