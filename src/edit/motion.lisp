(defpackage #:pine.edit.motion
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:cmd #:pine.repl.command)
                    (#:mode #:pine.repl.mode) (#:buffer #:pine.edit.buffer)
                    (#:parser #:pine.ts.parser) (#:runtime #:pine.ts.runtime)
                    (#:log #:pine/run/log))
  (:export #:install #:toward #:times #:*count* #:counting #:reset! #:digit!
           #:negative! #:universal!))

(in-package #:pine.edit.motion)

(defvar *count* (d:box nil))
(defparameter +kinds+
  '(:forward-sexp :backward-sexp :beginning-of-defun :end-of-defun))

(defun counting () (d:held *count*))

(defun reset! () (d:put! *count* nil))

(defun times (&optional (default 1))
  "How many times the next command runs, and forget it. A prefix argument is
spent by whoever asks for it."
  (let ((held (counting)))
    (reset!)
    (cond ((null held) default)
          ((eq held :more) 4)
          ((eq held :minus) -1)
          (t held))))

(defun universal! ()
  (let ((held (counting)))
    (d:put! *count* (cond ((null held) :more)
                         ((eq held :more) 16)
                         ((integerp held) (* held 4))
                         (t :more)))))

(defun digit! (n)
  (d:swap! *count* (lambda (held)
                    (cond ((integerp held) (+ (* 10 held) n))
                          ((eq held :minus) (- n))
                          (t n)))))

(defun negative! ()
  (d:swap! *count* (lambda (held) (if (integerp held) (- held) :minus))))

(defun toward (b kind)
  "Where KIND says to go from point, asked of the parse. Nil when the buffer is
in no language or the parse has nothing to say."
  (let ((p (parser:parser-for b)))
    (when p
      (parser:wait b)
      (multiple-value-bind (line col)
          (runtime:parse-motion (parser:state-of p) kind
                                (buffer:point-line b) (buffer:point-col b))
        (when line (list line col))))))

(defun %go (b kind)
  (let ((there (toward b kind)))
    (if there
        (buffer:goto! b (first there) (second there))
        (log:note "the parse says nothing there"))))

(defun install ()
  (cmd:defcommand "forward-sexp" () (:describes "over the form after point")
    (%go (buffer:current) :forward-sexp))
  (cmd:defcommand "backward-sexp" () (:describes "back over the form before point")
    (%go (buffer:current) :backward-sexp))
  (cmd:defcommand "beginning-of-defun" () (:describes "to the top of this definition")
    (%go (buffer:current) :beginning-of-defun))
  (cmd:defcommand "end-of-defun" () (:describes "to the end of this definition")
    (%go (buffer:current) :end-of-defun))
  (cmd:defcommand "mark-sexp" () (:describes "the region is the form after point")
    (let* ((b (buffer:current))
           (there (toward b :forward-sexp)))
      (when there
        (buffer:mark! b)
        (buffer:goto! b (first there) (second there)))))
  (cmd:defcommand "universal-argument" () (:describes "the next command, four times")
    (universal!)
    (log:note "C-u~@[ ~a~]" (counting)))
  (cmd:defcommand "negative-argument" () (:describes "the next command, backwards")
    (negative!)
    (log:note "C--"))
  (dolist (n '(0 1 2 3 4 5 6 7 8 9))
    (let ((n n))
      (cmd:command (format nil "digit-argument-~d" n)
                   (lambda () (digit! n) (log:note "C-u ~a" (counting)))
                   :describes "a digit of the count the next command takes")))
  (dolist (pair '(("C-M-f" . "forward-sexp") ("C-M-b" . "backward-sexp")
                  ("C-M-a" . "beginning-of-defun") ("C-M-e" . "end-of-defun")
                  ("C-M-SPC" . "mark-sexp")))
    (mode:bind "prog" (car pair) (cdr pair)))
  (mode:bind "text" "C-u" "universal-argument")
  (mode:bind "text" "M--" "negative-argument")
  (dolist (n '(0 1 2 3 4 5 6 7 8 9))
    (mode:bind "text" (format nil "M-~d" n) (format nil "digit-argument-~d" n)))
  t)
