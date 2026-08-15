(defpackage #:pine/edit/repl
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:cmd #:pine/repl/command)
                    (#:mode #:pine/repl/mode) (#:node #:pine/fs/node)
                    (#:session #:pine/repl/session) (#:buffer #:pine/edit/buffer)
                    (#:window #:pine/edit/window) (#:log #:pine/run/log))
  (:export #:install #:open-repl #:submit #:sessions #:session-for #:*name*))
(in-package #:pine/edit/repl)

(defvar *sessions* (d:box nil))
(defparameter *name* "*repl*")
(defparameter +prompt+ "pine> ")

(defun sessions () (d:all *sessions*))

(defun session-for (name)
  (or (cdr (assoc name (sessions) :test #'equal))
      (let ((s (session:open-session
                :name name
                :package (or (find-package :pine/user) (find-package :cl-user)))))
        (d:swap! *sessions* (lambda (all) (acons name s all)))
        s)))

(defun %say (b text)
  (buffer:move! b :buffer 1)
  (buffer:insert! b text))

(defun %prompt (b)
  (let ((line (buffer:line b (buffer:point-line b))))
    (unless (equal line +prompt+)
      (%say b (format nil "~%~a" +prompt+)))))

(defun %typed (b)
  "What was typed after the last prompt: the form this line submits."
  (let* ((line (buffer:line b (buffer:point-line b)))
         (at (search +prompt+ line)))
    (if at (subseq line (+ at (length +prompt+))) line)))

(defun submit (b &rest arguments)
  (declare (ignore arguments))
  (when (equal *name* (node:name b))
    (let ((text (%typed b))
          (s (session-for (node:name b))))
      (if (zerop (length (string-trim " " text)))
          (%say b (format nil "~%~a" +prompt+))
          (let ((e (session:evaluate s (session:read s text))))
            (%say b (format nil "~%~a~a~%~a"
                            (session:said e)
                            (if (session:fault e)
                                (format nil "; ~a" (session:fault e))
                                (format nil "~{~s~^, ~}" (session:answered e)))
                            +prompt+))))
      :submitted)))

(defun open-repl ()
  (let ((b (or (buffer:buffer-named *name*)
               (buffer:make-buffer *name* :mode "repl"))))
    (setf (buffer:mode-of b) "repl")
    (when (zerop (length (buffer:text-of b)))
      (setf (node:contents b) +prompt+))
    (buffer:move! b :buffer 1)
    (setf (buffer:current) b)
    (window:show! (window:focused) b)
    b))

(defun install ()
  (mode:mode "repl" :parent "lisp" :settings '(:indicator "REPL" :grammar :commonlisp))
  (mode:handle "repl" :newline #'submit)
  (cmd:defcommand "open-repl" () (:describes "a lisp prompt in a buffer")
    (node:name (open-repl)))
  (cmd:defcommand "repl-clear" () (:describes "empty the repl buffer")
    (let ((b (buffer:buffer-named *name*)))
      (when b
        (setf (node:contents b) +prompt+)
        (buffer:move! b :buffer 1))
      (and b t)))
  (mode:bind "text" "C-x r" "open-repl")
  t)
