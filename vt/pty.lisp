;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock.term)

(cffi:defcstruct (pty-winsize :size 8)
  (row    :uint16 :offset 0)
  (col    :uint16 :offset 2)
  (xpixel :uint16 :offset 4)
  (ypixel :uint16 :offset 6))

(defconstant +tiocswinsz+
  #+darwin #x80087467
  #+linux  #x00005414)

(cffi:defcfun ("ioctl" pty-ioctl) :int
  (fd :int) (request :unsigned-long) &rest)

(defun set-pty-size (fd rows cols)
  (cffi:with-foreign-object (ws '(:struct pty-winsize))
    (cffi:with-foreign-slots ((row col xpixel ypixel) ws (:struct pty-winsize))
      (setf row rows col cols xpixel 0 ypixel 0))
    (pty-ioctl fd +tiocswinsz+ :pointer ws)))

(defun make-terminal-env (&optional (term-type "xterm-256color"))
  (let ((env (hemlock.text:posix-environ)))
    (flet ((set-var (name value)
             (setf env (cons (format nil "~A=~A" name value)
                             (remove-if (lambda (e)
                                          (and (> (length e) (1+ (length name)))
                                               (string= e name
                                                        :end1 (length name))
                                               (char= (char e (length name))
                                                      #\=)))
                                        env)))))
      (set-var "TERM" term-type)
      (set-var "COLORTERM" "truecolor")
      (set-var "TERMCAP" "")
      (set-var "INSIDE_HEMLOCK" "")
      env)))

(defun spawn-pty-process (command &key (rows 24) (cols 80))
  (multiple-value-bind (master agent agent-name)
      (hemlock::find-a-pty)
    (set-pty-size master rows cols)
    (let ((pc (hemlock::make-process-connection
               (list "/bin/sh" "-c" command)
               :agent-pty-name agent-name
               :agent-fd agent
               :environment (make-terminal-env))))
      (hemlock.text:posix-close agent)
      (values master pc))))
