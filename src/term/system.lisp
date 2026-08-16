(defpackage #:pine/term
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:command #:pine/run/command) (#:log #:pine/run/log)
                    (#:doc #:pine/text/document) (#:mode #:pine/text/mode)
                    (#:window #:pine/edit/window)
                    (#:emode #:pine/term/mode)
                    (#:terminal #:pine/term/terminal))
  (:export #:term #:current))
(in-package #:pine/term)

(defvar *counter* 0)

(defparameter +keys+
  '(("C-c C-c" . "terminal-interrupt")
    ("C-c C-k" . "terminal-close"))
  "What a chord means in a terminal beyond what the program takes. Everything
else goes to the program, because that is what PRESS on this mode does.")

(defclass term (system:system) ()
  (:documentation "Programs with screens of their own, as documents.

A terminal is a document and a thread at once, so a window shows one the way it
shows any other and a command acts on one the same way."))

(system:offers 'term)

(defun current ()
  (let ((it (doc:current)))
    (and (typep it 'terminal:terminal) it)))

(defun %fit (term win)
  "Give the program the size of the window showing it, so what it draws is what
fits."
  (when (and term win)
    (terminal:resize term (max 1 (window:cols win)) (max 1 (window:lines win)))))

(defun %open (&key runs name)
  (let* ((name (or name (format nil "*shell*~[~:;-~:*~d~]" (incf *counter*))))
         (win (window:focused))
         (term (terminal:open-terminal name :runs runs
                                            :wide (if win (window:cols win) 80)
                                            :tall (if win (window:lines win) 24))))
    (setf (doc:current) term)
    (when win (window:show win term))
    (%fit term win)
    term))

(defmethod job:start ((s term))
  (setf emode:*send* (lambda (document said) (terminal:send document said)))
  (command:defcommand "terminal" (&optional line)
      (:describes "a program with a screen of its own, in a document")
    (node:full-name (%open :runs (and line (princ-to-string line)))))
  (command:defcommand "terminal-interrupt" ()
      (:describes "interrupt what the terminal is running")
    (let ((term (current)))
      (when term (terminal:send term (string (code-char 3))) t)))
  (command:defcommand "terminal-close" () (:describes "end this terminal")
    (let ((term (current)))
      (when term
        (job:stop term)
        (job:forget (node:name term))
        (doc:kill (node:name term))
        t)))
  (command:defcommand "terminals" () (:describes "every terminal there is")
    (loop :for each :in (terminal:terminals)
          :collect (list (node:name each) (terminal:runs each)
                         (job:state each))))
  (loop :for (chord . name) :in +keys+ :do (mode:bind 'emode:shell chord name))
  (mode:bind 'mode:text "C-x t" "terminal")
  s)

(defmethod job:stop ((s term))
  (setf emode:*send* nil)
  (dolist (each (terminal:terminals))
    (job:stop each)
    (job:forget (node:name each))
    (doc:kill (node:name each)))
  (dolist (name '("terminal" "terminal-interrupt" "terminal-close" "terminals"))
    (command:forget name))
  s)
