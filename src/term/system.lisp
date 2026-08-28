(defpackage #:pine/term
  (:use #:cl)
  (:local-nicknames (#:edit #:pine/edit)
                    (#:text #:pine/text)
                    (#:node #:pine/fs/node)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:command #:pine/run/command)
                    (#:terminal #:pine/term/terminal))
  (:export #:term #:current))
(in-package #:pine/term)

(defvar *counter* 0)

(defclass term (system:system) ()
  (:documentation "Programs with screens of their own, as documents.

A terminal is a document and a thread at once, so a window shows one the way it
shows any other and a command acts on one the same way."))

(system:offers 'term)

(defun current ()
  (let ((it (text:current)))
    (and (typep it 'terminal:terminal) it)))

(defun %fit (term win)
  "Give the program the size of the window showing it, so what it draws is what
fits."
  (when (and term win)
    (terminal:resize term (max 1 (edit:across win)) (max 1 (edit:down win)))))

(defun %open (&key runs name)
  (let* ((name (or name (format nil "*shell*~[~:;-~:*~d~]" (incf *counter*))))
         (win (edit:focused))
         (term (terminal:open-terminal name :runs runs
                                            :wide (if win (edit:across win) 80)
                                            :tall (if win (edit:down win) 24))))
    (setf (text:current) term)
    (when win (edit:show win term))
    (%fit term win)
    term))

(command:defcommand "terminal" (&optional line)
    (:describes "a program with a screen of its own, in a document"
     :on '(text "C-x t"))
  (node:full-name (%open :runs (and line (princ-to-string line)))))

(command:defcommand "terminal-interrupt" ()
    (:describes "interrupt what the terminal is running" :on '(shell "C-c C-c"))
  (let ((term (current)))
    (when term (terminal:send term (string (code-char 3))) t)))

(command:defcommand "terminal-close" ()
    (:describes "end this terminal" :on '(shell "C-c C-k"))
  (let ((term (current)))
    (when term
      (job:stop term)
      (job:forget (node:name term))
      (text:kill (node:name term))
      t)))

(command:defcommand "terminals" () (:describes "every terminal there is")
  (loop :for each :in (terminal:terminals)
        :collect (list (node:name each) (terminal:runs each)
                       (job:state each))))

(defmethod job:start ((s term)) s)

(defmethod job:stop ((s term))
  (dolist (each (terminal:terminals))
    (job:stop each)
    (job:forget (node:name each))
    (text:kill (node:name each)))
  s)
