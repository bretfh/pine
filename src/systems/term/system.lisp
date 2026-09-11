(in-package #:pine/term)

(defvar *counter* 0)

(defclass term (module:module) ())

(defun current ()
  (let ((it (text:current)))
    (and (typep it 'terminal) it)))

(defun %fit (term win)
  (when (and term win)
    (resize term (max 1 (edit:width win)) (max 1 (edit:height win)))))

(defun %open (&key runs name)
  (let* ((name (or name (format nil "*shell*~[~:;-~:*~d~]"
                                 (sb-ext:atomic-update *counter* (lambda (old) (1+ old))))))
         (win (edit:focused))
         (term (open-terminal name :runs runs
                                            :width (if win (edit:width win) 80)
                                            :height (if win (edit:height win) 24))))
    (setf (text:current) term)
    (when win (edit:show win term))
    (%fit term win)
    term))

(command:defcommand "terminal" (&optional line)
    (:describes "a program with a screen of its own, in a buffer"
     :on '(text "C-x t"))
  (fs:full-name (%open :runs (and line (princ-to-string line)))))

(command:defcommand "terminal-interrupt" ()
    (:describes "interrupt what the terminal is running" :on '(shell "C-c C-c"))
  (let ((term (current)))
    (when term (send term (string (code-char 3))) t)))

(command:defcommand "terminal-close" ()
    (:describes "end this terminal" :on '(shell "C-c C-k"))
  (let ((term (current)))
    (when term
      (job:stop term)
      (job:forget (fs:name term))
      (text:kill (fs:name term))
      t)))

(command:defcommand "terminals" () (:describes "every terminal there is")
  (loop :for each :in (terminals)
        :collect (list (fs:name each) (runs each)
                       (job:state each))))

(defmethod job:start ((s term)) s)

(defmethod job:stop ((s term))
  (dolist (each (terminals))
    (job:stop each)
    (job:forget (fs:name each))
    (text:kill (fs:name each)))
  s)
