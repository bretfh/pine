(in-package #:pine/term)

(defvar *counter* 0)

(defclass term (system:system) ()
  (:documentation "Programs with screens of their own, as documents.

A terminal is a document and a thread at once, so a window shows one the way it
shows any other and a command acts on one the same way."))


(defun current ()
  (let ((it (text:current)))
    (and (typep it 'terminal) it)))

(defun %fit (term win)
  "Give the program the size of the window showing it, so what it draws is what
fits."
  (when (and term win)
    (resize term (max 1 (edit:across win)) (max 1 (edit:down win)))))

(defun %open (&key runs name)
  (let* ((name (or name (format nil "*shell*~[~:;-~:*~d~]"
                                 (d:swap *counter* #'1+))))
         (win (edit:focused))
         (term (open-terminal name :runs runs
                                            :wide (if win (edit:across win) 80)
                                            :tall (if win (edit:down win) 24))))
    (setf (text:current) term)
    (when win (edit:show win term))
    (%fit term win)
    term))

(command:defcommand "terminal" (&optional line)
    (:describes "a program with a screen of its own, in a document"
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
