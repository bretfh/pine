(in-package #:pine/edit)

(defun %same-file-p (origin path)
  (and origin
       (or (equal origin path)
           (let ((a (probe-file origin)) (b (probe-file path)))
             (and a b (equal (namestring a) (namestring b)))))))

(defun %buffer-name (path)
  (let ((base (or (fault:or-nothing "a node's path is not a file name"
                    (file-namestring (pathname path)))
                  path)))
    (loop :for i :from 1
          :for name := (if (= i 1) base (format nil "~a<~d>" base i))
          :for had := (fs:at "/text" name)
          :when (or (null had) (%same-file-p (text:origin had) path))
            :do (return name))))

(command:defcommand "find-file" (path)
    (:describes "open a file in a buffer"
     :asks (list (list :prompt "Find  " :category :file :history :files))
     :on '(text "C-x C-f"))
  (let ((path (expanded (princ-to-string path))))
    (if (uiop:directory-exists-p path)
        (log:note "~a is a directory" path)
        (let* ((name (%buffer-name path))
               (buffer (or (fs:at "/text" name) (text:make-buffer name))))
          (text:visit buffer path)
          (setf (text:current) buffer)
          (let ((win (focused)))
            (when win (show win buffer)))
          (fs:full-name buffer)))))

(command:defcommand "find-recent" ()
    (:describes "a file opened here before" :on '(text "C-x C-r"))
  (let ((found (text:recent)))
    (if found
        (progn (ask "Recent: " :must-match t :candidates found
                           :then (lambda (said)
                                   (command:run "find-file" (list said))))
               :asking)
        (log:note "nothing has been opened yet"))))

(command:defcommand "save-buffer" ()
    (:describes "write the buffer back where it came from"
     :on '(text "C-x C-s"))
  (let ((buffer (text:current)))
    (if (text:source buffer)
        (text:save buffer)
        (command:run "write-file"))))

(command:defcommand "write-file" (path)
    (:describes "write the buffer to a file you name"
     :asks (list (list :prompt "Write  " :category :file :history :files))
     :on '(text "C-x C-w"))
  (let ((buffer (text:current)))
    (text:save buffer (expanded (princ-to-string path)))
    (log:note "wrote ~a" (text:origin buffer))
    (text:origin buffer)))

(command:defcommand "revert-buffer" (&optional said)
    (:describes "the file again, as it is on disk"
     :asks '((:prompt "Revert from disk? " :candidates ("yes" "no")
              :must-match t)))
  (let ((buffer (text:current)))
    (cond ((not (equal "yes" (princ-to-string (or said "no")))) nil)
          ((text:source buffer)
           (and (text:revert buffer)
                (log:note "reverted ~a" (text:origin buffer))
                t))
          (t (log:note "~a is on nothing to read again"
                       (fs:name buffer))))))

(command:defcommand "switch-to-buffer" (name)
    (:describes "show a buffer here, making it if there is none"
     :asks '((:prompt "Document: " :category :buffer))
     :on '(text "C-x b"))
  (let* ((name (princ-to-string name))
         (buffer (or (fs:at "/text" name) (text:make-buffer name))))
    (setf (text:current) buffer)
    (fs:full-name buffer)))

(command:defcommand "new-buffer" (name)
    (:describes "an empty buffer"
     :asks '((:prompt "Document name: "))
     :on '(text "C-x n"))
  (let ((buffer (text:make-buffer (princ-to-string name))))
    (setf (text:current) buffer)
    (show (focused) buffer)
    (fs:full-name buffer)))

(command:defcommand "kill-buffer" (&optional name)
    (:describes "forget a buffer"
     :asks '((:prompt "Kill buffer: " :category :buffer :must-match t))
     :on '(text "C-x k"))
  (let* ((name (princ-to-string (or name (fs:name (text:current)))))
         (gone (fs:at "/text" name)))
    (when gone
      (text:forget name)
      (text:kill name)
      (let ((instead (text:current)))
        (dolist (win (panes))
          (when (eq gone (shows win))
            (show win instead)))))
    (and gone t)))
