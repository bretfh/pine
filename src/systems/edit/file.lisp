(in-package #:pine/edit)

(defun %same-file-p (origin path)
  "Whether two names stand for one file. Compared as the disk resolves them, so a
link and what it points at are not two documents."
  (and origin
       (or (equal origin path)
           (let ((a (probe-file origin)) (b (probe-file path)))
             (and a b (equal (namestring a) (namestring b)))))))

(defun %document-name (path)
  "What to call a document opened on PATH: the file's own name, unless another
document is already open on a different file with the same one, which takes
NAME<2>. Two files called system.lisp are two documents; opening one of them
twice is still one."
  (let ((base (or (fault:or-nothing "a node's path is not a file name"
                    (file-namestring (pathname path)))
                  path)))
    (loop :for i :from 1
          :for name := (if (= i 1) base (format nil "~a<~d>" base i))
          :for had := (tree:at "/text" name)
          :when (or (null had) (%same-file-p (text:origin had) path))
            :do (return name))))

(command:defcommand "find-file" (path)
    (:describes "open a file in a document"
     :asks (list (list :prompt "Find  " :category :file :history :files))
     :on '(text "C-x C-f"))
  (let ((path (expanded (princ-to-string path))))
    (if (uiop:directory-exists-p path)
        (log:note "~a is a directory" path)
        (let* ((name (%document-name path))
               (document (or (tree:at "/text" name) (text:make-document name))))
          (text:visit document path)
          (setf (text:current) document)
          (let ((win (focused)))
            (when win (show win document)))
          (node:full-name document)))))

(command:defcommand "find-recent" ()
    (:describes "a file opened here before" :on '(text "C-x C-r"))
  (let ((found (text:recent)))
    (if found
        (progn (ask "Recent: " :must-match t :candidates found
                           :then (lambda (said)
                                   (command:run "find-file" (list said))))
               :asking)
        (log:note "nothing has been opened yet"))))

(command:defcommand "save-document" ()
    (:describes "write the document back where it came from"
     :on '(text "C-x C-s"))
  (let ((document (text:current)))
    (if (text:source document)
        (text:save document)
        (command:run "write-file"))))

(command:defcommand "write-file" (path)
    (:describes "write the document to a file you name"
     :asks (list (list :prompt "Write  " :category :file :history :files))
     :on '(text "C-x C-w"))
  (let ((document (text:current)))
    (text:save document (expanded (princ-to-string path)))
    (log:note "wrote ~a" (text:origin document))
    (text:origin document)))

(command:defcommand "revert-document" (&optional said)
    (:describes "the file again, as it is on disk"
     :asks '((:prompt "Revert from disk? " :candidates ("yes" "no")
              :must-match t)))
  (let ((document (text:current)))
    (cond ((not (equal "yes" (princ-to-string (or said "no")))) nil)
          ((text:source document)
           (and (text:revert document)
                (log:note "reverted ~a" (text:origin document))
                t))
          (t (log:note "~a is on nothing to read again"
                       (node:name document))))))

(command:defcommand "switch-to-document" (name)
    (:describes "show a document here, making it if there is none"
     :asks '((:prompt "Document: " :category :document))
     :on '(text "C-x b"))
  (let* ((name (princ-to-string name))
         (document (or (tree:at "/text" name) (text:make-document name))))
    (setf (text:current) document)
    (node:full-name document)))

(command:defcommand "new-document" (name)
    (:describes "an empty document"
     :asks '((:prompt "Document name: "))
     :on '(text "C-x n"))
  (let ((document (text:make-document (princ-to-string name))))
    (setf (text:current) document)
    (show (focused) document)
    (node:full-name document)))

(command:defcommand "kill-document" (&optional name)
    (:describes "forget a document"
     :asks '((:prompt "Kill document: " :category :document :must-match t))
     :on '(text "C-x k"))
  (let* ((name (princ-to-string (or name (node:name (text:current)))))
         (gone (tree:at "/text" name)))
    (when gone
      (text:forget name)
      (text:kill name)
      (let ((instead (text:current)))
        (dolist (win (windows))
          (when (eq gone (shows win))
            (show win instead)))))
    (and gone t)))
