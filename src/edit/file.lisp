(defpackage #:pine/edit/file
  (:use #:cl)
  (:local-nicknames (#:fault #:pine/run/fault)
                    (#:node #:pine/fs/node) (#:command #:pine/run/command)
                    (#:log #:pine/run/log)
                    (#:doc #:pine/text/document) (#:text #:pine/text)
                    (#:parser #:pine/text/ts/parser)
                    (#:window #:pine/edit/window) (#:prompt #:pine/edit/prompt)
                    (#:match #:pine/edit/matching))
  (:export))
(in-package #:pine/edit/file)

(defun %of () (doc:current))

(command:defcommand "find-file" (path)
    (:describes "open a file in a document"
     :asks (list (list :prompt "Find file: " :category :file :history :files))
     :on '(text "C-x C-f"))
  (let ((path (match:expanded (princ-to-string path))))
    (if (uiop:directory-exists-p path)
        (log:note "~a is a directory" path)
        (let* ((name (or (fault:or-nothing "a node's path is not a file name"
                           (file-namestring (pathname path)))
                         path))
               (document (or (doc:named name) (doc:make-document name))))
          (text:visit document path)
          (setf (doc:current) document)
          (let ((win (window:focused)))
            (when win (window:show win document)))
          (node:full-name document)))))

(command:defcommand "find-recent" ()
    (:describes "a file opened here before" :on '(text "C-x C-r"))
  (let ((found (text:recent)))
    (if found
        (progn (prompt:ask "Recent: " :must-match t :candidates found
                           :then (lambda (said)
                                   (command:run "find-file" (list said))))
               :asking)
        (log:note "nothing has been opened yet"))))

(command:defcommand "save-document" ()
    (:describes "write the document back where it came from"
     :on '(text "C-x C-s"))
  (let ((document (%of)))
    (if (doc:source document)
        (text:save document)
        (command:run "write-file"))))

(command:defcommand "write-file" (path)
    (:describes "write the document to a file you name"
     :asks (list (list :prompt "Write file: " :category :file :history :files))
     :on '(text "C-x C-w"))
  (let ((document (%of)))
    (text:save document (match:expanded (princ-to-string path)))
    (log:note "wrote ~a" (doc:origin document))
    (doc:origin document)))

(command:defcommand "revert-document" (&optional said)
    (:describes "the file again, as it is on disk"
     :asks '((:prompt "Revert from disk? " :candidates ("yes" "no")
              :must-match t)))
  (let ((document (%of)))
    (cond ((not (equal "yes" (princ-to-string (or said "no")))) nil)
          ((doc:source document)
           (and (text:revert document)
                (log:note "reverted ~a" (doc:origin document))
                t))
          (t (log:note "~a is on nothing to read again"
                       (node:name document))))))

(command:defcommand "switch-to-document" (name)
    (:describes "show a document here, making it if there is none"
     :asks '((:prompt "Document: " :category :document))
     :on '(text "C-x b"))
  (let* ((name (princ-to-string name))
         (document (or (doc:named name) (doc:make-document name))))
    (setf (doc:current) document)
    (node:full-name document)))

(command:defcommand "new-document" (name)
    (:describes "an empty document"
     :asks '((:prompt "Document name: "))
     :on '(text "C-x n"))
  (let ((document (doc:make-document (princ-to-string name))))
    (setf (doc:current) document)
    (window:show (window:focused) document)
    (node:full-name document)))

(command:defcommand "kill-document" (&optional name)
    (:describes "forget a document"
     :asks '((:prompt "Kill document: " :category :document :must-match t))
     :on '(text "C-x k"))
  (let* ((name (princ-to-string (or name (node:name (%of)))))
         (gone (doc:named name)))
    (when gone
      (parser:forget name)
      (doc:kill name)
      (let ((instead (doc:current)))
        (dolist (win (window:windows))
          (when (eq gone (window:shows win))
            (window:show win instead)))))
    (and gone t)))
