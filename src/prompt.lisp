(in-package :pine.editor)

(defun prompt (prompt-text cb)
  (let ((client (pine.client:current-client)))
    (setf (pine.client:prompt-callback client) cb
          (pine.client:prompt-active client) t))
  (pine.echo:message prompt-text)
  (pine.echo:show-input prompt-text))

(defun cancel-prompt ()
  (let ((client pine.client:*client*))
    (cond
      ((and client (completing-read-active-p))
       (completion-cancel))
      ((and client (pine.client:prompt-active client))
       (setf (pine.client:prompt-active client) nil
             (pine.client:prompt-callback client) nil)
       (pine.echo:hide-input)
       (pine.echo:message "cancelled")))))

(defun on-minibuffer-accept (text)
  (let ((client (pine.client:current-client)))
    (cond
      ((completing-read-active-p)
       (completion-update-input text)
       (completion-accept))
      ((pine.client:prompt-active client)
       (let ((cb (pine.client:prompt-callback client)))
         (setf (pine.client:prompt-active client) nil
               (pine.client:prompt-callback client) nil)
         (pine.echo:hide-input)
         (when cb
           (handler-case (funcall cb text)
             (error (c)
               (pine.echo:message
                       (format nil "error: ~a" c)))))))
      (t
       (handler-case
           (let ((result (eval (read-from-string text))))
             (pine.echo:hide-input)
             (pine.echo:message (format nil "=> ~s" result)))
         (error (c)
           (pine.echo:hide-input)
           (pine.echo:message (format nil "error: ~a" c))))))))
