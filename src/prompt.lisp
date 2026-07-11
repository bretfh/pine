(in-package :pine.editor)

(defun prompt (prompt-text cb)
  (let ((client (pine.client:current-client)))
    (setf (pine.client:prompt-callback client) cb
          (pine.client:prompt-active client) t))
  #+lqml (pine.qml:update-status-text prompt-text)
  #+lqml (pine.qml:show-status-input prompt-text))

(defun cancel-prompt ()
  (let ((client pine.client:*client*))
    (cond
      ((and client (completing-read-active-p))
       (completion-cancel))
      ((and client (pine.client:prompt-active client))
       (setf (pine.client:prompt-active client) nil
             (pine.client:prompt-callback client) nil)
       #+lqml (pine.qml:hide-status-input)
       #+lqml (pine.qml:update-status-text "cancelled")))))

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
         #+lqml (pine.qml:hide-status-input)
         (when cb
           (handler-case (funcall cb text)
             (error (c)
               #+lqml (pine.qml:update-status-text
                       (format nil "error: ~a" c)))))))
      (t
       (handler-case
           (let ((result (eval (read-from-string text))))
             #+lqml (pine.qml:hide-status-input)
             #+lqml (pine.qml:update-status-text (format nil "=> ~s" result)))
         (error (c)
           #+lqml (pine.qml:hide-status-input)
           #+lqml (pine.qml:update-status-text (format nil "error: ~a" c))))))))
