(defpackage #:pine.editor.repl
  (:use :cl)
  (:export
   #:start-repl
   #:repl-eval
   #:repl-submit
   #:repl-buffer))

(in-package #:pine.editor.repl)

(defparameter +prompt+ "pine> ")

(defvar *history* (fset:empty-seq))
(defvar *history-index* -1)

(defun repl-buffer ()
  (pine.editor.frame:repl-buffer (pine.editor.frame:current-client)))

(defun start-repl ()
  (when (zerop (fset:size *history*))
    (setf *history* (fset:convert 'fset:seq
                                  (reverse (pine.state.store:store-items :repl-history)))))
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:make-buffer "*repl*" :content +prompt+)))
    (setf (pine.editor.frame:repl-buffer client) buf)
    (pine.editor.frame:set-buffer-mode buf :repl-mode)
    (let* ((snap (sento.actor:ask-s buf '(:get-snapshot) :time-out 5))
           (line (pine.text.buffer:line-count snap))
           (col (length +prompt+)))
      (sento.actor:tell buf (list :move-point :line (1- line) :col col)))
    buf))

(defun repl-eval (buffer input)
  (setf *history* (fset:with-last *history* input))
  (setf *history-index* -1)
  (pine.state.store:store-push :repl-history input :unique nil :max 500)
  (cond
    ((and (plusp (length input)) (char= (char input 0) #\!))
     (run-shell-command buffer (subseq input 1)))
    (t (eval-lisp buffer input))))

(defun eval-lisp (buffer input)
  ;; the one eval path, honouring *eval-target*: :local runs in the daemon image,
  ;; a set target runs in that agent's image. Off-thread, so a slow/looping/
  ;; erroring form can't hang the repl and errors reach the debugger surface.
  (pine.editor.target:eval-in-target
   input
   (find-package :cl-user)
   :on-done
   (lambda (ev)
     (append-output
      buffer
      (case (pine.core.eval:evaluation-status ev)
        (:ok (let ((out (pine.core.eval:evaluation-output ev)))
               (format nil "~@[~a~%~]~{~s~^~%~}"
                       (and (plusp (length out)) out)
                       (pine.core.eval:evaluation-values ev))))
        (:aborted "; aborted")
        (t (format nil "; error: ~a" (pine.core.eval:evaluation-condition ev))))))))

(defun run-shell-command (buffer cmd)
  (handler-case
      (let ((output (uiop:run-program cmd
                                      :output :string
                                      :error-output :string
                                      :ignore-error-status t)))
        (append-output buffer (string-trim '(#\Newline #\Space) output)))
    (error (c)
      (append-output buffer (format nil "shell error: ~a" c)))))

(defun append-output (buffer text)
  (when buffer
    (sento.actor:tell buffer (list :append-with-prompt
                                   :text text
                                   :prompt +prompt+))))

(defun repl-input (state)
  "The text after the prompt on STATE's last line."
  (let* ((last-line (1- (pine.text.buffer:line-count-of state)))
         (line (fset:@ (pine.text.buffer:lines state) last-line)))
    (if (> (length line) (length +prompt+))
        (subseq line (length +prompt+))
        "")))

(defun repl-submit (buffer state)
  "Evaluate the input on STATE's last line, appending the result to BUFFER.

Runs inside BUFFER's own receive, so it reads the state it was handed and takes
the buffer as an argument. Asking the actor for its state would be asking the
thread that is running this, and reaching for the current client would fault: no
buffer actor binds *client*."
  (let ((input (repl-input state)))
    (when (plusp (length input))
      (repl-eval buffer input))))

(defmethod pine.editor.mode:dispatch-message ((mode pine.editor.mode:repl-mode) self tag plist)
  (declare (ignore plist))
  (case tag
    (:newline (repl-submit self (first sento.actor:*state*)))
    (t (call-next-method))))
