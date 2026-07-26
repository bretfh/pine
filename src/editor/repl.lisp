(defpackage #:pine.editor.repl
  (:use :cl)
  (:export
   #:+buffer-name+
   #:start-repl
   #:repl-buffer
   #:repl-eval
   #:repl-input
   #:repl-submit
   #:repl-history))

(in-package #:pine.editor.repl)

;;;; The repl is a mode, and nothing about it is exempt: it is found by name like
;;;; any buffer, its prompt is its own meta, and its history is in the store. What
;;;; the mode's dispatch needs, the receive it runs in was handed.

(defparameter +buffer-name+ "*repl*")

(defparameter +prompt+ "pine> "
  "The prompt a new repl buffer starts with. Each buffer carries its own in meta
from then on, so a second repl or an agent's can prompt differently.")

(defun repl-buffer ()
  "The repl buffer, or nil when none is open."
  (pine.editor.frame:buffer +buffer-name+))

(defun repl-history ()
  "The forms submitted to a repl, most recent first."
  (pine.state.store:store-items :repl-history))

(defun start-repl ()
  "Open the repl buffer with a prompt on its last line, and answer it."
  (let ((buf (pine.editor.frame:make-buffer +buffer-name+ :content +prompt+)))
    (pine.editor.frame:set-buffer-mode buf :repl-mode)
    (sento.actor:tell buf (list :set-meta :key :prompt :value +prompt+))
    (let* ((snap (pine.core.actor:ask buf '(:get-snapshot) :timeout 5))
           (line (pine.text.buffer:line-count snap)))
      (sento.actor:tell buf (list :move-point :line (1- line) :col (length +prompt+))))
    buf))

(defun prompt-of (state)
  "STATE's prompt."
  (pine.text.buffer:buffer-local state :prompt +prompt+))

(defun repl-input (state)
  "The text after the prompt on STATE's last line."
  (let* ((prompt (prompt-of state))
         (last-line (1- (pine.text.buffer:line-count-of state)))
         (line (fset:@ (pine.text.buffer:lines state) last-line)))
    (if (> (length line) (length prompt))
        (subseq line (length prompt))
        "")))

(defun repl-eval (buffer state input)
  "Evaluate INPUT for BUFFER: a shell command when it starts with !, else lisp."
  (pine.state.store:store-push :repl-history input :unique nil :max 500)
  (if (and (plusp (length input)) (char= (char input 0) #\!))
      (run-shell-command buffer state (subseq input 1))
      (eval-lisp buffer state input)))

(defun eval-lisp (buffer state input)
  "Evaluate INPUT through the current eval target, appending the result to BUFFER.

Off-thread, so a slow, looping or erroring form cannot hold the repl and errors
reach the debugger surface. :local runs in the daemon image, a set target in that
agent's."
  (let ((prompt (prompt-of state)))
    (pine.editor.target:eval-in-target
     input
     (find-package :cl-user)
     :on-done
     (lambda (ev)
       (append-output
        buffer prompt
        (case (pine.core.eval:evaluation-status ev)
          (:ok (let ((out (pine.core.eval:evaluation-output ev)))
                 (format nil "~@[~a~%~]~{~s~^~%~}"
                         (and (plusp (length out)) out)
                         (pine.core.eval:evaluation-values ev))))
          (:aborted "; aborted")
          (t (format nil "; error: ~a" (pine.core.eval:evaluation-condition ev)))))))))

(defun run-shell-command (buffer state cmd)
  "Run CMD through the shell and append its output to BUFFER."
  (let ((prompt (prompt-of state)))
    (handler-case
        (let ((output (uiop:run-program cmd
                                        :output :string
                                        :error-output :string
                                        :ignore-error-status t)))
          (append-output buffer prompt (string-trim '(#\Newline #\Space) output)))
      (error (c)
        (append-output buffer prompt (format nil "shell error: ~a" c))))))

(defun append-output (buffer prompt text)
  (when buffer
    (sento.actor:tell buffer (list :append-with-prompt :text text :prompt prompt))))

(defun repl-submit (buffer state)
  "Evaluate the input on STATE's last line, appending the result to BUFFER.

Runs inside BUFFER's own receive, so it reads the state it was handed and takes
the buffer as an argument. Asking the actor for its state would be asking the
thread that is running this, and reaching for the current client would fault: no
buffer actor binds *client*."
  (let ((input (repl-input state)))
    (when (plusp (length input))
      (repl-eval buffer state input))))

(defmethod pine.editor.mode:dispatch-message ((mode pine.editor.mode:repl-mode) self tag plist)
  (declare (ignore plist))
  (case tag
    (:newline (repl-submit self (first sento.actor:*state*)))
    (t (call-next-method))))
