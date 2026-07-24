(in-package :pine.editor)

;;;; Raw-text prompt (no completion): eval-expression, new-buffer. It activates
;;;; the minibuffer buffer with a callback; Return -> minibuffer-accept fires it.
;;;; All the editing keys are the ordinary buffer commands; only accept/abort are
;;;; minibuffer-mode bindings.

(defun prompt (prompt-text cb)
  (let ((client (pine.client:current-client)))
    (setf (pine.client:prompt-callback client) cb)
    (activate-minibuffer client prompt-text)))

;;;; Back-compat entry points still referenced elsewhere: accept/cancel routed
;;;; to the minibuffer commands.

(defun on-minibuffer-accept (text)
  (declare (ignore text))
  (minibuffer-accept))

(defun cancel-prompt ()
  (minibuffer-abort))
