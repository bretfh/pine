(in-package :pine.editor)

;;;; The minibuffer as a real buffer. While a prompt is active, *minibuffer* is
;;;; the current buffer, so every editing command -- motion, kill, yank, word
;;;; ops, isearch -- operates on the input through the ordinary dispatch, exactly
;;;; like any buffer. minibuffer-mode (a minor mode) adds only the completion and
;;;; exit keys. A controller subscribed to the buffer re-filters the candidate
;;;; list and repaints on every edit, so the completion UI stays live no matter
;;;; which command changed the input.

(defun ensure-minibuffer (client)
  "The client's *minibuffer* buffer + its controller, created once."
  (or (pine.client:minibuffer-buffer client)
      (let* ((srv (pine.client:server-of client))
             (sys (pine.server:actor-system srv))
             (buf (pine.buffer:make-buffer-actor sys "*minibuffer*"))
             (ctrl (sento.actor-context:actor-of sys
                     :name (format nil "mb-ctrl-~a" (gensym))
                     :receive (lambda (msg)
                                (let ((pine.client:*client* client))
                                  (when (eq (first msg) :snapshot)
                                    (ignore-errors
                                     (minibuffer-changed
                                      client (getf (rest msg) :snapshot))))
                                  nil)))))
        (sento.actor:tell buf (list :subscribe :renderer ctrl))
        (setf (pine.client:minibuffer-buffer client) buf
              (pine.client:minibuffer-controller client) ctrl)
        buf)))

(defun minibuffer-active-p ()
  (let ((c (pine.client:current-client)))
    (and c (pine.client:prompt-active c))))

(defun %snap-line0 (snap)
  (if (and snap (plusp (pine.buffer:line-count snap)))
      (fset:@ (pine.buffer:lines snap) 0)
      ""))

(defun minibuffer-text ()
  "The current input, read synchronously from the buffer (accept path)."
  (let* ((c (pine.client:current-client))
         (mb (pine.client:minibuffer-buffer c)))
    (if mb (or (ignore-errors (sento.actor:ask-s mb '(:get-text) :time-out 5)) "") "")))

(defun minibuffer-set-text (text)
  "Replace the input with TEXT and put point at its end. Used by Tab completion
and file-name descent."
  (let* ((c (pine.client:current-client))
         (mb (pine.client:minibuffer-buffer c)))
    (when mb
      (sento.actor:tell mb (list :replace-content :content text))
      (sento.actor:tell mb (list :move-point :line 0 :col (length text))))))

(defun minibuffer-changed (client snap)
  "Controller callback: on each input edit, cache the snapshot, re-filter the
candidate list, and repaint."
  (setf (pine.client:minibuffer-snap client) snap)
  (when (pine.client:prompt-active client)
    (when (completing-read-active-p)
      (completion-update-input (%snap-line0 snap)))
    (let ((r (pine.client:renderer client)))
      (when r (sento.actor:tell r '(:force-render))))))

(defun activate-minibuffer (client prompt-text &key (initial ""))
  "Enter the minibuffer: make it the current buffer, enable minibuffer-mode, set
the initial input. The previous buffer is saved for restore."
  (let ((mb (ensure-minibuffer client)))
    ;; current-buffer must be the minibuffer BEFORE enabling minibuffer-mode:
    ;; minor-mode enablement is keyed on the current buffer.
    (setf (pine.client:saved-buffer client) (pine.client:current-buffer client)
          (pine.client:current-buffer client) mb
          (pine.client:prompt-active client) t)
    (pine.mode:set-buffer-mode mb :text-mode)
    (ignore-errors (pine.mode:enable-minor-mode client :minibuffer-mode))
    (pine.echo:show-input prompt-text)
    (sento.actor:tell mb (list :replace-content :content initial))
    (sento.actor:tell mb (list :move-point :line 0 :col (length initial)))
    mb))

(defun deactivate-minibuffer (client)
  "Leave the minibuffer: restore the previous buffer and clear the prompt."
  (ignore-errors (pine.mode:disable-minor-mode client :minibuffer-mode))
  (setf (pine.client:current-buffer client) (pine.client:saved-buffer client)
        (pine.client:saved-buffer client) nil
        (pine.client:prompt-active client) nil
        (pine.client:minibuffer-snap client) nil)
  (pine.echo:hide-input)
  (let ((r (pine.client:renderer client)))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %safe-call (fn arg)
  (when fn
    (handler-case (funcall fn arg)
      (error (e) (pine.echo:message (format nil "error: ~a" e))))))

;;;; Accept / abort / complete / candidate motion -- the minibuffer-mode command
;;;; bodies (the defcmd wrappers live in editor.lisp).

(defun minibuffer-accept ()
  (let* ((client (pine.client:current-client))
         (text (minibuffer-text)))
    (cond
      ((file-completion-active-p) (file-name-accept))
      ((completing-read-active-p)
       (let* ((c (completion))
              (result (if (and (>= (pine.client:index c) 0)
                               (< (pine.client:index c) (length (pine.client:filtered c))))
                          (nth (pine.client:index c) (pine.client:filtered c))
                          text))
              (cb (pine.client:callback c)))
         (completion-cleanup)
         (deactivate-minibuffer client)
         (%safe-call cb result)))
      ((pine.client:prompt-callback client)
       (let ((cb (pine.client:prompt-callback client)))
         (setf (pine.client:prompt-callback client) nil)
         (deactivate-minibuffer client)
         (%safe-call cb text)))
      (t (deactivate-minibuffer client)))))

(defun minibuffer-abort ()
  (let ((client (pine.client:current-client)))
    (when (completing-read-active-p) (completion-cleanup))
    (setf (pine.client:prompt-callback client) nil)
    (deactivate-minibuffer client)
    (pine.echo:message "quit")))

(defun minibuffer-complete ()
  (cond ((file-completion-active-p) (file-name-complete))
        ((completing-read-active-p)
         ;; insert the selected candidate as the input
         (let* ((c (completion))
                (i (pine.client:index c))
                (f (pine.client:filtered c)))
           (when (and (>= i 0) (< i (length f)))
             (minibuffer-set-text (nth i f)))))))
