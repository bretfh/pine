(in-package :pine.editor)

(defconstant +ctrl+  #x04000000)
(defconstant +meta+  #x10000000)
(defconstant +alt+   #x08000000)
(defconstant +shift+ #x02000000)

(defun commands-table ()
  (pine.server:commands
   (pine.client:server-of (pine.client:current-client))))

(defun global-keymap ()
  (pine.server:global-keymap
   (pine.client:server-of (pine.client:current-client))))

(defun key-id (key modifiers)
  (cons (truncate key) (truncate modifiers)))

(defun register-command (name fn)
  (setf (gethash name (commands-table)) fn))

(defmacro defcommand (name args &body body)
  `(register-command ,name (lambda ,args ,@body)))

(defun bind-key (command-name &rest key-specs)
  (let ((table (global-keymap)))
    (loop for (key mods) on key-specs by #'cddr
          for remaining = (rest (rest (member key key-specs)))
          for id = (key-id key mods)
          do (if (null remaining)
                 (setf (gethash id table) command-name)
                 (let ((next (gethash id table)))
                   (unless (hash-table-p next)
                     (setf next (make-hash-table :test 'equal))
                     (setf (gethash id table) next))
                   (setf table next))))))

(defun bind-key-simple (command-name key &optional (mods 0))
  (setf (gethash (key-id key mods) (global-keymap)) command-name))

(defun bind-key-seq (command-name key1 mods1 key2 mods2)
  (let ((id1 (key-id key1 mods1))
        (id2 (key-id key2 mods2))
        (km (global-keymap)))
    (let ((sub (gethash id1 km)))
      (unless (hash-table-p sub)
        (setf sub (make-hash-table :test 'equal))
        (setf (gethash id1 km) sub))
      (setf (gethash id2 sub) command-name))))

(defun bind-mode-key (mode-name command-name key &optional (mods 0))
  "Bind a key in the keymap of MODE-NAME (keyword)."
  (let ((m (pine.mode:find-mode mode-name)))
    (unless m (error "No mode ~s" mode-name))
    (setf (gethash (key-id key mods) (pine.mode:keymap m)) command-name)))

(defun run-command (name)
  (let ((fn (gethash name (commands-table))))
    (when fn
      (handler-case (progn (funcall fn)
                           (setf (pine.client:last-command (pine.client:current-client)) name))
        (error (c)
          #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))))

(defun %focused-mode ()
  (pine.mode:current-buffer-mode))

(defun dispatch-key (key modifiers text)
  (handler-case
      (let* ((client (pine.client:current-client))
             (mods (truncate (or modifiers 0)))
             (id (key-id key mods)))
        ;; C-g / Escape cancel
        (when (or (equal id (key-id 71 +ctrl+))
                  (equal id (key-id 16777216 0)))
          (setf (pine.client:pending-keys client) nil)
          (cancel-prompt)
          (return-from dispatch-key))

        ;; Completion nav during completing-read
        (when (completing-read-active-p)
          (cond
            ((equal id (key-id 78 +ctrl+))
             (completion-next)
             (return-from dispatch-key))
            ((equal id (key-id 80 +ctrl+))
             (completion-prev)
             (return-from dispatch-key))))

        ;; Lookup chain: pending-keys → mode chain → global.
        (let* ((mode (%focused-mode))
               (entry (or (and (pine.client:pending-keys client)
                               (gethash id (pine.client:pending-keys client)))
                          (pine.mode:mode-lookup mode id)
                          (gethash id (global-keymap)))))
          (cond
            ((hash-table-p entry)
             (setf (pine.client:pending-keys client) entry)
             #+lqml (pine.qml:update-status-text "C-x ..."))
            ((stringp entry)
             (setf (pine.client:pending-keys client) nil)
             (run-command entry))
            (t
             (setf (pine.client:pending-keys client) nil)
             (let ((fb (pine.mode:mode-fallback mode)))
               (when fb
                 (funcall fb client key mods text)))))))
    (error (c)
      (let ((client pine.client:*client*))
        (when client (setf (pine.client:pending-keys client) nil)))
      #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))

(defun on-key (key modifiers text)
  (dispatch-key key modifiers text))

(defun install-default-commands ()
  (progn
    (defcommand "backspace" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf (sento.actor:tell buf '(:backspace)))))

    (defcommand "delete-char" ()
      (let* ((client (pine.client:current-client))
             (buf (pine.client:current-buffer client)))
        (when buf
          (let ((snap (focused-snap)))
            (when snap
              (sento.actor:tell buf
                (list :delete-region
                      :start-line (pine.buffer:point-line snap)
                      :start-col (pine.buffer:point-col snap)
                      :end-line (pine.buffer:point-line snap)
                      :end-col (1+ (pine.buffer:point-col snap)))))))))

    (defcommand "newline" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf (pine.buffer:tell buf :newline))))

    (defcommand "undo" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf (sento.actor:tell buf '(:undo)))))

    (defcommand "move-up" () (move-point-up))
    (defcommand "move-down" () (move-point-down))
    (defcommand "move-left" () (move-point-left))
    (defcommand "move-right" () (move-point-right))

    (defcommand "forward-char" () (move-point-right))
    (defcommand "backward-char" () (move-point-left))
    (defcommand "next-line" () (move-point-down))
    (defcommand "previous-line" () (move-point-up))

    (defcommand "beginning-of-line" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf
          (let ((snap (focused-snap)))
            (when snap
              (sento.actor:tell buf
                (list :move-point :line (pine.buffer:point-line snap) :col 0)))))))

    (defcommand "end-of-line" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf
          (let ((snap (focused-snap)))
            (when snap
              (let ((len (length (fset:@ (pine.buffer:lines snap)
                                         (pine.buffer:point-line snap)))))
                (sento.actor:tell buf
                  (list :move-point :line (pine.buffer:point-line snap) :col len))))))))

    (defcommand "beginning-of-buffer" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf
          (sento.actor:tell buf (list :move-point :line 0 :col 0)))))

    (defcommand "end-of-buffer" ()
      (let ((buf (pine.client:current-buffer (pine.client:current-client))))
        (when buf
          (let ((snap (focused-snap)))
            (when snap
              (let* ((last-line (1- (pine.buffer:line-count snap)))
                     (last-col (length (fset:@ (pine.buffer:lines snap) last-line))))
                (sento.actor:tell buf
                  (list :move-point :line last-line :col last-col))))))))

    (defcommand "scroll-down" ()
      (let ((w (pine.client:focused-window (pine.client:current-client))))
        (when w (scroll-window (- (pine.buffer:win-height w) 2)))))

    (defcommand "scroll-up" ()
      (let ((w (pine.client:focused-window (pine.client:current-client))))
        (when w (scroll-window (- 2 (pine.buffer:win-height w))))))

    (defcommand "scroll-down-line" () (scroll-window 3))
    (defcommand "scroll-up-line" () (scroll-window -3))

    (defcommand "set-mark" () (set-mark))

    (defcommand "kill-line" () (kill-line-cmd))
    (defcommand "kill-region" () (kill-region-cmd))
    (defcommand "copy-region" () (copy-region-cmd))
    (defcommand "yank" () (yank-cmd))
    (defcommand "yank-pop" () (yank-pop-cmd))

    (defcommand "find-file" ()
      (prompt "Find file: "
        (lambda (path)
          (handler-case (pine.file:find-file path)
            (error (c)
              #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))))

    (defcommand "save-file" ()
      (handler-case (pine.file:save-current-buffer)
        (error (c)
          #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))

    (defcommand "switch-buffer" ()
      (completing-read "Switch to: "
        (pine.buffer:list-buffers)
        (lambda (name)
          (let* ((client (pine.client:current-client))
                 (buf (pine.buffer:switch-buffer name)))
            (when buf
              (sento.actor:tell (pine.client:renderer client)
                                (list :switch-buffer :buffer buf :name name))
              (pine.render:subscribe-to-buffer buf))))))

    (defcommand "list-buffers" ()
      #+lqml (pine.qml:update-status-text
              (format nil "buffers: ~{~a~^, ~}" (pine.buffer:list-buffers))))

    (defcommand "execute-command" ()
      (let ((names (sort (loop for k being the hash-keys of (commands-table) collect k)
                         #'string<)))
        (completing-read "M-x " names (lambda (name) (run-command name)))))

    (defcommand "eval-expression" ()
      (prompt "Eval: "
        (lambda (text)
          (handler-case
              (let ((result (eval (read-from-string text))))
                #+lqml (pine.qml:update-status-text (format nil "=> ~s" result)))
            (error (c)
              #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))))

    (defcommand "eval-last-sexp" () (eval-last-sexp))
    (defcommand "eval-buffer" ()    (eval-buffer))

    (defcommand "new-buffer" ()
      (prompt "New buffer: "
        (lambda (name)
          (let* ((client (pine.client:current-client))
                 (buf (pine.buffer:make-buffer name)))
            (pine.render:subscribe-to-buffer buf)))))

    (defcommand "open-terminal" ()
      (handler-case (pine.term:open-terminal)
        (error (c)
          #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))

    (defcommand "open-repl" ()
      (handler-case
          (let* ((client (pine.client:current-client))
                 (existing (pine.client:repl-buffer client))
                 (buf (or existing (pine.shell:start-repl))))
            (pine.buffer:switch-buffer "*repl*")
            (pine.render:subscribe-to-buffer buf)
            (sento.actor:tell (pine.client:renderer client)
                              (list :switch-buffer :buffer buf :name "*repl*")))
        (error (c)
          #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))))

(defun install-default-bindings ()
  "Install *truly global* bindings only: chord prefixes and M-x.
Text editing keys belong to text-mode (see install-text-mode-bindings)."
  (bind-key-seq "find-file" 88 +ctrl+ 70 +ctrl+)
  (bind-key-seq "save-file" 88 +ctrl+ 83 +ctrl+)
  (bind-key-seq "switch-buffer" 88 +ctrl+ 66 0)
  (bind-key-seq "new-buffer" 88 +ctrl+ 78 0)
  (bind-key-seq "open-repl" 88 +ctrl+ 82 0)
  (bind-key-seq "eval-last-sexp" 88 +ctrl+ 69 +ctrl+)
  (bind-key-simple "execute-command" 88 +meta+))

(defun install-text-mode-bindings ()
  "Text editing keys. Applies to text-mode and anything that derives
from it (lisp-mode, repl-mode)."
  (bind-mode-key :text-mode "backspace" 16777219)
  (bind-mode-key :text-mode "newline" 16777220)
  (bind-mode-key :text-mode "move-up" 16777235)
  (bind-mode-key :text-mode "move-down" 16777237)
  (bind-mode-key :text-mode "move-left" 16777234)
  (bind-mode-key :text-mode "move-right" 16777236)

  (bind-mode-key :text-mode "forward-char" 70 +ctrl+)
  (bind-mode-key :text-mode "backward-char" 66 +ctrl+)
  (bind-mode-key :text-mode "next-line" 78 +ctrl+)
  (bind-mode-key :text-mode "previous-line" 80 +ctrl+)
  (bind-mode-key :text-mode "beginning-of-line" 65 +ctrl+)
  (bind-mode-key :text-mode "end-of-line" 69 +ctrl+)
  (bind-mode-key :text-mode "delete-char" 68 +ctrl+)

  (bind-mode-key :text-mode "beginning-of-buffer" 60 +meta+)
  (bind-mode-key :text-mode "end-of-buffer" 62 +meta+)

  (bind-mode-key :text-mode "set-mark" 32 +ctrl+)
  (bind-mode-key :text-mode "kill-line" 75 +ctrl+)
  (bind-mode-key :text-mode "kill-region" 87 +ctrl+)
  (bind-mode-key :text-mode "copy-region" 87 +meta+)
  (bind-mode-key :text-mode "yank" 89 +ctrl+)
  (bind-mode-key :text-mode "yank-pop" 89 +meta+)

  (bind-mode-key :text-mode "scroll-down" 86 +ctrl+)
  (bind-mode-key :text-mode "scroll-up" 86 +meta+)
  (bind-mode-key :text-mode "scroll-up" 16777238)
  (bind-mode-key :text-mode "scroll-down" 16777239)

  (bind-mode-key :text-mode "undo" 90 +ctrl+))
