(in-package #:pine.editor)

(defun start-editor ()
  (let* ((client (pine.editor.frame:current-client))
         (server (pine.editor.frame:server-of client)))
    (pine.text.buffer:start-buffer-registry server)
    (handler-case (pine.ts:ensure-ts (pine.core.server:ts-runtime server))
      (error () nil))
    (pine.ui.render:start-renderer client)
    (pine.editor.minibuffer:ensure-minibuffer client)
    (setf pine.editor.command:*terminal-handler* #'pine.term:terminal-dispatch)
    (setf pine.core.eval:*on-debug* #'pine.editor.debugger:%eval-error)
    (let ((buf (pine.editor.frame:make-buffer "scratch")))
      (pine.editor.frame:make-window buf "scratch"
                               :row 0 :col 0 :width 80 :height 29 :focused t)
      (pine.ui.render:subscribe-to-buffer buf)
      (pine.editor.frame:set-buffer-mode buf :text-mode)
      (pine.editor.ask:tell buf :set-local :key :package :value :pine-user))
    (pine.ui.render:relayout)))

;;;; Commands

(defmacro defcmd (name (&rest args) &body body)
  `(pine.editor.command:define-command ,name ,args ,@body))

(defcmd "keyboard-quit" ()
  (setf (pine.editor.frame:pending-keys (pine.editor.frame:current-client)) nil)
  ;; C-g while attending a local eval interrupts it into its abort restart
  ;; (kills a runaway loop) and resolves the session.
  (let ((s pine.editor.debugger:*attended-session*))
    (when (and s (eq (pine.editor.debugger:dbg-session-kind s) :local) (pine.editor.debugger:dbg-session-ev s))
      (pine.core.eval:abort-evaluation (pine.editor.debugger:dbg-session-ev s))
      (pine.editor.debugger:%resolve-session s)))
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (sento.actor:tell buf (list :set-meta :key :mark-line :value nil))
      (sento.actor:tell buf (list :set-meta :key :mark-col :value nil))))
  (pine.editor.minibuffer:cancel-prompt))
(defcmd "backspace" ()
  (let ((buf (pine.editor.motion:cur-buffer))) (when buf (sento.actor:tell buf '(:backspace)))))
(defcmd "delete-char" ()
  (let ((buf (pine.editor.motion:cur-buffer)) (snap (pine.editor.motion:focused-snap)))
    (when (and buf snap)
      (sento.actor:tell buf
        (list :delete-region
              :start-line (pine.text.buffer:point-line snap) :start-col (pine.text.buffer:point-col snap)
              :end-line (pine.text.buffer:point-line snap) :end-col (1+ (pine.text.buffer:point-col snap)))))))
(defcmd "newline" ()
  (let ((buf (pine.editor.motion:cur-buffer))) (when buf (pine.editor.ask:tell buf :newline))))
(defcmd "undo" ()
  (let ((buf (pine.editor.motion:cur-buffer))) (when buf (sento.actor:tell buf '(:undo)))))
(defcmd "redo" ()
  (let ((buf (pine.editor.motion:cur-buffer))) (when buf (sento.actor:tell buf '(:redo)))))

(defcmd "forward-char" (n)  (:interactive :number) (pine.editor.motion:move-chars n))
(defcmd "backward-char" (n) (:interactive :number) (pine.editor.motion:move-chars (- n)))
(defcmd "next-line" (n)     (:interactive :number) (pine.editor.motion:move-lines n))
(defcmd "previous-line" (n) (:interactive :number) (pine.editor.motion:move-lines (- n)))
(defcmd "forward-word" (n)  (:interactive :number) (pine.editor.motion:move-words n))
(defcmd "backward-word" (n) (:interactive :number) (pine.editor.motion:move-words (- n)))
(defcmd "kill-word" (n)          (:interactive :number) (kill-words-cmd n))
(defcmd "backward-kill-word" (n) (:interactive :number) (kill-words-cmd (- n)))
(defcmd "isearch-forward" ()  (isearch-start :forward))
(defcmd "isearch-backward" () (isearch-start :backward))
(defcmd "universal-argument" () (:prefix)
  (let ((c (pine.editor.frame:current-client)))
    (setf (pine.editor.frame:prefix-arg c)
          (list (* 4 (pine.editor.command:prefix-numeric-value
                      (pine.editor.frame:prefix-arg c)))))))
(defcmd "digit-argument" () (:prefix)
  (let* ((c (pine.editor.frame:current-client))
         (key (pine.editor.frame:this-command-key c))
         (d (and key (digit-char-p (char (pine.editor.key:key-sym key) 0))))
         (cur (pine.editor.frame:prefix-arg c)))
    (when d
      (setf (pine.editor.frame:prefix-arg c)
            (cond ((eq cur '-) (- d))
                  ((and (integerp cur) (minusp cur)) (- (+ (* 10 (- cur)) d)))
                  ((integerp cur) (+ (* 10 cur) d))
                  (t d))))))
(defcmd "negative-argument" () (:prefix)
  (let* ((c (pine.editor.frame:current-client))
         (cur (pine.editor.frame:prefix-arg c)))
    (setf (pine.editor.frame:prefix-arg c)
          (cond ((eq cur '-) nil)
                ((integerp cur) (- cur))
                (t '-)))))

(defcmd "beginning-of-line" ()
  (let ((buf (pine.editor.motion:cur-buffer)) (snap (pine.editor.motion:focused-snap)))
    (when (and buf snap)
      (sento.actor:tell buf (list :move-point :line (pine.text.buffer:point-line snap) :col 0)))))
(defcmd "end-of-line" ()
  (let ((buf (pine.editor.motion:cur-buffer)) (snap (pine.editor.motion:focused-snap)))
    (when (and buf snap)
      (let ((len (length (fset:@ (pine.text.buffer:lines snap) (pine.text.buffer:point-line snap)))))
        (sento.actor:tell buf (list :move-point :line (pine.text.buffer:point-line snap) :col len))))))
(defcmd "beginning-of-buffer" ()
  (let ((buf (pine.editor.motion:cur-buffer))) (when buf (sento.actor:tell buf (list :move-point :line 0 :col 0)))))
(defcmd "end-of-buffer" ()
  (let ((buf (pine.editor.motion:cur-buffer)) (snap (pine.editor.motion:focused-snap)))
    (when (and buf snap)
      (let* ((ll (1- (pine.text.buffer:line-count snap)))
             (lc (length (fset:@ (pine.text.buffer:lines snap) ll))))
        (sento.actor:tell buf (list :move-point :line ll :col lc))))))

(defcmd "scroll-down" ()
  (let ((w (pine.editor.frame:focused-window (pine.editor.frame:current-client))))
    (when w (pine.editor.window:scroll-window (- (pine.text.window:win-height w) 2)))))
(defcmd "scroll-up" ()
  (let ((w (pine.editor.frame:focused-window (pine.editor.frame:current-client))))
    (when w (pine.editor.window:scroll-window (- 2 (pine.text.window:win-height w))))))

(defcmd "forward-sexp" ()      (pine.editor.motion:%sexp-move :forward-sexp))
(defcmd "backward-sexp" ()     (pine.editor.motion:%sexp-move :backward-sexp))
(defcmd "beginning-of-defun" () (pine.editor.motion:%sexp-move :beginning-of-defun))
(defcmd "end-of-defun" ()      (pine.editor.motion:%sexp-move :end-of-defun))
(defcmd "mark-sexp" ()
  (set-mark)
  (pine.editor.motion:%sexp-move :forward-sexp))

(defcmd "set-mark" ()     (set-mark))
(defcmd "kill-line" ()    (kill-line-cmd))
(defcmd "kill-region" ()  (kill-region-cmd))
(defcmd "copy-region" ()  (copy-region-cmd))
(defcmd "yank" ()         (yank-cmd))
(defcmd "yank-pop" ()     (yank-pop-cmd))

(defcmd "find-file" ()
  (pine.editor.minibuffer:read-file-name "Find file: "
    (lambda (path)
      (handler-case (pine.editor.file:find-file path)
        (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
    :history :files))
(defcmd "find-recent" ()
  (let ((items (pine.state.store:store-items :recent-files)))
    (if items
        (pine.editor.minibuffer:completing-read "Recent: " items
          (lambda (path)
            (handler-case (pine.editor.file:find-file path)
              (error (c) (pine.editor.echo:message (format nil "error: ~a" c))))))
        (pine.editor.echo:message "no recent files"))))
(defcmd "save-file" ()
  (handler-case
      (let ((buf (pine.editor.motion:cur-buffer)))
        ;; opt-in apheleia-style reformat before write; the reindent is a tell
        ;; and save's :get-state queues behind it in the actor mailbox, so the
        ;; write sees the formatted text
        (when (and buf (pine.state.var:var :format-on-save buf))
          (let ((snap (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)))
            (pine.editor.ask:tell buf :indent-lines
                              :from 0 :to (1- (pine.text.buffer:line-count snap)))))
        (pine.editor.file:save-current-buffer))
    (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
(defcmd "split-window-below" () (pine.editor.window:%split-window :column))
(defcmd "split-window-right" () (pine.editor.window:%split-window :row))
(defcmd "delete-window" () (pine.editor.window:delete-window-cmd))
(defcmd "delete-other-windows" () (pine.editor.window:delete-other-windows-cmd))
(defcmd "other-window" () (pine.editor.window:other-window-cmd))
(defcmd "switch-buffer" ()
  (pine.editor.minibuffer:completing-read "Switch to: " (pine.editor.frame:list-buffers)
    (lambda (name)
      (let* ((client (pine.editor.frame:current-client))
             (buf (pine.editor.frame:switch-buffer name)))
        (when buf
          (sento.actor:tell (pine.editor.frame:renderer client)
                            (list :switch-buffer :buffer buf :name name))
          (pine.ui.render:subscribe-to-buffer buf))))))
(defcmd "list-buffers" ()
  (pine.editor.echo:message (format nil "buffers: ~{~a~^, ~}" (pine.editor.frame:list-buffers))))
(defcmd "execute-command" ()
  (pine.editor.minibuffer:completing-read "M-x " (pine.editor.command:all-command-names)
    (lambda (name) (pine.editor.command:call-command name))
    :history :commands))
(defcmd "eval-expression" ()
  (pine.editor.frame:prompt "Eval: "
    (lambda (text)
      (let ((pkg (let ((buf (pine.editor.motion:cur-buffer)))
                   (if buf
                       (pine.editor.motion:%buffer-package (sento.actor:ask-s buf '(:get-state) :time-out 5))
                       (find-package :cl-user)))))
        (pine.editor.evaluate:%eval-form-string text pkg)))
    :history :eval))
(defcmd "choose-restart" ()
  (let ((names (and pine.editor.debugger:*attended-session*
                    (remove nil (mapcar #'first
                                        (pine.editor.debugger:dbg-session-restarts pine.editor.debugger:*attended-session*))))))
    (if names
        (pine.editor.minibuffer:completing-read "Restart: " names
          (lambda (name) (pine.editor.debugger:invoke-pending-restart name)))
        (pine.editor.echo:message "no evaluation in the debugger"))))
(defcmd "debugger-abort" ()
  (pine.editor.debugger:invoke-pending-restart "ABORT"))
(defcmd "debugger-quit" ()
  (pine.editor.debugger:%debugger-quit))
(defcmd "debugger-next-session" ()
  "Page to the next live debugger session without resolving the current one."
  (let ((ordered (reverse pine.editor.debugger:*debugger-sessions*)))
    (if (> (length ordered) 1)
        (let* ((pos (or (position pine.editor.debugger:*attended-session* ordered) 0))
               (next (nth (mod (1+ pos) (length ordered)) ordered)))
          (pine.editor.debugger:%attend-session next))
        (pine.editor.echo:message "only one debugger session"))))
(defcmd "debugger" ()
  "Reopen the *debugger* on the attended session (after q), if one is parked."
  (if pine.editor.debugger:*attended-session*
      (pine.editor.debugger:%attend-session pine.editor.debugger:*attended-session*)
      (pine.editor.echo:message "no debugger session")))
(defcmd "toggle-debug-on-error" ()
  (let ((new (not (pine.state.var:var :debug-on-error))))
    (setf (pine.state.var:var :debug-on-error) new)
    (pine.editor.echo:message (format nil "debug-on-error ~:[disabled~;enabled~]" new))))
(defcmd "jobs" ()
  (pine.editor.layout:show-layout "*jobs*" (pine.editor.debugger:%jobs-builder)))
(defcmd "eval-last-sexp" () (pine.editor.evaluate:eval-last-sexp))
(defcmd "eval-defun" ()     (pine.editor.evaluate:eval-defun))
(defcmd "eval-buffer" ()    (pine.editor.evaluate:eval-buffer))
(defcmd "find-definition" () (pine.editor.evaluate:find-definition))
(defcmd "arglist" ()        (pine.editor.evaluate:symbol-arglist))
(defcmd "complete-symbol" () (pine.editor.evaluate:complete-symbol))
(defcmd "load-file" ()      (pine.editor.evaluate:load-file))
(defcmd "set-eval-target" ()
  (pine.editor.minibuffer:completing-read
   "Eval in: "
   (cons "local"
         (mapcar #'pine.core.actor:agent-info-name
                 (pine.core.actor:list-agents
                  (pine.editor.frame:server-of (pine.editor.frame:current-client)))))
   (lambda (name)
     (setf pine.editor.target:*eval-target* (if (string= name "local") :local name))
     (pine.editor.echo:message (format nil "eval target: ~a" name)))))
(defcmd "new-buffer" ()
  (pine.editor.frame:prompt "New buffer: "
    (lambda (name)
      (let ((buf (pine.editor.frame:make-buffer name))) (pine.ui.render:subscribe-to-buffer buf)))))
(defcmd "open-repl" ()
  (handler-case
      (let* ((client (pine.editor.frame:current-client))
             (buf (or (pine.editor.frame:repl-buffer client) (pine.editor.repl:start-repl))))
        (pine.editor.frame:switch-buffer "*repl*")
        (pine.ui.render:subscribe-to-buffer buf)
        (sento.actor:tell (pine.editor.frame:renderer client)
                          (list :switch-buffer :buffer buf :name "*repl*")))
    (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
(defcmd "terminal" ()
  (handler-case
      (let* ((client (pine.editor.frame:current-client))
             (f (pine.editor.frame:frame client))
             (cols (pine.text.window:frame-cols f))
             (rows (max 1 (- (pine.text.window:frame-rows f) 2)))
             (buf (pine.editor.frame:make-buffer "*terminal*")))
        (pine.term:open-terminal client buf :rows rows :cols cols)
        (pine.editor.frame:set-buffer-mode buf :terminal-mode)
        (pine.editor.frame:switch-buffer "*terminal*")
        (sento.actor:tell (pine.editor.frame:renderer client)
                          (list :switch-buffer :buffer buf :name "*terminal*")))
    (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
(defcmd "overwrite-mode" ()
  (let ((on (pine.editor.frame:toggle-minor-mode (pine.editor.frame:current-client) :overwrite-mode)))
    (pine.editor.echo:message (if on "Overwrite mode enabled" "Overwrite mode disabled"))))
(defcmd "describe-key" ()
  (pine.editor.echo:message "Describe key: ")
  (pine.editor.command:read-next-key
   (pine.editor.frame:current-client)
   (lambda (key) (pine.editor.echo:message (pine.editor.help:%describe-key-text key)))))
(defcmd "describe-bindings" ()
  (pine.editor.layout:show-layout "*bindings*" (pine.editor.debugger:%text-layout (pine.editor.help:%bindings-text))))
(defcmd "describe-mode" ()
  (pine.editor.layout:show-layout "*mode*" (pine.editor.debugger:%text-layout (pine.editor.help:%mode-text))))
(defcmd "describe-variables" ()
  (pine.editor.layout:show-layout "*variables*" (pine.editor.debugger:%text-layout (pine.editor.help:%variables-text))))
(defcmd "insert-tab" ()
  (let* ((c (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer c))
         (n (max 0 (pine.state.var:var :tab-width buf))))
    (when buf
      (pine.editor.ask:tell buf :insert :text (make-string n :initial-element #\Space)))))
(defcmd "indent-for-tab-command" ()
  "Reindent the current line to the column its mode dictates."
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf (pine.editor.ask:tell buf :indent-lines))))
(defcmd "indent-region" ()
  "Reindent every line spanned by the region."
  (let* ((buf (pine.editor.motion:cur-buffer))
         (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5))))
    (when state
      (multiple-value-bind (sl sc el ec) (pine.text.buffer:region-bounds state)
        (declare (ignore sc ec))
        (if sl
            (pine.editor.ask:tell buf :indent-lines :from sl :to el)
            (pine.editor.echo:message "no region"))))))
(defcmd "format-buffer" ()
  "Reindent the whole buffer off the parse tree, point preserved (in-image)."
  (let* ((buf (pine.editor.motion:cur-buffer))
         (snap (and buf (sento.actor:ask-s buf '(:get-snapshot) :time-out 5))))
    (when snap
      (pine.editor.ask:tell buf :indent-lines
                        :from 0 :to (1- (pine.text.buffer:line-count snap))))))
;; layout buffers: selection nav + activation on the node tree
(defcmd "layout-next" () (pine.editor.layout:layout-select 1))
(defcmd "layout-prev" () (pine.editor.layout:layout-select -1))
(defcmd "layout-activate" () (pine.editor.layout:layout-activate))
;; minibuffer-mode: the only keys the pine.editor.frame:prompt binds; everything else is the
;; ordinary buffer editing commands, so the pine.editor.frame:prompt edits like any buffer.
(defcmd "minibuffer-accept" () (pine.editor.minibuffer:minibuffer-accept))
(defcmd "minibuffer-abort" () (pine.editor.minibuffer:minibuffer-abort))
(defcmd "minibuffer-complete" () (pine.editor.minibuffer:minibuffer-complete))
(defcmd "minibuffer-next-candidate" () (pine.editor.minibuffer:completion-next))
(defcmd "minibuffer-prev-candidate" () (pine.editor.minibuffer:completion-prev))
(defcmd "minibuffer-history-prev" () (pine.editor.minibuffer:minibuffer-history-prev))
(defcmd "minibuffer-history-next" () (pine.editor.minibuffer:minibuffer-history-next))


(pine.editor.keymap:define-keys :global
  "C-g"      "keyboard-quit"
  "Escape"   "keyboard-quit"
  "C-x C-f"  "find-file"
  "C-x C-r"  "find-recent"
  "C-x C-s"  "save-file"
  "C-x b"    "switch-buffer"
  "C-x 2"    "split-window-below"
  "C-x 3"    "split-window-right"
  "C-x 0"    "delete-window"
  "C-x 1"    "delete-other-windows"
  "C-x o"    "other-window"
  "C-x n"    "new-buffer"
  "C-x r"    "open-repl"
  "C-x t"    "terminal"
  "C-x C-e"  "eval-last-sexp"
  "C-M-x"    "eval-defun"
  "M-."      "find-definition"
  "M-x"      "execute-command"
  "Insert"   "overwrite-mode"
  ;; prefix argument
  "C-u"      "universal-argument"
  "M--"      "negative-argument"
  ;; help
  "C-h k"    "describe-key"
  "C-h b"    "describe-bindings"
  "C-h m"    "describe-mode"
  "C-h v"    "describe-variables")

(let ((map (pine.editor.keymap:keymap :global)))
  (dotimes (d 10)
    (pine.editor.keymap:define-key map (pine.editor.key:parse-chord (format nil "M-~d" d))
                            "digit-argument")))

(pine.editor.keymap:define-keys :text-mode
  "BackSpace"   "backspace"
  "Return"      "newline"
  "Tab"         "indent-for-tab-command"
  "Up"          "previous-line"
  "Down"        "next-line"
  "Left"        "backward-char"
  "Right"       "forward-char"
  "C-f"         "forward-char"
  "C-b"         "backward-char"
  "C-n"         "next-line"
  "C-p"         "previous-line"
  "C-a"         "beginning-of-line"
  "C-e"         "end-of-line"
  "C-d"         "delete-char"
  "M-f"         "forward-word"
  "M-b"         "backward-word"
  "M-d"         "kill-word"
  "M-BackSpace" "backward-kill-word"
  "C-s"         "isearch-forward"
  "C-r"         "isearch-backward"
  "M-<"         "beginning-of-buffer"
  "M->"         "end-of-buffer"
  "C-space"     "set-mark"
  "C-k"         "kill-line"
  "C-w"         "kill-region"
  "M-w"         "copy-region"
  "C-y"         "yank"
  "M-y"         "yank-pop"
  "C-v"         "scroll-down"
  "M-v"         "scroll-up"
  "Prior"       "scroll-up"
  "Next"        "scroll-down"
  "C-z"         "undo"
  "C-/"         "undo"
  "C-?"         "redo"
  ;; structural navigation (tree-sitter)
  "C-M-f"       "forward-sexp"
  "C-M-b"       "backward-sexp"
  "C-M-a"       "beginning-of-defun"
  "C-M-e"       "end-of-defun"
  "C-M-space"   "mark-sexp"
  ;; Tab reindents (mode-aware); pine.editor.minibuffer:completion has the Emacs binding, and
  ;; region and whole-buffer reformat get their own keys
  "C-M-\\"      "indent-region")

;;;; lisp-mode: the SLIME chord set.
(pine.editor.keymap:define-keys :lisp-mode
  "C-c C-c"  "eval-defun"
  "C-c C-k"  "eval-buffer"
  "C-c C-l"  "load-file"
  "C-c C-d"  "arglist"
  "C-M-i"    "complete-symbol"
  "M-Tab"    "complete-symbol")

;;;; debugger-mode: the restart rows answer to layout-mode's Return/C-n/C-p;
;;;; these are the extras.
(pine.editor.keymap:define-keys :debugger-mode
  "a"    "debugger-abort"
  "q"    "debugger-quit"
  "Tab"  "debugger-next-session")

;;;; layout-mode: selection nav and activation on a layout buffer.
(pine.editor.keymap:define-keys :layout-mode
  "Down"    "layout-next"
  "C-n"     "layout-next"
  "Up"      "layout-prev"
  "C-p"     "layout-prev"
  "Return"  "layout-activate")

;;;; minibuffer-mode: accept, abort, pine.editor.completion:complete, pine.editor.completion:candidate motion. Every other
;;;; key falls through to text-mode, so the pine.editor.frame:prompt has full editing.
(pine.editor.keymap:define-keys :minibuffer-mode
  "Return"  "minibuffer-accept"
  "Escape"  "minibuffer-abort"
  "C-g"     "minibuffer-abort"
  "Tab"     "minibuffer-complete"
  "Down"    "minibuffer-next-candidate"
  "C-n"     "minibuffer-next-candidate"
  "Up"      "minibuffer-prev-candidate"
  "C-p"     "minibuffer-prev-candidate"
  "M-p"     "minibuffer-history-prev"
  "M-n"     "minibuffer-history-next")
