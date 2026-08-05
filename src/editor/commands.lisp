(defpackage #:pine.editor.commands
  (:use :cl #:pine.editor.isearch)
  (:export #:start-editor))

(in-package #:pine.editor.commands)
(named-readtables:in-readtable pine.path:syntax)

(defun start-editor ()
  (let* ((client (pine.editor.frame:current-client))
         (server (pine.editor.frame:server-of client)))
    (handler-case (pine.ts.runtime:ensure-ts (pine.core.server:ts-runtime server))
      (error () nil))
    (pine.editor.render:start-renderer client)
    (pine.echo:ensure)
    (setf pine.key:*terminal-handler* #'pine.term:terminal-dispatch)
    (pine.ns:raise :err)
    (pine.editor.debugger:install)
    (let ((buf (pine.editor.frame:make-buffer "scratch")))
      (pine.editor.frame:set-buffer-mode buf :text)
      (pine.buf:put buf :package :pine-user))
    (pine.editor.render:relayout)))

;;;; Commands

(defmacro defcmd (name (&rest args) &body body)
  `(pine.cmd:defcmd ,name ,args ,@body))

(defcmd "keyboard-quit" ()
  (setf (pine.key:said :pending) nil)
  ;; C-g while a fault is up interrupts a runaway evaluation into its abort
  ;; restart rather than only offering it
  (pine.editor.debugger:abort-attended)
  (let ((buf (pine.buf:current-name)))
    (when buf
      (pine.buf:put buf :mark nil)))
  (pine.echo:abort))
(defcmd "backspace" ()
  (let ((buf (pine.buf:current-name))) (when buf (pine.buf:delete-back buf))))
(defcmd "delete-char" ()
  (let ((buf (pine.buf:current-name)) (snap (pine.buf:focused-snap)))
    (when (and buf snap)
      (let ((line (pine.text:point-line snap))
            (col (pine.text:point-col snap)))
        (pine.buf:edit
         buf (fset:seq :delete (fset:seq line col) (fset:seq line (1+ col))))))))
(defcmd "newline" ()
  (let ((buf (pine.buf:current-name))) (when buf (pine.buf:edit buf (fset:seq :newline)))))
(defcmd "undo" ()
  (let ((buf (pine.buf:current-name))) (when buf (pine.buf:edit buf (fset:seq :undo)))))
(defcmd "redo" ()
  (let ((buf (pine.buf:current-name))) (when buf (pine.buf:edit buf (fset:seq :redo)))))

(defcmd "forward-char" () (pine.buf:move-chars (pine.key:times)))
(defcmd "backward-char" () (pine.buf:move-chars (- (pine.key:times))))
(defcmd "next-line" () (pine.buf:move-lines (pine.key:times)))
(defcmd "previous-line" () (pine.buf:move-lines (- (pine.key:times))))
(defcmd "forward-word" () (pine.buf:move-words (pine.key:times)))
(defcmd "backward-word" () (pine.buf:move-words (- (pine.key:times))))
(defcmd "kill-word" () (pine.kill:kill-words (pine.key:times)))
(defcmd "backward-kill-word" () (pine.kill:kill-words (- (pine.key:times))))
(defcmd "isearch-forward" ()  (isearch-start :forward))
(defcmd "isearch-backward" () (isearch-start :backward))
;; the prefix commands write the prefix rather than using it, which is the
;; whole of what makes them different: a command that leaves it alone has it
;; cleared when it returns.
(defcmd "universal-argument" ()
  (setf (pine.key:prefix) (list (* 4 (pine.key:times)))))
(defcmd "digit-argument" ()
  (let* ((key (pine.key:key-of))
         (d (and key (digit-char-p (char (pine.key:key-sym key) 0))))
         (cur (pine.key:prefix)))
    (when d
      (setf (pine.key:prefix)
            (cond ((eq cur '-) (- d))
                  ((and (integerp cur) (minusp cur)) (- (+ (* 10 (- cur)) d)))
                  ((integerp cur) (+ (* 10 cur) d))
                  (t d))))))
(defcmd "negative-argument" ()
  (let ((cur (pine.key:prefix)))
    (setf (pine.key:prefix)
          (cond ((eq cur '-) nil)
                ((integerp cur) (- cur))
                (t '-)))))

(defcmd "beginning-of-line" ()
  (let ((buf (pine.buf:current-name)) (snap (pine.buf:focused-snap)))
    (when (and buf snap)
      (pine.buf:put-point buf (pine.text:point-line snap) 0))))
(defcmd "end-of-line" ()
  (let ((buf (pine.buf:current-name)) (snap (pine.buf:focused-snap)))
    (when (and buf snap)
      (let ((len (length (fset:@ (pine.text:lines snap) (pine.text:point-line snap)))))
        (pine.buf:put-point buf (pine.text:point-line snap) len)))))
(defcmd "beginning-of-buffer" ()
  (let ((buf (pine.buf:current-name))) (when buf (pine.buf:put-point buf 0 0))))
(defcmd "end-of-buffer" ()
  (let ((buf (pine.buf:current-name)) (snap (pine.buf:focused-snap)))
    (when (and buf snap)
      (let* ((ll (1- (pine.text:line-count snap)))
             (lc (length (fset:@ (pine.text:lines snap) ll))))
        (pine.buf:put-point buf ll lc)))))

;; a page is the pane's own height, less the two lines an Emacs page keeps for
;; context. How tall the pane is, is a place the renderer publishes.
(defun %page ()
  (let ((at (or (pine.win:focused) (pine.buf:at "current"))))
    (max 1 (- (or (pine.ns:read (pine.path:path at "height")) 24) 2))))

(defcmd "scroll-down" () (pine.editor.win:scroll-window (%page)))
(defcmd "scroll-up" ()   (pine.editor.win:scroll-window (- (%page))))

(defcmd "forward-sexp" ()      (pine.buf:move-sexp :forward-sexp))
(defcmd "backward-sexp" ()     (pine.buf:move-sexp :backward-sexp))
(defcmd "beginning-of-defun" () (pine.buf:move-sexp :beginning-of-defun))
(defcmd "end-of-defun" ()      (pine.buf:move-sexp :end-of-defun))
(defcmd "mark-sexp" ()
  (pine.kill:set-mark)
  (pine.buf:move-sexp :forward-sexp))

(defcmd "set-mark" ()     (pine.kill:set-mark))
(defcmd "kill-line" ()    (pine.kill:kill-line))
(defcmd "kill-region" ()  (pine.kill:kill-region))
(defcmd "copy-region" ()  (pine.kill:copy-region))
(defcmd "yank" ()         (pine.kill:yank))
(defcmd "yank-pop" ()     (pine.kill:yank-pop))

(defun %said (fault)
  "Say what a fault was, in the echo line. It is at /err either way; this is the
line the person who asked for it reads."
  (when fault
    (pine.echo:message (format nil "error: ~a" (pine.err:fault-condition fault))))
  nil)

(defun %visiting (path)
  (multiple-value-bind (buf fault)
      (pine.err:attempt (lambda () (pine.buf:find-file path))
                        (format nil "visiting ~a" path))
    (or buf (%said fault))))

(defcmd "find-file" ()
  (pine.ns:write /echo (fset:map (:prompt "Find file: ")
                                 (:complete :file)
                                 (:history :files)
                                 (:then #'%visiting))))
(defcmd "find-recent" ()
  (let ((items (pine.data:vals (pine.ns:read /recent/* (fset:empty-map)))))
    (if items
        (pine.ns:write /echo (fset:map (:prompt "Recent: ")
                                       (:complete items)
                                       (:then #'%visiting)))
        (pine.echo:message "no recent files"))))
(defcmd "save-file" ()
  (%said
   (nth-value
    1
    (pine.err:attempt
     (lambda ()
       (let ((buf (pine.buf:current-name)))
         ;; opt-in apheleia-style reformat before write; the reindent is a tell
         ;; and save's :get-state queues behind it in the actor mailbox, so the
         ;; write sees the formatted text
         (when (and buf (pine.editor.help:setting :format-on-save))
           (let ((snap (pine.buf:snapshot-of buf)))
             (pine.buf:indent (pine.buf:name-of buf) 0
                              (1- (pine.text:line-count snap)))))
         (pine.buf:save-current)))
     "saving the buffer"))))
(defcmd "split-window-below" () (pine.editor.win:split-window :column))
(defcmd "split-window-right" () (pine.editor.win:split-window :row))
(defcmd "delete-window" () (pine.editor.win:delete-window-cmd))
(defcmd "delete-other-windows" () (pine.editor.win:delete-other-windows-cmd))
(defcmd "other-window" () (pine.editor.win:other-window-cmd))
(defcmd "switch-buffer" ()
  (pine.ns:write /echo
                 (fset:map
                  (:prompt "Switch to: ")
                  (:complete (pine.editor.frame:list-buffers))
                  (:then (lambda (name)
                           (let* ((client (pine.editor.frame:current-client))
                                  (buf (pine.editor.frame:switch-buffer name)))
                             (when buf
                               (sento.actor:tell
                                (pine.editor.frame:renderer client)
                                (list :switch-buffer :buffer buf :name name)))))))))
(defcmd "list-buffers" ()
  (pine.echo:message (format nil "buffers: ~{~a~^, ~}" (pine.editor.frame:list-buffers))))
(defcmd "execute-command" ()
  (pine.ns:write /echo (fset:map (:prompt "M-x ")
                                 (:complete (pine.cmd:names))
                                 (:history :commands)
                                 (:then #'pine.key:call-command))))
(defcmd "eval-expression" ()
  (pine.ns:write /echo
                 (fset:map
                  (:prompt "Eval: ")
                  (:history :eval)
                  (:then (lambda (text)
                           ;; M-: reads in the language of the buffer it was
                           ;; typed from, so (ns:read /win/0) is evaluable
                           (let* ((buf (pine.buf:current-name))
                                  (state (and buf (pine.buf:state-of buf)))
                                  (pkg (if state
                                           (pine.text:buffer-package state)
                                           (find-package :cl-user))))
                             (pine.eval:form-string
                              text pkg
                              :readtable (and state
                                              (pine.text:buffer-readtable-name
                                               state)))))))))
(defcmd "choose-restart" ()
  (let ((names (remove nil (pine.editor.debugger:restarts))))
    (if names
        (pine.ns:write /echo (fset:map (:prompt "Restart: ")
                                       (:complete names)
                                       (:then #'pine.editor.debugger:take)))
        (pine.echo:message "no fault in the debugger"))))
(defcmd "debugger-abort" ()
  (pine.editor.debugger:take "ABORT"))
(defcmd "debugger-quit" ()
  (pine.editor.debugger:quit))
(defcmd "debugger-next-session" ()
  "Page to the next live fault without deciding this one."
  (pine.editor.debugger:next))
(defcmd "debugger" ()
  "Reopen the *debugger* on the attended fault (after q), if one is there."
  (if (pine.editor.debugger:attended)
      (pine.editor.debugger:attend (pine.editor.debugger:attended))
      (pine.echo:message "no fault at /err")))
(defcmd "toggle-debug-on-error" ()
  (let ((new (not (pine.ns:read (pine.path:parse "/debug-on-error")))))
    (pine.ns:write (pine.path:parse "/debug-on-error") new)
    (pine.echo:message (format nil "debug-on-error ~:[disabled~;enabled~]" new))))
(defcmd "jobs" ()
  (pine.view:show "*jobs*" (pine.editor.debugger:jobs-builder)))
(defcmd "eval-last-sexp" () (pine.eval:last-sexp))
(defcmd "eval-defun" ()     (pine.eval:defun-at-point))
(defcmd "eval-buffer" ()    (pine.eval:buffer))
(defcmd "find-definition" () (pine.eval:definition))
(defcmd "find-references" () (pine.eval:references))
(defcmd "describe-thing" () (pine.eval:hover))
(defcmd "arglist" ()        (pine.eval:arglist))
(defcmd "complete-symbol" () (pine.eval:complete-symbol))
(defcmd "load-file" ()      (pine.eval:load))
(defcmd "set-eval-target" ()
  (pine.ns:write
   /echo
   (fset:map
    (:prompt "Eval in: ")
    (:complete (cons "local"
                     (mapcar #'pine.core.actor:agent-info-name
                             (pine.core.actor:list-agents
                              (pine.editor.frame:server-of
                               (pine.editor.frame:current-client))))))
    (:then (lambda (name)
             (setf (pine.eval:target) (if (string= name "local") :local name))
             (pine.echo:message (format nil "eval target: ~a" name)))))))
(defcmd "new-buffer" ()
  (pine.ns:write /echo (fset:map (:prompt "New buffer: ")
                                 (:then #'pine.editor.frame:make-buffer))))
(defcmd "open-repl" ()
  (let* ((client (pine.editor.frame:current-client))
         (name pine.editor.repl:+buffer-name+)
         (buf (or (pine.editor.repl:repl-buffer) (pine.editor.repl:start-repl))))
    (pine.editor.frame:switch-buffer name)
    
    (sento.actor:tell (pine.editor.frame:renderer client)
                      (list :switch-buffer :buffer buf :name name))))
(defcmd "terminal" ()
  (%said
   (nth-value
    1
    (pine.err:attempt
     (lambda ()
       (let* ((client (pine.editor.frame:current-client))
              (cols (pine.editor.frame:cols client))
              (rows (max 1 (- (pine.editor.frame:rows client) 2)))
              (wake (pine.editor.frame:terminal-wake client))
              (buf (pine.editor.frame:make-buffer "*terminal*")))
         (pine.term:open-terminal
          buf :rows rows :cols cols
          :on-output (lambda () (sb-thread:signal-semaphore wake)))
         (pine.editor.frame:set-buffer-mode buf :terminal)
         (pine.editor.frame:switch-buffer "*terminal*")
         (sento.actor:tell (pine.editor.frame:renderer client)
                           (list :switch-buffer :buffer buf :name "*terminal*"))))
     "opening a terminal"))))
(defcmd "overwrite-mode" ()
  (let ((on (pine.editor.frame:toggle-minor-mode :overwrite)))
    (pine.echo:message (if on "Overwrite mode enabled" "Overwrite mode disabled"))))
(defcmd "describe-key" ()
  (pine.echo:message "Describe key: ")
  (pine.key:read-next-key
   (lambda (key) (pine.echo:message (pine.editor.help:describe-key-text key)))))
(defcmd "describe-bindings" ()
  (pine.view:show "*bindings*" (function pine.editor.help:bindings)))
(defcmd "describe-mode" ()
  (pine.view:show "*mode*" (function pine.editor.help:modes)))
(defcmd "describe-variables" ()
  (pine.view:show "*variables*" (function pine.editor.help:variables)))
(defcmd "insert-tab" ()
  (let* ((buf (pine.editor.frame:current-buffer))
         (n (max 0 (or (pine.editor.help:setting :tab-width) 8))))
    (when buf
      (pine.ns:write (pine.buf:at (pine.buf:name-of buf) :text)
                     (fset:seq :insert (make-string n :initial-element #\Space))))))
(defcmd "indent-for-tab-command" ()
  "Reindent the current line to the column its mode dictates."
  (let ((buf (pine.buf:current-name)))
    (when buf (pine.buf:indent (pine.buf:name-of buf)))))
(defcmd "indent-region" ()
  "Reindent every line spanned by the region."
  (let* ((buf (pine.buf:current-name))
         (state (and buf (pine.buf:state-of buf))))
    (when state
      (multiple-value-bind (sl sc el ec) (pine.text:region-bounds state)
        (declare (ignore sc ec))
        (if sl
            (pine.buf:indent (pine.buf:name-of buf) sl el)
            (pine.echo:message "no region"))))))
(defcmd "format-buffer" ()
  "Reindent the whole buffer off the parse tree, point preserved (in-image)."
  (let* ((buf (pine.buf:current-name))
         (snap (and buf (pine.buf:snapshot-of buf))))
    (when snap
      (pine.buf:indent (pine.buf:name-of buf) 0
                       (1- (pine.text:line-count snap))))))
;; UI buffers: the selection moves and the row acts, as verbs on the buffer
(defcmd "list-next" () (pine.ns:write (pine.buf:at "current") (fset:seq :select 1)))
(defcmd "list-prev" () (pine.ns:write (pine.buf:at "current") (fset:seq :select -1)))
(defcmd "list-activate" () (pine.ns:write (pine.buf:at "current") (fset:seq :activate)))
;; minibuffer-mode: the only keys the prompt binds; everything else is the
;; ordinary buffer editing commands, so the prompt edits like any buffer.
(defcmd "minibuffer-accept" () (pine.echo:accept))
(defcmd "minibuffer-abort" () (pine.echo:abort))
(defcmd "minibuffer-complete" () (pine.echo:complete-selection))
(defcmd "minibuffer-next-candidate" () (pine.echo:next))
(defcmd "minibuffer-prev-candidate" () (pine.echo:previous))
(defcmd "minibuffer-history-prev" () (pine.echo:history-previous))
(defcmd "minibuffer-history-next" () (pine.echo:history-next))


(pine.key:define-keys :global
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
  "M-?"      "find-references"
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

(dotimes (d 10)
  (pine.key:bind :global (format nil "M-~d" d) "digit-argument"))

(pine.key:define-keys :text
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
  ;; Tab reindents (mode-aware); completion has the Emacs binding, and
  ;; region and whole-buffer reformat get their own keys
  "C-M-\\"      "indent-region")

;;;; lisp-mode: the SLIME chord set.
(pine.key:define-keys :lisp
  "C-c C-c"  "eval-defun"
  "C-c C-k"  "eval-buffer"
  "C-c C-l"  "load-file"
  "C-c C-d"  "arglist"
  "C-M-i"    "complete-symbol"
  "M-Tab"    "complete-symbol")

;;;; the debugger: the restart rows answer to a UI buffer's Return/C-n/C-p;
;;;; these are the extras.
(pine.key:define-keys :debugger
  "a"    "debugger-abort"
  "q"    "debugger-quit"
  "Tab"  "debugger-next-session")

;;;; a UI buffer: the selection moves and the row acts.
(pine.key:define-keys :list
  "Down"    "list-next"
  "C-n"     "list-next"
  "Up"      "list-prev"
  "C-p"     "list-prev"
  "Return"  "list-activate")

;;;; minibuffer-mode: accept, abort, complete, candidate motion. Every other
;;;; key falls through to text-mode, so the prompt has full editing.
(pine.key:define-keys :minibuffer
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
