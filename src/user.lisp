(defpackage :pine.user
  (:nicknames :pine-user)
  (:use :cl)
  (:local-nicknames (#:world #:pine.state.world))
  ;; one namespace and three verbs: everything a config says, it says with
  ;; these, and a path is a literal the reader builds
  (:shadowing-import-from :pine.ns #:read #:write #:watch)
  (:import-from :pine.ui.build
                #:column #:row #:centerbox #:label #:icon #:button #:ring
                #:gap #:rule #:rows #:choice)
  (:import-from :pine.editor.completion
                #:candidate #:register-source #:register-actions
                #:candidate-actions #:completion-widget)
  (:import-from :pine.ui.face
                #:deftheme #:defface #:load-theme #:color #:metric #:face-fg)
  (:import-from :pine.editor.frame
                #:make-buffer #:kill-buffer #:switch-buffer #:list-buffers
                #:current-client #:current-buffer
                #:current-buffer-mode #:set-buffer-mode)
  (:import-from :pine.editor.ask #:ask #:tell)
  (:import-from :pine.state.ref #:defref #:ref)
  (:import-from :pine.state.var #:defonce)
  (:import-from :pine.source #:defsource #:defpoll #:start-stream #:start-poll
                #:split #:lines #:starts-with #:first-number #:read-int-file
                #:json)
  (:import-from :pine.editor.command #:call-command)
  (:import-from :pine.editor.minibuffer
                #:completing-read #:read-file-name #:prompt)
  (:import-from :pine.editor.echo #:message)
  (:import-from :pine.editor.evaluate #:eval-last-sexp #:eval-buffer)
  (:import-from :pine #:*frontends*)
  (:export
   ;; the frontends the daemon owns
   #:*frontends*
   ;; surfaces
   #:defsurface #:show #:hide #:toggle
   ;; widgets: containers
   #:column #:row #:centerbox #:center #:box #:scroll
   ;; widgets: content
   #:label #:icon #:image #:rule #:gap
   ;; widgets: controls
   #:button #:slider #:ring #:choice #:rows
   ;; widgets: views -- a window is a live view of the buffer you name
   #:calendar #:window #:buffer #:terminal #:modeline #:echo #:minibuffer
   ;; completion facility
   #:candidate #:register-source #:register-actions #:candidate-actions
   #:completion-widget
   ;; the namespace
   #:read #:write #:watch
   ;; style
   #:deftheme #:defface #:load-theme #:color #:metric #:face-fg
   ;; data
   #:defref #:ref #:defpoll #:defsource #:sh #:launch
   ;; writing a source: the stream/poll primitives and their helpers
   #:start-stream #:start-poll
   #:split #:lines #:starts-with #:first-number #:read-int-file #:json
   ;; style rules
   #:defrules
   ;; behavior
   #:defcommand #:call-command
   ;; keys
   #:kbd #:keymap #:define-key #:global-set-key
   ;; modes: a mode is a map at /mode, so a config writes one
   #:enable-minor-mode #:disable-minor-mode #:toggle-minor-mode
   ;; editor variables
   #:defonce #:var
   ;; prompts + echo
   #:completing-read #:read-file-name #:prompt #:message
   ;; processes
   #:defagent #:spawn #:kill
   ;; buffers / editor
   #:make-buffer #:kill-buffer #:switch-buffer #:list-buffers
   #:ask #:tell #:current-client #:current-buffer
   #:current-buffer-mode #:set-buffer-mode
   #:eval-last-sexp #:eval-buffer))

(in-package #:pine.user)

;;;; The pine.user language: the vocabulary a user writes in init.lisp. It gathers
;;;; the primitives from every subsystem under one set of clean, symbol-named
;;;; forms, so a config reads as one language and never qualifies a package. The
;;;; catalog is doc/pine-user.org.
;;;; Widget renames. The layout constructors carry terser class names; the
;;;; language uses the plain ones.

(defun center (&rest args) (apply #'pine.ui.build:centered args))
(defun box    (&rest args) (apply #'pine.ui.build:boxed args))
(defun scroll (&rest args) (apply #'pine.ui.build:viewport args))
(defun slider (&rest args) (apply #'pine.ui.build:meter args))
(defun calendar (&rest args) (apply #'pine.ui.build:cal args))
(defun image  (path &rest args) (apply #'pine.ui.build:pic path args))

;;;; Views. A WINDOW is a live view of exactly the buffer you name; in the
;;;; editor surface the declared tree is the LIVE arrangement the split
;;;; commands mutate. MODELINE and ECHO are widgets you place where you want.

(defun buffer (name) (pine.editor.frame:make-buffer name))          ; get or create by name
(defun window (x &rest props)
  "A live view of X's buffer (an actor or a name string): visible lines,
highlights, region, terminal grid, or layout rows, rendered at the rect the
tree arranges it into. Props are node style: :opacity :font-px :class :expand."
  (apply #'pine.editor.session:editor-window-node x props))
(defun terminal (x &rest props)
  "A window view of a terminal buffer."
  (apply #'pine.editor.session:editor-terminal-node x props))
(defun modeline (&optional x)
  "The mode line: the focused window's by default, or X's buffer's."
  (pine.editor.session:editor-modeline-node x))
(defun echo () (pine.editor.session:editor-echo-node))
(defun minibuffer () (echo))

;;;; Data. SH captures a command's output for a poll or source body; LAUNCH runs
;;;; one detached, returning a thunk for :on-click. REF reads a ref, and the
;;;; read is tracked, so a surface re-renders when a ref it read changes.

(defun sh (command)
  (or (ignore-errors
        (string-trim '(#\newline #\space)
                     (uiop:run-program (list "sh" "-c" command)
                                       :output :string :ignore-error-status t)))
      ""))

(defun launch (command)
  (lambda () (ignore-errors (uiop:launch-program (list "sh" "-c" command)))))

;;;; Behavior.

(defmacro defcommand (name args &body body)
  `(pine.cmd:defcmd ,name ,args ,@body))

;;;; Keys (Emacs voice). KBD parses a chord sequence; KEYMAP names one; a
;;;; command is bound by its string name. A defcommand becomes reachable by key.

(defun kbd (spec)
  "Parse a key sequence -- \"C-x C-f\", \"M-.\", \"Return\" -- into a key (one
chord) or a list of keys (a sequence), exactly what DEFINE-KEY takes."
  (let ((keys (mapcar #'pine.editor.key:parse-key
                      (remove "" (uiop:split-string spec :separator '(#\space))
                              :test #'string=))))
    (if (= 1 (length keys)) (first keys) keys)))

(defun keymap (designator)
  "The map DESIGNATOR names, which is a segment under /key: :global for the
map no mode owns, :wm for the window manager's (whose chords the compositor
delivers instead of the focused window), or a mode keyword."
  designator)

(defun define-key (where keys command)
  "Bind KEYS (from KBD) to COMMAND in WHERE -- :global, :wm, or a mode
keyword. COMMAND is a command designator, a write-map or a function."
  (pine.editor.keymap:bind (keymap where) keys command))

(defun global-set-key (keys command)
  "Bind KEYS to COMMAND in the map no mode owns."
  (pine.editor.keymap:bind :global keys command))

;;;; Modes. A mode is a map at /mode, so defining one is writing it:
;;;;
;;;;   (write /mode/python {:parent :prog :grammar :python :files ["*.py"]
;;;;                        :on {:newline (fn (buf) {...})}})
;;;;
;;;; There is no defmode, no class and no registration step, and what a mode
;;;; changes about a verb is the :on entry the verb finds.

;;;; Minor-mode toggles, client-implicit.

(defun enable-minor-mode  (name) (pine.editor.frame:enable-minor-mode  (current-client) name))
(defun disable-minor-mode (name) (pine.editor.frame:disable-minor-mode (current-client) name))
(defun toggle-minor-mode  (name) (pine.editor.frame:toggle-minor-mode  (current-client) name))

;;;; Editor variables. The buffer is implicit here and explicit below: which
;;;; buffer is current is a client's business, and pine.state.var sits under every
;;;; client, so it never guesses.

(defun var (name &optional (buffer (pine.editor.frame:buffer-in-scope)))
  "Editor variable NAME: buffer-local in BUFFER (the current buffer by
default), else global, else the declared default."
  (pine.state.var:var name buffer))

(defun (setf var) (value name &optional buffer)
  (setf (pine.state.var:var name buffer) value))

;;;; Style rules. Add to the one stylesheet the cairo painter and the cell
;;;; render both read; user rules win the cascade. VALUES are evaluated, so
;;;; (color :accent) resolves against the active theme.

(defmacro defrules (&rest rules)
  "Add style rules. Each is (SELECTOR PROP VALUE ...): SELECTOR is a keyword
(one class), a list of keywords (compound), or a selector string; values are
lisp values (numbers for px/opacity) or CSS strings. Redefining a selector
replaces it. Reload-safe; the rules reach every attached frontend."
  `(pine.ui.rules:add-rules
    (list ,@(mapcar (lambda (rule)
                      (destructuring-bind (sel &rest props) rule
                        `(list ,(if (and (consp sel) (every #'keywordp sel))
                                    `',sel
                                    sel)
                               (list ,@props))))
                    rules))))

;;;; Processes. What keeps one running is its declaration under /proc.

(defun spawn (name) (pine.core.actor:spawn-agent pine.core.server:*server* (string name)))
(defun kill (name)  (pine.core.actor:kill-agent  pine.core.server:*server* (string name)))

(defmacro defagent (name &body body)
  "Spawn a process agent named NAME and run BODY in its own image."
  (let ((n (string name)))
    `(progn
       (spawn ,n)
       (pine.core.actor:agent-eval pine.core.server:*server* ,n
                              ,(format nil "~s" `(progn ,@body)))
       ,n)))

;;;; Surfaces. A name is a symbol; the desktop machinery keys by its downcased
;;;; string. SHOW / HIDE / TOGGLE act on the desktop client in scope, bound while
;;;; a builder runs and while a click handler fires.

(defmacro defsurface (name (&rest opts) &body body)
  "Define surface NAME (a symbol). OPTS declare placement (:as :bar / :panel /
:echo / :toplevel). BODY is the widget tree.

A surface is a path: the tree is written to /surface/NAME as an expression, so
it is built again whenever anything it read moves, and :as is the leaf under it
that says where wayland puts it."
  (let ((key (string-downcase (string name))))
    `(progn
       (write ,(pine.path:path (pine.path:parse "/surface") key "as") ',(getf opts :as))
       (write ,(pine.path:path (pine.path:parse "/surface") key) (progn ,@body)))))

;;;; SHOW / HIDE / TOGGLE take a bare surface symbol and produce a thunk (like
;;;; LAUNCH), so :on-click (show audio) does the right thing: shows on click, on
;;;; the client in scope. Call the thunk directly to act now.

(defmacro show (name)
  `(lambda () (pine.desktop:show-panel pine.desktop:*surface-client* ,(string-downcase (string name)))))
(defmacro hide (name)
  `(lambda () (pine.desktop:hide-panel pine.desktop:*surface-client* ,(string-downcase (string name)))))
(defmacro toggle (name)
  `(lambda () (pine.desktop:show-panel pine.desktop:*surface-client* ,(string-downcase (string name)))))
