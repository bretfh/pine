(in-package :cl-user)

;;;; The pine.user language: the vocabulary a user writes in init.lisp. It gathers
;;;; the primitives from every subsystem under one set of clean, symbol-named
;;;; forms, so a config reads as one language and never qualifies a package. The
;;;; catalog is doc/pine-user.org.

(in-package :pine.user)

;;;; Widget renames. The layout constructors carry terser class names; the
;;;; language uses the plain ones.

(defun center (&rest args) (apply #'pine.layout:centered args))
(defun box    (&rest args) (apply #'pine.layout:boxed args))
(defun scroll (&rest args) (apply #'pine.layout:viewport args))
(defun slider (&rest args) (apply #'pine.layout:meter args))
(defun calendar (&rest args) (apply #'pine.layout:cal args))
(defun image  (path &rest args) (apply #'pine.layout:pic path args))

;;;; Views. A WINDOW is a live view of exactly the buffer you name; in the
;;;; editor surface the declared tree is the LIVE arrangement the split
;;;; commands mutate. MODELINE and ECHO are widgets you place where you want.

(defun buffer (name) (pine.client:make-buffer name))          ; get or create by name
(defun window (x &rest props)
  "A live view of X's buffer (an actor or a name string): visible lines,
highlights, region, terminal grid, or layout rows, rendered at the rect the
tree arranges it into. Props are node style: :opacity :font-px :class :expand."
  (apply #'pine.editor:editor-window-node x props))
(defun terminal (x &rest props)
  "A window view of a terminal buffer."
  (apply #'pine.editor:editor-terminal-node x props))
(defun modeline (&optional x)
  "The mode line: the focused window's by default, or X's buffer's."
  (pine.editor:editor-modeline-node x))
(defun echo () (pine.editor:editor-echo-node))
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
  `(pine.command:define-command ,name ,args ,@body))

;;;; Keys (Emacs voice). KBD parses a chord sequence; KEYMAP names one; a
;;;; command is bound by its string name. A defcommand becomes reachable by key.

(defun kbd (spec)
  "Parse a key sequence -- \"C-x C-f\", \"M-.\", \"Return\" -- into a key (one
chord) or a list of keys (a sequence), exactly what DEFINE-KEY takes."
  (let ((keys (mapcar #'pine.key:parse-key
                      (remove "" (uiop:split-string spec :separator '(#\space))
                              :test #'string=))))
    (if (= 1 (length keys)) (first keys) keys)))

(defun keymap (designator)
  "The keymap named DESIGNATOR: :global for the global map, :wm for the window
manager's (whose chords the compositor delivers instead of the focused
window), or a mode keyword for that mode's map."
  (case designator
    (:global (pine.mode:global-keymap))
    (:wm (pine.wm:wm-keymap))
    (t (let ((m (find-mode designator)))
         (if m (pine.mode:mode-keymap m) (error "no mode ~s" designator))))))

(defun define-key (where keys command)
  "Bind KEYS (from KBD) to COMMAND (a command designator) in WHERE -- a keymap,
:global, :wm, or a mode keyword."
  (pine.keymap:define-key
   (if (pine.keymap:keymap-p where) where (keymap where))
   keys (pine.command:command-key command)))

(defun global-set-key (keys command)
  "Bind KEYS to COMMAND in the global keymap."
  (pine.keymap:define-key (pine.mode:global-keymap) keys
                          (pine.command:command-key command)))

;;;; Modes. DEFMODE / DEFMINOR make a real CLOS class (named NAME) and register
;;;; a mode instance (keyword :NAME) with its own keymap. Behaviour is plain
;;;; CLOS on the class: (defmethod dispatch-message ((m NAME) self tag plist) ..)
;;;; for buffer actions, (defmethod execute ((m NAME) command arg) ..) for
;;;; command layering. Activate with :NAME (set-buffer-mode / auto-mode) or
;;;; enable-minor-mode. Reload-safe: redefining replaces the class and the mode.

(defmacro defmode (name (&key (parent :text-mode) (indicator "") ts-language)
                   &body body)
  "Define major mode NAME (a symbol) deriving from PARENT (a mode keyword). Its
keymap is parented to PARENT's. BODY (bindings, defmethods) runs after the class
exists. Returns the mode keyword :NAME."
  (let ((kw (intern (string-upcase (string name)) :keyword)))
    `(let* ((parent-mode (or (find-mode ,parent)
                             (error "defmode: no parent mode ~s" ,parent))))
       (c2mop:ensure-class ',name :direct-superclasses (list (class-of parent-mode)))
       (pine.mode:register-mode
        (make-instance ',name :name ,kw :parent-mode parent-mode
                       :indicator ,indicator :ts-language ,ts-language
                       :keymap (pine.keymap:make-keymap
                                :name ,kw
                                :parent (pine.mode:mode-keymap parent-mode))))
       ,@body
       ,kw)))

(defmacro defminor (name (&key (precedence 5) transparent (indicator "")) &body body)
  "Define minor mode NAME (a symbol) with its own standalone keymap. BODY runs
after the class exists. Returns :NAME."
  (let ((kw (intern (string-upcase (string name)) :keyword)))
    `(progn
       (c2mop:ensure-class ',name
                           :direct-superclasses (list (find-class 'pine.mode:minor-mode)))
       (pine.mode:register-mode
        (make-instance ',name :name ,kw :precedence ,precedence
                       :transparent ,transparent :indicator ,indicator
                       :keymap (pine.keymap:make-keymap :name ,kw)))
       ,@body
       ,kw)))

;;;; Minor-mode toggles, client-implicit.

(defun enable-minor-mode  (name) (pine.client:enable-minor-mode  (current-client) name))
(defun disable-minor-mode (name) (pine.client:disable-minor-mode (current-client) name))
(defun toggle-minor-mode  (name) (pine.client:toggle-minor-mode  (current-client) name))

;;;; Editor variables. The buffer is implicit here and explicit below: which
;;;; buffer is current is a client's business, and pine.var sits under every
;;;; client, so it never guesses.

(defun var (name &optional (buffer (pine.client:buffer-in-scope)))
  "Editor variable NAME: buffer-local in BUFFER (the current buffer by
default), else global, else the declared default."
  (pine.var:var name buffer))

(defun (setf var) (value name &optional buffer)
  (setf (pine.var:var name buffer) value))

;;;; Style rules. Add to the one stylesheet the cairo painter and the cell
;;;; render both read; user rules win the cascade. VALUES are evaluated, so
;;;; (color :accent) resolves against the active theme.

(defmacro defrules (&rest rules)
  "Add style rules. Each is (SELECTOR PROP VALUE ...): SELECTOR is a keyword
(one class), a list of keywords (compound), or a selector string; values are
lisp values (numbers for px/opacity) or CSS strings. Redefining a selector
replaces it. Reload-safe; the rules reach every attached frontend."
  `(pine.buffer:add-rules
    (list ,@(mapcar (lambda (rule)
                      (destructuring-bind (sel &rest props) rule
                        `(list ,(if (and (consp sel) (every #'keywordp sel))
                                    `',sel
                                    sel)
                               (list ,@props))))
                    rules))))

;;;; Processes. SPAWN / SUPERVISE / KILL take the running server implicitly.

(defun spawn (name)     (pine.actor:spawn-agent     pine.server:*server* (string name)))
(defun supervise (name) (pine.actor:supervise-agent (string name)))
(defun kill (name)      (pine.actor:kill-agent       pine.server:*server* (string name)))

(defmacro defagent (name &body body)
  "Spawn a supervised process agent named NAME and run BODY in its own image."
  (let ((n (string name)))
    `(progn
       (spawn ,n)
       (pine.actor:agent-eval pine.server:*server* ,n
                              ,(format nil "~s" `(progn ,@body)))
       (supervise ,n)
       ,n)))

;;;; Surfaces. A name is a symbol; the desktop machinery keys by its downcased
;;;; string. SHOW / HIDE / TOGGLE act on the desktop client in scope, bound while
;;;; a builder runs and while a click handler fires.

(defmacro defsurface (name (&rest opts) &body body)
  "Define surface NAME (a symbol). OPTS declare placement (:as :bar / :panel /
:echo / :window). BODY returns the widget tree, with the desktop client in
scope. The :as role travels to the client, which maps it to the wayland surface."
  (let ((client (gensym "CLIENT"))
        (key (string-downcase (string name))))
    `(progn
       (pine.desktop:defsurface ,key
         (lambda (,client) (declare (ignorable ,client)) ,@body))
       (pine.desktop:set-surface-role ,key ,(getf opts :as)))))

;;;; SHOW / HIDE / TOGGLE take a bare surface symbol and produce a thunk (like
;;;; LAUNCH), so :on-click (show audio) does the right thing: shows on click, on
;;;; the client in scope. Call the thunk directly to act now.

(defmacro show (name)
  `(lambda () (pine.desktop:show-panel pine.desktop:*surface-client* ,(string-downcase (string name)))))
(defmacro hide (name)
  `(lambda () (pine.desktop:hide-panel pine.desktop:*surface-client* ,(string-downcase (string name)))))
(defmacro toggle (name)
  `(lambda () (pine.desktop:show-panel pine.desktop:*surface-client* ,(string-downcase (string name)))))
