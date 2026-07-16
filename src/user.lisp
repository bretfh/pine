(in-package :cl-user)

;;;; The pine.user language: the vocabulary a user writes in init.lisp. It gathers
;;;; the primitives from every subsystem under one set of clean, symbol-named
;;;; forms, so a config reads as one language and never qualifies a package. The
;;;; catalog is doc/pine-user.org.

(defpackage :pine.user
  (:nicknames :pine-user)
  (:use :cl)
  (:import-from :pine.layout
                #:column #:row #:centerbox #:label #:icon #:button #:ring
                #:gap #:rule #:rows #:choice)
  (:import-from :pine.palette
                #:make-candidate #:register-source #:register-action #:palette-tree)
  (:import-from :pine.buffer
                #:deftheme #:defface #:load-theme #:color #:metric #:face-fg
                #:make-buffer #:kill-buffer #:switch-buffer #:list-buffers
                #:ask #:tell)
  (:import-from :pine.ref #:defref #:find-ref #:deref)
  (:import-from :pine.source #:defsource #:defpoll #:set!)
  (:import-from :pine.command #:call-command)
  (:import-from :pine.mode #:find-mode #:current-buffer-mode #:set-buffer-mode)
  (:import-from :pine.client #:current-client #:current-buffer)
  (:import-from :pine.editor #:eval-last-sexp #:eval-buffer)
  (:export
   ;; surfaces
   #:defsurface #:show #:hide #:toggle
   ;; widgets: containers
   #:column #:row #:centerbox #:center #:box #:scroll
   ;; widgets: content
   #:label #:icon #:image #:rule #:gap
   ;; widgets: controls
   #:button #:slider #:ring #:choice #:rows
   ;; widgets: views -- a window renders a buffer; mode line + echo are widgets
   #:calendar #:window #:buffer #:terminal #:modeline #:echo #:minibuffer #:current
   ;; completion facility
   #:make-candidate #:register-source #:register-action #:palette-tree
   ;; style
   #:deftheme #:defface #:load-theme #:color #:metric #:face-fg
   ;; data
   #:defref #:ref #:set! #:defpoll #:defsource #:sh #:launch
   ;; behavior
   #:defcommand #:call-command
   ;; processes
   #:defagent #:spawn #:supervise #:kill
   ;; buffers / editor
   #:make-buffer #:kill-buffer #:switch-buffer #:list-buffers
   #:ask #:tell #:current-client #:current-buffer
   #:find-mode #:current-buffer-mode #:set-buffer-mode
   #:eval-last-sexp #:eval-buffer))

(in-package :pine.user)

;;;; Widget renames. The layout constructors carry terser class names; the
;;;; language uses the plain ones.

(defun center (&rest args) (apply #'pine.layout:centered args))
(defun box    (&rest args) (apply #'pine.layout:boxed args))
(defun scroll (&rest args) (apply #'pine.layout:viewport args))
(defun slider (&rest args) (apply #'pine.layout:meter args))
(defun calendar (&rest args) (apply #'pine.layout:cal args))
(defun image  (path &rest args) (apply #'pine.layout:pic path args))

;;;; Views (content speaks Emacs). A WINDOW renders a BUFFER, with its MODELINE
;;;; and the ECHO area; CURRENT is the current buffer. A window on the current
;;;; buffer is `(window (current))'.

(defun current () (pine.editor:editor-current))
(defun buffer (name) (pine.buffer:make-buffer name))          ; get or create by name
(defun window (buffer) (pine.editor:editor-window-node buffer))
(defun terminal (&optional buffer) (pine.editor:editor-terminal-node buffer))
(defun modeline () (pine.editor:editor-modeline-node))
(defun echo () (pine.editor:editor-echo-node))
(defun minibuffer () (echo))

;;;; Data. SH captures a command's output for a poll or source body; LAUNCH runs
;;;; one detached, returning a thunk for :on-click. CELL reads a cell, and the
;;;; read is tracked, so a surface re-renders when a cell it read changes.

(defun sh (command)
  (or (ignore-errors
        (string-trim '(#\newline #\space)
                     (uiop:run-program (list "sh" "-c" command)
                                       :output :string :ignore-error-status t)))
      ""))

(defun launch (command)
  (lambda () (ignore-errors (uiop:launch-program (list "sh" "-c" command)))))

(defun ref (name &optional default)
  (let ((r (find-ref name))) (if r (deref r) default)))

;;;; Behavior.

(defmacro defcommand (name args &body body)
  `(pine.command:define-command ,name ,args ,@body))

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
