(defpackage #:pine.editor.frame
  (:use :cl)
  (:export
   #:client
   #:completion
   #:actor
   #:renderer
   #:paint-sink
   #:ts-actor
   #:render-state
   #:frame
   #:px-width
   #:px-height
   #:cell-w
   #:cell-h
   #:windows
   #:arrangement
   #:focused-window
   #:pending-keys
   #:prefix-arg
   #:this-command-key
   #:pending-key-reader
   #:mode-stack
   #:completion-state
   #:current-buffer
   #:buffer-modes
   #:buffer-minor-modes
   #:kill-ring
   #:kill-ring-max
   #:last-command
   #:server-of
   #:terminals
   #:terminal-map
   #:terminal-wake
   #:prompt-callback
   #:prompt-active
   #:prompt-history
   #:prompt-history-pos
   #:prompt-history-items
   #:minibuffer-buffer #:saved-buffer #:minibuffer-snap #:minibuffer-controller
   #:active-p
   #:candidates
   #:filtered
   #:index
   #:input
   #:callback
   #:prompt
   #:dynamic-fn
   #:popup-rows
   #:popup-tree
   #:*client*
   #:current-client
   #:buffer-in-scope
   #:make-window #:remove-window #:focus-window
   #:buffer-mode #:current-buffer-mode #:set-buffer-mode
   #:buffer-active-modes #:active-minor-modes #:active-keymaps
   #:active-modes-instance
   #:minor-mode-enabled-p #:enable-minor-mode #:disable-minor-mode
   #:toggle-minor-mode #:active-minor-mode-indicators
   #:buffer #:make-buffer #:kill-buffer #:switch-buffer
   #:buffer-of-id #:restore-buffer #:snapshot-source
   #:list-buffers #:buffer-count
   #:current-buffer-text #:current-buffer-snapshot
   #:start-client
   #:stop-client))

(in-package #:pine.editor.frame)

(defclass completion ()
  ((active-p   :initarg :active-p   :accessor active-p   :initform nil)
   (candidates :initarg :candidates :accessor candidates :initform nil)
   (filtered   :initarg :filtered   :accessor filtered   :initform nil)
   (index      :initarg :index      :accessor index      :initform -1)
   (input      :initarg :input      :accessor input      :initform "")
   (callback   :initarg :callback   :accessor callback   :initform nil)
   (prompt     :initarg :prompt     :accessor prompt     :initform "")
   ;; when set, a function of the current input returning fresh candidates
   ;; (filesystem path completion), instead of filtering a fixed list.
   (dynamic-fn :initarg :dynamic-fn :accessor dynamic-fn :initform nil)
   ;; the candidate list rendered to styled rows + its arranged tree
   ;; (render output); render-chrome blits the rows above the
   ;; echo row while the prompt is active.
   (popup-rows :initarg :popup-rows :accessor popup-rows :initform nil)
   (popup-tree :initarg :popup-tree :accessor popup-tree :initform nil)))

(defclass client ()
  ((server-of       :initarg :server-of       :accessor server-of       :initform nil)
   (actor           :initarg :actor           :accessor actor           :initform nil)
   (renderer        :initarg :renderer        :accessor renderer        :initform nil)
   ;; a local paint fn (grid cursor-row cursor-col); when set, the renderer
   ;; paints the focused window here instead of the daemon pushing frames.
   (paint-sink      :initarg :paint-sink      :accessor paint-sink      :initform nil)
   (ts-actor        :initarg :ts-actor        :accessor ts-actor        :initform nil)
   (render-state    :initarg :render-state    :accessor render-state
                    :initform (fset:map (:dirty nil)))
   (frame           :initarg :frame           :accessor frame           :initform nil)
   ;; the attached surface's pixel geometry, reported with :resize; when set,
   ;; the editor tree arranges once, in pixels, on the daemon
   (px-width        :initarg :px-width        :accessor px-width        :initform nil)
   (px-height       :initarg :px-height       :accessor px-height       :initform nil)
   (cell-w          :initarg :cell-w          :accessor cell-w          :initform nil)
   (cell-h          :initarg :cell-h          :accessor cell-h          :initform nil)
   (windows         :initarg :windows         :accessor windows         :initform nil)
   ;; the window arrangement: a pine.text.window:window leaf, or (:column ...) /
   ;; (:row ...) over such trees -- the split shape C-x 2/3/0/1 rewrite.
   ;; nil means the single window in WINDOWS.
   (arrangement     :initarg :arrangement     :accessor arrangement     :initform nil)
   (focused-window  :initarg :focused-window  :accessor focused-window  :initform nil)
   (pending-keys    :initarg :pending-keys    :accessor pending-keys    :initform nil)
   (prefix-arg      :initarg :prefix-arg      :accessor prefix-arg      :initform nil)
   (this-command-key :initarg :this-command-key :accessor this-command-key :initform nil)
   (pending-key-reader :initarg :pending-key-reader :accessor pending-key-reader
                    :initform nil)
   (mode-stack      :initarg :mode-stack      :accessor mode-stack      :initform nil)
   (completion-state
     :initarg :completion-state :accessor completion-state
     :initform (make-instance 'completion))
   (current-buffer  :initarg :current-buffer  :accessor current-buffer  :initform nil)
   (buffer-modes    :initarg :buffer-modes    :accessor buffer-modes
                    :initform (make-hash-table :test 'eq))
   (buffer-minor-modes :initarg :buffer-minor-modes :accessor buffer-minor-modes
                    :initform (make-hash-table :test 'eq))
   (kill-ring       :initarg :kill-ring       :accessor kill-ring       :initform nil)
   (kill-ring-max   :initarg :kill-ring-max   :accessor kill-ring-max   :initform 60)
   (last-command    :initarg :last-command    :accessor last-command    :initform nil)
   (terminals       :initarg :terminals       :accessor terminals       :initform nil)
   (terminal-map    :initarg :terminal-map    :accessor terminal-map    :initform nil)
   (terminal-wake   :accessor terminal-wake   :initform (sb-thread:make-semaphore))
   (prompt-callback :initarg :prompt-callback :accessor prompt-callback :initform nil)
   (prompt-active   :initarg :prompt-active   :accessor prompt-active   :initform nil)
   ;; the active prompt's history: the store list it reads/pushes, the cycle
   ;; position (nil = not cycling, 0 = newest), and the items fetched once at
   ;; the first M-p.
   (prompt-history  :accessor prompt-history  :initform nil)
   (prompt-history-pos :accessor prompt-history-pos :initform nil)
   (prompt-history-items :accessor prompt-history-items :initform nil)
   ;; the minibuffer as a real buffer: the input buffer, the buffer that was
   ;; current before the prompt (restored on exit), its latest snapshot (for the
   ;; renderer), and the controller that re-filters + repaints on each edit.
   (minibuffer-buffer     :accessor minibuffer-buffer     :initform nil)
   (saved-buffer          :accessor saved-buffer          :initform nil)
   (minibuffer-snap       :accessor minibuffer-snap       :initform nil)
   (minibuffer-controller :accessor minibuffer-controller :initform nil)))

(defvar *client* nil)

(defun current-client ()
  (or *client* (error "No *client* bound.")))

(defun buffer-in-scope ()
  "The current buffer, or nil when no client is bound."
  (let ((c *client*)) (and c (current-buffer c))))

(defun start-client (server)
  (let* ((c (make-instance 'client
                :server-of server
                :frame (make-instance 'pine.text.window:frame)
                :terminal-map (make-hash-table :test 'eq)
                :kill-ring (pine.state.store:store :kill-ring))))
    (push c (pine.core.server:clients server))
    c))

;;;; Windows belong to the client that shows them: the window itself is a view
;;;; of a buffer, but which windows exist and which one has focus is this
;;;; client's business, so the text layer never has to know a client exists.

(defun make-window (buffer-actor name &key (row 0) (col 0) (width 80) (height 24) focused)
  "A window on BUFFER-ACTOR, registered on the client in scope when there is
one. Without a client the window is detached: a read-only view for a panel or
a layout buffer."
  (let ((w (make-instance 'pine.text.window:window
             :buffer buffer-actor :name name
             :row row :col col :width width :height height
             :focused focused))
        (c *client*))
    (when c
      (push w (windows c))
      (when focused (setf (focused-window c) w)))
    w))

(defun remove-window (w)
  (let ((c *client*))
    (when c
      (setf (windows c) (remove w (windows c)))
      (when (eq w (focused-window c))
        (setf (focused-window c) (first (windows c)))))))

(defun focus-window (w)
  (let ((c *client*))
    (when c
      (let ((prev (focused-window c)))
        (when prev (setf (pine.text.window:focusedp prev) nil)))
      (setf (pine.text.window:focusedp w) t
            (focused-window c) w))))

(defun stop-client (c)
  (let ((srv (server-of c)))
    (when srv
      (setf (pine.core.server:clients srv) (remove c (pine.core.server:clients srv)))))
  (when (eq *client* c)
    (setf *client* nil))
  c)

(defun buffer-mode (buffer-or-snap)
  (let ((name (pine.text.buffer:buffer-local buffer-or-snap :mode :base-mode)))
    (or (pine.editor.mode:find-mode name) (pine.editor.mode:find-mode :base-mode))))

(defun current-buffer-mode ()
  (let* ((c (current-client))
         (buf (current-buffer c))
         (name (and buf (gethash buf (buffer-modes c)))))
    (or (and name (pine.editor.mode:find-mode name)) (pine.editor.mode:find-mode :base-mode))))

(defun set-buffer-mode (buffer-actor mode-name)
  (unless (pine.editor.mode:find-mode mode-name) (error "No mode named ~s" mode-name))
  ;; the buffer's own :mode meta drives highlighting, so set it first and
  ;; unconditionally; recording it on the client (for the modeline) is
  ;; best-effort and must not stop the buffer from learning its mode.
  (sento.actor:tell buffer-actor (list :set-local :key :mode :value mode-name))
  (let ((c *client*))
    (when c
      (setf (gethash buffer-actor (buffer-modes c)) mode-name)))
  (pine.editor.mode:find-mode mode-name))

;;;; Minor modes. Per-buffer, precedence-numbered (higher = more specific).
;;;; The enabled set feeds both the active-keymap list and the synthesized
;;;; dispatch class, so a minor mode augments via keymap bindings and via
;;;; execute method combination (:before/:after transparent, :around opaque).

(defun %minor-names (client)
  (gethash (current-buffer client) (buffer-minor-modes client)))

(defun (setf %minor-names) (names client)
  (setf (gethash (current-buffer client) (buffer-minor-modes client)) names))

(defun active-minor-modes (client)
  "Active minor-mode singletons for the current buffer, most specific first."
  (stable-sort
   (loop :for name :in (%minor-names client)
         :for m = (pine.editor.mode:find-mode name)
         :when (typep m 'pine.editor.mode:minor-mode) :collect m)
   #'> :key #'pine.editor.mode:precedence))

(defun minor-mode-enabled-p (client name)
  (and (member name (%minor-names client)) t))

(defun enable-minor-mode (client name)
  (unless (typep (pine.editor.mode:find-mode name) 'pine.editor.mode:minor-mode)
    (error "~s is not a minor mode" name))
  (pushnew name (%minor-names client))
  t)

(defun disable-minor-mode (client name)
  (setf (%minor-names client) (remove name (%minor-names client)))
  nil)

(defun toggle-minor-mode (client name)
  (if (minor-mode-enabled-p client name)
      (disable-minor-mode client name)
      (enable-minor-mode client name)))

(defun active-minor-mode-indicators (client)
  (loop :for m :in (active-minor-modes client)
        :collect (pine.editor.mode:mode-indicator m)))

;;;; Active modes -> keymaps + a synthesized dispatch class

(defun buffer-active-modes (client)
  "Minor modes (most specific first) then the major mode. This is the
superclass order of the synthesized dispatch class, so minor-mode methods
run before the major mode's under CLOS method combination."
  (append (active-minor-modes client) (list (current-buffer-mode))))

(defun active-keymaps (client)
  "Minor-mode maps most specific first, then the major mode's, then global.
Dispatch flattens each one's own parent chain into the table list."
  (append (mapcar #'pine.editor.keymap:keymap (active-minor-modes client))
          (list (pine.editor.keymap:keymap (current-buffer-mode))
                (pine.editor.mode:global-keymap))))

(defun active-modes-instance (client)
  (make-instance (pine.editor.mode:modes-dispatch-class
                  (mapcar #'class-of (buffer-active-modes client)))))

(defun make-buffer (name &key (content "") id)
  (let* ((c (current-client))
         (srv (server-of c))
         (table (pine.text.buffer:buffer-table srv))
         (existing (gethash name table)))
    (when existing (return-from make-buffer existing))
    (let ((actor (pine.text.buffer:make-buffer-actor (pine.core.server:actor-system srv) name
                                                     :content content :id id)))
      (setf (gethash name table) actor)
      (sento.actor:tell (pine.core.server:buffer-registry srv)
                        (list :register :name name :actor actor))
      (when (null (current-buffer c))
        (setf (current-buffer c) actor))
      actor)))

(defun buffer-of-id (id)
  "The live buffer carrying ID, or nil."
  (let* ((c (current-client))
         (srv (and c (server-of c)))
         (table (and srv (pine.text.buffer:buffer-table srv))))
    (when table
      (loop :for actor :being :the :hash-values :of table
            :when (equal id (ignore-errors
                             (pine.text.buffer:buffer-local
                              (pine.core.actor:ask actor '(:get-state) :timeout 2) :id)))
              :return actor))))

(defun snapshot-source (id)
  "(values NAME META CONTENT TICK) for the live buffer ID, for the journal to
compact against. Asked from the writer's thread, which is not a receive."
  (let ((actor (buffer-of-id id)))
    (when actor
      (let ((state (pine.core.actor:ask actor '(:get-state) :timeout 5)))
        (when (typep state 'pine.text.buffer:buffer-state)
          (values (pine.text.buffer:buffer-local state :name "")
                  (pine.text.buffer:meta state)
                  (pine.text.buffer:state->string state)
                  (pine.text.buffer:tick state)))))))

(defun restore-buffer (id)
  "Bring back the buffer ID as the store last saw it: its snapshot, then the
edits recorded after it, replayed through the buffer's own verbs. Answers the
actor, or nil when the store has never seen ID."
  (multiple-value-bind (name meta content tick verbs) (pine.state.journal:restore-state id)
    (declare (ignore tick))
    (when name
      (let ((actor (make-buffer name :content content :id id)))
        ;; the mode comes back with it, so a restored lisp buffer is a lisp
        ;; buffer and its parser starts the same way it would have
        (let ((mode (and meta (fset:@ meta :mode))))
          (when mode (set-buffer-mode actor mode)))
        (dolist (verb verbs)
          (sento.actor:tell actor verb))
        actor))))

(defun kill-buffer (name)
  (let* ((c (current-client))
         (srv (server-of c))
         (table (pine.text.buffer:buffer-table srv))
         (actor (gethash name table)))
    (when actor
      (when (eq actor (current-buffer c))
        (setf (current-buffer c) nil))
      (remhash name table)
      ;; the buffer's parser is its own actor and its own thread, so it goes
      ;; with the buffer rather than outliving it. Asked before the stop, from
      ;; this thread, which is not a receive.
      (let ((parser (ignore-errors (pine.core.actor:ask actor '(:get-parser) :timeout 2))))
        (when (and parser (typep parser 'sento.actor:actor))
          (ignore-errors
           (sento.actor:tell parser '(:stop))
           (sento.actor-context:stop (pine.core.server:actor-system srv) parser))))
      (sento.actor-context:stop (pine.core.server:actor-system srv) actor)
      (sento.actor:tell (pine.core.server:buffer-registry srv)
                        (list :unregister :name name)))))

(defun switch-buffer (name)
  (let* ((c (current-client))
         (srv (server-of c))
         (actor (gethash name (pine.text.buffer:buffer-table srv))))
    (when actor
      (setf (current-buffer c) actor)
      actor)))


(defun list-buffers ()
  (let ((srv (server-of (current-client))))
    (loop for k being the hash-keys of (pine.text.buffer:buffer-table srv) collect k)))

(defun buffer-count ()
  (let ((srv (server-of (current-client))))
    (hash-table-count (pine.text.buffer:buffer-table srv))))

(defun current-buffer-text ()
  (let* ((c (current-client))
         (buf (current-buffer c)))
    (when buf
      (pine.core.actor:ask buf '(:get-text) :timeout 5))))

(defun current-buffer-snapshot ()
  (let* ((c (current-client))
         (buf (current-buffer c)))
    (when buf
      (pine.core.actor:ask buf '(:get-snapshot) :timeout 5))))


(defun buffer (x)
  "Coerce X to a buffer actor.
- nil            -> nil
- string         -> lookup by name in current server's buffer-table
- :current       -> current client's current-buffer
- :focused       -> focused window's buffer-ref
- actor ref      -> passthrough
Unknown keywords error; nothing silently falls through."
  (cond
    ((null x) nil)
    ((stringp x)
     (let ((srv (server-of (current-client))))
       (gethash x (pine.text.buffer:buffer-table srv))))
    ((eq x :current)
     (current-buffer (current-client)))
    ((eq x :focused)
     (let ((w (focused-window (current-client))))
       (when w (pine.text.window:buffer-ref w))))
    ((keywordp x)
     (error "unknown buffer target ~s; use :current, :focused, or a string name"
            x))
    (t x)))
