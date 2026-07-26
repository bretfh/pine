(in-package :pine.client)

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
   ;; (pine.layout:render output); render-chrome blits the rows above the
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
   ;; the window arrangement: a pine.buffer:window leaf, or (:column ...) /
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
   (repl-buffer     :initarg :repl-buffer     :accessor repl-buffer     :initform nil)
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
  "The current buffer, or nil when no client is bound. For the layers that
want the current buffer if there is one and no error if there is not."
  (let ((c *client*)) (and c (current-buffer c))))

(defun start-client (server)
  (let* ((c (make-instance 'client
                :server-of server
                :frame (make-instance 'pine.buffer::frame)
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
  (let ((w (make-instance 'pine.buffer:window
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
        (when prev (setf (pine.buffer:focusedp prev) nil)))
      (setf (pine.buffer:focusedp w) t
            (focused-window c) w))))

(defun stop-client (c)
  (let ((srv (server-of c)))
    (when srv
      (setf (pine.core.server:clients srv) (remove c (pine.core.server:clients srv)))))
  (when (eq *client* c)
    (setf *client* nil))
  c)
