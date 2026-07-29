(defpackage #:pine.editor.frame
  (:use :cl)
  (:local-nicknames (#:world #:pine.state.world))
  (:export
   #:client
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
   #:arrangement
   #:mode-stack
   #:current-buffer
   #:server-of
   #:terminals
   #:terminal-map
   #:terminal-wake
   #:minibuffer-buffer #:minibuffer-controller
   #:*client*
   #:current-client
   #:buffer-in-scope
   #:make-window #:remove-window #:focus-window #:window-of #:windows
   #:focused-window
   #:buffer-mode #:current-buffer-mode #:set-buffer-mode
   #:active-minor-modes #:active-keymaps
   #:minor-mode-enabled-p #:enable-minor-mode #:disable-minor-mode
   #:toggle-minor-mode #:active-minor-mode-indicators
   #:buffer #:make-buffer #:kill-buffer #:switch-buffer #:buffer-of-id
   #:list-buffers #:buffer-count
   #:current-buffer-text #:current-buffer-snapshot
   #:start-client
   #:stop-client))

(in-package #:pine.editor.frame)

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
   ;; the live editor tree, rebuilt from /win whenever the arrangement moves
   (arrangement     :initarg :arrangement     :accessor arrangement     :initform nil)
   (mode-stack      :initarg :mode-stack      :accessor mode-stack      :initform nil)
   (terminals       :initarg :terminals       :accessor terminals       :initform nil)
   (terminal-map    :initarg :terminal-map    :accessor terminal-map    :initform nil)
   (terminal-wake   :accessor terminal-wake   :initform (sb-thread:make-semaphore))
   ;; the minibuffer as a real buffer: what a prompt is typed into, and the
   ;; controller that re-filters and repaints on each edit. The prompt itself
   ;; is /echo.
   (minibuffer-buffer     :accessor minibuffer-buffer     :initform nil)
   (minibuffer-controller :accessor minibuffer-controller :initform nil)))

(defvar *client* nil)

(defun current-client ()
  (or *client* (error "No *client* bound.")))

(defun buffer-in-scope ()
  "The current buffer, or nil when no client is bound."
  (let ((c *client*)) (and c (current-buffer c))))

;;;; Which buffer is current is /buf/current, not a slot. A command that acts
;;;; on it writes a path, so it works from another image and from a script that
;;;; never heard of a client.

(defun current-buffer (&optional client)
  "The buffer /buf/current names."
  (declare (ignore client))
  (let ((value (pine.ns:held (pine.buf:at "current"))))
    (when value (pine.path:leaf value))))

(defun (setf current-buffer) (name &optional client)
  (declare (ignore client))
  (let ((name (pine.text.buffer:name-of name)))
    (pine.ns:write (pine.buf:at "current") (and name (pine.buf:at name))))
  name)

(defun start-client (server)
  (let* ((c (make-instance 'client
                :server-of server
                :frame (make-instance 'pine.text.window:frame)
                :terminal-map (make-hash-table :test 'eq))))
    (push c (pine.core.server:clients server))
    c))

;;;; A window is /win/?n, and the object here is the view of it the renderer
;;;; paints: the same window as long as the path is, so its snapshot and its
;;;; display cache survive everything except that window going away.

(defvar *views* (sento.atomic:make-atomic-reference :value (fset:empty-map))
  "Window path to the object showing it.")

(defun %fresh-view (path)
  (let ((buf (pine.win:buf-of path)))
    (make-instance 'pine.text.window:window
                   :buffer (and buf (buffer (pine.path:leaf buf)))
                   :name (and buf (pine.path:leaf buf)))))

(defun window-of (path)
  "The window object showing PATH, made on first use and kept in step with
what the path says."
  (when path
    (let* ((key (pine.path:text path))
           (view (or (fset:lookup (sento.atomic:atomic-get *views*) key)
                     (let ((fresh (%fresh-view path)))
                       (sento.atomic:atomic-swap
                        *views* (lambda (m) (fset:with m key fresh)))
                       fresh)))
           (buf (pine.win:buf-of path)))
      (when buf
        (let ((name (pine.path:leaf buf)))
          (unless (equal name (pine.text.window:window-name view))
            (setf (pine.text.window:window-name view) name
                  (pine.text.window:buffer-ref view) (buffer name)
                  (pine.text.window:snap view) nil
                  (pine.text.window:win-display view) nil))))
      (setf (pine.text.window:scroll-top view) (pine.win:scroll-of path)
            (pine.text.window:focusedp view)
            (fset:equal? path (pine.win:focused)))
      view)))

(defun %view-path (w)
  "The path the window object W shows, or NIL for a detached view."
  (loop :for path :in (pine.win:windows)
        :when (eq w (window-of path)) :return path))

(defun windows (&optional client)
  "Every window there is, in tree order."
  (declare (ignore client))
  (mapcar #'window-of (pine.win:windows)))

(defun focused-window (&optional client)
  (declare (ignore client))
  (window-of (pine.win:focused)))

(defun (setf focused-window) (w &optional client)
  (declare (ignore client))
  (let ((path (and w (%view-path w))))
    (when path (pine.win:focus path)))
  w)

(defun make-window (buffer-actor name &key (row 0) (col 0) (width 80) (height 24)
                                        focused)
  "A fixed view of BUFFER-ACTOR: a panel, a tool buffer, a modeline's subject.

The arrangement's windows are not made here -- they are /win/?n, and the object
showing one comes from WINDOW-OF."
  (declare (ignore focused))
  (make-instance 'pine.text.window:window
                 :buffer buffer-actor :name name
                 :row row :col col :width width :height height))

(defun remove-window (w)
  (let ((path (%view-path w)))
    (when path (pine.ns:write (pine.path:parse "/win/focused") (fset:seq :close)))))

(defun focus-window (w)
  (let ((path (%view-path w)))
    (when path (pine.win:focus path))
    w))

(defun stop-client (c)
  (let ((srv (server-of c)))
    (when srv
      (setf (pine.core.server:clients srv) (remove c (pine.core.server:clients srv)))))
  (when (eq *client* c)
    (setf *client* nil))
  c)

;;;; A mode is a keyword and a map at /mode. Nothing here holds one: which mode
;;;; a buffer is in is /buf/?name/mode, and which minor modes are on is
;;;; /buf/?name/minor, so a mode survives a restart and reads the same from
;;;; another image.

(defun buffer-mode (buffer-or-snap)
  "The mode a buffer or a snapshot of one is in."
  (or (pine.text.buffer:buffer-local buffer-or-snap :mode nil) :text))

(defun %current-name ()
  (let ((buf (current-buffer)))
    (and buf (pine.text.buffer:name-of buf))))

(defun current-buffer-mode ()
  (let ((name (%current-name)))
    (or (and name (pine.ns:read (pine.buf:at name :mode))) :text)))

(defun set-buffer-mode (buffer-actor mode-name)
  "Put BUFFER-ACTOR in MODE-NAME. The mode is a place, so this is a write and
it has landed when this answers."
  (let ((name (pine.text.buffer:name-of buffer-actor)))
    (when name (pine.ns:write (pine.buf:at name :mode) mode-name)))
  mode-name)

;;;; Minor modes: a set at /buf/?name/minor, ordered by /minor/?m/precedence.

(defun %minor-names (client)
  (declare (ignore client))
  (let ((name (%current-name)))
    (and name (pine.mode:minors name))))

(defun active-minor-modes (client)
  "The minor modes on in the current buffer, most specific first."
  (%minor-names client))

(defun minor-mode-enabled-p (client name)
  (and (member name (%minor-names client)) t))

(defun enable-minor-mode (client name)
  (declare (ignore client))
  (let ((buf (%current-name)))
    (when buf (pine.ns:write (pine.buf:at buf :minor) (fset:seq :conj name))))
  t)

(defun disable-minor-mode (client name)
  (declare (ignore client))
  (let ((buf (%current-name)))
    (when buf (pine.ns:write (pine.buf:at buf :minor) (fset:seq :disj name))))
  nil)

(defun toggle-minor-mode (client name)
  (if (minor-mode-enabled-p client name)
      (disable-minor-mode client name)
      (enable-minor-mode client name)))

(defun active-minor-mode-indicators (client)
  (loop :for m :in (active-minor-modes client)
        :for indicator = (pine.ns:read (pine.path:path (pine.path:parse "/minor")
                                                       m :indicator))
        :when indicator :collect indicator))

(defun active-keymaps (client)
  "Minor-mode maps most specific first, then the major mode's and every mode it
falls back to, then global. The keymap chain is the mode chain, read now, so a
mode that gained a parent since its map was made still falls back through it."
  (pine.editor.keymap:roots (current-buffer-mode) (active-minor-modes client)))

;;;; A buffer is its name and its leaves. Nothing holds an object for one, so
;;;; every one of these is a read or a write of /buf.

(defun make-buffer (name &key (content "") id)
  "The buffer NAME, made if it is not there. Answers the name."
  (let ((c *client*))
    (unless (pine.ns:held (pine.buf:at name :text))
      (pine.text.buffer:make-buffer name :content content :id (or id (world:id))))
    (when (and c (null (current-buffer)))
      (setf (current-buffer) name)))
  name)

(defun buffer-of-id (id)
  "The buffer carrying ID, or NIL. How an image holding only the id reaches it."
  (loop :for name :in (pine.buf:names)
        :when (equal id (pine.ns:read (pine.buf:at name :id)))
          :return name))

(defun kill-buffer (name)
  "Drop NAME: its leaves go, and its parser with them."
  (let* ((c (current-client))
         (srv (server-of c))
         (parser (pine.buf:drop name)))
    (when parser
      (ignore-errors
       (sento.actor-context:stop (pine.core.server:actor-system srv) parser)))
    (when (equal name (current-buffer))
      (setf (current-buffer) nil))
    (pine.ns:write (pine.buf:at name) nil)
    name))

(defun switch-buffer (name)
  "Show NAME in the focused window. What a window shows is /win/?n/buf, so this
is a write and the window follows it."
  (let ((at (pine.win:focused)))
    (when (pine.ns:held (pine.buf:at name :text))
      (when at
        (pine.ns:write (fset:map ((pine.path:child at "buf") (pine.buf:at name))
                                 ((pine.path:child at "scroll") 0))))
      (setf (current-buffer) name)
      name)))

(defun list-buffers () (pine.buf:names))

(defun buffer-count () (length (pine.buf:names)))

(defun current-buffer-text ()
  (let ((buf (current-buffer))) (when buf (pine.text.buffer:text-of buf))))

(defun current-buffer-snapshot ()
  (let ((buf (current-buffer))) (when buf (pine.text.buffer:snapshot-of buf))))


(defun buffer (x)
  "X as a buffer name: NIL, a name, :CURRENT, or :FOCUSED.

A buffer is its name -- there is no object to coerce to -- so this is only
which buffer is meant."
  (cond
    ((null x) nil)
    ((stringp x) (when (pine.ns:held (pine.buf:at x :text)) x))
    ((eq x :current) (current-buffer))
    ((eq x :focused)
     (let ((w (focused-window)))
       (when w (pine.text.window:buffer-ref w))))
    ((keywordp x)
     (error "unknown buffer target ~s; use :current, :focused, or a name" x))
    (t x)))
