(defpackage #:pine.editor.frame
  (:use :cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export
   #:client #:renderer #:paint-sink #:render-state #:tree
   #:cols #:rows #:px-width #:px-height #:cell-w #:cell-h #:terminal-wake
   #:server-of #:*client* #:current-client #:buffer-in-scope
   #:current-buffer
   #:buffer-mode #:current-buffer-mode #:set-buffer-mode
   #:active-minor-modes #:active-keymaps
   #:minor-mode-enabled-p #:enable-minor-mode #:disable-minor-mode
   #:toggle-minor-mode #:active-minor-mode-indicators
   #:make-buffer #:kill-buffer #:switch-buffer #:buffer-of-id
   #:list-buffers #:buffer-count
   #:current-buffer-text #:current-buffer-snapshot
   #:start-client #:stop-client))

(in-package #:pine.editor.frame)
(named-readtables:in-readtable pine.path:syntax)

;;;; A client is one frontend's own I/O and nothing else: where it paints, how
;;;; big its surface is, and the tree it is painting. What the panes in that
;;;; tree show, where they are scrolled and which has the keyboard are paths, so
;;;; none of it is here.

(defvar *client* nil)

(defclass client ()
  ((server-of    :initarg :server-of :accessor server-of    :initform nil)
   (renderer     :initarg :renderer  :accessor renderer     :initform nil)
   ;; where a frame goes when one is due: the seam to the attached frontend
   (paint-sink   :initarg :paint-sink :accessor paint-sink  :initform nil)
   (render-state :accessor render-state :initform (fset:map (:dirty nil)))
   ;; the attached surface, reported with :resize: how many cells it is,
   ;; and the pixels behind them when the frontend paints in pixels
   (cols         :initarg :cols      :accessor cols         :initform 80)
   (rows         :initarg :rows      :accessor rows         :initform 30)
   (px-width     :initarg :px-width  :accessor px-width     :initform nil)
   (px-height    :initarg :px-height :accessor px-height    :initform nil)
   (cell-w       :initarg :cell-w    :accessor cell-w       :initform nil)
   (cell-h       :initarg :cell-h    :accessor cell-h       :initform nil)
   (terminal-wake :accessor terminal-wake :initform (sb-thread:make-semaphore))
   ;; the tree this client paints, rebuilt from /win when the arrangement moves
   (tree         :accessor tree      :initform nil)))

(defun current-client ()
  (or *client* (error "No *client* bound.")))

;;;; Which buffer is current is /buf/current, not a slot.

(defun current-buffer ()
  (pine.buf:name-of :current))

(defun buffer-in-scope ()
  (and *client* (current-buffer)))

(defun (setf current-buffer) (name)
  (let ((name (pine.buf:name-of name)))
    (ns:write (pine.buf:at "current") (and name (pine.buf:at name))))
  name)

(defun start-client (server)
  (let ((c (make-instance 'client :server-of server)))
    (push c (pine.core.server:clients server))
    c))

(defun stop-client (c)
  (let ((srv (server-of c)))
    (when srv
      (setf (pine.core.server:clients srv)
            (remove c (pine.core.server:clients srv)))))
  (when (eq *client* c) (setf *client* nil))
  c)

;;;; A mode is a keyword and a map at /mode. Nothing here holds one: which mode
;;;; a buffer is in is /buf/?name/mode, and which minor modes are on is
;;;; /buf/?name/minor.

(defun buffer-mode (x)
  (or (pine.buf:local x :mode nil) :text))

(defun current-buffer-mode ()
  (let ((name (current-buffer)))
    (or (and name (ns:read (pine.buf:at name :mode))) :text)))

(defun set-buffer-mode (buffer-actor mode-name)
  (let ((name (pine.buf:name-of buffer-actor)))
    (when name (ns:write (pine.buf:at name :mode) mode-name)))
  mode-name)

(defun active-minor-modes ()
  "The minor modes on in the current buffer, most specific first."
  (let ((name (current-buffer)))
    (and name (pine.mode:minors name))))

(defun minor-mode-enabled-p (name)
  (and (member name (active-minor-modes)) t))

(defun enable-minor-mode (name)
  (let ((buf (current-buffer)))
    (when buf (ns:write (pine.buf:at buf :minor) (fset:seq :conj name))))
  t)

(defun disable-minor-mode (name)
  (let ((buf (current-buffer)))
    (when buf (ns:write (pine.buf:at buf :minor) (fset:seq :disj name))))
  nil)

(defun toggle-minor-mode (name)
  (if (minor-mode-enabled-p name)
      (disable-minor-mode name)
      (enable-minor-mode name)))

(defun active-minor-mode-indicators ()
  (loop :for m :in (active-minor-modes)
        :for indicator = (ns:read (p:path /minor m :indicator))
        :when indicator :collect indicator))

(defun active-keymaps ()
  "Minor-mode maps most specific first, then the major mode's and every mode it
falls back to, then global. Read now, so a mode that gained a parent since its
map was made still falls back through it."
  (pine.key:roots (current-buffer-mode) (active-minor-modes)))

;;;; A buffer is its name and its leaves. Nothing holds an object for one.

(defun make-buffer (name &key (content ""))
  (unless (ns:held (pine.buf:at name :text))
    (pine.buf:make name :content content))
  (when (and *client* (null (current-buffer)))
    (setf (current-buffer) name))
  name)

(defun buffer-of-id (id)
  "The buffer carrying ID, or NIL. How an image holding only the id reaches it."
  (loop :for name :in (pine.buf:names)
        :when (equal id (ns:read (pine.buf:at name :id)))
          :return name))

(defun kill-buffer (name)
  "Drop NAME: its leaves go, and its parser with them."
  (pine.buf:kill name)
  (when (equal name (current-buffer))
    (setf (current-buffer) nil))
  name)

(defun switch-buffer (name)
  "Show NAME in the focused window. What a window shows is /win/?n/buf, so this
is a write and the window follows it."
  (when (ns:held (pine.buf:at name :text))
    (pine.buf:show name)))

(defun list-buffers () (pine.buf:names))

(defun buffer-count () (length (pine.buf:names)))

(defun current-buffer-text ()
  (let ((buf (current-buffer))) (when buf (pine.buf:text-of buf))))

(defun current-buffer-snapshot ()
  (let ((buf (current-buffer))) (when buf (pine.buf:snapshot-of buf))))
