(defpackage #:pine/edit/session
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:attach #:pine/net/attach) (#:parser #:pine/ts/parser)
                    (#:render #:pine/edit/render) (#:key #:pine/edit/key)
                    (#:css #:pine/ui/css) (#:wire #:pine/ui/wire)
                    (#:layout #:pine/ui/layout)
                    (#:node #:pine/fs/node) (#:computed #:pine/fs/computed)
                    (#:surface #:pine/app/surface)
                    (#:fault #:pine/run/fault) (#:log #:pine/run/log)
                    (#:endpoint #:pine/run/agent))
  (:export #:session #:sessions #:install #:push-frame #:received #:drawing
           #:close-all
           #:cols #:rows #:px-width #:px-height #:cell-w #:cell-h #:font-px
           #:generation #:client-of #:surface #:repaint #:restyle))
(in-package #:pine/edit/session)

(defvar *sessions* (d:table))
(defvar *surface* "editor")

(defclass session ()
  ((client-of  :initarg :client :reader client-of)
   (cols       :initform 80 :accessor cols)
   (rows       :initform 24 :accessor rows)
   (px-width   :initform nil :accessor px-width)
   (px-height  :initform nil :accessor px-height)
   (cell-w     :initform nil :accessor cell-w)
   (cell-h     :initform nil :accessor cell-h)
   (font-px    :initform nil :accessor font-px)
   (sent       :initform nil :accessor sent)
   (drawing    :initform nil :accessor drawing)
   (generation :initform 0  :accessor generation)))

(defmethod print-object ((s session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~dx~d gen ~d" (cols s) (rows s) (generation s))))

(defun sessions () (d:vals (d:all *sessions*)))

(defun %for (client) (d:at (d:all *sessions*) (attach:client-id client)))

(defun surface ()
  (or (surface:surface-named *surface*)
      (surface:surface *surface* (lambda () (render:frame-tree)) :as :toplevel)))

(defun %tree (s)
  (let ((render:*cols* (cols s)) (render:*rows* (rows s))
        (render:*font-px* (font-px s)))
    (computed:recompute (surface))))

(defun %cell-metric (s)
  "Measuring for a monospace grid: what the frontend told us one cell is."
  (let ((cw (cell-w s)) (ch (cell-h s)))
    (lambda (text font-px)
      (declare (ignore font-px))
      (values (* (length text) cw) ch))))

(defun %arrange (s tree)
  "Arrange the frame in pixels when the frontend has said what its cells
measure, so the rects cross the wire and are painted as they are: chrome that
is not the cell grid lines up with it instead of being laid out twice."
  (when (and tree (cell-w s) (cell-h s) (px-width s) (px-height s)
             (plusp (cell-w s)) (plusp (cell-h s)))
    (let ((layout:*text-size* (%cell-metric s))
          (width (px-width s))
          (height (px-height s)))
      (layout:measure tree width height)
      (layout:arrange tree 0 0 width height)))
  tree)

(defun push-frame (s &key whole)
  "The frame, as what changed since the last one where that can say it. A
keystroke moves a line or two; shipping the whole tree for each of them is what
made typing cost what it did."
  (let ((tree (%arrange s (%tree s))))
    (when tree
      (let* ((form (wire:node->wire tree))
             (patch (unless whole (wire:rows-patch (sent s) form))))
        (incf (generation s))
        (setf (sent s) form)
        (if patch
            (attach:push-to (client-of s) :rows-patch
                            :surface *surface*
                            :patch patch
                            :generation (generation s))
            (attach:push-to (client-of s) :widgets
                            :surface *surface*
                            :tree form
                            :as (surface:as (surface))
                            :generation (generation s)))
        (generation s)))))

(defun %draw (s)
  (fault:attempt (lambda () (push-frame s :whole (null (sent s)))) "a frame"))

(defun %work (s message)
  (case (first message)
    (:key (fault:attempt (lambda () (key:dispatch nil (second message))) "a key"))
    (:resize
     (destructuring-bind (&key cols rows width height cell-w cell-h font-px
                          &allow-other-keys)
         (rest message)
       (when (and cols rows)
         (setf (cols s) (max 1 cols) (rows s) (max 2 rows)))
       (when (and width height (plusp width) (plusp height))
         (setf (px-width s) width (px-height s) height))
       (when (and cell-w cell-h (plusp cell-w) (plusp cell-h))
         (setf (cell-w s) cell-w (cell-h s) cell-h))
       (when (and font-px (plusp font-px)) (setf (font-px s) font-px)))
     (setf (sent s) nil))
    (:refresh (setf (sent s) nil)))
  (%draw s))

(defun %drawing (s)
  "The session's own endpoint. Keys are dispatched and frames drawn there, so
neither a slow command nor a slow frame holds up the loop reading from the
client, and the keys still land in the order they were typed. Pinned: a command
can stand in a fault, and a receive that parks takes its worker with it."
  (setf (drawing s)
        (endpoint:agent (format nil "editor-~d" (attach:client-id (client-of s)))
                     (lambda (message) (%work s message))
                     :dispatcher :pinned)))

(defun %key (message)
  (destructuring-bind (&key key-str ctrl meta shift super) (rest message)
    (when (and key-str (plusp (length key-str)))
      (key:make-key key-str :ctrl ctrl :meta meta :shift shift :super super))))

(defun received (client message)
  (let ((s (%for client)))
    (when s
      (case (first message)
        (:resize (endpoint:tell (drawing s) message))
        (:key (let ((k (%key message)))
                (when k (endpoint:tell (drawing s) (list :key k)))))
        (:refresh (endpoint:tell (drawing s) (list :refresh)))
        (t nil)))))

(defun restyle (styles)
  (dolist (s (sessions))
    (attach:push-to (client-of s) :style :styles styles)
    (if (drawing s)
        (endpoint:tell (drawing s) (list :refresh))
        (progn (setf (sent s) nil) (push-frame s :whole t)))))

(defun %attached (client)
  (let ((s (make-instance 'session :client client)))
    (d:keep! *sessions* (attach:client-id client) s)
    (pushnew #'restyle css:*listeners*)
    (let ((styles (css:styles)))
      (when styles (attach:push-to client :style :styles styles)))
    (log:note "editor attached as client ~d" (attach:client-id client))
    (%drawing s)
    (endpoint:tell (drawing s) (list :refresh))
    s))

(defun %detached (client)
  (let ((s (%for client)))
    (when (and s (drawing s)) (endpoint:stop (drawing s))))
  (d:drop! *sessions* (attach:client-id client))
  client)

(defun close-all ()
  "Let go of every attached editor. The threads they draw on are this image's,
so they go when it does rather than outliving it stopped."
  (dolist (s (sessions))
    (when (drawing s) (endpoint:stop (drawing s))))
  (d:put! *sessions* (d:no-map))
  t)

(defun repaint (&optional b)
  (declare (ignore b))
  (dolist (s (sessions))
    (if (drawing s)
        (endpoint:tell (drawing s) (list :draw))
        (fault:attempt (lambda () (push-frame s)) "a frame"))))

(defun install ()
  (setf parser:*on-parse* #'repaint
        pine/edit/term:*on-refresh* #'repaint)
  (attach:app :editor
              (d:map :doc "the editor window: buffers, windows, the modeline"
                     :attached #'%attached
                     :received #'received
                     :detached #'%detached))
  t)
