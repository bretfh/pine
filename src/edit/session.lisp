(defpackage #:pine.edit.session
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:attach #:pine.net.attach) (#:parser #:pine.ts.parser)
                    (#:render #:pine.edit.render) (#:key #:pine.edit.key)
                    (#:css #:pine.ui.css) (#:wire #:pine.ui.wire)
                    (#:node #:pine.fs.node) (#:computed #:pine.fs.computed)
                    (#:surface #:pine.app.surface)
                    (#:fault #:pine.run.fault) (#:log #:pine.run.log)
                    (#:task #:pine.run.task) (#:box #:pine.run.mailbox))
  (:export #:session #:sessions #:install #:push-frame #:received #:drawing
           #:close-all
           #:cols #:rows #:generation #:client-of #:surface #:repaint #:restyle))

(in-package #:pine.edit.session)

(defvar *sessions* (c:cell (d:no-map)))
(defvar *surface* "editor")

(defclass session ()
  ((client-of  :initarg :client :reader client-of)
   (cols       :initform 80 :accessor cols)
   (rows       :initform 24 :accessor rows)
   (sent       :initform nil :accessor sent)
   (drawing    :initform nil :accessor drawing)
   (generation :initform 0  :accessor generation)))

(defmethod print-object ((s session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~dx~d gen ~d" (cols s) (rows s) (generation s))))

(defun sessions () (d:vals (c:held *sessions*)))

(defun %for (client) (d:at (c:held *sessions*) (attach:client-id client)))

(defun surface ()
  (or (surface:surface-named *surface*)
      (surface:surface *surface* (lambda () (render:frame-tree)) :as :toplevel)))

(defun %tree (s)
  (let ((render:*cols* (cols s)) (render:*rows* (rows s)))
    (computed:recompute (surface))))

(defun push-frame (s &key whole)
  "The frame, as what changed since the last one where that can say it. A
keystroke moves a line or two; shipping the whole tree for each of them is what
made typing cost what it did."
  (let ((tree (%tree s)))
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
  "Draw, unless there is more waiting: a burst of keys is one frame at the end
of it rather than one frame each."
  (let ((tk (drawing s)))
    (when (or (null tk) (box:emptyp (task:inbox tk)))
      (fault:attempt (lambda () (push-frame s :whole (null (sent s)))) "a frame"))))

(defun %work (s message)
  (case (first message)
    (:key (fault:attempt (lambda () (key:dispatch nil (second message))) "a key"))
    (:resize
     (destructuring-bind (&key cols rows &allow-other-keys) (rest message)
       (when (and cols rows)
         (setf (cols s) (max 1 cols) (rows s) (max 2 rows))))
     (setf (sent s) nil))
    (:refresh (setf (sent s) nil)))
  (%draw s))

(defun %drawing (s)
  "The session's own thread. Keys are dispatched and frames drawn on it, so
neither a slow command nor a slow frame holds up the loop reading from the
client, and the keys still land in the order they were typed."
  (setf (drawing s)
        (task:actor (format nil "editor ~d" (attach:client-id (client-of s)))
                    (lambda (message) (%work s message)))))

(defun %key (message)
  (destructuring-bind (&key key-str ctrl meta shift super) (rest message)
    (when (and key-str (plusp (length key-str)))
      (key:make-key key-str :ctrl ctrl :meta meta :shift shift :super super))))

(defun received (client message)
  (let ((s (%for client)))
    (when s
      (case (first message)
        (:resize (task:tell (drawing s) message))
        (:key (let ((k (%key message)))
                (when k (task:tell (drawing s) (list :key k)))))
        (:refresh (task:tell (drawing s) (list :refresh)))
        (t nil)))))

(defun restyle (styles)
  (dolist (s (sessions))
    (attach:push-to (client-of s) :style :styles styles)
    (if (drawing s)
        (task:tell (drawing s) (list :refresh))
        (progn (setf (sent s) nil) (push-frame s :whole t)))))

(defun %attached (client)
  (let ((s (make-instance 'session :client client)))
    (c:swap *sessions* (lambda (all) (d:with all (attach:client-id client) s)))
    (pushnew #'restyle css:*listeners*)
    (let ((styles (css:styles)))
      (when styles (attach:push-to client :style :styles styles)))
    (log:note "editor attached as client ~d" (attach:client-id client))
    (%drawing s)
    (task:tell (drawing s) (list :refresh))
    s))

(defun %detached (client)
  (let ((s (%for client)))
    (when (and s (drawing s)) (task:stop (drawing s))))
  (c:swap *sessions* (lambda (all) (d:without all (attach:client-id client))))
  client)

(defun close-all ()
  "Let go of every attached editor. The threads they draw on are this image's,
so they go when it does rather than outliving it stopped."
  (dolist (s (sessions))
    (when (drawing s) (task:stop (drawing s))))
  (c:put *sessions* (d:no-map))
  t)

(defun repaint (&optional b)
  (declare (ignore b))
  (dolist (s (sessions))
    (if (drawing s)
        (task:tell (drawing s) (list :draw))
        (fault:attempt (lambda () (push-frame s)) "a frame"))))

(defun install ()
  (setf parser:*on-parse* #'repaint
        pine.edit.term:*on-refresh* #'repaint)
  (attach:app :editor
              (d:map :doc "the editor window: buffers, windows, the modeline"
                     :attached #'%attached
                     :received #'received
                     :detached #'%detached))
  t)
