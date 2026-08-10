(defpackage #:pine.edit.session
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:attach #:pine.net.attach) (#:parser #:pine.ts.parser)
                    (#:render #:pine.edit.render) (#:key #:pine.edit.key)
                    (#:css #:pine.ui.css) (#:wire #:pine.ui.wire)
                    (#:node #:pine.fs.node) (#:computed #:pine.fs.computed)
                    (#:surface #:pine.app.surface)
                    (#:fault #:pine.run.fault) (#:log #:pine.run.log))
  (:export #:session #:sessions #:install #:push-frame #:received
           #:cols #:rows #:generation #:client-of #:surface #:repaint #:restyle))

(in-package #:pine.edit.session)

(defvar *sessions* (c:cell (d:no-map)))
(defvar *surface* "editor")

(defclass session ()
  ((client-of  :initarg :client :reader client-of)
   (cols       :initform 80 :accessor cols)
   (rows       :initform 24 :accessor rows)
   (sent       :initform nil :accessor sent)
   (inbox      :initform nil :accessor inbox)
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

(defun %key (message)
  (destructuring-bind (&key key-str ctrl meta shift super) (rest message)
    (when (and key-str (plusp (length key-str)))
      (key:make-key key-str :ctrl ctrl :meta meta :shift shift :super super))))

(defun received (client message)
  (let ((s (%for client)))
    (when s
      (case (first message)
        (:resize
         (destructuring-bind (&key cols rows &allow-other-keys) (rest message)
           (when (and cols rows)
             (setf (cols s) (max 1 cols) (rows s) (max 2 rows)))
           (setf (sent s) nil)
           (push-frame s :whole t)))
        (:key
         (let ((k (%key message)))
           (when k
             (fault:attempt (lambda () (key:dispatch nil k)) "a key")
             (push-frame s))))
        (:refresh (setf (sent s) nil) (push-frame s :whole t))
        (t nil)))))

(defun restyle (styles)
  (dolist (s (sessions))
    (attach:push-to (client-of s) :style :styles styles)
    (setf (sent s) nil)
    (push-frame s :whole t)))

(defun %attached (client)
  (let ((s (make-instance 'session :client client)))
    (c:swap *sessions* (lambda (all) (d:with all (attach:client-id client) s)))
    (pushnew #'restyle css:*listeners*)
    (let ((styles (css:styles)))
      (when styles (attach:push-to client :style :styles styles)))
    (log:note "editor attached as client ~d" (attach:client-id client))
    (push-frame s :whole t)
    s))

(defun %detached (client)
  (c:swap *sessions* (lambda (all) (d:without all (attach:client-id client))))
  client)

(defun repaint (&optional b)
  (declare (ignore b))
  (dolist (s (sessions)) (fault:attempt (lambda () (push-frame s)) "a frame")))

(defun install ()
  (setf parser:*on-parse* #'repaint
        pine.edit.term:*on-refresh* #'repaint)
  (attach:app :editor
              (d:map :doc "the editor window: buffers, windows, the modeline"
                     :attached #'%attached
                     :received #'received
                     :detached #'%detached))
  t)
