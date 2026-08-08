(defpackage #:pine.edit.session
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:attach #:pine.net.attach)
                    (#:buffer #:pine.edit.buffer) (#:window #:pine.edit.window)
                    (#:render #:pine.edit.render) (#:key #:pine.edit.key)
                    (#:css #:pine.ui.css) (#:wire #:pine.ui.wire)
                    (#:fault #:pine.run.fault) (#:log #:pine.run.log))
  (:export #:session #:sessions #:install #:push-frame #:received
           #:cols #:rows #:generation #:client-of #:surface))

(in-package #:pine.edit.session)

(defvar *sessions* (c:cell (d:no-map)))
(defvar *surface* "editor")

(defclass session ()
  ((client-of  :initarg :client :reader client-of)
   (cols       :initform 80 :accessor cols)
   (rows       :initform 24 :accessor rows)
   (generation :initform 0  :accessor generation)))

(defmethod print-object ((s session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~dx~d gen ~d" (cols s) (rows s) (generation s))))

(defun sessions () (d:vals (c:held *sessions*)))

(defun %for (client) (d:at (c:held *sessions*) (attach:client-id client)))

(defun surface () *surface*)

(defun %tree (s)
  (let ((w (window:focused)))
    (when w
      (setf (window:width-of w) (cols s)
            (window:height-of w) (max 1 (1- (rows s)))))
    (render:frame-tree)))

(defun push-frame (s)
  (let ((tree (%tree s)))
    (when tree
      (incf (generation s))
      (attach:push-to (client-of s) :widgets
                      :surface *surface*
                      :tree (wire:node->wire tree)
                      :as :toplevel
                      :generation (generation s))
      (generation s))))

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
           (push-frame s)))
        (:key
         (let ((k (%key message)))
           (when k
             (fault:attempt (lambda () (key:dispatch nil k)) "a key")
             (push-frame s))))
        (:refresh (push-frame s))
        (t nil)))))

(defun %attached (client)
  (let ((s (make-instance 'session :client client)))
    (c:swap *sessions* (lambda (all) (d:with all (attach:client-id client) s)))
    (let ((styles (css:styles)))
      (when styles (attach:push-to client :style :styles styles)))
    (log:note "editor attached as client ~d" (attach:client-id client))
    (push-frame s)
    s))

(defun %detached (client)
  (c:swap *sessions* (lambda (all) (d:without all (attach:client-id client))))
  client)

(defun install ()
  (attach:app :editor
              (d:map :doc "the editor window: buffers, windows, the modeline"
                     :attached #'%attached
                     :received #'received
                     :detached #'%detached))
  t)
