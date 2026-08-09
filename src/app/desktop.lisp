(defpackage #:pine.app.desktop
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:watch #:pine.fs.watch)
                    (#:surface #:pine.app.surface) (#:attach #:pine.net.attach)
                    (#:cmd #:pine.repl.command) (#:wire #:pine.ui.wire)
                    (#:css #:pine.ui.css) (#:fault #:pine.run.fault)
                    (#:computed #:pine.fs.computed))
  (:export #:session #:sessions #:install #:push-surface #:push-all #:received
           #:client-of #:acting #:mine-p #:upp #:repaint #:*client*))

(in-package #:pine.app.desktop)

(defvar *sessions* (c:cell (d:no-map)))
(defvar *client* nil)


(defclass session ()
  ((client-of :initarg :client :reader client-of)
   (acting    :initform (make-hash-table) :reader acting)
   (ids       :initform (make-hash-table :test 'equal) :reader ids)
   (watching  :initform nil :accessor watching)
   (counter   :initform 0 :accessor counter)))

(defmethod print-object ((s session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "client ~d, ~d action~:p"
            (attach:client-id (client-of s)) (hash-table-count (acting s)))))

(defun sessions () (d:vals (c:held *sessions*)))

(defun %for (client) (d:at (c:held *sessions*) (attach:client-id client)))

(defun %forget-actions (s name)
  (dolist (id (gethash name (ids s)))
    (remhash id (acting s)))
  (setf (gethash name (ids s)) nil))

(defun %keep (s name thunk)
  (let ((id (incf (counter s))))
    (setf (gethash id (acting s)) thunk)
    (push id (gethash name (ids s)))
    id))

(defun push-surface (s surface)
  (let ((name (node:name surface))
        (*client* (client-of s)))
    (%forget-actions s name)
    (let ((tree (fault:attempt (lambda () (node:contents surface))
                               (format nil "building ~a" name))))
      (when tree
        (attach:push-to (client-of s) :widgets
                        :surface name
                        :tree (wire:node->wire
                               tree :on-action (lambda (thunk) (%keep s name thunk)))
                        :as (surface:as surface))
        name))))

(defun mine-p (surface)
  (not (eq :toplevel (surface:as surface))))

(defun upp (surface)
  (and (mine-p surface)
       (or (not (surface:panelp surface)) (surface:shownp surface))))

(defun push-all (s)
  (dolist (surface (surface:surfaces))
    (when (upp surface) (push-surface s surface))))

(defun repaint ()
  (dolist (s (sessions)) (push-all s)))

(defun %shown (s surface)
  (attach:push-to (client-of s) :panel
                  :name (node:name surface)
                  :show (and (surface:shownp surface) t)))

(defun %moved (surface)
  (when (mine-p surface)
    (dolist (s (sessions))
      (when (or (not (surface:panelp surface)) (surface:shownp surface))
        (push-surface s surface))
      (when (surface:panelp surface) (%shown s surface)))))

(defun %watch-surfaces (s)
  (setf (watching s)
        (mapcar (lambda (surface)
                  (watch:watch surface (lambda (of value)
                                         (declare (ignore value))
                                         (%moved of))))
                (surface:surfaces))))

(defun %attached (client)
  (let ((s (make-instance 'session :client client)))
    (c:swap *sessions* (lambda (all) (d:with all (attach:client-id client) s)))
    (let ((styles (css:styles)))
      (when styles (attach:push-to client :style :styles styles)))
    (%watch-surfaces s)
    (push-all s)
    s))

(defun %detached (client)
  (let ((s (%for client)))
    (when s (mapc #'watch:unwatch (watching s)))
    (c:swap *sessions* (lambda (all) (d:without all (attach:client-id client))))
    client))

(defun received (client message)
  (let ((s (%for client)))
    (when s
      (case (first message)
        (:widget-action
         (destructuring-bind (&key id args) (rest message)
           (let ((thunk (gethash id (acting s))))
             (when thunk
               (let ((*client* client))
                 (fault:attempt (lambda () (if args (apply thunk args) (funcall thunk)))
                                "a widget"))))))
        (:refresh (push-all s))
        (:hint
         (destructuring-bind (&key text) (rest message)
           (setf (node:contents (pine.world.world:ensure pine.world.world:*world*
                                                        "echo" "hint"))
                 (or text ""))))
        (t nil)))))

(defun install ()
  (cmd:defcommand "show-surface" (name) (:describes "put a surface up")
    (and (surface:show! name) (%moved (surface:surface-named name)) t))
  (cmd:defcommand "hide-surface" (name) (:describes "take a surface down")
    (and (surface:hide! name) (%moved (surface:surface-named name)) t))
  (cmd:defcommand "toggle-surface" (name) (:describes "the panel, either way")
    (let ((was (mapcar #'node:name (surface:surfaces))))
      (surface:toggle! name)
      (dolist (each was t) (%moved (surface:surface-named each)))))
  (cmd:defcommand "surfaces" () (:describes "every surface there is")
    (loop :for each :in (surface:surfaces)
          :collect (list (node:name each) (surface:as each) (surface:shownp each))))
  (cmd:defcommand "reload-desktop" () (:describes "push every surface again")
    (repaint)
    t)
  (attach:app :desktop
              (d:map :doc "the bar, the echo strip and the panels"
                     :attached #'%attached
                     :received #'received
                     :detached #'%detached))
  t)
