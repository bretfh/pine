(defpackage #:pine.app.desktop
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:watch #:pine.fs.watch)
                    (#:surface #:pine.app.surface) (#:attach #:pine.net.attach)
                    (#:cmd #:pine.repl.command) (#:wire #:pine.ui.wire)
                    (#:css #:pine.ui.css) (#:fault #:pine.run.fault)
                    (#:computed #:pine.fs.computed) (#:task #:pine.run.task))
  (:export #:session #:sessions #:install #:push-surface #:push-all #:received
           #:close-all
           #:client-of #:acting #:mine-p #:upp #:repaint #:restyle #:declared #:*client*))

(in-package #:pine.app.desktop)

(defvar *sessions* (c:cell (d:no-map)))
(defvar *client* nil)
(defvar *acts* 0)


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

(defun %watch-one (s surface)
  (let ((w (watch:watch surface (lambda (of value)
                                  (declare (ignore value))
                                  (%moved of)))))
    (push w (watching s))
    w))

(defun %watch-surfaces (s)
  (setf (watching s) nil)
  (dolist (surface (surface:surfaces) (watching s))
    (%watch-one s surface)))

(defun declared (surface)
  "A surface a config declared after a frontend attached is watched and shown
like any other; without this it never reaches the screen."
  (dolist (s (sessions) surface)
    (%watch-one s surface)
    (when (upp surface) (push-surface s surface))))

(defun restyle (styles)
  (dolist (s (sessions))
    (attach:push-to (client-of s) :style :styles styles)))

(defun %attached (client)
  (let ((s (make-instance 'session :client client)))
    (c:swap *sessions* (lambda (all) (d:with all (attach:client-id client) s)))
    (pushnew #'restyle css:*listeners*)
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

(defun close-all ()
  "Let go of every attached frontend, and of what each was watching for."
  (dolist (s (sessions)) (mapc #'watch:unwatch (watching s)))
  (c:put *sessions* (d:no-map))
  t)

(defun received (client message)
  (let ((s (%for client)))
    (when s
      (case (first message)
        (:widget-action
         (destructuring-bind (&key id args) (rest message)
           (let ((thunk (gethash id (acting s))))
             (when thunk (%act client thunk args)))))
        (:refresh (push-all s))
        (:hint
         (destructuring-bind (&key text) (rest message)
           (setf (node:contents (pine.world.world:ensure pine.world.world:*world*
                                                        "echo" "hint"))
                 (or text ""))))
        (t nil)))))

(defun %act (client thunk args)
  "A widget's action runs on a thread of its own: one that shells out, asks a
question or stands in a fault holds up neither this client's next message nor
anybody else's."
  (task:once (format nil "widget ~d" (incf *acts*))
             (lambda ()
               (let ((*client* client))
                 (fault:attempt (lambda ()
                                  (if args (apply thunk args) (funcall thunk)))
                                "a widget")))))

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
  (setf surface:*on-declare* #'declared)
  (attach:app :desktop
              (d:map :doc "the bar, the echo strip and the panels"
                     :attached #'%attached
                     :received #'received
                     :detached #'%detached))
  t)
