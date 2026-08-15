(defpackage #:pine/app/desktop
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node) (#:watch #:pine/fs/watch)
                    (#:surface #:pine/app/surface) (#:attach #:pine/net/attach)
                    (#:cmd #:pine/repl/command) (#:wire #:pine/ui/wire)
                    (#:css #:pine/ui/css) (#:fault #:pine/run/fault)
                    (#:computed #:pine/fs/computed) (#:task #:pine/run/task)
                    (#:timer #:pine/run/timer))
  (:export #:session #:sessions #:install #:push-surface #:push-all #:received #:flush
           #:close-all
           #:client-of #:acting #:mine-p #:upp #:repaint #:restyle #:declared #:*client*))
(in-package #:pine/app/desktop)

(defvar *sessions* (d:table))
(defvar *client* nil)
(defvar *acts* 0)
(defparameter *pace* 1/20
  "How often a surface that moved is pushed. A bar reads a clock, a mixer and a
window title; the world behind those moves dozens of times a second and the
screen does not need to. What changed is remembered and sent on the next tick.")


(defclass session ()
  ((client-of :initarg :client :reader client-of)
   (acting    :initform (d:table) :reader acting)
   (watching  :initform nil :accessor watching)
   (dirty     :initform (d:box (d:no-set)) :reader dirty)
   (sent      :initform (d:table) :reader sent)))

(defmethod print-object ((s session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "client ~d, ~d action~:p"
            (attach:client-id (client-of s)) (d:size (d:all (acting s))))))

(defun sessions () (d:vals (d:all *sessions*)))

(defun %for (client) (d:at (d:all *sessions*) (attach:client-id client)))

(defun %id (name at) (format nil "~a/~d" name at))

(defun %keep (s name index thunk)
  "What clicking the widget being built means, under the name of where it sits.

A surface is pushed again whenever anything it reads moves, several times a
second. An id that counted pushes was forgotten before the pointer could send
it back, so a click landed on nothing; the same button in the same place keeps
its id, and what it means is whatever the latest push says."
  (let ((id (%id name (d:swap! index #'1+))))
    (d:keep! (acting s) id thunk)
    id))

(defun %trim (s name from)
  "Drop the actions of a surface that has fewer widgets than it had."
  (loop :for at :from from
        :for id := (%id name at)
        :while (d:at (d:all (acting s)) id)
        :do (d:drop! (acting s) id)))

(defun push-surface (s surface)
  "Push what the surface says now, unless it says what it said last time. What
the world behind a bar does between two ticks is not news if the bar reads the
same as before."
  (let ((name (node:name surface))
        (index (d:box -1))
        (*client* (client-of s)))
    (let ((tree (fault:attempt (lambda () (node:contents surface))
                               (format nil "building ~a" name))))
      (when tree
        (let ((wire (wire:node->wire
                     tree :on-action (lambda (thunk) (%keep s name index thunk)))))
          (%trim s name (1+ (d:held index)))
          (cond ((equal wire (d:at (d:all (sent s)) name))
                 nil)
                (t (d:keep! (sent s) name wire)
                   (attach:push-to (client-of s) :widgets
                                   :surface name
                                   :tree wire
                                   :as (surface:as surface))
                   name)))))))

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
  "Say the surface moved. Whether it is a panel that just came up is told at
once; its tree goes on the next tick."
  (when (mine-p surface)
    (dolist (s (sessions))
      (d:swap! (dirty s) (lambda (all) (d:with all (node:name surface))))
      (when (surface:panelp surface) (%shown s surface)))))

(defun %took-dirty (s)
  (loop :for had := (d:held (dirty s))
        :when (d:cas (dirty s) had (d:no-set)) :do (return had)))

(defun flush (s)
  "Push what moved since the last tick."
  (let ((names (%took-dirty s)))
    (d:do-each (name names names)
      (let ((surface (surface:surface-named name)))
        (when (and surface (upp surface))
          (fault:attempt (lambda () (push-surface s surface))
                         (format nil "pushing ~a" name)))))))

(defun %pacing (s)
  (timer:every-seconds *pace* (lambda () (flush s))
                       :as (list :desktop (attach:client-id (client-of s)))
                       :what "pushing what moved"))

(defun %unpacing (s)
  (timer:cancel (list :desktop (attach:client-id (client-of s)))))

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
    (d:keep! *sessions* (attach:client-id client) s)
    (pushnew #'restyle css:*listeners*)
    (let ((styles (css:styles)))
      (when styles (attach:push-to client :style :styles styles)))
    (%watch-surfaces s)
    (push-all s)
    (%pacing s)
    s))

(defun %detached (client)
  (let ((s (%for client)))
    (when s (%unpacing s) (mapc #'watch:unwatch (watching s)))
    (d:drop! *sessions* (attach:client-id client))
    client))

(defun close-all ()
  "Let go of every attached frontend, and of what each was watching for."
  (dolist (s (sessions)) (%unpacing s) (mapc #'watch:unwatch (watching s)))
  (d:put! *sessions* (d:no-map))
  t)

(defun received (client message)
  (let ((s (%for client)))
    (when s
      (case (first message)
        (:widget-action
         (destructuring-bind (&key id args) (rest message)
           (let ((thunk (d:at (d:all (acting s)) id)))
             (when thunk (%act client thunk args)))))
        (:refresh (d:clear! (sent s)) (push-all s))
        (:hint
         (destructuring-bind (&key text) (rest message)
           (setf (node:contents (pine/world/world:ensure pine/world/world:*world*
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
