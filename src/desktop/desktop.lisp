(defpackage #:pine.desktop
  (:use #:cl)
  (:export #:defsurface #:set-surface-role #:push-surface #:show-panel #:hide-panel
           #:refresh-all #:*surface-client*))

(in-package #:pine.desktop)

(defvar *surface-client* nil
  "The desktop client a surface builder is building for, or a click handler is
firing for. Bound so show / hide / toggle need no explicit client argument.")

;;;; The daemon-side desktop machinery. It hosts NAMED surfaces -- the bar and
;;;; each toggled panel, all declared in the user's init.lisp -- as declarative
;;;; trees built from refs. A surface's tree is serialized (node->wire) with its
;;;; click handlers registered under ids, and pushed to the desktop app as
;;;; (:widgets :surface NAME :tree DATA :as ROLE). The app renders each surface;
;;;; interacting sends (:widget-action :id N ...) back and the daemon runs the
;;;; closure. Panels toggle via (:panel :name NAME :show B). The tree crosses as
;;;; data; the closures and the refs stay here. The surfaces themselves live in
;;;; init.lisp, not here.

;;;; Surface registry: name -> builder (aclient -> node tree).

(defvar *surfaces* (make-hash-table :test 'equal))
(defun defsurface (name builder) (setf (gethash name *surfaces*) builder))

(defvar *surface-roles* (make-hash-table :test 'equal)
  "surface name -> placement role (:bar :echo :panel :window ...), pushed to the
client so it maps the role to the wayland surface.")
(defun set-surface-role (name role) (setf (gethash name *surface-roles*) role))
(defun surface-role (name) (gethash name *surface-roles*))

(defstruct dsession
  (actions (make-hash-table))                     ; id -> closure
  (surface-ids (make-hash-table :test 'equal))    ; surface -> ids, to drop stale
  (counter 0)
  (open nil)                                       ; currently open panel, or nil
  (views (make-hash-table :test 'equal)))          ; surface -> reactive view

(defun push-surface (aclient name)
  "Build surface NAME's tree, serialize it (fresh ids, dropping this surface's
old ones), and push it to the app."
  (let ((s (pine.core.attach:attached-client-session aclient))
        (builder (gethash name *surfaces*)))
    (when (and s builder)
      (dolist (id (gethash name (dsession-surface-ids s)))
        (remhash id (dsession-actions s)))
      (setf (gethash name (dsession-surface-ids s)) nil)
      (let ((data (pine.ui.wire:node->wire
                   (let ((*surface-client* aclient)) (funcall builder aclient))
                   :on-action (lambda (cb)
                                (let ((id (incf (dsession-counter s))))
                                  (setf (gethash id (dsession-actions s)) cb)
                                  (push id (gethash name (dsession-surface-ids s)))
                                  id)))))
        (pine.core.attach:push-to-app aclient :widgets :surface name :tree data
                                 :as (surface-role name))))))

(defun surface-view (aclient name)
  "A reactive view that re-pushes surface NAME when a ref it reads changes."
  (let (view)
    (setf view (pine.state.ref:make-view
                (lambda () (push-surface aclient name))
                (lambda ()
                  (pine.core.eval:attempt (lambda () (pine.state.ref:render-view view))
                                     (format nil "surface ~a" name)))))
    view))

(defun make-desktop-session (aclient)
  (let ((s (make-dsession)))
    (setf (pine.core.attach:attached-client-session aclient) s)
    (when pine.text.buffer:*user-rules*
      (pine.core.attach:push-to-app aclient :rules :rules pine.text.buffer:*user-rules*))
    (dolist (name '("bar" "echo"))
      (let ((v (surface-view aclient name)))
        (setf (gethash name (dsession-views s)) v)
        (pine.state.ref:render-view v)))))

(defun show-panel (aclient name)
  "Toggle panel NAME: open it (closing any other) and keep it live while open, or
close it if it is already the open one."
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (when s
      (let ((cur (dsession-open s)))
        (cond
          ((equal cur name)
           (setf (dsession-open s) nil)
           (remhash name (dsession-views s))
           (pine.core.attach:push-to-app aclient :panel :name name :show nil))
          (t
           (when cur
             (remhash cur (dsession-views s))
             (pine.core.attach:push-to-app aclient :panel :name cur :show nil))
           (setf (dsession-open s) name)
           (let ((v (surface-view aclient name)))
             (setf (gethash name (dsession-views s)) v)
             (pine.state.ref:render-view v))
           (pine.core.attach:push-to-app aclient :panel :name name :show t)))))))

(defun hide-panel (aclient name)
  "Close panel NAME if it is the open one."
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (when (and (dsession-p s) (equal (dsession-open s) name))
      (setf (dsession-open s) nil)
      (remhash name (dsession-views s))
      (pine.core.attach:push-to-app aclient :panel :name name :show nil))))

(defun desktop-input (aclient msg)
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (case (first msg)
      (:widget-action
       ;; run the handler through pine.core.eval (its own thread), never inline on the
       ;; pool -- a handler that blocks on IO or errors cannot stall the daemon.
       (destructuring-bind (&key id args) (rest msg)
         (let ((cb (and s (gethash id (dsession-actions s)))))
           (when cb
             (pine.core.eval:evaluate-thunk
              (lambda () (let ((*surface-client* aclient)) (apply cb args)))
              :package (find-package :pine-user))))))
      ;; the app asks for a fresh push once its surfaces exist (its first push on
      ;; attach can arrive before the windows are up). Re-push the bar and any
      ;; open panel.
      (:refresh
       (push-surface aclient "bar")
       (push-surface aclient "echo")
       (when (and s (dsession-open s)) (push-surface aclient (dsession-open s))))
      (:hint
       (destructuring-bind (&key text) (rest msg)
         (pine.state.ref:set-ref (pine.state.ref:defref :hint "") (or text "")))))))

(defclass desktop-app (pine.core.attach:app)
  ()
  (:default-initargs :kind :desktop)
  (:documentation "The bar, the echo strip, and the panels."))

(defmethod pine.core.attach:attached ((app desktop-app) client)
  (make-desktop-session client))

(defmethod pine.core.attach:received ((app desktop-app) client message)
  (desktop-input client message))

(defmethod pine.core.attach:detached ((app desktop-app) client)
  "Drop the surfaces' reactive views, so a dead frontend stops being rendered
for."
  (let ((s (pine.core.attach:attached-client-session client)))
    (when (dsession-p s)
      (maphash (lambda (name view)
                 (declare (ignore name))
                 (pine.state.ref:dispose-view view))
               (dsession-views s))
      (clrhash (dsession-views s)))))

(pine.core.attach:register-app (make-instance 'desktop-app))

(pine.editor.command:define-command "reload-desktop" ()
  "Re-push every surface for every attached desktop client."
  (refresh-all))

(defun refresh-all ()
  "Re-push every surface for every attached desktop client. Call it (M-x
reload-desktop) after redefining a builder so the change shows at once."
  (dolist (c pine.core.attach:*clients*)
    (when (eq (pine.core.attach:attached-client-kind c) :desktop)
      (let ((s (pine.core.attach:attached-client-session c)))
        (when (dsession-p s)
          (push-surface c "bar")
          (push-surface c "echo")
          (when (dsession-open s) (push-surface c (dsession-open s))))))))
