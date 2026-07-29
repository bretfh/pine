(defpackage #:pine.desktop
  (:use #:cl)
  (:export #:surface-at #:surface-tree #:surface-role #:shownp #:names
           #:push-surface #:show-panel #:hide-panel
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

;;;; A surface is what wayland shows, and it is a path:
;;;;
;;;;   /surface/?name        the widget tree
;;;;   /surface/?name/as     the placement: :bar :panel :overlay :toplevel :echo
;;;;   /surface/?name/shown  whether it is up; [:toggle] flips it
;;;;
;;;; The tree is written as an expression, so it is computed again whenever
;;;; anything it read moves and the client is pushed the new one. Nothing polls
;;;; and nothing subscribes.

(defun surface-at (name) (pine.path:path (pine.path:parse "/surface") name))

(defun surface-role (name)
  (pine.ns:read (pine.path:child (surface-at name) "as")))

(defun shownp (name)
  (pine.ns:read (pine.path:child (surface-at name) "shown")))

(defun surface-tree (name)
  "NAME's widget tree, or NIL. The tree is the value at the path; what is under
it says where it goes."
  (let ((value (pine.ns:held (surface-at name))))
    (if (fset:map? value) (fset:lookup value :tree) value)))

(defun names ()
  "Every surface there is."
  (sort (mapcar (lambda (path) (pine.path:leaf path))
                (pine.data:keys (pine.ns:read (pine.path:parse "/surface/*")
                                              (fset:empty-map))))
        #'string<))


(defstruct dsession
  (actions (make-hash-table))                     ; id -> closure
  (surface-ids (make-hash-table :test 'equal))    ; surface -> ids, to drop stale
  (counter 0)
  )

(defun push-surface (aclient name)
  "Serialize surface NAME's tree (fresh ids, dropping this surface's old ones)
and push it to the app."
  (let ((s (pine.core.attach:attached-client-session aclient))
        (tree (surface-tree name)))
    (when (and s tree)
      (dolist (id (gethash name (dsession-surface-ids s)))
        (remhash id (dsession-actions s)))
      (setf (gethash name (dsession-surface-ids s)) nil)
      (let ((data (pine.ui.wire:node->wire
                   tree
                   :on-action (lambda (cb)
                                (let ((id (incf (dsession-counter s))))
                                  (setf (gethash id (dsession-actions s)) cb)
                                  (push id (gethash name (dsession-surface-ids s)))
                                  id)))))
        (pine.core.attach:push-to-app aclient :widgets :surface name :tree data
                                      :as (surface-role name))))))

(defun mount (aclient)
  "Push a surface whenever its tree moves, and show or hide it when its shown
moves. The tree is an expression at a path, so what re-renders it is the same
rule that recomputes anything else."
  (pine.ns:watch (pine.path:parse "/surface")
                 (lambda (value)
                   (declare (ignore value))
                   (let* ((path (pine.ns:here))
                          (leaf (pine.path:leaf path))
                          (name (if (member leaf '("shown" "as") :test #'string=)
                                    (pine.path:leaf (pine.path:parent path))
                                    leaf)))
                     (pine.err:attempt
                      (lambda ()
                        (let ((*surface-client* aclient))
                          (if (string= leaf "shown")
                              (progn
                                (when (shownp name) (push-surface aclient name))
                                (pine.core.attach:push-to-app
                                 aclient :panel :name name :show (shownp name)))
                              (push-surface aclient name))))
                      (format nil "surface ~a" name)))
                   (fset:empty-map))
                 :as (list :surface aclient)))

(defun make-desktop-session (aclient)
  (let ((s (make-dsession)))
    (setf (pine.core.attach:attached-client-session aclient) s)
    (when pine.ui.rules:*user-rules*
      (pine.core.attach:push-to-app aclient :rules :rules pine.ui.rules:*user-rules*))
    (mount aclient)
    (let ((*surface-client* aclient))
      (dolist (name (names))
        (unless (eq :panel (surface-role name))
          (pine.err:attempt (lambda () (push-surface aclient name))
                            (format nil "surface ~a" name)))))))

(defun show-panel (aclient name)
  "Toggle panel NAME: open it, closing any other, or close it if it is the one
that is open. Which panel is up is /surface/?name/shown."
  (declare (ignore aclient))
  (let ((up (shownp name)))
    (dolist (other (names))
      (when (and (not (equal other name)) (shownp other))
        (pine.ns:write (pine.path:child (surface-at other) "shown") nil)))
    (pine.ns:write (pine.path:child (surface-at name) "shown") (not up))))

(defun hide-panel (aclient name)
  "Close panel NAME if it is up."
  (declare (ignore aclient))
  (when (shownp name)
    (pine.ns:write (pine.path:child (surface-at name) "shown") nil)))

(defun desktop-input (aclient msg)
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (case (first msg)
      (:widget-action
       ;; run the handler through pine.err (its own thread), never inline on the
       ;; pool -- a handler that blocks on IO or errors cannot stall the daemon.
       (destructuring-bind (&key id args) (rest msg)
         (let ((cb (and s (gethash id (dsession-actions s)))))
           (when cb
             (pine.err:evaluate-thunk
              (lambda () (let ((*surface-client* aclient)) (apply cb args)))
              :package (find-package :pine-user))))))
      ;; the app asks for a fresh push once its surfaces exist (its first push on
      ;; attach can arrive before the windows are up). Re-push the bar and any
      ;; open panel.
      (:refresh
       (let ((*surface-client* aclient))
         (dolist (name (names))
           (when (or (not (eq :panel (surface-role name))) (shownp name))
             (push-surface aclient name)))))
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
  "Stop pushing to a frontend that is gone: the watch over /surface goes with
it, and nothing else was holding anything for it."
  (pine.ns:watch (pine.path:parse "/surface") nil :as (list :surface client)))

(pine.core.attach:register-app (make-instance 'desktop-app))

(pine.cmd:defcmd "reload-desktop" ()
  "Re-push every surface for every attached desktop client."
  (refresh-all))

(defun refresh-all ()
  "Re-push every surface for every attached desktop client. Call it (M-x
reload-desktop) after re-loading a config, so what it wrote shows at once."
  (dolist (c pine.core.attach:*clients*)
    (when (eq (pine.core.attach:attached-client-kind c) :desktop)
      (let ((s (pine.core.attach:attached-client-session c)))
        (when (dsession-p s)
          (let ((*surface-client* c))
            (dolist (name (names))
              (when (or (not (eq :panel (surface-role name))) (shownp name))
                (pine.err:attempt (lambda () (push-surface c name))
                                  (format nil "surface ~a" name))))))))))
