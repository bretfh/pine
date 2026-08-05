(defpackage #:pine.desktop
  (:use #:cl)
  (:export #:surface-at #:surface-tree #:surface-role #:shownp #:names
           #:push-surface #:show-panel #:hide-panel #:server
           #:refresh-all #:*surface-client*))

(in-package #:pine.desktop)
(named-readtables:in-readtable pine.path:syntax)

;;;; A surface is what wayland shows, and it is a path:
;;;;
;;;;   /surface/?name        the widget tree
;;;;   /surface/?name/as     the placement: :bar :panel :overlay :toplevel :echo
;;;;   /surface/?name/shown  whether it is up; [:toggle] flips it
;;;;
;;;; The tree is written as an expression, so it is computed again whenever
;;;; anything it read moves, and the client is pushed the new one. Nothing polls
;;;; and nothing subscribes.
;;;;
;;;; What crosses to the desktop app is the tree as data, with each click handler
;;;; registered under an id: (:widgets :surface NAME :tree DATA :as ROLE) out,
;;;; (:widget-action :id N) back. The closures stay here. The surfaces
;;;; themselves live in a config, not in this file.

(defstruct dsession
  (actions (make-hash-table))                     ; id -> closure
  (surface-ids (make-hash-table :test 'equal))    ; surface -> ids, to drop stale
  (counter 0))

(defvar *surface-client* nil
  "The desktop client a surface builder is building for, or a click handler is
firing for. Bound so show / hide / toggle need no explicit client argument.")

(defun surface-at (name) (pine.path:path /surface name))

(defun surface-role (name)
  (pine.ns:read (pine.path:path (surface-at name) "as")))

(defun shownp (name)
  (pine.ns:read (pine.path:path (surface-at name) "shown")))

(defun surface-tree (name)
  "NAME's widget tree, or NIL. The tree is the value at the path; what is under
it says where it goes."
  (let ((value (pine.ns:held (surface-at name))))
    (if (fset:map? value) (fset:lookup value :tree) value)))

(defun names ()
  "Every surface there is."
  (sort (mapcar (lambda (path) (pine.path:leaf path))
                (pine.data:keys (pine.ns:read /surface/* (fset:empty-map))))
        #'string<))

(defun provider ()
  "What a surface is. The clauses say only what the paths are for, so a surface
is still the expression someone wrote there."
  (pine.ns:provider
   (/surface/?name/as
    {:doc "where wayland puts it: :bar :panel :overlay :toplevel :background :echo"})
   (/surface/?name/shown {:doc "whether it is up; [:toggle] flips it"})
   (/surface/?name/anchor {:doc "raw placement, when :as is not enough"})
   (/surface/?name
    {:in (pine.data:fn [v] (if (fset:map? v) v (fset:map (:tree v))))
     :doc "the widget tree, as an expression"})
   (/surface {:doc "what pine draws on wayland"})))

(defun push-surface (aclient name)
  "Serialize surface NAME's tree, with fresh ids and this surface's old ones
dropped, and push it to the app."
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
  (pine.ns:watch /surface
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
    (let ((styles (pine.ui.css:styles)))
      (when styles
        (pine.core.attach:push-to-app aclient :style :styles styles)))
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
        (pine.ns:write (pine.path:path (surface-at other) "shown") nil)))
    (pine.ns:write (pine.path:path (surface-at name) "shown") (not up))))

(defun hide-panel (aclient name)
  "Close panel NAME if it is up."
  (declare (ignore aclient))
  (when (shownp name)
    (pine.ns:write (pine.path:path (surface-at name) "shown") nil)))

(defun refresh-all ()
  "Re-push every surface for every attached desktop client, which is what
M-x reload-desktop does after a config is loaded again."
  (dolist (c pine.core.attach:*clients*)
    (when (eq (pine.core.attach:attached-client-kind c) :desktop)
      (let ((s (pine.core.attach:attached-client-session c)))
        (when (dsession-p s)
          (let ((*surface-client* c))
            (dolist (name (names))
              (when (or (not (eq :panel (surface-role name))) (shownp name))
                (pine.err:attempt (lambda () (push-surface c name))
                                  (format nil "surface ~a" name))))))))))

(defun desktop-input (aclient msg)
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (case (first msg)
      (:widget-action
       ;; the handler runs through pine.err, on its own thread, so one that
       ;; blocks on IO or errors cannot stall the daemon
       (destructuring-bind (&key id args) (rest msg)
         (let ((cb (and s (gethash id (dsession-actions s)))))
           (when cb
             (pine.err:evaluate-thunk
              (lambda ()
                (let ((*surface-client* aclient))
                  (if (and (functionp cb) args)
                      (apply cb args)
                      (pine.cmd:run cb))))
              :package (find-package :pine-user))))))
      ;; the app asks for a fresh push once its surfaces exist: its first push on
      ;; attach can arrive before the windows are up
      (:refresh
       (let ((*surface-client* aclient))
         (dolist (name (names))
           (when (or (not (eq :panel (surface-role name))) (shownp name))
             (push-surface aclient name)))))
      (:hint
       (destructuring-bind (&key text) (rest msg)
         (pine.ns:write /hint (or text "")))))))

(defclass server (pine.ns:server) ()
  (:default-initargs :name :surface :serves (list /surface))
  (:documentation "What pine draws on wayland, as expressions at paths."))

(defmethod pine.ns:raise ((s server) &key &allow-other-keys)
  (pine.ns:write /surface (provider)))

(defclass desktop-app (pine.core.attach:app) ()
  (:default-initargs :kind :desktop)
  (:documentation "The bar, the echo strip, and the panels."))

(defmethod pine.core.attach:attached ((app desktop-app) client)
  (make-desktop-session client))

(defmethod pine.core.attach:received ((app desktop-app) client message)
  (desktop-input client message))

(defmethod pine.core.attach:detached ((app desktop-app) client)
  "Stop pushing to a frontend that is gone: the watch over /surface goes with
it, and nothing else was holding anything for it."
  (pine.ns:watch /surface nil :as (list :surface client)))

(pine.cmd:defcmd "reload-desktop" ()
  "Re-push every surface for every attached desktop client."
  (refresh-all))

(pine.ns:register (make-instance 'server))
(pine.core.attach:register-app (make-instance 'desktop-app))
