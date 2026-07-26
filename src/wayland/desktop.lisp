(in-package #:pine.wayland)

;;;; The daemon-attached desktop client. It attaches to the running daemon as
;;;; :kind :desktop, receives (:widgets :surface NAME :tree DATA) pushes,
;;;; rebuilds each surface's node tree with pine.ui.node:wire->node (click
;;;; handlers become ids sent back), and paints it onto a wlr-layer-shell
;;;; surface with cairo. Panels toggle via (:panel :name :show). Interaction
;;;; sends (:widget-action)/(:hint) to the daemon, which runs the closures --
;;;; the tree crosses as data, the closures and refs stay on the daemon.
;;;;
;;;; Threading: wayland calls must all run on the loop thread, but sento delivers
;;;; the display actor's messages on pool threads. So the actor handler only
;;;; enqueues closures; the loop drains them between (non-blocking) wayland
;;;; dispatches. Outgoing tells are async and thread-safe, so they need no
;;;; marshalling.

(defclass desktop-client ()
  ((sys      :initarg :sys  :accessor client-sys :initform nil)
   (conn     :initarg :conn :reader conn)
   (ref      :initform nil  :accessor client-ref)          ; remote-ref to daemon
   (surfaces :initform (make-hash-table :test 'equal) :reader client-surfaces)
   (wire     :initform (make-hash-table :test 'equal) :reader client-wire)
   (roles    :initform (make-hash-table :test 'equal) :reader client-roles)
   (pump     :initarg :pump :reader client-pump)
   (done     :initform nil  :accessor client-done)))

;;;; Cross-thread command queue.

;;;; Outgoing: input back to the daemon.

(defun send-widget-action (client id args)
  (a:when-let ((ref (client-ref client)))
    (ignore-errors (sento.actor:tell ref (list :widget-action :id id :args args)))))

(defun send-hint (client text)
  (a:when-let ((ref (client-ref client)))
    (ignore-errors (sento.actor:tell ref (list :hint :text text)))))

(defun send-refresh (client)
  (a:when-let ((ref (client-ref client)))
    (ignore-errors (sento.actor:tell ref (list :refresh)))))

(defun action-sender (client)
  "wire->node's :on-action: given the daemon's action id, return the handler that
sends it back with any interaction args (the slider value, or none for buttons)."
  (lambda (id) (lambda (&rest args) (send-widget-action client id args))))

(defun surface-tree-fn (client name)
  (lambda ()
    (l:wire->node (gethash name (client-wire client))
                  :on-action (action-sender client))))

;;;; Per-surface layout: how each named surface anchors and sizes. :axis says
;;;; which dimensions come from the tree; the other is 0 so the compositor sizes
;;;; it (a full-height bar, a full-width echo).

(defun spec-for-role (role)
  "The wayland placement for a surface :as ROLE -- the layer-shell mapping for the
desktop's furniture. (:toplevel is xdg-shell and is handled by the editor
frontend, never here.) The daemon declares the role in Wayland's vocabulary; the
client maps it to a layer, anchor, and exclusive zone.

:exclusive is the zone the surface keeps for itself: :w or :h claims the size
measured on that axis, and windows are laid out in what is left. The furniture
that frames the session -- the bar and the echo strip -- claims its space; a
panel or an overlay is transient and floats over the windows instead."
  (case role
    (:background (list :layer :background :anchor '(:top :left :bottom :right) :axis :wh
                       :avail 3840 :exclusive 0 :margin '(0 0 0 0)))
    (:bar     (list :layer :top :anchor '(:top :left :bottom) :axis :w :avail 160
                    :exclusive :w :margin '(0 0 0 0)))
    (:echo    (list :layer :top :anchor '(:bottom :left :right) :axis :h :avail 3840
                    :exclusive :h :margin '(0 0 0 0)))
    (:overlay (list :layer :overlay :anchor '(:top :right) :axis :wh :avail 460
                    :exclusive 0 :margin '(8 8 0 0)))
    (t        (list :layer :overlay :anchor '(:top :left) :axis :wh :avail 460    ; :panel
                    :exclusive 0 :margin '(8 0 0 8)))))

(defun default-role (name)
  "Fallback role for the shipped surfaces, which register no :as."
  (cond ((string= name "bar") :bar) ((string= name "echo") :echo) (t :panel)))

(defun create-surface (client name)
  "Open the layer surface for NAME, placed by its declared :as role."
  (let* ((role (or (gethash name (client-roles client)) (default-role name)))
         (spec (spec-for-role role))
         (tfn  (surface-tree-fn client name)))
    (multiple-value-bind (mw mh) (measure-panel tfn :avail-w (getf spec :avail))
      (let* ((axis (getf spec :axis))
             (w (if (member axis '(:w :wh)) mw 0))
             (h (if (member axis '(:h :wh)) mh 0))
             (e (getf spec :exclusive))
             (ls (open-layer-surface (conn client) tfn
                                     :layer (getf spec :layer) :anchor (getf spec :anchor)
                                     :width w :height h
                                     :exclusive (case e (:w w) (:h h) (t e))
                                     :margin (getf spec :margin)
                                     :on-closed (lambda () (destroy-surface client name)))))
        (setf (gethash name (client-surfaces client)) ls)))))

(defun destroy-surface (client name)
  (a:when-let ((ls (gethash name (client-surfaces client))))
    (ignore-errors (zwlr-layer-surface-v1.destroy (ls-layer-surf ls)))
    (ignore-errors (wl-surface.destroy (ls-wl-surface ls)))
    (setf (wl-conn-surfaces (conn client))
          (remove ls (wl-conn-surfaces (conn client)) :key #'cdr))
    (remhash name (client-surfaces client))))

;;;; Message handling (runs on the loop thread, via the queue).

(defun on-widgets (client name tree as)
  "A fresh tree for surface NAME: cache it and its role, then update or create.
An always-on surface (:bar / :echo) is created on its first tree; a panel waits
for (:panel :show)."
  (setf (gethash name (client-wire client)) tree)
  (when as (setf (gethash name (client-roles client)) as))
  (let ((ls (gethash name (client-surfaces client)))
        (role (or (gethash name (client-roles client)) (default-role name))))
    (cond (ls (build-tree ls) (paint-surface ls))
          ((member role '(:bar :echo)) (create-surface client name)))))

(defun on-panel (client name show)
  (if show
      (unless (gethash name (client-surfaces client))
        (when (gethash name (client-wire client)) (create-surface client name)))
      (destroy-surface client name)))

(defun handle-desktop-message (client msg)
  "Runs on a sento pool thread: set the daemon ref, otherwise enqueue wayland
work for the loop thread."
  (case (first msg)
    (:attached
     (destructuring-bind (&key id client-uri) (rest msg)
       (declare (ignore id))
       (setf (client-ref client)
             (sento.remoting:make-remote-ref (client-sys client) client-uri))
       (send-refresh client)))
    (:widgets
     (destructuring-bind (&key surface tree as) (rest msg)
       (pine.frontend:enqueue (client-pump client) (lambda () (on-widgets client surface tree as)))))
    (:panel
     (destructuring-bind (&key name show) (rest msg)
       (pine.frontend:enqueue (client-pump client) (lambda () (on-panel client name show)))))
    (:rules
     (destructuring-bind (&key rules) (rest msg)
       (pine.text.buffer:install-rules rules)
       (pine.frontend:enqueue (client-pump client)
                (lambda ()
                  (maphash (lambda (name ls) (declare (ignore name))
                             (build-tree ls)
                             (paint-surface ls))
                           (client-surfaces client))))))))

;;;; The loop: drain queued wayland work, dispatch pending events, idle.



(defun run-desktop (&key (host pine.core.server:*host*) (port pine.core.server:*port*))
  "Attach to the daemon at HOST:PORT and run the bar, echo, and toggled panels as
wayland layer surfaces. Opens surfaces on the current display -- run it yourself;
the daemon (make daemon) must already be up."
  (let* ((conn (connect-desktop))
         (client (make-instance 'desktop-client :conn conn
                                :pump (pine.frontend:make-pump))))
    (setf *on-hover* (lambda (node) (send-hint client (or (and node (l:hint node)) ""))))
    (setf (client-sys client)
          (pine.frontend:attach :desktop (lambda (msg) (handle-desktop-message client msg))
                                :host host :port port))
    (unwind-protect
         (pine.frontend:run (wl-conn-backing conn) (client-pump client)
                            :done (lambda () (client-done client)))
      (pine.frontend:close-pump (client-pump client)))))

(defmethod pine.core.attach:run-frontend ((app pine.desktop::desktop-app))
  (declare (ignore app))
  (run-desktop))
