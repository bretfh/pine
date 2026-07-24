(in-package #:pine.wayland)

;;;; The daemon-attached desktop client. It attaches to the running daemon as
;;;; :kind :desktop, receives (:widgets :surface NAME :tree DATA) pushes,
;;;; rebuilds each surface's node tree with pine.layout:wire->node (click
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
  ((sys      :initarg :sys  :reader client-sys)
   (conn     :initarg :conn :reader conn)
   (ref      :initform nil  :accessor client-ref)          ; remote-ref to daemon
   (surfaces :initform (make-hash-table :test 'equal) :reader client-surfaces)
   (wire     :initform (make-hash-table :test 'equal) :reader client-wire)
   (roles    :initform (make-hash-table :test 'equal) :reader client-roles)
   (queue    :initform nil  :accessor client-queue)
   (qlock    :initform (bt:make-lock) :reader client-qlock)
   (done     :initform nil  :accessor client-done)))

;;;; Cross-thread command queue.

(defun enqueue (client thunk)
  (bt:with-lock-held ((client-qlock client)) (push thunk (client-queue client))))

(defun drain (client)
  (let (items)
    (bt:with-lock-held ((client-qlock client))
      (setf items (nreverse (client-queue client)) (client-queue client) nil))
    (dolist (th items) (ignore-errors (funcall th)))))

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
client maps it to a layer, anchor, and exclusive zone."
  (case role
    (:background (list :layer :background :anchor '(:top :left :bottom :right) :axis :wh
                       :avail 3840 :exclusive 0 :margin '(0 0 0 0)))
    (:bar     (list :layer :top :anchor '(:top :left :bottom) :axis :w :avail 160
                    :exclusive :w :margin '(0 0 0 0)))
    (:echo    (list :layer :top :anchor '(:bottom :left :right) :axis :h :avail 3840
                    :exclusive 0 :margin '(0 0 0 0)))
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
                                     :exclusive (if (eq e :w) w e)
                                     :margin (getf spec :margin))))
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

(defun handle-display (client msg)
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
       (enqueue client (lambda () (on-widgets client surface tree as)))))
    (:panel
     (destructuring-bind (&key name show) (rest msg)
       (enqueue client (lambda () (on-panel client name show)))))
    (:rules
     (destructuring-bind (&key rules) (rest msg)
       (pine.buffer:install-rules rules)
       (enqueue client
                (lambda ()
                  (maphash (lambda (name ls) (declare (ignore name))
                             (build-tree ls)
                             (paint-surface ls))
                           (client-surfaces client))))))))

;;;; The loop: drain queued wayland work, dispatch pending events, idle.

(defun run-loop (client)
  (let ((display (wl-conn-display (conn client))))
    (loop until (client-done client) do
      (drain client)
      (loop while (wl-display-listen display) do (wl-display-dispatch-event display))
      (sleep 0.006))))

(defun run-desktop (&key (host pine.server:*host*) (port pine.server:*port*))
  "Attach to the daemon at HOST:PORT and run the bar, echo, and toggled panels as
wayland layer surfaces. Opens surfaces on the current display -- run it yourself;
the daemon (make daemon) must already be up."
  (unless pine.server:*server*
    (setf pine.server:*server* (make-instance 'pine.server:server)))
  (ignore-errors (pine.buffer:install-default-faces))
  (let* ((sys (sento.actor-system:make-actor-system
               '(:dispatchers (:shared (:workers 2 :strategy :random)))))
         (conn (connect))
         (client (make-instance 'desktop-client :sys sys :conn conn)))
    (sento.remoting:enable-remoting sys :host pine.server:*host* :port 0)
    (setf *on-hover* (lambda (node) (send-hint client (or (and node (l:hint node)) ""))))
    (sento.actor-context:actor-of sys :name "display"
      :receive (lambda (msg) (handle-display client msg) nil))
    (pine.attach:attach-to-daemon sys
      (pine.server:daemon-uri "attach" :host host :port port)
      (pine.server:local-uri "display" (sento.remoting:remoting-port sys))
      :kind :desktop)
    ;; serve this image as agent "desktop": daemon-driven eval into the frontend,
    ;; and errors here ship their restarts home like any process agent
    (ignore-errors
     (pine.agent:serve sys :name "desktop" :master-host host :master-port port
                           :self-port (sento.remoting:remoting-port sys)))
    (run-loop client)))

(defun daemon-listening-p (port)
  (ignore-errors
    (let ((s (usocket:socket-connect pine.server:*host* port :timeout 1)))
      (usocket:socket-close s) t)))

(defun run-all (&key (port pine.server:*port*))
  "`pine start': the shim. Kick the daemon in its own process if it is not already
up, then return. The daemon reads init.lisp and spawns + supervises the frontends
(editor + desktop) as their own processes -- this shim renders nothing itself, so
the daemon, the editor, and the desktop are three separate images."
  (if (daemon-listening-p port)
      (format t "pine: daemon already up on ~a:~d~%" pine.server:*host* port)
      (progn
        (let ((self (first sb-ext:*posix-argv*)))
          (uiop:launch-program (list self "daemon")
                               :output "/tmp/pine-daemon.log"
                               :error-output "/tmp/pine-daemon.log"
                               :if-output-exists :supersede
                               :if-error-output-exists :supersede))
        (loop repeat 120 when (daemon-listening-p port) do (return)
              do (sleep 0.5)
              finally (format t "pine: daemon did not come up (log /tmp/pine-daemon.log)~%")
                      (return-from run-all))
        (format t "pine: daemon up on ~a:~d -- editor + desktop spawning.~%"
                pine.server:*host* port)))
  (finish-output))

(setf pine::*start-hook* #'run-all
      pine::*desktop-hook* #'run-desktop)
