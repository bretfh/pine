(in-package #:pine.editor)

(defstruct sess
  client aclient sink
  (inbox nil)
  (lock (bordeaux-threads:make-lock))
  (cvar (bordeaux-threads:make-condition-variable))
  (stop nil) thread pump)

;;;; The editor as a live tree. The `editor' surface's node tree is seeded once
;;;; per session (from init.lisp's builder or the engine default) and then LIVES:
;;;; the split commands mutate it, each frame arranges it through the layout
;;;; engine and renders its view leaves, and node->wire ships it to the attached
;;;; frontend like any other surface.

(defun editor-font-px ()
  "The editor's cell font size -- the one theme metric both the daemon's window
nodes and the editor frontend derive their cell grid from, so a buffer laid out
at N cols x rows lands exactly in the frontend's cells."
  (pine.buffer:metric :font-px 15))

(defun %in-actor-p ()
  "True on a thread inside an actor's receive, where a blocking ask is
forbidden (the liveness contract)."
  (and (boundp 'sento.actor:*self*) sento.actor:*self*))

(defun %resolve-buffer (x)
  "Coerce X to a buffer actor: an actor passes through; a name string resolves
through the client when one is in scope (creating the buffer if missing), else
through the server's buffer table."
  (etypecase x
    (string (if pine.client:*client*
                (pine.buffer:make-buffer x)
                (let ((srv pine.server:*server*))
                  (and srv (pine.server:buffer-table srv)
                       (gethash x (pine.server:buffer-table srv))))))
    (t x)))

(defun %buffer-name (x buf)
  "BUF's registered name, without asking the actor: the designator string
itself, or a reverse lookup in the server's buffer table."
  (if (stringp x)
      x
      (let ((srv pine.server:*server*) (name ""))
        (when (and srv (pine.server:buffer-table srv))
          (maphash (lambda (k v) (when (eq v buf) (setf name k)))
                   (pine.server:buffer-table srv)))
        name)))

(defun %backing-window (buf name)
  "A pine.buffer:window viewing BUF. Registered on the client's window list
when one is in scope (an editor view the commands can focus and split), else
detached (a read-only view in a panel or a layout buffer). The snapshot comes
through the renderer subscription for client windows; a detached window
fetches one only off-actor, so seeding never blocks a receive."
  (let ((w (if pine.client:*client*
               (pine.buffer:make-window buf name)
               (make-instance 'pine.buffer:window :buffer buf :name name))))
    (unless (or pine.client:*client* (%in-actor-p))
      (setf (pine.buffer:snap w) (ignore-errors (pine.buffer:ask buf :snapshot))))
    w))

(defun editor-window-node (x &rest props)
  "A live view of X's buffer as a window leaf: visible lines, highlights,
region, terminal grid, or layout rows, rendered at the rect the tree arranges
it into. In an editor session the leaf joins the live tree (rows refresh every
frame, the focused one carries the caret); elsewhere it renders once at build."
  (let* ((buf (%resolve-buffer x))
         (name (%buffer-name x buf))
         (w (and buf (%backing-window buf name)))
         (node (apply #'pine.layout:window nil :of w :kind :window
                      (append props (list :font-px (editor-font-px))))))
    (when (and buf pine.client:*client*)
      (pine.render:subscribe-to-buffer buf))
    (unless pine.client:*client*
      (when w
        (setf (pine.buffer:win-width w) 80 (pine.buffer:win-height w) 24)
        (setf (pine.layout:window-rows node)
              (nth-value 0 (pine.render:render-window-rows w)))))
    node))

(defun editor-terminal-node (x &rest props)
  "A window leaf on a terminal buffer; the emulator grid renders in its rect."
  (apply #'editor-window-node x props))

(defun editor-modeline-node (&optional x &rest props)
  "The mode line as its own leaf: the focused window's by default, or X's."
  (let ((w (when x
             (let ((buf (%resolve-buffer x)))
               (and buf (%backing-window buf (%buffer-name x buf)))))))
    (apply #'pine.layout:window nil :of w :kind :modeline
           (append props (list :font-px (editor-font-px))))))

(defun editor-echo-node (&rest props)
  "The echo/minibuffer line as a leaf: the message or the active prompt with
its input, the completion popup floating above it as an overlay (one in-flow
row, so the input line never moves), and the minibuffer caret."
  (apply #'pine.layout:window nil :kind :echo :base 1
         (append props (list :font-px (editor-font-px)))))

(defun %default-editor-tree ()
  "The engine's editor surface, used when init.lisp declares none: one window
on scratch, the echo line, the mode line."
  (pine.layout:column :align :stretch
    (editor-window-node "scratch" :expand 1)
    (editor-echo-node)
    (editor-modeline-node)))

(defun %seed-editor-tree (client)
  "Seed CLIENT's live editor tree from the registered `editor' surface builder
(init.lisp) or the engine default, focusing the first window leaf."
  (let ((pine.client:*client* client)
        (builder (gethash "editor"
                          (symbol-value (find-symbol "*SURFACES*" :pine.desktop)))))
    (setf (pine.client:windows client) nil
          (pine.client:focused-window client) nil)
    (let ((tree (if builder (funcall builder nil) (%default-editor-tree))))
      (setf (pine.client:arrangement client) tree)
      (let ((w (first (last (pine.client:windows client)))))
        (when w
          (pine.buffer:focus-window w)
          (setf (pine.client:current-buffer client) (pine.buffer:buffer-ref w))))
      tree)))

(defun reseed-editor-sessions ()
  "Re-seed every attached editor session's live tree from the (re)loaded
`editor' builder and repaint."
  (dolist (c pine.attach:*clients*)
    (when (eq (pine.attach:attached-client-kind c) :editor)
      (let ((s (pine.attach:attached-client-session c)))
        (when (sess-p s)
          (%seed-editor-tree (sess-client s))
          (let ((pine.client:*client* (sess-client s)))
            (pine.render:relayout))
          (sento.actor:tell (pine.client:renderer (sess-client s))
                            '(:force-render)))))))

(defun push-editor-surface (aclient s)
  "Refresh the session's live tree (arrange, fit, render its leaves) and push
it to the app as the `editor' surface."
  (let* ((client (sess-client s))
         (tree (pine.render:refresh-editor-tree client)))
    (when tree
      (pine.attach:push-to-app aclient :widgets :surface "editor"
                               :tree (pine.layout:node->wire tree)
                               :as :toplevel))))

(defun editor-frame (aclient)
  "Renderer trigger for an editor session: refresh the live tree and push it."
  (let ((s (pine.attach:attached-client-session aclient)))
    (when (sess-p s)
      (push-editor-surface aclient s))))

(defun sess-signal (s msg)
  (bordeaux-threads:with-lock-held ((sess-lock s))
    (setf (sess-inbox s) (nconc (sess-inbox s) (list msg)))
    (bordeaux-threads:condition-notify (sess-cvar s))))

(defun make-editor-session (aclient &key sink (server pine.server:*server*))
  (let ((client (pine.client:start-client server)))
    (pine.render:start-renderer client)
    (setf (pine.client:paint-sink client) sink)
    (let ((pine.client:*client* client))
      (let ((buf (pine.buffer:make-buffer "scratch")))
        (setf (pine.client:current-buffer client) buf)
        (pine.mode:set-buffer-mode buf :lisp-mode)
        (ignore-errors (pine.buffer:tell buf :set-local :key :package :value :pine-user)))
      (%seed-editor-tree client)
      (let ((f (pine.client:frame client)))
        (setf (pine.buffer:frame-cols f) 80 (pine.buffer:frame-rows f) 29)
        (pine.render:relayout))
      (ensure-minibuffer client)
      (let ((s (make-sess :client client :aclient aclient :sink sink)))
        (when aclient
          (setf (pine.attach:attached-client-session aclient) s)
          (when pine.buffer:*user-rules*
            (pine.attach:push-to-app aclient :rules
                                     :rules pine.buffer:*user-rules*)))
        (setf (sess-thread s)
              (bordeaux-threads:make-thread (lambda () (session-loop s))
                                            :name "pine-editor-input"))
        (start-term-pump s)
        s))))

(defun start-term-pump (s)
  "Periodically ask the renderer to drain pending terminal output and repaint,
while a terminal buffer is live."
  (let ((client (sess-client s)))
    (setf (sess-pump s)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop until (sess-stop s) do
               (sleep 1/30)
               (let ((tm (pine.client:terminal-map client)))
                 (when (and tm (plusp (hash-table-count tm)))
                   (ignore-errors
                    (sento.actor:tell (pine.client:renderer client) '(:term-tick)))))))
           :name "pine-term-pump"))))

(defun session-input (aclient msg)
  (let ((s (pine.attach:attached-client-session aclient)))
    (when (sess-p s) (sess-signal s msg))))

(defun session-feed (s msg)
  "Send one input message (a (:key ...) or (:resize ...) plist) to session S."
  (when (sess-p s) (sess-signal s msg)))

(defun apply-input (s msg)
  (let ((client (sess-client s)))
    (case (first msg)
      (:key
       (destructuring-bind (&key key-str text ctrl meta shift super &allow-other-keys)
           (rest msg)
         (let ((str (or key-str text)))
           (when str
             (pine.command:dispatch
              client (pine.key:make-key str :ctrl ctrl :meta meta :shift shift :super super))
             (sento.actor:tell (pine.client:renderer client) '(:force-render))))))
      (:resize
       (sento.actor:tell (pine.client:renderer client) msg)))))

(defun session-loop (s)
  (let ((pine.client:*client* (sess-client s)))
    (loop until (sess-stop s) do
      (let (msgs)
        (bordeaux-threads:with-lock-held ((sess-lock s))
          (loop until (or (sess-inbox s) (sess-stop s))
                do (bordeaux-threads:condition-wait (sess-cvar s) (sess-lock s)))
          (setf msgs (sess-inbox s) (sess-inbox s) nil))
        ;; No silent swallow: dispatch/call-command already surface command
        ;; errors through the debugger/echo; anything that still escapes (a
        ;; :resize handler, say) goes to the same surface rather than vanishing.
        (dolist (m msgs)
          (handler-case (apply-input s m)
            (error (c) (pine.command:command-error c))))))))

(defun install-editor-sessions ()
  (ignore-errors (install-variables))
  (setf pine.command:*terminal-handler*   #'pine.term:terminal-dispatch
        pine.eval:*on-debug*              #'%eval-error)
  ;; a process agent's error comes home to this editor's restart menu; jobs
  ;; chains on top of this hook, so both fire.
  (let ((prev pine.actor:*agent-debug-hook*))
    (setf pine.actor:*agent-debug-hook*
          (lambda (msg)
            (when prev (ignore-errors (funcall prev msg)))
            (ignore-errors (%agent-debug-surface msg)))))
  (pine.attach:register-app-kind :editor
    :on-attach (lambda (c)
                 (make-editor-session
                  c :sink (lambda (&rest _) (declare (ignore _)) (editor-frame c))))
    :on-input  (lambda (c msg) (session-input c msg))))
