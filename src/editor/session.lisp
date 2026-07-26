(in-package #:pine.editor)

(defstruct sess
  client aclient sink
  (inbox nil)
  (lock (bordeaux-threads:make-lock))
  (cvar (bordeaux-threads:make-condition-variable))
  (stop nil) thread pump
  ;; the frame this session last sent, so the next one can carry only the
  ;; lines that moved, and the generation the frontend must be holding for a
  ;; patch to mean anything
  (sent-wire nil) (generation 0))

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
                (pine.client:make-buffer x)
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
               (pine.client:make-window buf name)
               (make-instance 'pine.buffer:window :buffer buf :name name))))
    (unless (or pine.client:*client* (%in-actor-p))
      (setf (pine.buffer:snap w) (ignore-errors (pine.ask:ask buf :snapshot))))
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

;;;; World: the arrangement serialized in the builder language. %tree->form
;;;; walks the live tree into (:column ... (:window "name" :expand 1) (:rule)
;;;; (:echo) (:modeline)) -- pure readable data, the same vocabulary init.lisp
;;;; speaks -- and %form->tree rebuilds it with the same constructors the seed
;;;; uses. A node outside this vocabulary makes the whole form nil, so exotic
;;;; init chrome falls back to its source of truth, the builder.

(defun %node-props (n)
  "The declared props worth persisting on N: :expand and :class, when set.
Font size is derived and opacity is a style rule -- neither is identity."
  (append (let ((e (pine.layout:expand-of n)))
            (when (and (numberp e) (not (zerop e))) (list :expand e)))
          (let ((c (pine.layout:css-class n)))
            (when c (list :class c)))))

(defun %tree->form (n)
  "N in the builder language, or nil when N (or a descendant) is outside the
editor vocabulary."
  (typecase n
    (pine.layout:window-node
     (case (pine.layout:window-kind n)
       (:window (let ((w (pine.layout:window-of n)))
                  (and w (list* :window (pine.buffer:window-name w)
                                (%node-props n)))))
       (:modeline (let ((w (pine.layout:window-of n)))
                    (if w (list :modeline (pine.buffer:window-name w))
                        (list :modeline))))
       (:echo (list :echo))))
    (pine.layout:separator (list :rule))
    ((or pine.layout:vstack pine.layout:hstack)
     (let ((kids (mapcar #'%tree->form (pine.layout:nodes n))))
       (unless (member nil kids)
         (list* (if (typep n 'pine.layout:vstack) :column :row)
                (append
                 (let ((s (pine.layout:spacing n)))
                   (unless (zerop s) (list :spacing s)))
                 (let ((a (pine.layout:align n)))
                   (unless (eq a :start) (list :align a)))
                 (%node-props n)
                 kids)))))))

(defun %live-name (name)
  "NAME when that buffer still exists, else scratch -- a restored window never
points at nothing (killed buffers, dead terminals)."
  (let ((srv pine.server:*server*))
    (if (and (stringp name) srv (pine.server:buffer-table srv)
             (gethash name (pine.server:buffer-table srv)))
        name
        "scratch")))

(defun %split-props (rest)
  "(values PLIST CHILD-FORMS): the leading keyword pairs, then the subtrees."
  (loop for tail on rest by #'cddr
        while (keywordp (first tail))
        append (list (first tail) (second tail)) into props
        finally (return (values props tail))))

(defun %form->tree (form)
  "Rebuild a live node from FORM with the seed's own constructors."
  (destructuring-bind (tag . rest) form
    (ecase tag
      (:window (apply #'editor-window-node (%live-name (first rest))
                      (rest rest)))
      (:modeline (if (stringp (first rest))
                     (editor-modeline-node (%live-name (first rest)))
                     (editor-modeline-node)))
      (:echo (editor-echo-node))
      (:rule (pine.layout:rule))
      ((:column :row)
       (multiple-value-bind (props kids) (%split-props rest)
         (apply (if (eq tag :column) #'pine.layout:column #'pine.layout:row)
                (append props (mapcar #'%form->tree kids))))))))

(defun %arrangement-data ()
  "The :arrangement world entry for the client in scope: the tree in builder
form plus the focused and current buffer names. nil when no client is bound
or the tree has foreign nodes."
  (let ((client pine.client:*client*))
    (when client
      (let* ((tree (pine.client:arrangement client))
             (form (and tree (%tree->form tree))))
        (when form
          (let ((fw (pine.client:focused-window client))
                (cur (pine.client:current-buffer client)))
            (list :tree form
                  :focus (and fw (pine.buffer:window-name fw))
                  :current (and cur (%buffer-name cur cur)))))))))

(pine.world:register :arrangement :save #'%arrangement-data)

(defun %restore-arrangement (client saved)
  "Build SAVED's tree for CLIENT and land its focus. nil when building fails
(the seed then falls back to the builder)."
  (handler-case
      (let ((tree (%form->tree (getf saved :tree))))
        (setf (pine.client:arrangement client) tree)
        (let* ((ws (pine.client:windows client))
               (w (or (find (getf saved :focus) ws
                            :key #'pine.buffer:window-name :test #'equal)
                      (first (last ws)))))
          (when w
            (pine.client:focus-window w)
            (setf (pine.client:current-buffer client)
                  (or (let ((srv pine.server:*server*))
                        (and srv (pine.server:buffer-table srv)
                             (gethash (getf saved :current)
                                      (pine.server:buffer-table srv))))
                      (pine.buffer:buffer-ref w)))))
        tree)
    (error (c)
      (format *error-output* "world arrangement restore failed: ~a~%" c)
      nil)))

(defun %seed-editor-tree (client &key (world t))
  "Seed CLIENT's live editor tree: the saved world arrangement when WORLD and
one exists, else the registered `editor' surface builder (init.lisp), else the
engine default. Focus lands on the saved focus or the first window leaf."
  (let ((pine.client:*client* client)
        (builder (gethash "editor"
                          (symbol-value (find-symbol "*SURFACES*" :pine.desktop))))
        (saved (and world
                    (ignore-errors (pine.var:var :world-save nil))
                    (pine.store:store '(:world :arrangement)))))
    (setf (pine.client:windows client) nil
          (pine.client:focused-window client) nil)
    (or (and saved (getf saved :tree) (%restore-arrangement client saved))
        (progn
          (setf (pine.client:windows client) nil
                (pine.client:focused-window client) nil)
          (let ((tree (if builder (funcall builder nil) (%default-editor-tree))))
            (setf (pine.client:arrangement client) tree)
            (let ((w (first (last (pine.client:windows client)))))
              (when w
                (pine.client:focus-window w)
                (setf (pine.client:current-buffer client)
                      (pine.buffer:buffer-ref w))))
            tree)))))

(defun reseed-editor-sessions ()
  "Re-seed every attached editor session's live tree from the (re)loaded
`editor' builder and repaint."
  (dolist (c pine.attach:*clients*)
    (when (eq (pine.attach:attached-client-kind c) :editor)
      (let ((s (pine.attach:attached-client-session c)))
        (when (sess-p s)
          ;; :reload is the declared reset: the builder wins, and the relayout
          ;; below saves the reasserted tree as the new world.
          (%seed-editor-tree (sess-client s) :world nil)
          (let ((pine.client:*client* (sess-client s)))
            (pine.render:relayout))
          (sento.actor:tell (pine.client:renderer (sess-client s))
                            '(:force-render)))))))

(defun push-editor-surface (aclient s)
  "Refresh the session's live tree and push it as the `editor' surface.

Sends the lines that changed when the frame differs from the last one only in
its rows and cursor, and the whole tree otherwise. Nearly all of a frame is
those rows, and a keystroke moves one of them."
  (let* ((client (sess-client s))
         (tree (pine.render:refresh-editor-tree client)))
    (when tree
      (let* ((wire (pine.layout:node->wire tree))
             (patch (pine.layout:rows-patch (sess-sent-wire s) wire)))
        (incf (sess-generation s))
        (cond
          (patch
           (pine.attach:push-to-app aclient :rows-patch :surface "editor"
                                    :patch patch
                                    :generation (sess-generation s)))
          (t
           (pine.attach:push-to-app aclient :widgets :surface "editor"
                                    :tree wire :as :toplevel
                                    :generation (sess-generation s))))
        (setf (sess-sent-wire s) wire)))))

(defun editor-frame (aclient)
  "Renderer trigger for an editor session: refresh the live tree and push it."
  (let ((s (pine.attach:attached-client-session aclient)))
    (when (sess-p s)
      (push-editor-surface aclient s))))

(defun sess-signal (s msg)
  (bordeaux-threads:with-lock-held ((sess-lock s))
    (setf (sess-inbox s) (nconc (sess-inbox s) (list msg)))
    (bordeaux-threads:condition-notify (sess-cvar s))))

(defun %scratch-text ()
  "Scratch's text straight off the server table -- no client needed, so the
shutdown sweep can save it."
  (let* ((srv pine.server:*server*)
         (buf (and srv (pine.server:buffer-table srv)
                   (gethash "scratch" (pine.server:buffer-table srv))))
         (text (and buf (ignore-errors
                         (sento.actor:ask-s buf '(:get-text) :time-out 2)))))
    (and (stringp text) (plusp (length text)) text)))

(pine.world:register :scratch
  :save #'%scratch-text
  :restore (lambda (text)
             (let ((buf (pine.client:make-buffer "scratch")))
               (sento.actor:tell buf (list :replace-content :content text)))))

(defvar *world-restored* nil
  "The buffer/scratch restore runs once per daemon life, at the first editor
attach (a client must be in scope); the arrangement applies on every seed.")

(defun make-editor-session (aclient &key sink (server pine.server:*server*))
  (let ((client (pine.client:start-client server)))
    (pine.render:start-renderer client)
    (setf (pine.client:paint-sink client) sink)
    (let ((pine.client:*client* client))
      (let ((buf (pine.client:make-buffer "scratch")))
        (setf (pine.client:current-buffer client) buf)
        (pine.client:set-buffer-mode buf :lisp-mode)
        (ignore-errors (pine.ask:tell buf :set-local :key :package :value :pine-user)))
      (unless *world-restored*
        (setf *world-restored* t)
        (pine.world:restore-world))
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
  "Ask the renderer to drain pending terminal output and repaint.

Woken by the pty readers, then paced: one repaint per 30th of a second however
much output arrived, and nothing at all while no terminal is producing any."
  (let* ((client (sess-client s))
         (wake (pine.client:terminal-wake client)))
    (setf (sess-pump s)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop :until (sess-stop s)
                   :do (sb-thread:wait-on-semaphore wake)
                       (loop :while (sb-thread:try-semaphore wake))
                       (unless (sess-stop s)
                         (sleep 1/30)
                         (sento.actor:tell (pine.client:renderer client)
                                           '(:term-tick)))))
           :name "pine-term-pump"))))

(defun stop-session (s)
  "End SESS's threads. Both wait rather than poll, so both are signalled."
  (setf (sess-stop s) t)
  (bordeaux-threads:with-lock-held ((sess-lock s))
    (bordeaux-threads:condition-notify (sess-cvar s)))
  (sb-thread:signal-semaphore (pine.client:terminal-wake (sess-client s))))

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
       (sento.actor:tell (pine.client:renderer client) msg))
      (:refresh
       ;; the frontend is holding a frame this session cannot patch onto;
       ;; forget what was sent so the next push carries the whole tree
       (setf (sess-sent-wire s) nil)
       (sento.actor:tell (pine.client:renderer client) '(:force-render))))))

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

(defclass editor-app (pine.attach:app)
  ()
  (:default-initargs :kind :editor)
  (:documentation "The editor window: buffers, windows, the mode line."))

(defmethod pine.attach:attached ((app editor-app) client)
  (make-editor-session
   client :sink (lambda (&rest _) (declare (ignore _)) (editor-frame client))))

(defmethod pine.attach:received ((app editor-app) client message)
  (session-input client message))

(defmethod pine.attach:detached ((app editor-app) client)
  (let ((s (pine.attach:attached-client-session client)))
    (when (sess-p s) (stop-session s))))

(pine.attach:register-app (make-instance 'editor-app))

(defun install-editor-sessions ()
  (ignore-errors (install-variables))
  (setf pine.command:*terminal-handler*   #'pine.term:terminal-dispatch
        pine.eval:*on-debug*              #'%eval-error)
  ;; a process agent's error comes home to this editor's restart menu; jobs
  ;; chains on top of this hook, so both fire.
  (let ((prev pine.actor:*agent-debug-hook*))
    (setf pine.actor:*agent-debug-hook*
          (lambda (msg)
            (when prev
              (pine.eval:attempt (lambda () (funcall prev msg)) "agent debug relay"))
            (pine.eval:attempt (lambda () (%agent-debug-surface msg))
                               "agent debug surface"))))
  nil)
