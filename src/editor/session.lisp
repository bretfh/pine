(defpackage #:pine.editor.session
  (:use #:cl)
  (:local-nicknames (#:world #:pine.state.world))
  (:export #:*world-restored*
           #:sess #:make-sess #:sess-p
           #:sess-client #:sess-aclient #:sess-sink #:sess-inbox #:sess-lock
           #:sess-cvar #:sess-stop #:sess-thread #:sess-pump
           #:sess-sent-wire #:sess-generation #:sess-signal
           #:editor-app #:editor-font-px #:editor-frame
           #:editor-window-node #:editor-terminal-node
           #:editor-modeline-node #:editor-echo-node
           #:editor-tree #:make-editor-session #:reseed-editor-sessions
           #:push-editor-surface #:start-term-pump #:stop-session
           #:session-input #:session-feed #:session-loop #:apply-input))

(in-package #:pine.editor.session)

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
  (pine.ui.face:metric :font-px 15))

(defun %resolve-buffer (x)
  "Coerce X to a buffer actor: an actor passes through; a name string resolves
through the client when one is in scope (creating the buffer if missing), else
through the server's buffer table."
  (etypecase x
    (string (if pine.editor.frame:*client*
                (pine.editor.frame:make-buffer x)
                (and (pine.ns:held (pine.buf:at x :text)) x)))
    (t x)))

(defun %buffer-name (x buf)
  "BUF's registered name, without asking the actor: the designator string
itself, or a reverse lookup in the server's buffer table."
  (declare (ignore buf))
  (if (stringp x) x ""))

(defun %backing-window (buf name)
  ;; a detached view: a panel, or a tool buffer rendered once
  "A pine.text.window:window viewing BUF. Registered on the client's window list
when one is in scope (an editor view the commands can focus and split), else
detached (a read-only view in a panel or a layout buffer).

It takes its first snapshot here, by reading the buffer's leaves. Nothing has to
be told about a window that has just appeared."
  (let ((w (if pine.editor.frame:*client*
               (pine.editor.frame:make-window buf name)
               (make-instance 'pine.text.window:window :buffer buf :name name))))
    (setf (pine.text.window:snap w) (pine.text.buffer:snapshot-of buf))
    w))

(defun editor-window-node (&optional x &rest props)
  "A live view as a window leaf: visible lines, highlights, region, terminal
grid, or a tool buffer's rows, rendered at the rect the tree arranges it into.

With no buffer it is the arrangement -- every window there is, laid out the way
/win says. With one it is a fixed view of that buffer, which is what a panel or
a detached view wants."
  (if (null x)
      (%arrangement-node)
      (let* ((buf (%resolve-buffer x))
             (name (%buffer-name x buf))
             (w (and buf (%backing-window buf name)))
             (node (apply #'pine.ui.build:window nil :of w :kind :window
                          (append props (list :font-px (editor-font-px))))))
        (unless pine.editor.frame:*client*
          (when w
            (setf (pine.text.window:win-width w) 80
                  (pine.text.window:win-height w) 24)
            (setf (pine.ui.node:window-rows node)
                  (nth-value 0 (pine.ui.render:render-window-rows w)))))
        node)))

(defun editor-terminal-node (x &rest props)
  "A window leaf on a terminal buffer; the emulator grid renders in its rect."
  (apply #'editor-window-node x props))

(defun editor-modeline-node (&optional x &rest props)
  "The mode line as its own leaf: the focused window's by default, or X's."
  (let ((w (when x
             (let ((buf (%resolve-buffer x)))
               (and buf (%backing-window buf (%buffer-name x buf)))))))
    (apply #'pine.ui.build:window nil :of w :kind :modeline
           (append props (list :font-px (editor-font-px))))))

(defun editor-echo-node (&rest props)
  "The echo/minibuffer line as a leaf: the message or the active prompt with
its input, the completion popup floating above it as an overlay (one in-flow
row, so the input line never moves), and the minibuffer caret."
  (apply #'pine.ui.build:window nil :kind :echo :base 1
         (append props (list :font-px (editor-font-px)))))

(defun %default-editor-tree ()
  "The engine's editor surface, used when init.lisp declares none: one window
on scratch, the echo line, the mode line."
  (pine.ui.build:column :align :stretch
    (editor-window-node)
    (editor-echo-node)
    (editor-modeline-node)))

;;;; The arrangement is /win, so the live tree is built from it rather than
;;;; mutated and serialized: a split writes the path, and this is what the
;;;; renderer paints of what the path says.

(defun %win-leaf (path)
  "A window leaf viewing PATH."
  (let ((w (pine.editor.frame:window-of path)))
    (pine.ui.build:window nil :of w :kind :window
                          :expand (max 1 (pine.win:weight-of path))
                          :font-px (editor-font-px))))

(defun %win-node (path)
  "PATH as live nodes: a window leaf, or the stack its parts make, with a
divider between them."
  (if (pine.win:stack-p path)
      (let* ((row (eq :row (pine.win:runs-of path)))
             (parts (mapcar #'%win-node (pine.win::%parts path)))
             (kids (loop :for part :in parts
                         :for first = t :then nil
                         :append (if first
                                     (list part)
                                     (list (pine.ui.build:rule
                                            :vertical row
                                            :face :border-inactive)
                                           part)))))
        (apply (if row #'pine.ui.build:row #'pine.ui.build:column)
               :align :stretch :expand 1 kids))
      (%win-leaf path)))

(defun %arrangement-node ()
  "Every window there is, as one node: what (window) with no buffer builds."
  (let ((parts (pine.win::%parts (pine.path:parse "/win"))))
    (cond ((null parts) (%win-leaf (pine.win:seed (pine.buf:at "scratch"))))
          ((null (rest parts)) (%win-node (first parts)))
          (t (apply #'pine.ui.build:column :align :stretch :expand 1
                    (mapcar #'%win-node parts))))))

(defun editor-tree (client)
  "CLIENT's live editor tree: the registered `editor' surface builder from
init.lisp, else the engine default. The arrangement inside it comes from /win,
which is where it survives a restart, so there is nothing to restore."
  (let ((pine.editor.frame:*client* client)
        (builder (gethash "editor"
                          (symbol-value (find-symbol "*SURFACES*" :pine.desktop)))))
    (if builder (funcall builder nil) (%default-editor-tree))))

(defun %seed-editor-tree (client &key (world t))
  "Build CLIENT's tree and land the focus."
  (declare (ignore world))
  (let ((pine.editor.frame:*client* client))
    (setf (pine.editor.frame:arrangement client) (editor-tree client))
    (let ((w (pine.editor.frame:focused-window)))
      (when w
        (setf (pine.editor.frame:current-buffer client)
              (pine.text.window:buffer-ref w))))
    (pine.editor.frame:arrangement client)))

(defun reseed-editor-sessions ()
  "Re-seed every attached editor session's live tree from the (re)loaded
`editor' builder and repaint."
  (dolist (c pine.core.attach:*clients*)
    (when (eq (pine.core.attach:attached-client-kind c) :editor)
      (let ((s (pine.core.attach:attached-client-session c)))
        (when (sess-p s)
          ;; :reload is the declared reset: the builder wins, and the relayout
          ;; below saves the reasserted tree as the new world.
          (%seed-editor-tree (sess-client s) :world nil)
          (let ((pine.editor.frame:*client* (sess-client s)))
            (pine.ui.render:relayout))
          (sento.actor:tell (pine.editor.frame:renderer (sess-client s))
                            '(:force-render)))))))

(defun push-editor-surface (aclient s)
  "Refresh the session's live tree and push it as the `editor' surface.

Sends the lines that changed when the frame differs from the last one only in
its rows and cursor, and the whole tree otherwise. Nearly all of a frame is
those rows, and a keystroke moves one of them."
  (let* ((client (sess-client s))
         (tree (pine.ui.render:refresh-editor-tree client)))
    (when tree
      (let* ((wire (pine.ui.wire:node->wire tree))
             (patch (pine.ui.wire:rows-patch (sess-sent-wire s) wire)))
        (incf (sess-generation s))
        (cond
          (patch
           (pine.core.attach:push-to-app aclient :rows-patch :surface "editor"
                                    :patch patch
                                    :generation (sess-generation s)))
          (t
           (pine.core.attach:push-to-app aclient :widgets :surface "editor"
                                    :tree wire :as :toplevel
                                    :generation (sess-generation s))))
        (setf (sess-sent-wire s) wire)))))

(defun editor-frame (aclient)
  "Renderer trigger for an editor session: refresh the live tree and push it."
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (when (sess-p s)
      (push-editor-surface aclient s))))

(defun sess-signal (s msg)
  (bordeaux-threads:with-lock-held ((sess-lock s))
    (setf (sess-inbox s) (nconc (sess-inbox s) (list msg)))
    (bordeaux-threads:condition-notify (sess-cvar s))))

(defun %scratch-text ()
  "Scratch's text, read from its leaves -- no client needed, so the shutdown
sweep can save it."
  (let ((text (ignore-errors (pine.text.buffer:text-of "scratch"))))
    (and (stringp text) (plusp (length text)) text)))

(defmethod world:snapshot ((name (eql :scratch))) (%scratch-text))

(defmethod world:revive ((name (eql :scratch)) text)
  (let ((buf (pine.editor.frame:make-buffer "scratch")))
    (pine.ns:write (pine.text.buffer:at (pine.text.buffer:name-of buf) :text) text)))

(defvar *world-restored* nil
  "The buffer/scratch restore runs once per daemon life, at the first editor
attach (a client must be in scope); the arrangement applies on every seed.")

(defun make-editor-session (aclient &key sink (server pine.core.server:*server*))
  (let ((client (pine.editor.frame:start-client server)))
    (pine.ui.render:start-renderer client)
    (setf (pine.editor.frame:paint-sink client) sink)
    (let ((pine.editor.frame:*client* client))
      (let ((buf (pine.editor.frame:make-buffer "scratch")))
        (setf (pine.editor.frame:current-buffer client) buf)
        (pine.editor.frame:set-buffer-mode buf :lisp)
        (ignore-errors (pine.editor.ask:tell buf :set-local :key :package :value :pine-user)))
      (unless *world-restored*
        (setf *world-restored* t)
        (world:restore))
      (%seed-editor-tree client)
      (let ((f (pine.editor.frame:frame client)))
        (setf (pine.text.window:frame-cols f) 80 (pine.text.window:frame-rows f) 29)
        (pine.ui.render:relayout))
      (pine.editor.minibuffer:ensure-minibuffer client)
      (let ((s (make-sess :client client :aclient aclient :sink sink)))
        (when aclient
          (setf (pine.core.attach:attached-client-session aclient) s)
          (when pine.ui.rules:*user-rules*
            (pine.core.attach:push-to-app aclient :rules
                                     :rules pine.ui.rules:*user-rules*)))
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
         (wake (pine.editor.frame:terminal-wake client)))
    (setf (sess-pump s)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop :until (sess-stop s)
                   :do (sb-thread:wait-on-semaphore wake)
                       (loop :while (sb-thread:try-semaphore wake))
                       (unless (sess-stop s)
                         (sleep 1/30)
                         (sento.actor:tell (pine.editor.frame:renderer client)
                                           '(:term-tick)))))
           :name "pine-term-pump"))))

(defun stop-session (s)
  "End SESS's threads. Both wait rather than poll, so both are signalled."
  (setf (sess-stop s) t)
  (bordeaux-threads:with-lock-held ((sess-lock s))
    (bordeaux-threads:condition-notify (sess-cvar s)))
  (sb-thread:signal-semaphore (pine.editor.frame:terminal-wake (sess-client s))))

(defun session-input (aclient msg)
  (let ((s (pine.core.attach:attached-client-session aclient)))
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
             (pine.editor.command:dispatch
              client (pine.editor.key:make-key str :ctrl ctrl :meta meta :shift shift :super super))
             (sento.actor:tell (pine.editor.frame:renderer client) '(:force-render))))))
      (:resize
       (sento.actor:tell (pine.editor.frame:renderer client) msg))
      (:refresh
       ;; the frontend is holding a frame this session cannot patch onto;
       ;; forget what was sent so the next push carries the whole tree
       (setf (sess-sent-wire s) nil)
       (sento.actor:tell (pine.editor.frame:renderer client) '(:force-render)))
      ;; An input message this daemon does not know is two images disagreeing
      ;; about the protocol, which is worth hearing about the first time rather
      ;; than as a feature that quietly does nothing.
      (t (error "This daemon has no handler for the input message ~s." msg)))))

(defun session-loop (s)
  (let ((pine.editor.frame:*client* (sess-client s)))
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
            (error (c) (pine.editor.command:command-error c))))))))

(defclass editor-app (pine.core.attach:app)
  ()
  (:default-initargs :kind :editor)
  (:documentation "The editor window: buffers, windows, the mode line."))

(defmethod pine.core.attach:attached ((app editor-app) client)
  (make-editor-session
   client :sink (lambda (&rest _) (declare (ignore _)) (editor-frame client))))

(defmethod pine.core.attach:received ((app editor-app) client message)
  (session-input client message))

(defmethod pine.core.attach:detached ((app editor-app) client)
  (let ((s (pine.core.attach:attached-client-session client)))
    (when (sess-p s) (stop-session s))))

(pine.core.attach:register-app (make-instance 'editor-app))

(setf pine.editor.command:*terminal-handler* #'pine.term:terminal-dispatch
      pine.err:*on-debug*            #'pine.editor.debugger:eval-error)
(pine.err:mount)

(let ((prev pine.core.actor:*agent-debug-hook*))
  (setf pine.core.actor:*agent-debug-hook*
        (lambda (msg)
          (when prev
            (pine.err:attempt (lambda () (funcall prev msg)) "agent debug relay"))
          (pine.err:attempt (lambda () (pine.editor.debugger:agent-debug-surface msg))
                             "agent debug surface"))))
