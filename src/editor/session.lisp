(defpackage #:pine.editor.session
  (:use #:cl)
  (:export
   #:sess #:make-sess #:sess-p
   #:sess-client #:sess-aclient #:sess-sink #:sess-inbox #:sess-lock
   #:sess-cvar #:sess-stop #:sess-thread #:sess-pump
   #:sess-sent-wire #:sess-generation #:sess-signal
   #:editor-font-px #:editor-frame
   #:editor-tree #:make-editor-session #:reseed-editor-sessions
   #:watch-editor-surface #:watch-arrangement
   #:push-editor-surface #:start-term-pump #:stop-session
   #:session-input #:session-feed #:session-loop #:apply-input))

(in-package #:pine.editor.session)
(named-readtables:in-readtable pine.path:syntax)

;;;; The editor as a live tree. The `editor' surface's node tree is built from
;;;; /win, arranged through the layout engine each frame, its view leaves
;;;; rendered, and shipped to the attached frontend by node->wire like any other
;;;; surface. A split writes the path; this is what paints what the path says.

(defstruct sess
  client aclient sink
  (inbox nil)
  (lock (bordeaux-threads:make-lock))
  (cvar (bordeaux-threads:make-condition-variable))
  (stop nil) thread pump
  ;; the frame this session last sent, so the next one can carry only the lines
  ;; that moved, and the generation the frontend must be holding for a patch to
  ;; mean anything
  (sent-wire nil) (generation 0))

(defparameter +repaint-pause+ 0.033
  "Seconds between terminal repaints, however much output arrived.")

(defun editor-font-px ()
  "The editor's cell font size: the one theme metric both the daemon's window
nodes and the editor frontend derive their cell grid from, so a buffer laid out
at N cols x rows lands exactly in the frontend's cells."
  (pine.ui.face:metric :font-px 15))

(defun %default-editor-tree ()
  "The editor surface used when a config declares none: the arrangement, the
echo line, the mode line."
  (pine.ui.build:column :align :stretch
    (pine.pane:window)
    (pine.pane:echo)
    (pine.pane:modeline)))

(defun editor-tree (client)
  "CLIENT's live editor tree: the registered `editor' surface builder, else the
engine default. The arrangement inside it comes from /win, which is where it
survives a restart, so there is nothing to restore."
  (let ((pine.editor.frame:*client* client))
    (or (pine.desktop:surface-tree "editor")
        (%default-editor-tree))))

(defun %seed-editor-tree (client)
  "Build CLIENT's tree, and make the focused pane's buffer the current one."
  (let ((pine.editor.frame:*client* client))
    (setf (pine.editor.frame:tree client) (editor-tree client))
    (let ((focused (pine.win:focused)))
      (when focused
        (setf (pine.editor.frame:current-buffer)
              (pine.pane:subject focused))))
    (pine.editor.frame:tree client)))

(defun watch-arrangement (client)
  "Build CLIENT's tree again whenever /win moves. The arrangement is a path, so
what follows it is a watch."
  (pine.ns:watch /win
                 (lambda (value)
                   (declare (ignore value))
                   (%seed-editor-tree client)
                   (pine.editor.render:relayout)
                   nil)
                 :as (list :arrangement client)))

(defun reseed-editor-sessions ()
  "Re-seed every attached editor session's live tree from the `editor' builder
as it now stands, and repaint."
  (dolist (c pine.core.attach:*clients*)
    (when (eq (pine.core.attach:attached-client-kind c) :editor)
      (let ((s (pine.core.attach:attached-client-session c)))
        (when (sess-p s)
          (%seed-editor-tree (sess-client s))
          (let ((pine.editor.frame:*client* (sess-client s)))
            (pine.editor.render:relayout))
          (sento.actor:tell (pine.editor.frame:renderer (sess-client s))
                            '(:force-render)))))))

(defun watch-editor-surface ()
  "Follow /surface/editor. The tree an editor shows is a path, so a config
re-loaded over it is a write and the sessions come from that."
  (pine.ns:watch /surface/editor
                 (lambda (value)
                   (declare (ignore value))
                   (reseed-editor-sessions)
                   (fset:empty-map))
                 :as :editor-surface))

(defun push-editor-surface (aclient s)
  "Refresh the session's live tree and push it as the `editor' surface. Sends
the lines that changed when the frame differs from the last one only in its rows
and cursor, and the whole tree otherwise."
  (let* ((client (sess-client s))
         (tree (pine.editor.render:refresh-editor-tree client)))
    (when tree
      (let* ((wire (pine.ui.wire:node->wire tree))
             (patch (pine.ui.wire:rows-patch (sess-sent-wire s) wire)))
        (unless (sess-sent-wire s)
          (pine.log:note "editor frame 1: ~d pane~:p, ~d row~:p"
                         (length (pine.ui.wire:wire-views wire))
                         (loop :for w :in (pine.ui.wire:wire-views wire)
                               :sum (length (getf (second w) :rows)))))
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

(defun session-input (aclient msg)
  (let ((s (pine.core.attach:attached-client-session aclient)))
    (when (sess-p s) (sess-signal s msg))))

(defun session-feed (s msg)
  "Send one input message, a (:key ...) or (:resize ...) plist, to session S."
  (when (sess-p s) (sess-signal s msg)))

(defun apply-input (s msg)
  (let ((client (sess-client s)))
    (case (first msg)
      (:key
       (destructuring-bind (&key key-str text ctrl meta shift super &allow-other-keys)
           (rest msg)
         (let ((str (or key-str text)))
           (when str
             (pine.key:dispatch
              (pine.key:make-key str :ctrl ctrl :meta meta :shift shift
                                     :super super))
             (sento.actor:tell (pine.editor.frame:renderer client) '(:force-render))))))
      (:resize
       (sento.actor:tell (pine.editor.frame:renderer client) msg))
      (:refresh
       ;; the frontend is holding a frame this session cannot patch onto; forget
       ;; what was sent so the next push carries the whole tree
       (setf (sess-sent-wire s) nil)
       (sento.actor:tell (pine.editor.frame:renderer client) '(:force-render)))
      ;; an input message this daemon does not know is two images disagreeing
      ;; about the protocol, which is worth hearing about the first time
      (t (error "This daemon has no handler for the input message ~s." msg)))))

(defun session-loop (s)
  (let ((pine.editor.frame:*client* (sess-client s)))
    (loop until (sess-stop s) do
      (let (msgs)
        (bordeaux-threads:with-lock-held ((sess-lock s))
          (loop until (or (sess-inbox s) (sess-stop s))
                do (bordeaux-threads:condition-wait (sess-cvar s) (sess-lock s)))
          (setf msgs (sess-inbox s) (sess-inbox s) nil))
        ;; dispatch already surfaces command errors through the debugger or the
        ;; echo line; anything that still escapes goes to the same surface
        (dolist (m msgs)
          (handler-case (apply-input s m)
            (error (c) (pine.key:command-error c))))))))

(defun start-term-pump (s)
  "Ask the renderer to drain pending terminal output and repaint. Woken by the
pty readers, then paced: one repaint per thirtieth of a second however much
output arrived, and nothing at all while no terminal is producing any."
  (let* ((client (sess-client s))
         (wake (pine.editor.frame:terminal-wake client)))
    (setf (sess-pump s)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop :until (sess-stop s)
                   :do (sb-thread:wait-on-semaphore wake)
                       (loop :while (sb-thread:try-semaphore wake))
                       (unless (sess-stop s)
                         (sleep +repaint-pause+)
                         (sento.actor:tell (pine.editor.frame:renderer client)
                                           '(:term-tick)))))
           :name "pine-term-pump"))))

(defun stop-session (s)
  "End S's threads. Both wait rather than poll, so both are signalled."
  (setf (sess-stop s) t)
  (bordeaux-threads:with-lock-held ((sess-lock s))
    (bordeaux-threads:condition-notify (sess-cvar s)))
  (sb-thread:signal-semaphore (pine.editor.frame:terminal-wake (sess-client s))))

(defun make-editor-session (aclient &key sink (server pine.core.server:*server*))
  (watch-editor-surface)
  ;; an editor is here, so something is in a position to decide a fault: this is
  ;; what makes one park holding its restarts rather than take its abort
  (pine.editor.debugger:install)
  (setf pine.key:*terminal-handler* #'pine.term:terminal-dispatch)
  (let ((client (pine.editor.frame:start-client server)))
    (pine.editor.render:start-renderer client)
    (setf (pine.editor.frame:paint-sink client) sink)
    (let ((pine.editor.frame:*client* client))
      (let ((buf (pine.editor.frame:make-buffer "scratch")))
        (setf (pine.editor.frame:current-buffer) buf)
        (pine.editor.frame:set-buffer-mode buf :lisp)
        (ignore-errors (pine.buf:put buf :package :pine-user)))
      (%seed-editor-tree client)
      (watch-arrangement client)
      (setf (pine.editor.frame:cols client) 80
            (pine.editor.frame:rows client) 29)
      (pine.editor.render:relayout)
      (pine.echo:ensure)
      (let ((s (make-sess :client client :aclient aclient :sink sink)))
        (when aclient
          (setf (pine.core.attach:attached-client-session aclient) s)
          (let ((styles (pine.ui.css:styles)))
            (when styles
              (pine.core.attach:push-to-app aclient :style :styles styles))))
        (setf (sess-thread s)
              (bordeaux-threads:make-thread (lambda () (session-loop s))
                                            :name "pine-editor-input"))
        (start-term-pump s)
        s))))

(pine.core.attach:app :editor
  {:doc "the editor window: buffers, windows, the mode line"
   :attached (lambda (client)
               (make-editor-session
                client
                :sink (lambda (&rest _)
                        (declare (ignore _))
                        (editor-frame client))))
   :received (lambda (client message) (session-input client message))
   :detached (lambda (client)
               (let ((s (pine.core.attach:attached-client-session client)))
                 (when (sess-p s) (stop-session s))))})
