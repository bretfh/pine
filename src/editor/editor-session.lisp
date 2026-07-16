(in-package #:pine.editor)

(defstruct sess
  client win aclient sink
  (rows nil) (crow 0) (ccol 0)               ; latest rendered frame, for the editor surface
  (inbox nil)
  (lock (bordeaux-threads:make-lock))
  (cvar (bordeaux-threads:make-condition-variable))
  (stop nil) thread pump)

;;;; The editor as a surface. The renderer lays the current buffer out into rows;
;;;; the `editor' surface declared in init.lisp --
;;;; (defsurface editor (:as :toplevel) (column (window (current)) (modeline) (echo)))
;;;; -- reads those rows through the `window'/`modeline'/`echo' nodes. So the
;;;; editor is one surface tree over the one wire, like everything else.

(defvar *editor-session* nil
  "The editor session whose rows the editor-surface windows read, bound while
that surface's tree is built.")

(defun editor-current ()
  "The current buffer of the editor session in scope."
  (let ((s *editor-session*))
    (and s (sess-client s) (pine.client:current-buffer (sess-client s)))))

(defun editor-font-px ()
  "The editor's cell font size -- the one theme metric both the daemon's window
nodes and the editor frontend derive their cell grid from, so a buffer laid out at
N cols x rows lands exactly in the frontend's cells."
  (pine.buffer:metric :font-px 15))

(defun editor-opacity ()
  "The editor background alpha from the :editor-opacity ref (1.0 = opaque)."
  (let ((r (pine.ref:find-ref :editor-opacity)))
    (if r (pine.ref:deref r) 1.0)))

(defun editor-window-node (buffer)
  "An Emacs window: a pane rendering BUFFER, from the session's latest rows (all
frame rows but the mode line and echo line), carrying the point. One window per
session today, so BUFFER selects nothing yet."
  (declare (ignore buffer))
  (let* ((s *editor-session*) (rows (and s (sess-rows s))) (n (length rows)))
    (pine.layout:window (if (> n 2) (subseq rows 0 (- n 2)) rows)
                        :crow (if s (sess-crow s) -1)
                        :ccol (if s (sess-ccol s) -1)
                        :font-px (editor-font-px) :opacity (editor-opacity) :expand 1)))

(defun editor-terminal-node (buffer) (editor-window-node buffer))

(defun editor-modeline-node ()
  (let* ((s *editor-session*) (rows (and s (sess-rows s))) (n (length rows)))
    (pine.layout:window (when (>= n 2) (list (nth (- n 2) rows)))
                        :font-px (editor-font-px) :opacity (editor-opacity))))

(defun editor-echo-node ()
  (let* ((s *editor-session*) (rows (and s (sess-rows s))) (n (length rows)))
    (pine.layout:window (when (>= n 1) (list (nth (1- n) rows)))
                        :font-px (editor-font-px) :opacity (editor-opacity))))

(defun push-editor-surface (aclient s)
  "Build the `editor' surface from S's latest rows and push it as a widget tree."
  (let ((builder (gethash "editor" (symbol-value (find-symbol "*SURFACES*" :pine.desktop)))))
    (when builder
      (let* ((*editor-session* s)
             (data (pine.layout:node->wire (funcall builder nil))))
        (pine.attach:push-to-app aclient :widgets :surface "editor"
                                 :tree data :as :toplevel)))))

(defun editor-frame (aclient)
  "Renderer trigger for an editor session: read the freshly rendered window
(render-window), store its rows, and push the editor surface as a widget tree."
  (let ((s (pine.attach:attached-client-session aclient)))
    (when (sess-p s)
      (multiple-value-bind (rows crow ccol) (pine.render:render-window (sess-client s))
        (setf (sess-rows s) rows (sess-crow s) crow (sess-ccol s) ccol))
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
      (let* ((buf (pine.buffer:make-buffer "scratch"))
             (w (pine.buffer:make-window buf "scratch"
                                         :row 0 :col 0 :width 80 :height 29 :focused t)))
        (setf (pine.client:windows client) (list w)
              (pine.client:focused-window client) w
              (pine.client:current-buffer client) buf)
        (pine.mode:set-buffer-mode buf :lisp-mode)
        (ignore-errors (pine.buffer:tell buf :set-local :key :package :value :pine-user))
        (let ((f (pine.client:frame client)))
          (setf (pine.buffer:frame-cols f) 80 (pine.buffer:frame-rows f) 29)
          (pine.render:relayout)
          (pine.buffer:ensure-frame-cells f))
        (pine.render:subscribe-to-buffer buf)
        (let ((s (make-sess :client client :win w :aclient aclient :sink sink)))
          (when aclient (setf (pine.attach:attached-client-session aclient) s))
          (setf (sess-thread s)
                (bordeaux-threads:make-thread (lambda () (session-loop s))
                                              :name "pine-editor-input"))
          (start-term-pump s)
          s)))))

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
       (destructuring-bind (&key cols rows) (rest msg)
         (sento.actor:tell (pine.client:renderer client)
                           (list :resize :cols cols :rows rows)))))))

(defun session-loop (s)
  (let ((pine.client:*client* (sess-client s)))
    (loop until (sess-stop s) do
      (let (msgs)
        (bordeaux-threads:with-lock-held ((sess-lock s))
          (loop until (or (sess-inbox s) (sess-stop s))
                do (bordeaux-threads:condition-wait (sess-cvar s) (sess-lock s)))
          (setf msgs (sess-inbox s) (sess-inbox s) nil))
        (dolist (m msgs) (ignore-errors (apply-input s m)))))))

(defun install-editor-sessions ()
  (ignore-errors (install-variables))
  (setf pine.command:*minibuffer-handler* #'minibuffer-dispatch
        pine.command:*terminal-handler*   #'pine.term:terminal-dispatch
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
