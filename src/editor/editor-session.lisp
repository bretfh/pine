(in-package #:pine.editor)

(defstruct sess
  client win aclient sink
  (inbox nil)
  (lock (bordeaux-threads:make-lock))
  (cvar (bordeaux-threads:make-condition-variable))
  (stop nil) thread)

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
          s)))))

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
                  c :sink (lambda (rows crow ccol)
                            (pine.attach:push-to-app c :frame
                                                     :rows rows :crow crow :ccol ccol))))
    :on-input  (lambda (c msg) (session-input c msg))))
