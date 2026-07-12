(in-package #:pine.editor)

;;;; The daemon-side editor session. When an :editor app attaches, the daemon
;;;; owns the session: a pine client with a scratch buffer and a window. Input
;;;; from the app (keys, resize) dispatches here. Frames are pushed to the app
;;;; event-driven: a pusher actor subscribed to the buffer renders the window to
;;;; a frame (a flat cell grid, plain data) on each snapshot and pushes it. No
;;;; blocking ask on any shared thread -- a buffer edit notifies, the pusher
;;;; renders and tells the app. The app is a thin frame renderer + input source.

(defun render-frame (aclient cli w)
  "Render window W to CLI's frame and push it to the app. Non-blocking."
  (let ((pine.client:*client* cli))
    (pine.buffer:ensure-point-visible w)
    (pine.buffer:ensure-col-visible w)
    (setf (pine.buffer:win-display w) (pine.buffer:window-display-lines w))
    (pine.render:render-buffer-to-frame w)
    (let ((f (pine.client:frame cli)))
      (pine.attach:push-to-app aclient
        :frame
        :cols (pine.buffer:frame-cols f) :rows (pine.buffer:frame-rows f)
        :cursor-row (pine.buffer:frame-cursor-row f)
        :cursor-col (pine.buffer:frame-cursor-col f)
        :count (pine.buffer:frame-cell-count f)
        :cells (subseq (pine.buffer:frame-cells f) 0 (pine.buffer:frame-cell-count f))))))

(defun make-editor-session (aclient)
  "Set up a daemon-side editor session for a newly attached :editor app: a client
with a scratch buffer, a window, a renderer (ts/highlights), and a pusher that
renders + pushes a frame on every buffer snapshot. Stored on the attached-client."
  (let* ((server pine.server:*server*)
         (cli (pine.client:start-client server)))
    (pine.render:start-renderer cli)
    (let ((pine.client:*client* cli))
      (let* ((buf (pine.buffer:make-buffer "scratch"))
             (w (pine.buffer:make-window buf "scratch"
                                         :row 0 :col 0 :width 80 :height 29 :focused t)))
        (setf (pine.client:windows cli) (list w)
              (pine.client:focused-window cli) w
              (pine.client:current-buffer cli) buf)
        (pine.mode:set-buffer-mode buf :text-mode)
        (let ((f (pine.client:frame cli)))
          (setf (pine.buffer:frame-cols f) 80 (pine.buffer:frame-rows f) 29)
          (pine.render:relayout)
          (pine.buffer:ensure-frame-cells f))
        (pine.render:subscribe-to-buffer buf)
        (let ((pusher
                (sento.actor-context:actor-of (pine.server:actor-system server)
                  :name (format nil "pusher-~a" (pine.attach:attached-client-id aclient))
                  :receive
                  (lambda (msg)
                    (when (eq (first msg) :snapshot)
                      (destructuring-bind (&key snapshot &allow-other-keys) (rest msg)
                        (let ((pine.client:*client* cli))
                          (when snapshot (setf (pine.buffer:snap w) snapshot))
                          (ignore-errors (render-frame aclient cli w)))))
                    nil))))
          (sento.actor:tell buf (list :subscribe :renderer pusher)))))
    (setf (pine.attach:attached-client-session aclient) cli)
    cli))

(defun session-input (aclient msg)
  "Apply one input message from the app. Keys dispatch (the edit notifies the
pusher, which pushes a frame); resize relays out and pushes directly."
  (let ((cli (pine.attach:attached-client-session aclient)))
    (when cli
      (let ((pine.client:*client* cli))
        (case (first msg)
          (:key
           (destructuring-bind (&key text &allow-other-keys) (rest msg)
             (when text (pine.command:dispatch cli (pine.key:make-key text)))))
          (:resize
           (destructuring-bind (&key cols rows) (rest msg)
             (let ((f (pine.client:frame cli)))
               (setf (pine.buffer:frame-cols f) cols (pine.buffer:frame-rows f) rows)
               (pine.render:relayout)
               (pine.buffer:ensure-frame-cells f))
             (render-frame aclient cli (pine.client:focused-window cli)))))))))

(defun install-editor-sessions ()
  "Wire the daemon to host an editor session per attaching :editor app."
  (pine.attach:register-app-kind :editor
    :on-attach (lambda (c) (make-editor-session c))
    :on-input  (lambda (c msg) (session-input c msg))))
