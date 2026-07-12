(defpackage #:pine.desktop-session
  (:use #:cl)
  (:export #:install-desktop-sessions #:set-desktop-tree! #:push-tree))

(in-package #:pine.desktop-session)

;;;; The daemon-side desktop session. When a :desktop app attaches, the daemon
;;;; produces the declarative widget tree (from cells) and serializes it to plain
;;;; data with node->wire, registering each handler closure under an id. The app
;;;; rebuilds the tree, lays it out, and paints cairo through the layer-shell;
;;;; when the user interacts, it sends (:widget-action :id N :args ...) back and
;;;; the daemon runs the closure. The tree crosses as data; the closures stay
;;;; here. No pixels or fonts on the daemon.

(defvar *desktop-tree* nil
  "A thunk -> a pine.layout node tree (the declarative desktop). A desktop config
sets this; the daemon serializes what it returns and pushes it to the app.")

(defun set-desktop-tree! (thunk) (setf *desktop-tree* thunk))

(defstruct dsession actions (counter 0))

(defun push-tree (aclient)
  "Build the desktop tree, serialize it (handlers -> ids), and push it to the app."
  (let ((s (pine.attach:attached-client-session aclient)))
    (when (and s *desktop-tree*)
      (clrhash (dsession-actions s))
      (setf (dsession-counter s) 0)
      (let ((data (pine.layout:node->wire
                   (funcall *desktop-tree*)
                   :on-action (lambda (cb)
                                (let ((id (incf (dsession-counter s))))
                                  (setf (gethash id (dsession-actions s)) cb)
                                  id)))))
        (pine.attach:push-to-app aclient :widgets data)))))

(defun make-desktop-session (aclient)
  (setf (pine.attach:attached-client-session aclient)
        (make-dsession :actions (make-hash-table)))
  (push-tree aclient))

(defun desktop-input (aclient msg)
  (let ((s (pine.attach:attached-client-session aclient)))
    (case (first msg)
      (:widget-action
       ;; run the widget handler through pine.eval (a fresh thread), never inline
       ;; on this pool worker -- a handler that blocks (nmcli, a slow shell) or
       ;; errors then cannot stall the daemon pool. One eval path.
       (destructuring-bind (&key id args) (rest msg)
         (let ((cb (and s (gethash id (dsession-actions s)))))
           (when cb
             (pine.eval:evaluate-thunk (lambda () (apply cb args))
                                       :package (find-package :pine-user))))))
      (:refresh (push-tree aclient)))))

(defun install-desktop-sessions ()
  "Wire the daemon to host a desktop session per attaching :desktop app."
  (pine.attach:register-app-kind :desktop
    :on-attach (lambda (c) (make-desktop-session c))
    :on-input  (lambda (c msg) (desktop-input c msg))))
