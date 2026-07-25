(in-package #:pine.wm)

;;;; Window management policy, daemon side. The compositor holds the windows
;;;; and the frontend (pine.wl-wm) speaks the protocol, but every decision is
;;;; made here: which chords exist, what they run, and what the frontend is
;;;; then told to do. The split is the one design/wm.org specifies -- the
;;;; frontend answers river's sequences promptly from mirrored state, and
;;;; policy arrives from the daemon as asynchronous pushes.
;;;;
;;;; Bindings are an ordinary pine keymap. The frontend registers exactly the
;;;; chords this keymap holds with the compositor, so `define-key' on it is
;;;; the whole configuration story: no second list, no separate syntax.

(defvar *keymap* nil
  "The window manager's keymap. Chords here are registered with the
compositor, which delivers them to the window manager rather than to the
focused window.")

(defun wm-keymap ()
  "The window manager keymap, created on first use."
  (or *keymap*
      (setf *keymap* (pine.keymap:make-keymap :name :wm))))

(defvar *client* nil
  "The attached wm frontend (a pine.attach:attached-client), or nil when no
window manager is running. Actions are no-ops without one.")

(defvar *session-client* nil
  "A pine client for the window manager, so its commands run in the same
context every other command does. It carries no buffer and no renderer: a
window manager edits nothing.")

(defun attached-p () (and *client* t))

(defun %tell-frontend (&rest message)
  "Send MESSAGE to the wm frontend. Silent only when nothing is attached --
every other failure belongs to the caller."
  (when *client*
    (apply #'pine.attach:push-to-app *client* message)))

;;;; The actions a command can take. Each is applied by the frontend inside a
;;;; manage sequence, which is where the protocol requires these requests.

(defun spawn (command)
  "Launch COMMAND as a program in the compositor's session. The frontend runs
it: that process has the session environment (WAYLAND_DISPLAY and the rest)."
  (%tell-frontend :wm :action :spawn :command command))

(defun close-window ()
  "Ask the focused window to close."
  (%tell-frontend :wm :action :close))

(defun focus-next ()
  "Focus the next window in tree order."
  (%tell-frontend :wm :action :focus :direction :next))

(defun focus-prev ()
  "Focus the previous window in tree order."
  (%tell-frontend :wm :action :focus :direction :prev))

(defun exit-session ()
  "End the Wayland session: the compositor and every client in it exit."
  (%tell-frontend :wm :action :exit))

;;;; Commands. Ordinary pine commands, so they are M-x reachable and can be
;;;; rebound or redefined live like anything else.

(defun install-commands ()
  (pine.var:defonce :wm-terminal :default "foot"
    :documentation "The program wm-terminal launches.")
  (pine.command:define-command wm-terminal ()
    "Launch the terminal named by the :wm-terminal variable."
    (spawn (pine.var:var :wm-terminal)))
  (pine.command:define-command wm-close-window ()
    "Ask the focused window to close."
    (close-window))
  (pine.command:define-command wm-focus-next ()
    "Focus the next window."
    (focus-next))
  (pine.command:define-command wm-focus-prev ()
    "Focus the previous window."
    (focus-prev))
  (pine.command:define-command wm-exit ()
    "End the Wayland session."
    (exit-session)))

(defun install-bindings ()
  "The default window manager chords. init.lisp rebinds them the same way it
rebinds editor keys: (define-key (keymap :wm) (kbd \"s-Return\") 'wm-terminal)."
  (let ((m (wm-keymap)))
    (pine.keymap:define-key m (pine.key:parse-key "s-Return") "wm-terminal")
    (pine.keymap:define-key m (pine.key:parse-key "s-q") "wm-close-window")
    (pine.keymap:define-key m (pine.key:parse-key "s-j") "wm-focus-next")
    (pine.keymap:define-key m (pine.key:parse-key "s-k") "wm-focus-prev")
    (pine.keymap:define-key m (pine.key:parse-key "s-S-e") "wm-exit")))

;;;; The binding table crossing the wire: (CHORD-STRING . COMMAND-NAME) pairs,
;;;; which is exactly what keymap-bindings already produces. The frontend
;;;; turns each chord into a keysym plus a modifier mask and registers it.

(defun binding-table ()
  (pine.keymap:keymap-bindings (wm-keymap)))

(defun push-bindings ()
  "Send the current binding table to the frontend, which re-registers."
  (%tell-frontend :bindings :table (binding-table)))

(defun run-binding (chord)
  "Run the command CHORD is bound to. Chord lookup is direct rather than
through the mode stack: a window manager has no current buffer, and these
chords were registered with the compositor from this keymap alone."
  (let ((command (cdr (assoc chord (binding-table) :test #'string=))))
    (cond
      ((null command)
       (format *error-output* "pine wm: no command bound to ~a~%" chord))
      (t
       (let ((pine.client:*client* *session-client*))
         (pine.command:call-command command))))))

;;;; The session: one attached frontend, told the bindings on arrival.

(defun install-wm-sessions ()
  (install-commands)
  (install-bindings)
  (pine.attach:register-app-kind :wm
    :on-attach (lambda (client)
                 (setf *client* client
                       *session-client* (pine.client:start-client
                                         pine.server:*server*))
                 (push-bindings))
    :on-input (lambda (client message)
                (declare (ignore client))
                (case (first message)
                  (:binding
                   (destructuring-bind (&key keys) (rest message)
                     (run-binding keys)))
                  (t nil)))))
