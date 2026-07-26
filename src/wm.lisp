(defpackage #:pine.wm
  (:use #:cl)
  (:export #:wm-keymap #:binding-table #:push-bindings #:run-binding
           #:attached-p #:leaves #:focused-leaf
           #:spawn #:close-window #:focus-step #:split #:exit-session)
  (:documentation "Window management policy: the keymap whose chords are
registered with the compositor, the commands they run, and the actions sent
to the wm frontend. design/wm.org is the contract."))

(in-package #:pine.wm)

;;;; Window management policy, daemon side. The compositor holds the windows
;;;; and the backing (pine.wayland) speaks the protocol, but every decision is
;;;; made here: which chords exist, what they run, and where each window
;;;; goes. The frontend answers river's sequences from what it was last told
;;;; and never decides a rect.
;;;;
;;;; The arrangement is a live tree, the way the editor surface is: os-window
;;;; leaves under column and row with rule dividers and expand weights, in
;;;; the same node language, mutated in place by the split and delete
;;;; commands and arranged by the same engine. A window is a leaf in that
;;;; tree, not an entry in a list the tree is regenerated from.
;;;;
;;;; Bindings are an ordinary pine keymap. The frontend registers exactly the
;;;; chords this keymap holds with the compositor, so `define-key' on it is
;;;; the whole configuration story: no second list, no separate syntax.

(defstruct (os-window (:constructor %make-os-window) (:copier nil))
  "A window the compositor manages. ID is the protocol's stable identifier,
which names the window everywhere off this image."
  id title app-id)

(defstruct (session (:constructor %make-session) (:copier nil))
  "The window manager session: the attached frontend, a pine client so its
commands run in the same context every command does, and the live tree with
its focused leaf."
  app client tree focus (output nil) (split :row))

(defvar *keymap* (pine.editor.keymap:make-keymap :name :wm)
  "The window manager's keymap. Chords here are registered with the
compositor, which delivers them to the window manager rather than to the
focused window. It has no parent: there is no buffer and no mode to fall back
through.")

(defun wm-keymap () *keymap*)

(defvar *session* nil
  "The running window manager session, or nil when none is attached.")

(defun attached-p () (and *session* t))

(defun %tell-frontend (&rest message)
  "Send MESSAGE to the wm frontend. A message with nowhere to go is reported:
dropping window state silently is how geometry goes missing."
  (cond
    (*session* (apply #'pine.core.attach:push-to-app (session-app *session*) message))
    (t (format *error-output* "pine wm: no frontend attached, dropped ~s~%"
               (first message))
       (finish-output *error-output*))))

;;;; The tree.

(defun leaves (&optional (session *session*))
  "The os-window leaves of SESSION's tree, in tree order."
  (let ((tree (and session (session-tree session)))
        (acc nil))
    (when tree
      (labels ((walk (n)
                 (when (and (typep n 'pine.ui.node:window-node)
                            (eq (pine.ui.node:window-kind n) :os-window))
                   (push n acc))
                 (dolist (c (pine.ui.layout:nodes-of n)) (walk c))))
        (walk tree)))
    (nreverse acc)))

(defun leaf-window (leaf) (pine.ui.node:window-of leaf))
(defun leaf-id (leaf) (os-window-id (leaf-window leaf)))

(defun find-leaf (id &optional (session *session*))
  (find id (leaves session) :key #'leaf-id :test #'equal))

(defun focused-leaf (&optional (session *session*))
  (or (session-focus session) (first (leaves session))))

(defun %frame ()
  "The space a window's chrome occupies, as a layout margin.

The compositor draws borders outside the window's content, so the arrangement
has to leave room for them: without it, neighbouring windows sit edge to edge
and each one's border is drawn over the other's content."
  (let ((border (pine.ui.face:metric :border 2)))
    (list border border border border)))

(defun %leaf (window)
  "A fresh os-window leaf for WINDOW, holding room for its chrome."
  (pine.ui.build:window nil :of window :kind :os-window :expand 1
                          :margin (%frame)))

(defun %divider (orient)
  (pine.ui.build:rule :vertical (eq orient :row) :face :border-inactive))

(defun add-window (id title app-id)
  "Put a newly mapped window into the tree, beside the focused one."
  (let* ((session *session*)
         (window (%make-os-window :id id :title title :app-id app-id))
         (leaf (%leaf window))
         (focus (focused-leaf session)))
    (cond
      ((null (session-tree session))
       (setf (session-tree session) leaf))
      (t
       (let* ((orient (session-split session))
              (root (pine.ui.layout:split-node
                     (session-tree session) focus leaf orient
                     :divider (%divider orient))))
         (cond (root (setf (session-tree session) root))
               (t (format *error-output* "pine wm: cannot place window ~a~%" id)
                  (return-from add-window))))))
    (setf (session-focus session) leaf)
    (push-arrangement)))

(defun forget-window (id)
  "Drop a window that the compositor closed."
  (let* ((session *session*)
         (leaf (find-leaf id session)))
    (when leaf
      (let ((root (pine.ui.layout:remove-node (session-tree session) leaf)))
        (setf (session-tree session)
              (cond (root root)
                    ((eq (session-tree session) leaf) nil)
                    (t (session-tree session)))))
      (when (eq (session-focus session) leaf)
        (setf (session-focus session) (first (leaves session))))
      (push-arrangement))))

(defun arrange ()
  "Arrange the tree over the area the frontend reports and return the leaf rects
as (ID X Y WIDTH HEIGHT). Pixels are cells one pixel wide, so the engine's own
measure and arrange do the work unchanged. The area is in global coordinates
and need not start at the origin: layer surfaces such as a bar take their strip
of the output first."
  (let* ((session *session*)
         (tree (and session (session-tree session)))
         (output (and session (session-output session))))
    (when (and tree output)
      (destructuring-bind (x y width height) output
        (let ((pine.ui.layout:*text-size*
                (lambda (text font-px)
                  (declare (ignore font-px))
                  (values (length text) 1))))
          (pine.ui.layout:measure tree width height)
          (pine.ui.layout:arrange tree x y width height))
        (mapcar (lambda (leaf)
                  (list (leaf-id leaf)
                        (pine.ui.node:start-col leaf)
                        (pine.ui.node:start-line leaf)
                        (- (pine.ui.node:end-col leaf)
                           (pine.ui.node:start-col leaf))
                        (1+ (- (pine.ui.node:end-line leaf)
                               (pine.ui.node:start-line leaf)))))
                (leaves session))))))

(defun %border ()
  "The border the compositor should draw: its width, and the colour for the
focused window and for the rest.

The theme lives here, with the configuration that loads it, so the frontend is
told colours rather than resolving faces in an image that has no theme."
  (list :width (pine.ui.face:metric :border 2)
        :active (pine.ui.face:face-fg :border-active)
        :inactive (pine.ui.face:face-fg :border-inactive)))

(defun push-arrangement ()
  "Send the arranged rects, the focused window, and the border to the frontend."
  (let ((rects (arrange))
        (focus (focused-leaf)))
    (when rects
      (%tell-frontend :arrangement :rects rects
                                   :focus (and focus (leaf-id focus))
                                   :border (%border)))))

;;;; The actions only the frontend can take: it holds the protocol objects
;;;; and the compositor's session environment.

(defun spawn (command)
  "Launch COMMAND as a program in the compositor's session."
  (%tell-frontend :wm :action :spawn :command command))

(defun close-window ()
  "Ask the focused window to close."
  (let ((leaf (focused-leaf)))
    (when leaf
      (%tell-frontend :wm :action :close :id (leaf-id leaf)))))

(defun exit-session ()
  "End the Wayland session: the compositor and every client in it exit."
  (%tell-frontend :wm :action :exit))

;;;; Commands. Ordinary pine commands, so they are M-x reachable and can be
;;;; rebound or redefined live like anything else.

(defun focus-step (step)
  (let* ((all (leaves))
         (n (length all)))
    (when (plusp n)
      (let ((i (or (position (focused-leaf) all :test #'eq) 0)))
        (setf (session-focus *session*) (nth (mod (+ i step) n) all))
        (push-arrangement)))))

(defun split (orient)
  "Split along ORIENT: the next window to appear joins the focused one that
way. A window manager cannot show the same window twice, so a split states
where the next one lands rather than dividing the current one immediately."
  (setf (session-split *session*) orient))

(pine.state.var:defonce :wm-terminal :default "foot"
  :documentation "The program wm-terminal launches.")

(pine.editor.command:define-command wm-terminal ()
  "Launch the terminal named by the :wm-terminal variable."
  (spawn (pine.state.var:var :wm-terminal)))

(pine.editor.command:define-command wm-close-window ()
  "Ask the focused window to close."
  (close-window))

(pine.editor.command:define-command wm-focus-next ()
  "Focus the next window."
  (focus-step 1))

(pine.editor.command:define-command wm-focus-prev ()
  "Focus the previous window."
  (focus-step -1))

(pine.editor.command:define-command wm-split-below ()
  "The next window opens below the focused one."
  (split :column))

(pine.editor.command:define-command wm-split-beside ()
  "The next window opens beside the focused one."
  (split :row))

(pine.editor.command:define-command wm-exit ()
  "End the Wayland session."
  (exit-session))

(pine.editor.keymap:define-keys *keymap*
  "s-Return"  "wm-terminal"
  "s-q"       "wm-close-window"
  "s-j"       "wm-focus-next"
  "s-k"       "wm-focus-prev"
  "s-2"       "wm-split-below"
  "s-3"       "wm-split-beside"
  "s-S-e"     "wm-exit")

;;;; The binding table crossing the wire: (CHORD-STRING . COMMAND-NAME) pairs,
;;;; which is exactly what keymap-bindings already produces. The frontend
;;;; turns each chord into a keysym plus modifiers and registers it.

(defun binding-table ()
  (pine.editor.keymap:keymap-bindings (wm-keymap)))

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
       (let ((pine.editor.frame:*client* (session-client *session*)))
         (pine.editor.command:call-command command))))))

;;;; The session: one attached frontend, told the bindings on arrival, and
;;;; kept current with what the compositor reports.

(defclass wm-app (pine.core.attach:app)
  ()
  (:default-initargs :kind :wm)
  (:documentation "Window management: the compositor's windows as a layout
tree, and the chords that move them."))

(defmethod pine.core.attach:attached ((app wm-app) client)
  (setf *session* (%make-session
                   :app client
                   :client (pine.editor.frame:start-client pine.core.server:*server*)))
  (push-bindings))

(defmethod pine.core.attach:detached ((app wm-app) client)
  "Forget the session. Its windows belong to the compositor and outlive the
frontend; the next one to attach reports them again."
  (declare (ignore client))
  (setf *session* nil))

(defmethod pine.core.attach:received ((app wm-app) client message)
  (declare (ignore client))
  (case (first message)
    (:binding
     (destructuring-bind (&key keys) (rest message)
       (run-binding keys)))
    (:output
     (destructuring-bind (&key x y width height) (rest message)
       (setf (session-output *session*)
             (list (or x 0) (or y 0) width height))
       (push-arrangement)))
    (:window-added
     (destructuring-bind (&key id title app-id) (rest message)
       (add-window id title app-id)))
    (:window-closed
     (destructuring-bind (&key id) (rest message)
       (forget-window id)))
    (:window-focused
     (destructuring-bind (&key id) (rest message)
       (let ((leaf (find-leaf id)))
         (when leaf
           (setf (session-focus *session*) leaf)
           (push-arrangement)))))
    (t nil)))

(pine.core.attach:register-app (make-instance 'wm-app))

