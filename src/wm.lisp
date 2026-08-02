(defpackage #:pine.wm
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:win #:pine.win))
  (:export #:binding-table #:push-bindings #:run-binding
           #:attached-p #:windows #:focused #:server
           #:close-window #:focus-step #:split #:exit-session)
  (:documentation "Window management policy: the keymap whose chords are
registered with the compositor, the commands they run, and the actions sent to
the wm frontend."))

(in-package #:pine.wm)
(named-readtables:in-readtable pine.path:syntax)

;;;; Window management policy, daemon side. The compositor holds the windows
;;;; and the backing speaks the protocol; every decision is made here.
;;;;
;;;; The arrangement is /wm, the same subtree shape the editor's windows are:
;;;; /wm/0/buf is a window's id, splitting one makes a stack, and the layout
;;;; engine arranges what the paths say. There is no second tree and nothing to
;;;; serialize, so a frontend that reattaches finds the arrangement where it
;;;; left it.
;;;;
;;;; Bindings are an ordinary pine keymap. The frontend registers exactly the
;;;; chords /key/wm holds, so define-key is the whole configuration story.

(defun windows () (win:windows /wm))
(defun focused () (win:focused /wm))

(defun id-of (path) (win:buf-of path))

(defun window-at (id)
  (find id (windows) :key #'id-of :test #'equal))

;;;; The attached frontend, which is this image's, not a space's.

(defun app ()
  (find :wm pine.core.attach:*clients*
        :key #'pine.core.attach:attached-client-kind))

(defun attached-p () (and (app) t))

(defun %tell-frontend (&rest message)
  "Send MESSAGE to the wm frontend. A message with nowhere to go is reported:
dropping window state silently is how geometry goes missing."
  (let ((to (app)))
    (cond (to (apply #'pine.core.attach:push-to-app to message))
          (t (format *error-output* "pine wm: no frontend attached, dropped ~s~%"
                     (first message))
             (finish-output *error-output*)))))

;;;; The tree the compositor is told about, built from the paths.

(defun %frame ()
  "The space a window's chrome occupies, as a layout margin. The compositor
draws borders outside the content, so the arrangement leaves room for them."
  (let ((border (pine.ui.face:metric :border 2)))
    (list border border border border)))

(defun %leaf (path)
  (pine.ui.build:cells nil :of path :as 'pine.ui.node:os-window-view
                            :expand (max 1 (win:weight-of path))
                            :margin (%frame)))

(defun %node (path)
  (if (win:stack-p path)
      (let* ((row (eq :row (win:runs-of path)))
             (parts (mapcar #'%node (win:parts path))))
        (apply (if row #'pine.ui.build:row #'pine.ui.build:column)
               :align :stretch :expand 1
               (loop :for part :in parts
                     :for first := t :then nil
                     :append (if first
                                 (list part)
                                 (list (pine.ui.build:rule :vertical row
                                                           :face :border-inactive)
                                       part)))))
      (%leaf path)))

(defun %tree ()
  (let ((parts (win:parts /wm)))
    (cond ((null parts) nil)
          ((null (rest parts)) (%node (first parts)))
          (t (apply #'pine.ui.build:column :align :stretch :expand 1
                    (mapcar #'%node parts))))))

(defun arrange ()
  "Arrange over the area the frontend reported and answer the leaf rects as
(ID X Y WIDTH HEIGHT). Pixels are cells one pixel wide, so the engine's own
measure and arrange do the work unchanged."
  (let ((tree (%tree))
        (output (ns:read /wm/output)))
    (when (and tree output)
      (let ((x (fset:lookup output :x)) (y (fset:lookup output :y))
            (width (fset:lookup output :width))
            (height (fset:lookup output :height)))
        (let ((pine.ui.layout:*text-size*
                (lambda (text font-px)
                  (declare (ignore font-px))
                  (values (length text) 1))))
          (pine.ui.layout:measure tree width height)
          (pine.ui.layout:arrange tree x y width height))
        (let ((acc nil))
          (labels ((walk (n)
                     (when (typep n 'pine.ui.node:os-window-view)
                       (push (list (id-of (pine.ui.node:window-of n))
                                   (pine.ui.node:start-col n)
                                   (pine.ui.node:start-line n)
                                   (- (pine.ui.node:end-col n)
                                      (pine.ui.node:start-col n))
                                   (1+ (- (pine.ui.node:end-line n)
                                          (pine.ui.node:start-line n))))
                             acc))
                     (dolist (c (pine.ui.layout:nodes-of n)) (walk c))))
            (walk tree))
          (nreverse acc))))))

(defun %border ()
  "The border the compositor should draw. The theme lives here, so the frontend
is told colours rather than resolving faces in an image with no theme."
  (fset:map (:width (pine.ui.face:metric :border 2))
            (:active (pine.ui.face:face-fg :border-active))
            (:inactive (pine.ui.face:face-fg :border-inactive))))

(defun push-arrangement ()
  (let ((rects (arrange))
        (focus (focused)))
    (when rects
      (%tell-frontend :arrangement :rects rects
                                   :focus (and focus (id-of focus))
                                   :border (%border)))))

(defun add-window (id title app-id)
  "Put a newly mapped window into the arrangement, beside the focused one."
  (declare (ignore title app-id))
  (let ((focus (focused)))
    (cond
      ((null focus)
       (win:seed id /wm))
      (t
       (win:split focus (if (eq :row (ns:read /wm/split)) :beside :below) /wm)
       (ns:write (p:child (focused) "buf") id))))
  (push-arrangement))

(defun forget-window (id)
  "Drop the window the compositor closed. The last one leaves nothing: an
editor always keeps a window, a compositor with no windows has none."
  (let ((path (window-at id)))
    (when path
      (if (rest (windows))
          (win:close path /wm)
          (progn (ns:write path nil) (ns:write /wm/focused nil)))
      (push-arrangement))))

;;;; The actions only the frontend can take: it holds the protocol objects and
;;;; the compositor's session environment.

(defun close-window ()
  (let ((path (focused)))
    (when path (%tell-frontend :wm :action :close :id (id-of path)))))

(defun exit-session ()
  (%tell-frontend :wm :action :exit))

(defun focus-step (step)
  (let* ((all (windows))
         (n (length all)))
    (when (plusp n)
      (let ((i (or (position (focused) all :test #'fset:equal?) 0)))
        (win:focus (nth (mod (+ i step) n) all) /wm)
        (push-arrangement)))))

(defun split (orient)
  "Say where the next window lands. A window manager cannot show one window
twice, so a split states that rather than dividing the current one now."
  (ns:write /wm/split orient))

(pine.cmd:defcmd wm-terminal ()
  "Launch the terminal /wm-terminal names. Launching is a write to /sh, which
is the one place a command line is run."
  (ns:write (p:child /sh (or (ns:read /wm-terminal) "foot")) t))

(pine.cmd:defcmd wm-close-window () "Ask the focused window to close."
  (close-window))

(pine.cmd:defcmd wm-focus-next () "Focus the next window." (focus-step 1))

(pine.cmd:defcmd wm-focus-prev () "Focus the previous window." (focus-step -1))

(pine.cmd:defcmd wm-split-below () "The next window opens below." (split :column))

(pine.cmd:defcmd wm-split-beside () "The next window opens beside." (split :row))

(pine.cmd:defcmd wm-exit () "End the Wayland session." (exit-session))

(pine.key:define-keys :wm
  "s-Return"  "wm-terminal"
  "s-q"       "wm-close-window"
  "s-j"       "wm-focus-next"
  "s-k"       "wm-focus-prev"
  "s-2"       "wm-split-below"
  "s-3"       "wm-split-beside"
  "s-S-e"     "wm-exit")

;;;; The binding table crossing the wire: (CHORD . COMMAND) pairs, which is
;;;; what bindings already answers. The frontend turns each chord into a keysym
;;;; plus modifiers and registers it.

(defun binding-table ()
  (mapcar (lambda (entry)
            (cons (car entry)
                  (if (p:pathp (cdr entry))
                      (p:leaf (cdr entry))
                      (princ-to-string (cdr entry)))))
          (pine.key:bindings :wm)))

(defun push-bindings ()
  (%tell-frontend :bindings :table (binding-table)))

(defun run-binding (chord)
  "Run what CHORD is bound to. Looked up directly rather than through the mode
chain: a window manager has no current buffer, and these chords were registered
from this keymap alone."
  (let ((command (pine.key:lookup :wm chord)))
    (if (null command)
        (format *error-output* "pine wm: no command bound to ~a~%" chord)
        (pine.cmd:run command))))

;;;; The session

(defclass wm-app (pine.core.attach:app) ()
  (:default-initargs :kind :wm)
  (:documentation "The compositor's windows as an arrangement, and the chords
that move them."))

(defmethod pine.core.attach:attached ((app wm-app) client)
  (declare (ignore client))
  (push-bindings))

(defmethod pine.core.attach:detached ((app wm-app) client)
  "The windows belong to the compositor and outlive the frontend; the next one
to attach reports them again, and /wm still says how they were arranged."
  (declare (ignore client))
  nil)

(defmethod pine.core.attach:received ((app wm-app) client message)
  (declare (ignore client))
  (case (first message)
    (:binding
     (destructuring-bind (&key keys) (rest message) (run-binding keys)))
    (:output
     (destructuring-bind (&key x y width height) (rest message)
       (ns:write /wm/output (fset:map (:x (or x 0)) (:y (or y 0))
                                      (:width width) (:height height)))
       (push-arrangement)))
    (:window-added
     (destructuring-bind (&key id title app-id) (rest message)
       (add-window id title app-id)))
    (:window-closed
     (destructuring-bind (&key id) (rest message) (forget-window id)))
    (:window-focused
     (destructuring-bind (&key id) (rest message)
       (let ((path (window-at id)))
         (when path (win:focus path /wm) (push-arrangement)))))
    (t nil)))

(defclass server (ns:server) ()
  (:default-initargs :name :wm :serves (list /wm))
  (:documentation "The compositor's arrangement, as paths."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /wm
            (ns:provider
             (/wm/focused
              {:verbs {:split (pine.data:fn (&optional (side :below))
                                (let ((at (focused)))
                                  (when at (win:split at side /wm))))
                       :close (pine.data:fn []
                                (let ((at (focused)))
                                  (when at (win:close at /wm))))
                       :only (pine.data:fn []
                               (let ((at (focused)))
                                 (when at (win:only at /wm))))}
               :doc "the window with the keyboard; [:split] [:close] [:only]"})
             (/wm/?@at
              {:doc "a window's id and weight, or the halves of a stack"}))))

(ns:register (make-instance 'server))
(pine.core.attach:register-app (make-instance 'wm-app))
