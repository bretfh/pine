(defpackage #:pine/wayland/wm
  (:use #:cl #:wayflan-client #:pine/wayland/protocol)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:log #:pine/run/log)
                    (#:fault #:pine/run/fault) (#:chords #:pine/wayland/chords)
                    (#:display #:pine/wayland/display))
  (:export #:wm #:bind #:managingp #:apply-layout #:take #:laid #:wake
           #:on-said #:on-render #:on-chord #:wants-chords #:eat-next
           #:windows #:outputs #:focused #:manager
           #:*borders*))
(in-package #:pine/wayland/wm)

(defvar *borders* 2
  "How thick a border is drawn round a window, in pixels. The colours come from
the theme, over the wire, like everything else about how it looks.")

(defparameter +all-edges+ '(:top :bottom :left :right)
  "Every edge, as the protocol spells a bitfield: a list of what is set.")

(defclass window ()
  ((of      :initarg :of      :reader of)
   (node-of :initarg :node    :accessor node-of :initform nil)
   (id      :initarg :id      :reader id)
   (title   :initform ""      :accessor title)
   (app     :initform ""      :accessor app)
   (wide    :initform 0       :accessor wide)
   (tall    :initform 0       :accessor tall)
   (hidden  :initform nil     :accessor hidden)
   (clip    :initform nil     :accessor clip)
   (stack   :initform nil     :accessor stack))
  (:documentation "One window the compositor handed over, and the node that says
where it sits."))

(defclass wm ()
  ((manager :initarg :manager :reader manager)
   (layers  :initarg :layers  :accessor layers  :initform nil)
   (chords  :initarg :chords  :accessor chords  :initform nil)
   (wanted  :initform nil     :accessor wanted)
   (defaultp :initform nil    :accessor defaultp)
   (windows :initform nil     :accessor windows)
   (outputs :initform nil     :accessor outputs)
   (seats   :initform nil     :accessor seats)
   (focused :initform nil     :accessor focused)
   (counter :initform 0       :accessor counter)
   (on-said   :initarg :on-said   :accessor on-said   :initform nil)
   (on-render :initarg :on-render :accessor on-render :initform nil)
   (pending :initform nil     :accessor pending)
   (placedp :initform nil     :accessor placedp)
   (asking  :initform nil     :accessor asking)
   (dirty   :initform t       :accessor dirty))
  (:documentation "Pine as the compositor's window manager. It is told what there
is, says so, and puts each window where the answer says.

LAYERS is the compositor's layer shell support, which on a compositor of this kind
exists only because the window manager asked for it: bars and panels map because
pine said it knows what to do about them.

Where that answer comes from is not this file's business: it asks, and what asks
is a write and a read of the daemon's namespace."))

(defun managingp (w) (and w (manager w) t))

(defun %window (w proxy)
  (let ((it (make-instance 'window :of proxy :id (incf (counter w)))))
    (push it (windows w))
    it))

(defun %at (w proxy) (find proxy (windows w) :key #'of))

(defun %by-id (w id)
  (find (princ-to-string id) (windows w)
        :key (lambda (each) (princ-to-string (id each)))
        :test #'equal))

(defun %forget (w it)
  (setf (windows w) (remove it (windows w)))
  (when (eq it (focused w)) (setf (focused w) nil))
  (setf (dirty w) t))

(defun said (w)
  "What there is, as plain data: the windows, the outputs and what has the
keyboard. This is what crosses to whoever decides.

An output says the area left after the bars have taken their strip, where the
compositor says so: a window laid out over the bar is a window laid out wrong."
  (list :windows (loop :for each :in (reverse (windows w))
                       :collect (list :id (id each) :title (title each)
                                      :app (app each)
                                      :size (list (wide each) (tall each))
                                      :hidden (hidden each)))
        :outputs (loop :for each :in (outputs w)
                       :collect (list :name (getf each :name)
                                      :position (list (getf each :x)
                                                      (getf each :y))
                                      :size (list (getf each :wide)
                                                  (getf each :tall))
                                      :area (or (getf each :area)
                                                (list (getf each :x)
                                                      (getf each :y)
                                                      (getf each :wide)
                                                      (getf each :tall)))))
        :focused (and (focused w) (id (focused w)))))

(defun %shown (w it x y wide tall clip stack)
  (unless (and (= wide (wide it)) (= tall (tall it)))
    (setf (wide it) wide (tall it) tall)
    (river-window-v1.propose-dimensions (of it) wide tall))
  (unless (node-of it)
    (setf (node-of it) (river-window-v1.get-node (of it))))
  (when (hidden it)
    (river-window-v1.show (of it))
    (setf (hidden it) nil))
  (unless (equal clip (clip it))
    (setf (clip it) clip)
    (if clip
        (destructuring-bind (cx cy cw ch) clip
          (river-window-v1.set-clip-box (of it) cx cy cw ch))
        (river-window-v1.set-clip-box (of it) 0 0 wide tall)))
  (river-node-v1.set-position (node-of it) x y)
  (river-window-v1.set-tiled (of it) +all-edges+)
  (setf (stack it) stack)
  (case stack
    (:bottom (river-node-v1.place-bottom (node-of it)))
    ((nil :top) (river-node-v1.place-top (node-of it)))
    (t (let ((over (%by-id w stack)))
         (if (and over (node-of over))
             (river-node-v1.place-above (node-of it) (node-of over))
             (river-node-v1.place-top (node-of it))))))
  w)

(defun %gone (it)
  (unless (hidden it)
    (river-window-v1.hide (of it))
    (setf (hidden it) t))
  it)

(defun apply-layout (w layout)
  "Put each window where the layout says. The answer is total: a window it does
not name is hidden, which is what makes changing what is on screen one write
rather than a difference against what was there before."
  (let ((named nil))
    (dolist (each layout)
      (destructuring-bind (id x y wide tall &key clip stack) each
        (let ((it (%by-id w id)))
          (when it
            (push it named)
            (%shown w it x y wide tall clip stack)))))
    (dolist (it (windows w) w)
      (unless (member it named) (%gone it)))))

(defun %window-events (w it)
  (push (evlambda
          (:title (title) (setf (title it) (or title "")) (setf (dirty w) t))
          (:app-id (app-id) (setf (app it) (or app-id "")) (setf (dirty w) t))
          (:closed ()
           (%forget w it)
           (fault:or-nothing "the window is already gone from the compositor"
             (river-window-v1.destroy (of it))))
          (:dimensions (width height)
           (setf (wide it) width (tall it) height))
          (:dimensions-hint (min-width min-height max-width max-height)
           (declare (ignore min-width min-height max-width max-height)))
          (:parent (parent) (declare (ignore parent)))
          (:decoration-hint (hint) (declare (ignore hint)))
          (:presentation-hint (hint) (declare (ignore hint)))
          (:identifier (identifier) (declare (ignore identifier)))
          (:unreliable-pid (pid) (declare (ignore pid)))
          (:pointer-move-requested (seat) (declare (ignore seat)))
          (:pointer-resize-requested (seat edges) (declare (ignore seat edges)))
          (:show-window-menu-requested (x y) (declare (ignore x y)))
          (:maximize-requested () (river-window-v1.inform-unmaximized (of it)))
          (:unmaximize-requested () (river-window-v1.inform-unmaximized (of it)))
          (:fullscreen-requested (output)
           (declare (ignore output))
           (river-window-v1.inform-not-fullscreen (of it)))
          (:exit-fullscreen-requested ()
           (river-window-v1.inform-not-fullscreen (of it)))
          (:minimize-requested () nil))
        (wl-proxy-hooks (of it))))

(defun %layer-output (w out)
  "Ask about this output's layer surfaces: where they leave room, and let it be
where a bar with no output of its own goes."
  (let ((it (and (layers w)
                 (river-layer-shell-v1.get-output (layers w)
                                                  (getf out :proxy)))))
    (when it
      (setf (getf out :layer) it)
      (push (evlambda
              (:non-exclusive-area (x y width height)
               (setf (getf out :area) (list x y width height))
               (setf (dirty w) t)))
            (wl-proxy-hooks it)))
    it))

(defun %output-events (w proxy)
  (let ((out (list :proxy proxy :name 0 :x 0 :y 0 :wide 0 :tall 0
                   :layer nil :area nil)))
    (push out (outputs w))
    (%layer-output w out)
    (push (evlambda
            (:wl-output (name) (setf (getf out :name) name))
            (:position (x y) (setf (getf out :x) x (getf out :y) y))
            (:dimensions (width height)
             (setf (getf out :wide) width (getf out :tall) height)
             (setf (dirty w) t))
            (:removed ()
             (setf (outputs w) (remove out (outputs w)))
             (setf (dirty w) t)))
          (wl-proxy-hooks proxy))
    out))

(defun wants-chords (w chords)
  "Say which chords the window manager wants. Taken up on the next manage cycle,
because that is the only place the protocol lets a binding be enabled."
  (setf (wanted w) chords)
  (when (manager w) (river-window-manager-v1.manage-dirty (manager w)))
  chords)

(defun eat-next (w) (chords:eat-next (chords w)))

(defun %seat-events (w proxy)
  (push proxy (seats w))
  (chords:attend (chords w) proxy)
  (push (evlambda
          (:wl-seat (name) (declare (ignore name)))
          (:window-interaction (window)
           (let ((it (%at w window)))
             (when it (setf (focused w) it (dirty w) t))
             (river-seat-v1.focus-window proxy window)))
          (:shell-surface-interaction (shell-surface)
           (declare (ignore shell-surface)))
          (:pointer-enter (window) (declare (ignore window)))
          (:pointer-leave () nil)
          (:pointer-position (x y) (declare (ignore x y)))
          (:op-delta (dx dy) (declare (ignore dx dy)))
          (:op-release () nil)
          (:removed () (setf (seats w) (remove proxy (seats w)))))
        (wl-proxy-hooks proxy))
  proxy)

(defun wake (w)
  "Ask for a cycle. A manage sequence is always followed by a render one, which
is where anything of the window manager's own is committed."
  (when (and w (manager w))
    (river-window-manager-v1.manage-dirty (manager w))
    t))

(defun laid (w layout)
  "Take an answer about where the windows go. It arrives after the cycle that
asked for it, so this asks for another one.

An answer naming nothing is still an answer: a workspace with no windows on it
hides every window, and that is not the same as never having been told."
  (setf (pending w) layout (placedp w) t (asking w) nil)
  (when (manager w)
    (river-window-manager-v1.manage-dirty (manager w)))
  layout)

(defun %default-output (w)
  "Say which output a layer surface goes on when it does not ask. Window
management state, so only here."
  (unless (defaultp w)
    (let ((out (find-if (lambda (each) (getf each :layer)) (outputs w))))
      (when out
        (river-layer-shell-output-v1.set-default (getf out :layer))
        (setf (defaultp w) t)))))

(defun %manage (w)
  "One manage cycle: put the windows where the last answer said, finish, and ask
for a fresh answer if anything moved.

The answer comes from another image, so it is never waited for here: a manager
that waits is one the compositor gives up on, and it says so after three
seconds."
  (%default-output w)
  (when (wanted w) (chords:ask-for (chords w) (wanted w)) (setf (wanted w) nil))
  (when (placedp w) (apply-layout w (pending w)))
  (river-window-manager-v1.manage-finish (manager w))
  (when (and (dirty w) (on-said w) (not (asking w)))
    (when (funcall (on-said w) (said w))
      (setf (dirty w) nil (asking w) t))))

(defun focus (w id)
  (let ((it (%by-id w id)))
    (when it
      (setf (focused w) it)
      (dolist (seat (seats w) t)
        (river-seat-v1.focus-window seat (of it))))))

(defun close-focused (w)
  (let ((it (focused w)))
    (when it (river-window-v1.close (of it)) t)))

(defun %step (w by)
  (let* ((all (reverse (windows w)))
         (at (position (focused w) all)))
    (when (and all at)
      (focus w (id (nth (mod (+ at by) (length all)) all))))))

(defun take (w wants)
  "Do what the daemon asked for. What it asks for is a value, so this is a case
over data and not a protocol of its own."
  (dolist (each wants w)
    (let ((verb (first each)) (arguments (rest each)))
      (case verb
        (:focus (focus w (first arguments)))
        (:close (close-focused w))
        (:next (%step w 1))
        (:previous (%step w -1))
        (:hide (let ((it (%by-id w (first arguments)))) (when it (%gone it))))
        (:show (let ((it (%by-id w (first arguments))))
                 (when (and it (hidden it))
                   (river-window-v1.show (of it))
                   (setf (hidden it) nil))))
        (:exit (river-window-manager-v1.exit-session (manager w)))
        (t nil)))
    (setf (dirty w) t)))

(defun bind (d &key on-said on-render on-chord)
  "Become the compositor's window manager, if it is asking for one. Answers
nothing where the compositor manages its own windows."
  (let* ((it (display:of d))
         (registry (wl-display.get-registry it))
         (found nil)
         (layers nil)
         (chords nil))
    (push (evlambda
            (:global (name interface version)
             (case (alexandria:when-let ((c (find-interface-named interface)))
                     (class-name c))
               (river-window-manager-v1
                (setf found (wl-registry.bind registry name
                                              'river-window-manager-v1
                                              (min version 4))))
               (river-layer-shell-v1
                (setf layers (wl-registry.bind registry name
                                               'river-layer-shell-v1
                                               (min version 1))))
               (river-xkb-bindings-v1
                (setf chords (wl-registry.bind registry name
                                               'river-xkb-bindings-v1
                                               (min version 2)))))))
          (wl-proxy-hooks registry))
    (wl-display-roundtrip it)
    (when found
      (let ((w (make-instance 'wm :manager found :layers layers
                                  :chords (chords:make-chords chords
                                                              :told on-chord)
                                  :on-said on-said :on-render on-render)))
        (push (evlambda
                (:window (window) (%window-events w (%window w window)))
                (:output (output) (%output-events w output))
                (:seat (seat) (%seat-events w seat))
                (:manage-start () (%manage w))
                (:render-start ()
                 (when (on-render w) (funcall (on-render w)))
                 (river-window-manager-v1.render-finish found))
                (:session-locked () nil)
                (:session-unlocked () nil)
                (:unavailable ()
                 (log:note "another window manager has this compositor")
                 (fault:or-nothing "another manager has it, so let it go"
                   (river-window-manager-v1.destroy found)))
                (:finished ()
                 (fault:or-nothing "the compositor has finished with us"
                   (river-window-manager-v1.destroy found))))
              (wl-proxy-hooks found))
        (wl-display-roundtrip it)
        (log:note "pine is the window manager")
        w))))
