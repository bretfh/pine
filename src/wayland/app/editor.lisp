(defpackage #:pine/wayland/app/editor
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell #:pine/wayland/protocol #:pine/wayland/connection)
  (:local-nicknames (#:a #:alexandria) (#:c #:cl-cairo2) (#:shm #:posix-shm)
                    (#:wire #:pine/ui/wire) (#:paint #:pine/cairo/paint))
  (:export
   #:editor #:run-editor

   #:*keyboard-handler* #:send-input #:now-ms
   #:ed-held-keycode #:ed-held-msg #:ed-held-since-ms #:ed-last-repeat-ms
   #:ed-repeat-rate #:ed-repeat-delay-ms))
(in-package #:pine/wayland/app/editor)

(defvar *keyboard-handler* nil
  "When set, a function of (editor &rest wl-keyboard-event) -- the xkb keys file
installs it. Nil means keyboard events are ignored (render-only).")

(defun font-px ()
  "The editor cell font size, from the one theme metric the daemon's window nodes
also use -- so the frontend measures its :resize cell grid at the same size the
buffer rows were laid out for."
  (float (pine/ui/face:metric :font-px 15) 1d0))

(defclass editor ()
  ((sys        :initarg :sys :accessor ed-sys :initform nil)
   (display    :initarg :display :reader ed-display)
   (backing    :initarg :backing :reader ed-backing)
   (compositor :initform nil) (shm :initform nil) (xdg-wm-base :initform nil)
   (seat :initform nil) (keyboard :initform nil)
   (wl-surface :initform nil) (xdg-surface :initform nil) (xdg-toplevel :initform nil)
   (ref     :initform nil :accessor ed-ref)
   (width   :initform 800 :accessor ed-width)
   (height  :initform 500 :accessor ed-height)
   (tree    :initform nil :accessor ed-tree)
   (cell-w  :initform 9d0 :accessor ed-cell-w)
   (cell-h  :initform 18d0 :accessor ed-cell-h)
   (ascent  :initform 14d0 :accessor ed-ascent)
   (metricsp :initform nil :accessor ed-metricsp)
   (sent-cols :initform -1 :accessor ed-sent-cols)
   (sent-rows :initform -1 :accessor ed-sent-rows)
   (sent-width  :initform -1 :accessor ed-sent-width)
   (sent-height :initform -1 :accessor ed-sent-height)
   (pump    :initarg :pump :reader ed-pump)
   (wire    :initform nil :accessor ed-wire)
   (generation :initform 0 :accessor ed-generation)
   (dirty   :initform nil :accessor ed-dirty)

   (configured :initform nil :accessor ed-configured)
   (done    :initform nil :accessor ed-done)

   (repeat-rate     :initform 25 :accessor ed-repeat-rate)
   (repeat-delay-ms :initform 400 :accessor ed-repeat-delay-ms)
   (held-keycode    :initform nil :accessor ed-held-keycode)
   (held-msg        :initform nil :accessor ed-held-msg)
   (held-since-ms   :initform 0 :accessor ed-held-since-ms)
   (last-repeat-ms  :initform 0 :accessor ed-last-repeat-ms)))

(defun now-ms ()
  (values (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second)))

(defun check-repeat (ed)
  "Resend the held key's message once the repeat delay has elapsed, at the
compositor's repeat rate. Called from the run-loop tick."
  (let ((msg (ed-held-msg ed))
        (rate (ed-repeat-rate ed)))
    (when (and msg (plusp rate))
      (let ((now (now-ms)))
        (when (and (>= (- now (ed-held-since-ms ed)) (ed-repeat-delay-ms ed))
                   (>= (- now (ed-last-repeat-ms ed)) (floor 1000 rate)))
          (setf (ed-last-repeat-ms ed) now)
          (send-input ed msg))))))

(defun repeat-deadline (ed)
  "Milliseconds until the held key repeats again, or nil when none is held.

The only deadline the editor has. With no key down the loop waits for as long
as it takes."
  (let ((msg (ed-held-msg ed))
        (rate (ed-repeat-rate ed)))
    (when (and msg (plusp rate))
      (let ((due (max (+ (ed-held-since-ms ed) (ed-repeat-delay-ms ed))
                      (+ (ed-last-repeat-ms ed) (floor 1000 rate)))))
        (max 0 (- due (now-ms)))))))

(defun send-input (ed msg)
  (a:when-let ((ref (ed-ref ed))) (ignore-errors (sento.actor:tell ref msg))))

(defun ed-cols (ed) (max 1 (floor (ed-width ed) (max 1d0 (ed-cell-w ed)))))
(defun ed-rows (ed) (max 1 (floor (ed-height ed) (max 1d0 (ed-cell-h ed)))))

(defun ensure-metrics (ed)
  "Measure the monospace cell once, the same way the layout engine's window node
does, so a frame laid out at N cols x rows lands exactly in the cells."
  (unless (ed-metricsp ed)
    (c:select-font-face pine/cairo/grid:*font-family* :normal :normal)
    (c:set-font-size (font-px))
    (let ((fe (c:get-font-extents)))
      (multiple-value-bind (xb yb w h ax) (c:text-extents "M")
        (declare (ignore xb yb w h))
        (setf (ed-cell-w ed) (float (max 1 (ceiling ax)) 1d0)))
      (setf (ed-cell-h ed) (float (max 1 (ceiling (+ (c:font-ascent fe) (c:font-descent fe)))) 1d0)
            (ed-ascent ed) (c:font-ascent fe)))
    (setf (ed-metricsp ed) t)))

(defun say-size (ed)
  "Tell the daemon how big this surface is and what it is drawing text at. One
place says it: a second one that left something out is a frame laid out for
one grid and painted on another."
  (let ((cc (ed-cols ed)) (rr (ed-rows ed))
        (w (ed-width ed)) (h (ed-height ed)))
    (setf (ed-sent-cols ed) cc (ed-sent-rows ed) rr
          (ed-sent-width ed) w (ed-sent-height ed) h)
    (send-input ed (list :resize :cols cc :rows rr
                         :width w :height h
                         :cell-w (round (ed-cell-w ed))
                         :cell-h (round (ed-cell-h ed))
                         :font-px (round (font-px))))))

(defun maybe-resize (ed)
  "Say it again whenever it changes at all. The daemon arranges in pixels, so a
window that grew by less than a cell still has to be told: what it lays out is
what gets painted, and a frame laid out for the size before is cut off by the
surface it lands in."
  (unless (and (= (ed-cols ed) (ed-sent-cols ed)) (= (ed-rows ed) (ed-sent-rows ed))
               (= (ed-width ed) (ed-sent-width ed))
               (= (ed-height ed) (ed-sent-height ed)))
    (say-size ed)))

(defun paint-editor (ed)
  (with-slots (shm wl-surface width height) ed
    (when (and (plusp width) (plusp height))
      (let* ((stride (* width 4)) (size (* stride height)))
        (shm:with-open-shm-and-mmap* (obj data (:direction :io) (size))
          (let (buffer)
            (with-proxy (pool (wl-shm.create-pool shm (shm:shm-fd obj) size))
              (setf buffer (wl-shm-pool.create-buffer pool 0 width height stride :argb8888)))
            (c:with-surface-and-context
                (surf (c:create-image-surface-for-data data :argb32 width height stride))
              (c:set-operator :source)
              (c:set-source-rgba 0d0 0d0 0d0 0d0) (c:paint)
              (c:set-operator :over)
              (ensure-metrics ed)
              (maybe-resize ed)
              (when (ed-tree ed)
                (paint:with-cairo-layout
                  (if (wire:arranged-p (ed-tree ed))
                      (paint:paint-arranged (ed-tree ed))
                      (paint:paint-tree (ed-tree ed) width height)))))

            (when (uiop:getenv "PINE_FRAME_DUMP")
              (ignore-errors
               (c:with-surface-and-context
                   (dbg (c:create-image-surface-for-data data :argb32 width height stride))
                 (c:surface-write-to-png dbg (uiop:getenv "PINE_FRAME_DUMP")))))
            (wl-surface.attach wl-surface buffer 0 0)
            (wl-surface.damage-buffer wl-surface 0 0 width height)
            (wl-surface.commit wl-surface)
            (push (evelambda (:release () (destroy-proxy buffer))) (wl-proxy-hooks buffer)))))
      (setf (ed-dirty ed) nil))))

(defun connect-editor (ed)
  (with-slots (display compositor shm xdg-wm-base seat) ed
    (let ((registry (wl-display.get-registry display)))
      (push (evlambda
              (:global (name interface version)
               (declare (ignore version))
               (case (a:when-let ((it (find-interface-named interface))) (class-name it))
                 (wl-compositor (setf compositor (wl-registry.bind registry name 'wl-compositor 4)))
                 (wl-shm        (setf shm (wl-registry.bind registry name 'wl-shm 1)))
                 (xdg-wm-base
                  (setf xdg-wm-base (wl-registry.bind registry name 'xdg-wm-base 1))
                  (push (evelambda (:ping (serial) (xdg-wm-base.pong xdg-wm-base serial)))
                        (wl-proxy-hooks xdg-wm-base)))
                 (wl-seat
                  (setf seat (wl-registry.bind registry name 'wl-seat 5))
                  (push (a:curry #'handle-editor-seat ed) (wl-proxy-hooks seat))))))
            (wl-proxy-hooks registry))
      (wl-display-roundtrip display)
      (wl-display-roundtrip display))))

(defun handle-editor-seat (ed &rest event)
  (with-slots (seat keyboard) ed
    (event-case event
      (:capabilities (capabilities)
       (when (and (member :keyboard capabilities) (null keyboard))
         (setf keyboard (wl-seat.get-keyboard seat))
         (push (lambda (&rest ev)
                 (when *keyboard-handler* (apply *keyboard-handler* ed ev)))
               (wl-proxy-hooks keyboard))))
      (:name (name) (declare (ignore name))))))

(defun open-window (ed)
  (with-slots (compositor xdg-wm-base wl-surface xdg-surface xdg-toplevel) ed
    (setf wl-surface (wl-compositor.create-surface compositor)
          xdg-surface (xdg-wm-base.get-xdg-surface xdg-wm-base wl-surface)
          xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
    (push (evelambda
            (:configure (serial)
             (xdg-surface.ack-configure xdg-surface serial)
             (setf (ed-configured ed) t)
             (paint-editor ed)))
          (wl-proxy-hooks xdg-surface))
    (push (evlambda
            (:configure (w h states) (declare (ignore states))
             (unless (zerop w) (setf (ed-width ed) w))
             (unless (zerop h) (setf (ed-height ed) h)))
            (:close () (setf (ed-done ed) t)))
          (wl-proxy-hooks xdg-toplevel))
    (xdg-toplevel.set-title xdg-toplevel "pine")
    (wl-surface.commit wl-surface)))

(defun handle-editor-message (ed msg)
  (case (first msg)
    (:attached
     (setf (ed-ref ed) (pine/net/attach:accept-attached (ed-sys ed) msg))
     (say-size ed))
    (:style
     (destructuring-bind (&key styles) (rest msg)
       (pine/ui/css:install styles)
       (pine/frontend:enqueue (ed-pump ed) (lambda () (setf (ed-dirty ed) t)))))

    (:widgets
     (destructuring-bind (&key surface tree as generation) (rest msg)
       (declare (ignore surface as))
       (pine/frontend:enqueue (ed-pump ed) (lambda () (adopt-frame ed tree generation)))))
    (:rows-patch
     (destructuring-bind (&key surface patch generation) (rest msg)
       (declare (ignore surface))
       (pine/frontend:enqueue (ed-pump ed) (lambda () (patch-frame ed patch generation)))))))

(defun %build-tree (ed form)
  (setf (ed-wire ed) form
        (ed-tree ed) (wire:wire->node form :on-action
                                   (lambda (id) (declare (ignore id))
                                     (lambda (&rest args) (declare (ignore args)) nil)))
        (ed-dirty ed) t))

(defun adopt-frame (ed form generation)
  "Take a whole frame."
  (setf (ed-generation ed) generation)
  (%build-tree ed form))

(defun patch-frame (ed patch generation)
  "Apply the lines that changed to the frame being held.

A patch names the frame it follows. Holding a different one means a frame was
missed, which makes this frontend wrong rather than merely stale, so it asks
for a whole one instead of applying."
  (cond
    ((and (ed-wire ed) (eql generation (1+ (ed-generation ed))))
     (setf (ed-generation ed) generation)
     (%build-tree ed (wire:apply-rows-patch (ed-wire ed) patch)))
    (t
     (format *error-output* "pine editor: frame ~a does not follow ~a, refetching~%"
             generation (ed-generation ed))
     (finish-output *error-output*)
     (send-input ed (list :refresh)))))

(defun editor-loop (ed)
  (pine/frontend:run (ed-backing ed) (ed-pump ed)
   :done (lambda () (ed-done ed))
   :ready (lambda ()
            (check-repeat ed)
            (when (and (ed-dirty ed) (ed-configured ed)) (paint-editor ed)))
   :pending (lambda () (and (ed-dirty ed) (ed-configured ed)))
   :deadline (lambda () (repeat-deadline ed))))

(defun run-editor (&key (host pine/net/server:*host*) (port pine/net/server:*port*))
  "Attach an editor window to the daemon at HOST:PORT and paint the frames it
pushes. The daemon must be up."
  (let* ((backing (connect-display))
         (ed (make-instance 'editor :backing backing :display (display backing)
                                    :pump (pine/frontend:make-pump))))
      (connect-editor ed)
      (open-window ed)
      (setf (ed-sys ed) (pine/frontend:attach
                         :editor (lambda (msg) (handle-editor-message ed msg))
                         :host host :port port))
    (unwind-protect (editor-loop ed)
      (pine/frontend:close-pump (ed-pump ed))
      (pine/frontend:shutdown backing))))

(pine/net/attach:frontend :editor #'run-editor)
