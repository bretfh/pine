(defpackage #:pine/wayland/hands
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:log #:pine/fs/log) (#:pump #:pine/wayland/pump)
                    (#:shell #:pine/wayland/shell) (#:pane #:pine/wayland/pane)
                    (#:input #:pine/wayland/input) (#:wm #:pine/wayland/wm)
                    (#:screen #:pine/wayland/screen)
                    (#:fault #:pine/run/fault)))
(in-package #:pine/wayland/hands)

(defconstant +left+ #x110)

(defmethod screen:pointing ((s screen:screen) sh &rest event)
  (let ((at (screen:pointer s)))
    (event-case event
      (:enter (serial surface x y)
       (setf (input:pointer-serial at) serial
             (input:pointer-focus at) (shell:at-surface sh surface)
             (input:pointer-at-x at) x
             (input:pointer-at-y at) y))
      (:motion (time-ms x y)
       (declare (ignore time-ms))
       (setf (input:pointer-at-x at) x (input:pointer-at-y at) y)
       (when (input:pointer-drag at) (drag s)))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (setf (input:pointer-focus at) nil))
      (:button (serial time-ms button state)
       (declare (ignore serial time-ms))
       (when (= button +left+)
         (ecase state
           (:pressed (press s))
           (:released (release s)))))
      (:axis (time-ms axis delta) (declare (ignore time-ms axis delta)))
      (:frame ()))))

(defun over (s)
  (let* ((at (screen:pointer s))
         (p (input:pointer-focus at)))
    (when (and p (pane:tree p))
      (ui:under (pane:tree p) (round (input:pointer-at-y at))
              (round (input:pointer-at-x at))))))

(defun press (s)
  (let ((found (parent s)))
    (if (typep found 'ui:slider)
        (progn (setf (input:pointer-drag (screen:pointer s)) found) (drag s))
        (click s))))

(defun drag (s)
  (let ((slider (input:pointer-drag (screen:pointer s)))
        (p (input:pointer-focus (screen:pointer s))))
    (when (and slider p)
      (setf (ui:value slider)
            (ui:value-at slider (round (input:pointer-at-x (screen:pointer s)))))
      (pane:paint p))))

(defun release (s)
  (let ((slider (input:pointer-drag (screen:pointer s))))
    (when slider
      (setf (input:pointer-drag (screen:pointer s)) nil)
      (let ((fn (ui:changed slider)))
        (when fn (screen:tell s (lambda () (funcall fn (ui:value slider)))))))))

(defun click (s)
  (let* ((at (screen:pointer s))
         (p (input:pointer-focus at)))
    (when (and p (pane:tree p))
      (let ((thunk (ui:clicked-at (pane:tree p)
                                   (round (input:pointer-at-y at))
                                   (round (input:pointer-at-x at)))))
        (when thunk (screen:tell s thunk))))))

(defmethod screen:keyboard-said ((s screen:screen) sh &rest event)
  (declare (ignore sh))
  (let ((k (screen:keys s)))
    (event-case event
      (:keymap (format fd size)
       (assert (eq format :xkb-v1))
       (input:keymap k fd size))
      (:modifiers (serial depressed latched locked group)
       (declare (ignore serial))
       (input:modifiers k depressed latched locked group))
      (:key (serial time-ms key state)
       (declare (ignore serial time-ms))
       (case state
         (:pressed (let ((said (input:pressed k key)))
                     (when said (screen:typed s said))))
         (:released (input:released k key))))
      (:enter (serial surface keys) (declare (ignore serial surface keys)))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (input:forget-held k))
      (:repeat-info (rate delay)
       (setf (input:keys-rate k) rate (input:keys-delay k) delay)))))

(defmethod screen:typed ((s screen:screen) said)
  (screen:tell s (lambda ()
            (let ((n (tree:at "/key")))
              (if n
                  (setf (node:contents n) said)
                  (log:note "nothing at /key"))))))


(defmethod screen:chorded ((s screen:screen) said)
  "A chord the compositor took rather than giving to whatever has focus. It goes
to /wm/key, which is what says what it means; if that leaves the window manager
part way through a chord, the next key has to come here too."
  (screen:tell s
        (lambda ()
          (let ((n (tree:at "/wm/key")))
            (cond ((null n) (log:note "nothing at /wm/key"))
                  ((null said) (setf (node:contents n) ""))
                  (t (setf (node:contents n) said)
                     (when (plusp (length (node:contents n)))
                       (pump:hand (screen:pump s)
                                  (lambda () (wm:eat-next (screen:wm-of s))))))))))
  t)


(defmethod screen:chords-wanted ((s screen:screen))
  "Which chords the compositor is to take. What is bound is what is asked for."
  (let ((it (screen:wm-of s)))
    (when it
      (let ((chords (fault:or-nothing "pine/wm may not be loaded here"
                      (uiop:symbol-call :pine/wm/keys :chords))))
        (pump:hand (screen:pump s) (lambda () (wm:wants-chords it chords)))))))

