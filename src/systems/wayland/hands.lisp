(in-package #:pine/wayland)

(defconstant +left+ #x110)

(defmethod pointing ((s screen) sh &rest event)
  (let ((at (pointer s)))
    (event-case event
      (:enter (serial surface x y)
       (setf (pointer-serial at) serial
             (pointer-focus at) (at-surface sh surface)
             (pointer-at-x at) x
             (pointer-at-y at) y))
      (:motion (time-ms x y)
       (declare (ignore time-ms))
       (setf (pointer-at-x at) x (pointer-at-y at) y)
       (when (pointer-drag at) (drag s)))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (setf (pointer-focus at) nil))
      (:button (serial time-ms button state)
       (declare (ignore serial time-ms))
       (when (= button +left+)
         (ecase state
           (:pressed (press s))
           (:released (release s)))))
      (:axis (time-ms axis delta) (declare (ignore time-ms axis delta)))
      (:frame ()))))

(defun over (s)
  (let* ((at (pointer s))
         (p (pointer-focus at)))
    (when (and p (tree p))
      (ui:under (tree p) (round (pointer-at-y at))
              (round (pointer-at-x at))))))

(defun press (s)
  (let ((found (over s)))
    (if (typep found 'ui:slider)
        (progn (setf (pointer-drag (pointer s)) found) (drag s))
        (on-click s))))

(defun drag (s)
  (let ((slider (pointer-drag (pointer s)))
        (p (pointer-focus (pointer s))))
    (when (and slider p)
      (setf (ui:held slider)
            (ui:value-at slider (round (pointer-at-x (pointer s)))))
      (paint p))))

(defun release (s)
  (let ((slider (pointer-drag (pointer s))))
    (when slider
      (setf (pointer-drag (pointer s)) nil)
      (let ((fn (ui:on-change slider)))
        (when fn (tell s (lambda () (funcall fn (ui:held slider)))))))))

(defun on-click (s)
  (let* ((at (pointer s))
         (p (pointer-focus at)))
    (when (and p (tree p))
      (let ((thunk (ui:clicked-at (tree p)
                                   (round (pointer-at-y at))
                                   (round (pointer-at-x at)))))
        (when thunk (tell s thunk))))))

(defmethod keyboard-said ((s screen) sh &rest event)
  (declare (ignore sh))
  (let ((k (keys s)))
    (event-case event
      (:keymap (format fd size)
       (assert (eq format :xkb-v1))
       (keymap k fd size))
      (:modifiers (serial depressed latched locked group)
       (declare (ignore serial))
       (modifiers k depressed latched locked group))
      (:key (serial time-ms key state)
       (declare (ignore serial time-ms))
       (case state
         (:pressed (let ((said (pressed k key)))
                     (when said (typed s said))))
         (:released (released k key))))
      (:enter (serial surface keys) (declare (ignore serial surface keys)))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (forget-held k))
      (:repeat-info (rate delay)
       (setf (keys-rate k) rate (keys-delay k) delay)))))

(defmethod typed ((s screen) said)
  (tell s (lambda ()
            (let ((n (fs:at "/edit/key")))
              (if n
                  (setf (fs:contents n) said)
                  (log:note "nothing at /key"))))))

(defmethod chorded ((s screen) said)
  (tell s
        (lambda ()
          (let ((n (fs:at "/wm/key")))
            (cond ((null n) (log:note "nothing at /wm/key"))
                  ((null said) (setf (fs:contents n) ""))
                  (t (setf (fs:contents n) said)
                     (when (plusp (length (fs:contents n)))
                       (hand (pump s)
                                  (lambda () (eat-next (wm-of s))))))))))
  t)

(defmethod chords-wanted ((s screen))
  (let ((it (wm-of s)))
    (when it
      (let ((chords (fault:or-nothing "pine/wm may not be loaded here"
                      (uiop:symbol-call :pine/wm/keys :chords))))
        (hand (pump s) (lambda () (wants-chords it chords)))))))

