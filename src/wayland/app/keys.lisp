(defpackage #:pine.wayland.app.keys
  (:use #:cl #:wayflan-client #:pine.wayland.app.editor)
  (:local-nicknames (#:a #:alexandria) (#:shm #:posix-shm))
  (:export #:*ekb* #:+modifier-keysyms+ #:ekb #:handle-keyboard #:key->wire #:mod-active #:modifier-key-p))

(in-package #:pine.wayland.app.keys)

(defvar *ekb* (pine.data:table)
  "Editor to (list context keymap state).")

(defparameter +modifier-keysyms+
  '("Shift_L" "Shift_R" "Control_L" "Control_R" "Alt_L" "Alt_R"
    "Meta_L" "Meta_R" "Super_L" "Super_R" "Hyper_L" "Hyper_R"
    "Caps_Lock" "Num_Lock" "ISO_Level3_Shift" "ISO_Level5_Shift")
  "Keysyms that never arm key repeat: holding a bare modifier repeats nothing.")

(defun ekb (ed) (pine.data:at (pine.data:all *ekb*) ed))

(defun mod-active (state name)
  (plusp (xkb:xkb-state-mod-name-is-active state name :mods-effective)))

(defun modifier-key-p (msg)
  (member (getf (rest msg) :key-str) +modifier-keysyms+ :test #'string=))

(defun key->wire (state keycode)
  "Translate a keycode to the daemon's (:key :key-str S :ctrl .. :meta .. ) form."
  (let* ((sym  (xkb:xkb-state-key-get-one-sym state keycode))
         (utf8 (xkb:xkb-state-key-get-utf8 state keycode))
         (ctrl (mod-active state "Control"))
         (meta (mod-active state "Mod1"))
         (super (mod-active state "Mod4"))
         (shift (mod-active state "Shift"))
         (printable (and (plusp (length utf8))
                         (graphic-char-p (char utf8 0))
                         (not super)
                         (not (and ctrl (string= utf8 " "))))))
    (if printable
        (list :key :key-str utf8 :ctrl ctrl :meta meta :super super)
        (list :key :key-str (xkb:xkb-keysym-get-name sym)
              :ctrl ctrl :meta meta :shift shift :super super))))

(defun handle-keyboard (ed &rest event)
  (event-case event
    (:keymap (format fd size)
     (assert (eq format :xkb-v1))
     (let* ((cell (or (ekb ed)
                      (pine.data:keep! *ekb* ed
                                     (list (xkb:xkb-context-new ()) nil nil))))
            (context (first cell))
            (shmo (shm:make-shm fd)))
       (unwind-protect
            (shm:with-mmap (ptr shmo size :flags '(:private))
              (let* ((keymap (xkb:xkb-keymap-new-from-string context ptr :text-v1 ()))
                     (state (xkb:xkb-state-new keymap)))
                (when (second cell) (xkb:xkb-keymap-unref (second cell)))
                (when (third cell) (xkb:xkb-state-unref (third cell)))
                (setf (second cell) keymap (third cell) state)))
         (shm:close-shm shmo))))
    (:modifiers (serial depressed latched locked group)
     (declare (ignore serial))
     (a:when-let ((cell (ekb ed)))
       (xkb:xkb-state-update-mask (third cell) depressed latched locked 0 0 group)))
    (:key (serial time-ms key state)
     (declare (ignore serial time-ms))
     (case state
       (:pressed
        (a:when-let ((cell (ekb ed)))
          (let ((msg (key->wire (third cell) (+ 8 key))))
            (send-input ed msg)

            (if (modifier-key-p msg)
                (setf (ed-held-msg ed) nil (ed-held-keycode ed) nil)
                (setf (ed-held-keycode ed) key
                      (ed-held-msg ed) msg
                      (ed-held-since-ms ed) (now-ms)
                      (ed-last-repeat-ms ed) 0)))))
       (:released
        (when (eql key (ed-held-keycode ed))
          (setf (ed-held-msg ed) nil (ed-held-keycode ed) nil)))))
    (:enter (serial surface keys) (declare (ignore serial surface keys)))
    (:leave (serial surface)
     (declare (ignore serial surface))
     (setf (ed-held-msg ed) nil (ed-held-keycode ed) nil))
    (:repeat-info (rate delay)
     (setf (ed-repeat-rate ed) rate
           (ed-repeat-delay-ms ed) delay))))

(setf *keyboard-handler* #'handle-keyboard)
