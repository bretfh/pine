(defpackage #:pine/wayland/input
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:shm #:posix-shm))
  (:export #:keys #:make-keys #:chord #:heldp #:modifierp #:forget-held
           #:keys-rate #:keys-delay #:keymap #:modifiers #:pressed #:released
           #:repeating #:now-ms #:deadline
           #:pointer #:make-pointer #:pointer-at-x #:pointer-at-y
           #:pointer-focus #:pointer-drag #:pointer-serial
           #:+modifiers+))
(in-package #:pine/wayland/input)

(defparameter +modifiers+
  '("Shift_L" "Shift_R" "Control_L" "Control_R" "Alt_L" "Alt_R"
    "Meta_L" "Meta_R" "Super_L" "Super_R" "Hyper_L" "Hyper_R"
    "Caps_Lock" "Num_Lock" "ISO_Level3_Shift" "ISO_Level5_Shift")
  "Keys that never arm a repeat: holding a bare modifier repeats nothing.")

(defstruct (keys (:constructor make-keys))
  "The keyboard as xkb sees it, and what is being held down."
  (context (xkb:xkb-context-new ()))
  keymap
  state
  (held nil)
  (held-code nil)
  (since 0)
  (repeated 0)
  (rate 25)
  (delay 400))

(defstruct (pointer (:constructor make-pointer))
  "Where the pointer is, and what it is over."
  (at-x 0) (at-y 0) (serial 0) focus drag)

(defun now-ms ()
  (values (floor (* 1000 (get-internal-real-time))
                 internal-time-units-per-second)))

(defun modifierp (name) (member name +modifiers+ :test #'string=))

(defun forget-held (k)
  (setf (keys-held k) nil (keys-held-code k) nil)
  k)

(defun %activep (state name)
  (plusp (xkb:xkb-state-mod-name-is-active state name :mods-effective)))

(defun chord (k code)
  "The chord a keycode is, spelled the way pine spells one."
  (let* ((state (keys-state k))
         (sym (xkb:xkb-state-key-get-one-sym state code))
         (utf8 (xkb:xkb-state-key-get-utf8 state code))
         (ctrl (%activep state "Control"))
         (meta (%activep state "Mod1"))
         (super (%activep state "Mod4"))
         (shift (%activep state "Shift"))
         (printable (and (plusp (length utf8))
                         (graphic-char-p (char utf8 0))
                         (not super)
                         (not (and ctrl (string= utf8 " "))))))
    (let ((name (if printable utf8 (xkb:xkb-keysym-get-name sym))))
      (values (format nil "~:[~;C-~]~:[~;M-~]~:[~;s-~]~:[~;S-~]~a"
                      ctrl meta super (and shift (not printable)) name)
              name))))

(defun deadline (k)
  "Milliseconds until the held key repeats again, or nothing when none is held.
The only deadline a painter has: with no key down it waits as long as it takes."
  (let ((held (keys-held k)) (rate (keys-rate k)))
    (when (and held (plusp rate))
      (let ((due (max (+ (keys-since k) (keys-delay k))
                      (+ (keys-repeated k) (floor 1000 rate)))))
        (max 0 (- due (now-ms)))))))

(defun repeating (k)
  "The chord to send again, if the held one is due. Nothing otherwise."
  (let ((held (keys-held k)) (rate (keys-rate k)))
    (when (and held (plusp rate))
      (let ((now (now-ms)))
        (when (and (>= (- now (keys-since k)) (keys-delay k))
                   (>= (- now (keys-repeated k)) (floor 1000 rate)))
          (setf (keys-repeated k) now)
          held)))))

(defun heldp (k) (and (keys-held k) t))

(defun keymap (k fd size)
  "Take the keymap the compositor handed over."
  (let ((it (shm:make-shm fd)))
    (unwind-protect
         (shm:with-mmap (ptr it size :flags '(:private))
           (let* ((keymap (xkb:xkb-keymap-new-from-string (keys-context k) ptr
                                                          :text-v1 ()))
                  (state (xkb:xkb-state-new keymap)))
             (when (keys-keymap k) (xkb:xkb-keymap-unref (keys-keymap k)))
             (when (keys-state k) (xkb:xkb-state-unref (keys-state k)))
             (setf (keys-keymap k) keymap (keys-state k) state)))
      (shm:close-shm it))))

(defun modifiers (k depressed latched locked group)
  (when (keys-state k)
    (xkb:xkb-state-update-mask (keys-state k) depressed latched locked 0 0 group)))

(defun pressed (k code)
  "What was pressed, and hold it for repeating unless it is a bare modifier."
  (when (keys-state k)
    (multiple-value-bind (said name) (chord k (+ 8 code))
      (if (modifierp name)
          (forget-held k)
          (setf (keys-held-code k) code
                (keys-held k) said
                (keys-since k) (now-ms)
                (keys-repeated k) 0))
      said)))

(defun released (k code)
  (when (eql code (keys-held-code k)) (forget-held k))
  nil)
