(in-package #:pine.wl-editor)

;;;; xkb keyboard for the wayland editor. Sets *keyboard-handler* at load time so
;;;; run-editor types. A key becomes a printable utf8 string, or else the keysym
;;;; name, plus the ctrl/meta/shift/super modifier flags. Per-editor xkb context,
;;;; keymap, and state live in *ekb* keyed by the editor.

(defvar *ekb* (make-hash-table :test 'eq)
  "editor -> (list context keymap state).")

(defun ekb (ed) (gethash ed *ekb*))

(defun mod-active (state name)
  (plusp (xkb:xkb-state-mod-name-is-active state name :mods-effective)))

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
                      (setf (gethash ed *ekb*)
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
     (when (eq state :pressed)
       (a:when-let ((cell (ekb ed)))
         (send-input ed (key->wire (third cell) (+ 8 key))))))
    (:enter (serial surface keys) (declare (ignore serial surface keys)))
    (:leave (serial surface) (declare (ignore serial surface)))
    (:repeat-info (rate delay) (declare (ignore rate delay)))))

(setf *keyboard-handler* #'handle-keyboard)
