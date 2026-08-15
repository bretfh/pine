(defpackage #:pine.wayland.app.chord
  (:use #:cl)
  (:local-nicknames (#:key #:pine/edit/key))
  (:export #:keysym+modifiers))

(in-package #:pine.wayland.app.chord)

(defun keysym+modifiers (chord)
  "(values KEYSYM MODIFIERS) for a pine chord string such as \"s-Return\", or
nil when xkbcommon does not know the key name. MODIFIERS is the keyword list
river_seat_v1.modifiers is written in: pine's meta is the protocol's mod1 and
pine's super is its mod4."
  (let* ((key (key:parse-key chord))
         (keysym (xkb:xkb-keysym-from-name (key:key-sym key) '(:no-flags)))
         (modifiers (append (when (key:key-shift key) '(:shift))
                            (when (key:key-ctrl key)  '(:ctrl))
                            (when (key:key-meta key)  '(:mod1))
                            (when (key:key-super key) '(:mod4)))))
    (when (and keysym (plusp keysym))
      (values keysym modifiers))))
