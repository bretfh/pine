(in-package #:pine/user)

;; What this pine loads. Each is a system: a class that starts and stops, at
;; /system/<name>, and nothing here is more privileged than what you write.

(use :text)
(use :host)
(use :edit)
(use :term)
(use :desk)

(use-package '(#:pine/ui #:pine/mode #:pine/host #:pine/wm/tiles))

;; What the machine has, in the tree. A device is rows -- a name, how to read it
;; and how to write it -- so /dev/audio/volume is read and written like anything.
;; DEVICE puts one under /dev and follows it.

(device "audio")
(device "screen")
(device "power")
(device "net")
(device "media" :player "emms")
(device "clip")

;; A write makes the node if nothing has put one there yet.

(write /theme/active :ef-dream)
(write /wm-terminal "alacritty")

;; A mode is a class, so the chain is class inheritance and CALL-NEXT-METHOD is
;; the fallback. This one is org with a tab stop of its own.

(defclass notes (org) ())

(defmethod setting ((m notes) key)
  (case key (:tab-width 2) (t (call-next-method))))

(defmethod handles ((m notes)) '("*.org"))

(defcommand "hello" () (:describes "a command this config added")
  "hello from the config")

(bind 'text "C-c h" "hello")

;; The window manager has chords of its own, bound the same way. A chord in TEXT
;; is what a key means in a document, so it is heard while a document has the
;; keyboard; a chord in WM is one the compositor takes and hands over whatever is
;; focused, which is what makes it a window manager's rather than an editor's.

(write /wm-places "tiles")

(bind 'wm "s-Return" "wm-terminal")
(bind 'wm "s-q" "wm-close-window")
(bind 'wm "s-j" "wm-focus-next")
(bind 'wm "s-k" "wm-focus-previous")
(bind 'wm "s-w" "switch-to-window")

;; A layout is a class too, where pine is the one laying the windows out: one
;; ARRANGE method and it is offered like the rest.

(defclass sidebar (layout) ())

(defmethod arrange ((l sidebar) windows area)
  (destructuring-bind (x y width height) area
    (loop :for id :in windows
          :for i :from 0
          :collect (if (zerop i)
                       (list id x y 320 height)
                       (list id (+ x 320) y (- width 320) height)))))

;; A role is a class too, and it is the whole of what a kind of surface means:
;; one ANCHOR method puts a new one on screen and nothing showing it needs knowledge
;; of it, because the role crosses the wire with the surface. SHOWS says when one
;; comes up: :when-asked, which is what a panel says, or :always, which is this.

(defclass ticker (overlay) ())

(defmethod shows ((r ticker)) :always)

(defmethod anchor ((r ticker) width height)
  (placing :edges '(:bottom :right) :wide width :tall height
           :margin (inset :right 12 :bottom 12)))

(defsurface ticker (:as 'ticker)
  (row :class "ticker"
       (label (read /dev/media/title :else "nothing playing"))))

;; A reading can answer a list. /dev/audio/sinks is every sink there is, and
;; writing one of their names makes it the default. A row per thing is MAPCAR.

(defun sink-row (sink)
  (choice :class "sink" :click (map /dev/audio/sink (getf sink :name))
          (label (getf sink :name)
                 :class (if (getf sink :default) "sink-name on" "sink-name"))))

;; A surface reads nodes and follows them: nothing subscribes to anything, and a
;; write two levels down works this out again exactly once.

(defsurface sound (:as 'panel)
  (column :class "panel" :align :stretch
          (label "Sound" :class "panel-title")
          (row :align :center :spacing 12
               (button :class "mute" :click (lambda () (toggle /dev/audio/muted))
                       (label (if (read /dev/audio/muted) "muted" "on")))
               (slider /dev/audio/volume :class "level" :low 0 :high 100 :expand 1)
               (label (format nil "~d%" (read /dev/audio/volume :else 0))))
          (apply #'column :align :stretch :spacing 2
                 (mapcar #'sink-row (read /dev/audio/sinks :else (list))))))

;; A selector names classes, not elements. Pine draws widgets, and a widget has no
;; parts to reach into: a rule naming one matches nothing, and pine drops it. It
;; matches the widget wearing the class, not what is inside it, so a colour for a
;; label goes on the label. A slider's track is its background colour and what it
;; has filled is its colour.

(style ".panel" (list :background-color (css-glass :bg) :color (color :fg)
                      :border-radius (css-rad) :padding "12px"
                      :min-width "320px"))
(style ".panel-title" (list :font-size "17px" :font-weight "bold"))
(style ".level" (list :background-color (color :bg-alt) :color (color :accent)
                      :min-height "8px" :border-radius "5px"))
(style ".sink" (list :padding "6px 8px" :border-radius (css-rad)))
(style ".sink:hover" (list :background-color (color :bg-active)))
(style ".sink-name" (list :color (color :fg)))
(style ".sink-name.on" (list :color (color :green) :font-weight "bold"))
(style ".ticker" (list :background-color (css-glass :bg) :color (color :fg-dim)
                       :border-radius (css-rad) :padding "6px 12px"))
