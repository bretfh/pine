(in-package #:pine/user)

;; What this pine loads. Each is a system: a class that starts and stops, at
;; /system/<name>, and nothing here is more privileged than what you write.

(use :text)
(use :host)
(use :edit)
(use :term)
(use :desk)

;; What the machine has, in the tree. A device is rows -- a name, how to read it
;; and how to write it -- so /dev/audio/volume is read and written like anything.

(run "attend" '("audio"))
(run "attend" '("screen"))
(run "attend" '("power"))
(run "attend" '("net"))
(run "attend" '("media"))

(write /theme/active :ef-dream)
(write /wm-terminal "alacritty")

;; A mode is a class, so the chain is class inheritance and CALL-NEXT-METHOD is
;; the fallback. This one is org with a tab stop of its own.

(defclass notes (org) ())

(defmethod setting ((m notes) key)
  (case key (:tab-width 2) (t (call-next-method))))

(defmethod claims ((m notes)) '("*.org"))

(defcommand "hello" () (:describes "a command this config added")
  "hello from the config")

(bind 'text "C-c h" "hello")

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
;; one ANCHOR method puts a new one on screen and the painter needs no knowledge
;; of it, because the role crosses the wire with the surface.

(defclass ticker (overlay) ())

(defmethod anchor ((r ticker) width height)
  (map :edges '(:bottom :right) :wide width :tall height :keeps 0
       :margin '(0 12 12 0)))

(defsurface ticker (:as 'ticker)
  (row :class "ticker"
       (label (or (read /dev/media/title) "nothing playing"))))

;; A surface reads nodes and follows them: nothing subscribes to anything, and a
;; write two levels down works this out again exactly once.

(defsurface sound (:as 'panel)
  (column :class "panel" :align :stretch
          (label "Sound" :class "panel-title")
          (row :align :center :spacing 12
               (button :class "mute" :click /dev/audio/muted
                       (label (if (read /dev/audio/muted) "muted" "on")))
               (slider /dev/audio/volume :low 0 :high 100 :expand 1)
               (label (format nil "~d%" (or (read /dev/audio/volume) 0))))))

(style ".panel" (list :background-color (css-glass :bg) :color (color :fg)
                      :border-radius (css-rad) :padding "12px"
                      :min-width "320px"))
(style ".panel-title" (list :font-size "17px" :font-weight "bold"))
(style ".ticker" (list :background-color (css-glass :bg) :color (color :fg-dim)
                       :border-radius (css-rad) :padding "6px 12px"))
