(in-package :pine/user)

(procfs)
(audio)
(screen)
(power)
(network)
(media :player "emms")
(niri)

(write /active-theme :ef-dream)
(write /tab-width 2)
(write /wm-terminal "alacritty")

(mode "org" :parent "text" :settings '(:indicator "Org")
            :claims '((:files "*.org")))

(defcommand "hello" () (:describes "a command this config added")
  "hello from the config")

(bind "text" "C-c h" "hello")

(defsurface editor (:as :toplevel)
  (frame-tree))

(defsurface bar (:as :bar)
  (centerbox :orient :v :class "bar"
    :start (column :align :center
             (icon #x0F002 :class "picker" :hint "Search"
                           :click (map (at (root *world*) "sh" "setsid -f fuzzel") t)))
    :center (apply #'column :align :center :spacing 4
                   (mapcar (lambda (n)
                             (let ((where (at (root *world*) "wm" "workspaces" n)))
                               (icon n :class (if (getf (read where) :focused)
                                                  "ws ws-current"
                                                  "ws")
                                       :click where)))
                           (or (read /wm/workspaces) (list))))
    :end (column :align :center :spacing 10
           (icon #x0F028 :class "icon" :hint "Volume"
                         :click (map /surface/audio/shown (seq :toggle)))
           (label (format nil "~a:~a" (read /clock/hour) (read /clock/minute))
                  :class "clock"))))

(defsurface echo (:as :echo)
  (row :class "echo" :align :center
    (label (or (read /echo/hint) (run "wm-title") "") :class "echo-text" :expand 1)
    (label (format nil "~a   ~d%" (or (read /net/connection) "offline")
                   (or (read /audio/volume) 0))
           :class "echo-stat")))

(defsurface audio (:as :panel)
  (column :class "netmenu" :align :stretch
    (label "Audio" :class "nm-title")
    (row :align :center :spacing 12
      (icon (if (read /audio/muted) #x0F026 #x0F028) :class "audio-mute"
            :click (map /audio/muted (seq :toggle)))
      (slider /audio/volume :class "menu-slider" :min 0 :max 100 :expand 1)
      (label (format nil "~d%" (or (read /audio/volume) 0)) :class "audio-pct"))))

(style ".bar" (list :background-color (css-glass :bg) :color (color :fg)
                    :padding "8px 0 0 0"))
(style ".ws" (list :color (color :fg-alt) :min-width "28px" :padding "8px 0"
                   :border-radius (css-rad)))
(style ".ws-current" (list :color (color :accent-fg)
                           :background-color (color :accent)))
(style ".clock" (list :color (color :fg)))
(style ".echo-text" (list :color (color :fg) :padding "0 12px"))
(style ".echo-stat" (list :color (color :fg-dim) :padding "0 12px"))
(style ".netmenu" (list :background-color (css-glass :bg) :color (color :fg)
                        :border-radius (css-rad) :padding "12px" :min-width "320px"))
(style ".nm-title" (list :font-size "17px" :font-weight "bold"))

(bind "wm" "s-Return" "wm-terminal")
(bind "wm" "s-j" "wm-focus-next")
(bind "wm" "s-k" "wm-focus-previous")
(bind "wm" "s-q" "wm-close-window")

(declare-frontends '("editor" "desktop"))
