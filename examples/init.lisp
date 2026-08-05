(in-package :pine.user)

;;;; What this machine runs.

(write /sys    (procfs))
(write /audio  (pipewire))
(write /screen (backlight))
(write /power  (logind))
(write /net    (networkmanager))
(write /media  (mpris :player "emms"))
(write /wm     (niri))

(write /proc/editor  {:image "pine editor"})
(write /proc/desktop {:image "pine desktop"})

(write /theme :ef-dream)
(write /style/editor-view {:opacity 0.9})

;;;; The bar.

(defun ib (glyph class &rest args)
  (apply #'icon glyph :class class :glyph-class "bar-glyph" :font-px 15 args))

(defun launcher (glyph class hint cmd)
  (ib glyph class :hint hint :click {/sh/${cmd} t}))

(write /surface/bar {:as :bar})
(write /surface/bar
  (centerbox :orient :v :class "bar"
    :start
    (column :align :center :spacing 12
  (column :class "bgroup grp-overview" :align :center
    (ib #xF02C1 "viewer" :hint "Overview" :click {/wm [:overview]}))
      (column :class "bgroup grp-nav" :align :center :spacing 6
    (launcher #x0F002 "picker" "Search windows" "fuzzel")
        (rows /wm/workspaces/*
      (fn ()
            (icon (leaf /.) :font-px 15 :glyph-class "bar-glyph"
              :class (if (read /./focused) "ws ws-current" "ws")
                  :click {/./focused t})))))
    :center
    (column :class "bgroup grp-apps" :align :center :spacing 8
  (launcher #x0F003B "launch"    "Applications" "setsid -f fuzzel")
      (launcher #x0F120  "app term"  "Terminal"     "setsid -f alacritty")
      (launcher #x0F268  "app web"   "Browser"      "setsid -f google-chrome")
      (launcher #x0F07B  "app files" "Files"        "setsid -f nautilus")
      (launcher #x0F121  "app edit"  "Editor"       "emacsclient -c -n"))
    :end
    (column :align :center :spacing 12
  (column :class "bgroup grp-tray" :align :center :spacing 10
    (ib #x0F028 "icon"  :hint "Volume"  :click {/surface/audio/shown   [:toggle]})
        (ib #x0F001 "media" :hint "Media"   :click {/surface/media/shown   [:toggle]})
        (ib #x0F1EB "net"   :hint "Network" :click {/surface/network/shown [:toggle]}))
      (button :class "clock" :hint "Calendar"
          :click {/surface/calendar/shown [:toggle]}
    (column :align :center
      (label /clock/hour   :class "hour")
          (label /clock/min  :class "min")))
      (ib #x0F007 "corner-sq" :hint "System"
      :click {/surface/ctl/shown [:toggle]}))))

;;;; The echo strip.

(write /surface/echo {:as :echo})
(write /surface/echo
  (row :class "echo" :align :center
    (column :class "echo-lead")
    (row :class "echo-body" :align :center :expand 1
  (label (or (read /echo/hint)
                 (read /wm/focused/title)
                 (format nil "~a@~a" (read /sys/user) (read /sys/host)))
             :class "echo-text" :expand 1)
      (label (format nil "~a   ~a ~d%"
                 (or (read /net/connection) "offline")
                     (string (code-char (if (read /audio/muted) #x0F075F #x0F057E)))
                     (or (read /audio/volume) 0))
             :class "echo-stat"))))

;;;; The editor. This tree is the arrangement.

(write /surface/editor {:as :toplevel})
(write /surface/editor
  (column :align :stretch
    (window /buf/scratch :class :editor-view :expand 1)
    (echo)
    (modeline)))

;;;; Panels.

(defun nm-head (glyph title sub on)
  (row :class "nm-card nm-head" :align :center
    (icon glyph :class "nm-head-ico")
    (column :expand 1
  (label title :class "nm-title")
      (label sub :class (if on "nm-sub on" "nm-sub")))))

(defun mmss (seconds)
  (let ((s (or seconds 0)))
    (format nil "~d:~2,'0d" (floor s 60) (mod (floor s) 60))))

(defun signal-class (strength)
  (let ((s (or strength 0)))
    (cond ((>= s 66) "nm-sig hi")
          ((>= s 33) "nm-sig mid")
          (t "nm-sig lo"))))

(defun ring-face (cls)
  (cond ((string= cls "cpu")  :red)
        ((string= cls "ram")  :blue)
        ((string= cls "disk") :green)
        (t :yellow)))

(defun uptime-string (seconds)
  (let ((s (or seconds 0)))
    (format nil "up ~dh ~dm" (floor s 3600) (mod (floor s 60) 60))))

(write /surface/calendar {:as :panel})
(write /surface/calendar
  (column :class "netmenu cal-box" :align :stretch (calendar)))

(write /surface/audio {:as :panel})
(write /surface/audio
  (column :class "netmenu" :align :stretch
    (nm-head #x0F028 "Audio"
         (format nil "~d%~a" (or (read /audio/volume) 0)
                     (if (read /audio/muted) "  muted" ""))
             (not (read /audio/muted)))
    (column :class "nm-card" :align :stretch
  (row :align :center :spacing 12
    (icon (if (read /audio/muted) #x0F026 #x0F028) :class "audio-mute"
          :click {/audio/muted [:toggle]})
        (slider /audio/volume :class "menu-slider" :min 0 :max 100 :expand 1)
        (label (format nil "~d%" (or (read /audio/volume) 0)) :class "audio-pct")))
    (column :class "nm-card nm-list-card" :align :stretch
  (label "Output" :class "nm-subhead")
      (scroll :height 160 :class "nm-scroll"
    (rows /audio/sinks/*
      (fn ()
            (let ((current (equal (leaf /.) (read /audio/sink))))
              (choice :class (if current "nm-row active" "nm-row")
                      :click {/audio/sink ${(leaf /.)}}
            (row :align :center :spacing 10
              (icon #x0F028 :class "nm-sig")
                  (label (read /./desc) :expand 1
                     :class (if current "nm-name active" "nm-name"))
                  (label (if current (string (code-char #xF012C)) "")
                         :class "nm-check"))))))))))

(write /surface/network {:as :panel})
(write /surface/network
  (column :class "netmenu" :align :stretch
    (nm-head #x0F1EB "Network" (or (read /net/connection) "Disconnected")
             (read /net/online))
    (column :class "nm-card nm-list-card" :align :stretch
  (scroll :height 260 :class "nm-scroll"
    (rows /net/wifi/*
      (fn ()
            (let ((up (read /./in-use)))
              (choice :class (if up "nm-row active" "nm-row")
                      :click {/. ${(if up [:disconnect] [:connect])}}
            (row :align :center :spacing 10
              (icon #x0F1EB :class (signal-class (read /./signal)))
                  (label (leaf /.) :expand 1
                     :class (if up "nm-name active" "nm-name"))
                  (label (if (read /./secure) (string (code-char #xF0341)) "")
                         :class "nm-lock")))))))
      (row :class "nm-actions" :align :center :spacing 8
    (button :class "nm-btn" :click {/net/wifi [:rescan]} (label "Scan"))))))

(write /surface/media {:as :panel})
(write /surface/media
  (let ((status (read /media/status)))
    (column :class "netmenu" :align :stretch
  (nm-head #x0F001 "Media" (if (eq status :playing) "Playing" "Idle")
               (eq status :playing))
      (if (null (read /media/title))
          (column :class "nm-card media-none-box" :align :center
        (label "Nothing playing" :class "media-none"))
          (column :class "nm-card" :align :stretch
        (row :class "media-info" :align :center :spacing 14
          (center :class "media-art"
            (if (read /media/art)
                    (image /media/art)
                    (icon #x0F075A :class "media-art-ico")))
              (column :expand 1
            (label /media/title  :class "media-title")
                (label /media/artist :class "media-artist")))
            (slider /media/position :class "menu-slider"
                :min 0 :max (or (read /media/length) 1) :expand 1)
            (row :class "media-times" :align :center
          (label (mmss (read /media/position)) :class "media-time" :expand 1)
              (label (mmss (read /media/length))   :class "media-time"))
            (row :class "media-ctrl" :align :center :spacing 28
          (icon #x0F048 :class "media-btn" :click {/media [:previous]})
              (icon (if (eq status :playing) #x0F04C #x0F04B) :class "media-btn"
                :click {/media [:pause]})
              (icon #x0F051 :class "media-btn" :click {/media [:next]})))))))

(defun ring-tile (glyph p cls name &optional (unit "%"))
  (column :class (format nil "ctl-ring-box ~a" cls) :align :center
    (ring p :class (format nil "ctl-ring ~a" cls)
          :min 0 :max 100 :thickness 5 :diameter 58
      :arc-face (ring-face cls) :track-face :ring-track
  (icon glyph :class "ctl-ring-ico"))
    (label (format nil "~a ~d~a" name (or (read p) 0) unit)
           :class "ctl-ring-lbl")))

(write /surface/ctl {:as :panel})
(write /surface/ctl
  (column :class "netmenu ctlpanel-box" :align :stretch
    (column :class "ctl-card" :align :stretch :spacing 12
  (row :class "ctl-profile" :align :center
    (center :class "ctl-pfp" (icon #x0F007 :class "ctl-pfp-ico"))
        (column :expand 1
      (label /sys/user :class "ctl-user")
          (label (uptime-string (read /sys/uptime)) :class "ctl-uptime")))
      (row :class "ctl-power" :align :center :spacing 8
    (icon #x0F023 :class "pw-a lock"    :click {/power [:lock]})
        (icon #x0F08B :class "pw-a logout"  :click {/power [:logout]}
          :confirm "Log out?")
        (icon #x0F021 :class "pw-a reboot"  :click {/power [:reboot]}
          :confirm "Reboot?")
        (icon #x0F186 :class "pw-a suspend" :click {/power [:suspend]})
        (icon #x0F011 :class "pw-a off"     :click {/power [:poweroff]}
          :confirm "Power off?")))
    (row :class "ctl-card ctl-rings" :align :center
  (ring-tile #x0F2DB  /sys/cpu  "cpu"  "CPU")
      (ring-tile #x0F1C0  /sys/ram  "ram"  "RAM")
      (ring-tile #x0F02CA /sys/disk "disk" "DSK")
      (ring-tile #x0F2C9  /sys/temp "temp" "TMP" "C"))
    (column :class "ctl-card" :align :stretch :spacing 16
  (row :class "ctl-srow" :align :center :spacing 14
    (icon #x0F028 :class "ctl-sico vol")
        (slider /audio/volume :class "ctl-scale vol" :min 0 :max 100 :expand 1))
      (row :class "ctl-srow" :align :center :spacing 14
    (icon #x0F185 :class "ctl-sico bri")
        (slider /screen/brightness :class "ctl-scale bri"
            :min 0 :max 100 :expand 1)))))

;;;; Editing.

(write /tab-width 2 :keep t)
(write /buf/*[mode = :lisp]/minor [:conj :paren])

;;;; Keys.

(write /wm-terminal "alacritty")

(write /key/wm/s-Return {/sh/${(read /wm-terminal)} t})
(write /key/wm/s-j      {/wm/focused [:next]})
(write /key/wm/s-k      {/wm/focused [:prev]})
(write /key/wm/s-2      {/wm [:split :below]})
(write /key/wm/s-3      {/wm [:split :beside]})
(write /key/wm/s-q      {/wm [:close]})
(write /key/wm/s-S-e    {/wm [:exit]})
(write /key/wm/s-?n{1..9} {:run (fn () {/wm/workspaces/${n}/focused t})})


;;;; The look of the surfaces above. These classes are this config's -- the bar
;;;; it declared, the panels it declared -- so the styles for them are here and
;;;; not in pine, which styles only what pine itself draws. A selector names its
;;;; own path, so styling one is a write like any other, and what a config wrote
;;;; comes last in the cascade.

;; bar
(write /style/bar {:background-color (css-glass :bg) :color (color :fg) :padding "8px 0 0 0"})
(write /style/bgroup {:border-radius (css-rad) :padding "6px 4px"})
(write /style/${".viewer, .picker, .launch, .app, .icon, .net, .media, .ctl, .ws"}
       {:color (color :fg-alt) :min-width "28px" :padding "8px 0"
        :border-radius (css-rad) :margin "3px 0" :font-size "15px"})
(write /style/net {:color (color :cyan)})
(write /style/ctl {:color (color :accent)})
(write /style/ws-current {:color (color :accent-fg) :background-color (color :accent)
                          :min-width "28px" :padding "8px 0"
                          :border-radius (css-rad) :margin "3px 0"})
(write /style/ws {:font-size "12px"})
(write /style/${".viewer:hover, .picker:hover, .launch:hover, .app:hover, .icon:hover, .net:hover, .media:hover, .ctl:hover, .clock:hover, .ws:hover"} {:background-color (color :bg-active) :color (color :accent-fg)})
(write /style/clock {:color (color :fg) :margin-top "6px"})
(write /style/${".clock .hour"} {:font-size "15px" :font-weight "bold"})
(write /style/${".clock .min"} {:color (color :fg-dim) :font-size "15px"})
(write /style/corner-sq {:background-image (format nil "linear-gradient(135deg, ~a, ~a)"
                                          (color :accent) (color :magenta))
                   :color (color :accent-fg) :min-width "28px" :min-height "28px"
               :border-radius "9px" :font-size "15px"})
;; per-glyph offsets (nerd-font glyphs are not centred in their em box)
(write /style/${".viewer .bar-glyph"} {:margin-right "2px"})
(write /style/${".picker .bar-glyph"} {:margin-right "2px"})
(write /style/${".launch .bar-glyph"} {:margin-right "0px"})
(write /style/${".term .bar-glyph"} {:margin-right "0px"})
(write /style/${".web .bar-glyph"} {:margin-right "0px"})
(write /style/${".files .bar-glyph"} {:margin-right "0px"})
(write /style/${".edit .bar-glyph"} {:margin-right "0px"})
(write /style/${".icon .bar-glyph"} {:margin-right "3px"})
(write /style/${".media .bar-glyph"} {:margin-right "5px"})
(write /style/${".net .bar-glyph"} {:margin-right "6px"})
(write /style/${".ctl .bar-glyph"} {:margin-right "4px"})
(write /style/${".workspaces .ws .bar-glyph"} {:margin-left "2px"})
(write /style/${".corner-sq .bar-glyph"} {:color (color :accent-fg) :font-size "15px"})
;; calendar
(write /style/cal-box {:background-color (css-glass :bg-dim) :border-radius (css-rad) :padding "10px"})
;; popup chrome
(write /style/netmenu {:background-color (css-glass :bg) :color (color :fg)
                 :border-width "1px" :border-style "solid" :border-color (color :border)
                 :border-radius (css-rad) :padding "8px" :margin "8px"
             :box-shadow (format nil "0 0 5px 0 ~a" (color :shadow))
                 :min-width "360px"})
(write /style/nm-card {:background-color (css-glass :bg-dim) :border-radius (css-rad) :padding "12px" :margin "6px"})
(write /style/nm-head {:padding "12px 14px"})
(write /style/nm-head-ico {:font-size "22px" :color (color :accent) :margin-right "14px"})
(write /style/nm-title {:font-size "17px" :font-weight "bold" :color (color :fg)})
(write /style/nm-sub {:font-size "13px"})
(write /style/${".nm-sub.on"} {:color (color :green)})
(write /style/${".nm-sub.off"} {:color (color :fg-dim)})
(write /style/nm-subhead {:font-size "13px" :color (color :fg-dim) :margin "2px 4px 8px 4px"})
(write /style/nm-list-card {:padding "6px"})
(write /style/nm-scroll {:min-height "14rem"})
(write /style/nm-row {:border-radius (css-rad) :padding "10px 12px" :margin "2px"
            :transition "background-color 0.2s"})
(write /style/${".nm-row:hover"} {:background-color (color :bg-active)})
(write /style/${".nm-row.active"} {:background-color (color :bg-alt)})
(write /style/${".nm-row.sel"} {:background-color (color :bg-active) :box-shadow (format nil "inset 3px 0 0 0 ~a" (color :accent))})
(write /style/nm-sig {:font-size "14px" :color (color :fg-dim)})
(write /style/${".nm-sig.hi"} {:color (color :green)})
(write /style/${".nm-sig.mid"} {:color (color :yellow)})
(write /style/${".nm-sig.lo"} {:color (color :red)})
(write /style/nm-name {:font-size "14px" :color (color :fg)})
(write /style/${".nm-name.active"} {:color (color :green) :font-weight "bold"})
(write /style/nm-lock {:font-size "12px" :color (color :fg-dim)})
(write /style/nm-check {:font-size "13px" :color (color :green)})
(write /style/nm-actions {:padding "6px 4px 2px 4px"})
(write /style/nm-btn {:background-color (color :bg-active) :color (color :fg) :padding "10px 20px"
            :border-radius (css-rad) :transition "background-color 0.2s, color 0.2s"})
(write /style/${".nm-btn:hover"} {:background-color (color :bg-alt)})
(write /style/${".nm-btn.go"} {:background-color (color :accent) :color (color :accent-fg)})
(write /style/${".nm-btn.no"} {:color (color :red)})
;; menu sliders (audio, media):
;; the * reset already stripped the Adwaita chrome
(write /style/${".menu-slider trough"} {:background-color (color :bg-alt) :min-height "8px" :border-radius "5px"})
(write /style/${".menu-slider trough highlight"} {:background-color (color :accent) :border-radius "5px"})
(write /style/${".menu-slider slider"} {:min-width "16px" :min-height "16px" :border-radius "8px" :background-color (color :fg)})
;; audio
(write /style/audio-mute {:color (color :cyan) :font-size "20px" :padding "0 4px"})
(write /style/${".audio-mute:hover"} {:color (color :accent)})
(write /style/audio-pct {:color (color :fg-dim) :min-width "38px"})
;; media
(write /style/media-info {:padding "2px 2px 12px 2px"})
(write /style/media-art {:min-width "64px" :min-height "64px" :border-radius (css-rad) :background-color (color :bg-active)})
(write /style/media-art-ico {:font-size "26px" :color (color :fg-dim)})
(write /style/media-title {:font-size "15px" :font-weight "bold" :color (color :fg)})
(write /style/media-artist {:color (color :fg-dim)})
(write /style/media-times {:padding "6px 2px 2px 2px"})
(write /style/media-time {:font-size "11px" :color (color :fg-dim)})
(write /style/media-ctrl {:padding "10px 0 2px 0"})
(write /style/media-btn {:color (color :fg) :font-size "20px" :padding "6px"})
(write /style/${".media-btn:hover"} {:color (color :accent)})
(write /style/media-none-box {:padding "8px"})
(write /style/media-none {:color (color :fg-dim) :padding "14px"})
;; control panel
(write /style/ctlpanel-box {:padding "4px"})
(write /style/ctl-card {:background-color (css-glass :bg-dim) :border-radius (css-rad) :padding "14px" :margin "6px"})
(write /style/ctl-profile {:padding "2px"})
(write /style/ctl-pfp {:background-color (color :accent) :border-radius "100%" :min-width "58px"
             :min-height "58px" :margin-right "16px"})
(write /style/ctl-pfp-ico {:font-size "28px" :color (color :accent-fg)})
(write /style/ctl-user {:font-size "24px" :font-weight "bold" :color (color :accent)})
(write /style/ctl-uptime {:color (color :fg-dim)})
(write /style/ctl-power {:background-color (css-glass :bg) :border-radius (css-rad) :padding "12px 8px" :margin-top "14px"})
(write /style/ctl-confirm {:background-color (css-glass :bg) :border-radius (css-rad) :padding "10px 8px" :margin-top "14px"})
(write /style/ctl-confirm-lbl {:color (color :fg) :font-size "14px" :font-weight "bold"})
(write /style/ctl-cf {:font-size "18px" :padding "4px 14px"})
(write /style/${".ctl-cf.yes"} {:color (color :green)})
(write /style/${".ctl-cf.no"} {:color (color :red)})
(write /style/${".ctl-cf:hover"} {:color (color :fg)})
(write /style/pw-a {:font-size "22px" :padding "4px 12px"})
(write /style/${".pw-a.lock"} {:color (color :blue)})
(write /style/${".pw-a.logout"} {:color (color :yellow)})
(write /style/${".pw-a.reboot"} {:color (color :magenta)})
(write /style/${".pw-a.suspend"} {:color (color :cyan)})
(write /style/${".pw-a.off"} {:color (color :red)})
(write /style/${".pw-a:hover"} {:color (color :fg)})
(write /style/ctl-rings {:padding "4px 0"})
(write /style/ctl-ring-box {:background-color (css-glass :bg) :border-radius (css-rad) :padding "10px 8px" :margin "0 4px"})
(write /style/ctl-ring-ico {:font-size "18px" :margin "14px"})
(write /style/${".ctl-ring-box.cpu .ctl-ring-ico"} {:margin-left "11px" :margin-right "17px"})
(write /style/${".ctl-ring-box.ram .ctl-ring-ico"} {:margin-left "11px" :margin-right "17px"})
(write /style/${".ctl-ring-box.disk .ctl-ring-ico"} {:margin-left "11px" :margin-right "17px"})
(write /style/ctl-ring-lbl {:font-size "11px" :margin-top "6px" :color (color :fg-dim)})
(write /style/${".ctl-ring-box.cpu .ctl-ring-ico, .ctl-ring-box.cpu .ctl-ring-lbl"} {:color (color :red)})
(write /style/${".ctl-ring-box.ram .ctl-ring-ico, .ctl-ring-box.ram .ctl-ring-lbl"} {:color (color :blue)})
(write /style/${".ctl-ring-box.disk .ctl-ring-ico, .ctl-ring-box.disk .ctl-ring-lbl"} {:color (color :green)})
(write /style/${".ctl-ring-box.temp .ctl-ring-ico, .ctl-ring-box.temp .ctl-ring-lbl"} {:color (color :yellow)})
(write /style/ctl-srow {:padding "4px 8px"})
(write /style/${".ctl-sico.vol"} {:color (color :accent) :font-size "17px"})
(write /style/${".ctl-sico.bri"} {:color (color :yellow) :font-size "17px"})
(write /style/${".ctl-scale trough"} {:background-color (color :bg) :min-height "10px" :min-width "180px" :border-radius "50px"})
(write /style/${".ctl-scale.vol trough highlight"} {:background-color (color :accent) :border-radius "10px"})
(write /style/${".ctl-scale.bri trough highlight"} {:background-color (color :yellow) :border-radius "10px"})
(write /style/${".ctl-scale slider"} {:min-width "14px" :min-height "14px" :border-radius "7px" :background-color (color :fg)})
;; the echo strip
(write /style/echo {:background-color "transparent"})
(write /style/echo-lead {:min-width "44px" :background-color "transparent"})
(write /style/echo-body {:background-color (css-glass :bg)})
(write /style/echo-text {:color (color :fg) :font-size "13px" :padding "0 12px"})
(write /style/echo-stat {:color (color :fg-dim) :font-size "12px" :padding "0 16px 0 8px"})
