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
