(defpackage #:pine.desktop
  (:use #:cl)
  (:export #:install-desktop-sessions #:install-desktop-config
           #:defsurface #:push-surface #:show-panel))

(in-package #:pine.desktop)

;;;; The daemon-side desktop. It hosts NAMED surfaces -- the bar and each toggled
;;;; panel -- as declarative trees built from cells. A surface's tree is
;;;; serialized (node->wire) with its click handlers registered under ids, and
;;;; pushed to the desktop app as (:widgets :surface NAME :tree DATA). The app
;;;; renders each surface; interacting sends (:widget-action :id N ...) back and
;;;; the daemon runs the closure. Panels toggle via (:panel :name NAME :show B).
;;;; The tree crosses as data; the closures and the cells stay here.

;;;; Surface registry: name -> builder (aclient -> node tree).

(defvar *surfaces* (make-hash-table :test 'equal))
(defun defsurface (name builder) (setf (gethash name *surfaces*) builder))

(defstruct dsession
  (actions (make-hash-table))                     ; id -> closure
  (surface-ids (make-hash-table :test 'equal))    ; surface -> ids, to drop stale
  (counter 0)
  (open nil)                                       ; currently open panel, or nil
  (views (make-hash-table :test 'equal)))          ; surface -> reactive view

(defun push-surface (aclient name)
  "Build surface NAME's tree, serialize it (fresh ids, dropping this surface's
old ones), and push it to the app."
  (let ((s (pine.attach:attached-client-session aclient))
        (builder (gethash name *surfaces*)))
    (when (and s builder)
      (dolist (id (gethash name (dsession-surface-ids s)))
        (remhash id (dsession-actions s)))
      (setf (gethash name (dsession-surface-ids s)) nil)
      (let ((data (pine.layout:node->wire
                   (funcall builder aclient)
                   :on-action (lambda (cb)
                                (let ((id (incf (dsession-counter s))))
                                  (setf (gethash id (dsession-actions s)) cb)
                                  (push id (gethash name (dsession-surface-ids s)))
                                  id)))))
        (pine.attach:push-to-app aclient :widgets :surface name :tree data)))))

(defun surface-view (aclient name)
  "A reactive view that re-pushes surface NAME when a cell it reads changes."
  (let (view)
    (setf view (pine.cell:make-view
                (lambda () (push-surface aclient name))
                (lambda () (ignore-errors (pine.cell:render-view view)))))
    view))

(defun make-desktop-session (aclient)
  (let ((s (make-dsession)))
    (setf (pine.attach:attached-client-session aclient) s)
    (dolist (name '("bar" "echo"))
      (let ((v (surface-view aclient name)))
        (setf (gethash name (dsession-views s)) v)
        (pine.cell:render-view v)))))

(defun show-panel (aclient name)
  "Toggle panel NAME: open it (closing any other) and keep it live while open, or
close it if it is already the open one."
  (let ((s (pine.attach:attached-client-session aclient)))
    (when s
      (let ((cur (dsession-open s)))
        (cond
          ((equal cur name)
           (setf (dsession-open s) nil)
           (remhash name (dsession-views s))
           (pine.attach:push-to-app aclient :panel :name name :show nil))
          (t
           (when cur
             (remhash cur (dsession-views s))
             (pine.attach:push-to-app aclient :panel :name cur :show nil))
           (setf (dsession-open s) name)
           (let ((v (surface-view aclient name)))
             (setf (gethash name (dsession-views s)) v)
             (pine.cell:render-view v))
           (pine.attach:push-to-app aclient :panel :name name :show t)))))))

(defun desktop-input (aclient msg)
  (let ((s (pine.attach:attached-client-session aclient)))
    (case (first msg)
      (:widget-action
       ;; run the handler through pine.eval (its own thread), never inline on the
       ;; pool -- a handler that blocks (nmcli) or errors can't stall the daemon.
       (destructuring-bind (&key id args) (rest msg)
         (let ((cb (and s (gethash id (dsession-actions s)))))
           (when cb
             (pine.eval:evaluate-thunk (lambda () (apply cb args))
                                       :package (find-package :pine-user))))))
      ;; the app asks for a fresh push once its surfaces exist (its first push on
      ;; attach can arrive before the GTK windows are up). Re-push the bar and any
      ;; open panel.
      (:refresh
       (push-surface aclient "bar")
       (push-surface aclient "echo")
       (when (and s (dsession-open s)) (push-surface aclient (dsession-open s))))
      (:hint
       (destructuring-bind (&key text) (rest msg)
         (pine.cell:set-cell (pine.cell:defcell :hint "") (or text "")))))))

(defun install-desktop-sessions ()
  "Wire the daemon to host a desktop session per attaching :desktop app."
  (pine.attach:register-app-kind :desktop
    :on-attach (lambda (c) (make-desktop-session c))
    :on-input  (lambda (c msg) (desktop-input c msg))))


;;;; The widget vocabulary, daemon-side (pine.layout only, no GTK). A card is a
;;;; rounded padded box; a header an accent glyph + title + status; a pill a
;;;; padded rounded button; a gauge a glyph + meter + value.

(defun sh (cmd) (lambda () (ignore-errors (uiop:launch-program (list "sh" "-c" cmd)))))
(defun cell (name default)
  (let ((c (pine.cell:find-cell name))) (if c (pine.cell:cell-ref c) default)))

(defun ib (glyph class &key on-click hint)
  (pine.layout:icon glyph :class class :glyph-class "bar-glyph"
                    :on-click on-click :hint hint :font-px 15))

;;;; Shell actions.

(defun set-volume (v)
  (ignore-errors (uiop:launch-program
                  (list "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" (format nil "~a%" v)))))
(defun set-brightness (v)
  (ignore-errors (uiop:launch-program (list "brightnessctl" "set" (format nil "~a%" v)))))
(defun emms (form) (sh (format nil "emacsclient -e '~a'" form)))
(defun emms-seek (v)
  (ignore-errors (uiop:launch-program
                  (list "emacsclient" "-e" (format nil "(emms-seek-to ~a)" v)))))
(defun mmss (secs)
  (let ((s (truncate (or secs 0)))) (format nil "~d:~2,'0d" (floor s 60) (mod s 60))))

;;;; The bar.

(defun ws-tile (w)
  (let ((idx (getf w :idx)))
    (pine.layout:icon (princ-to-string idx) :font-px 15 :glyph-class "bar-glyph"
      :class (if (getf w :focused) "ws ws-current" "ws")
      :on-click (sh (format nil "niri msg action focus-workspace ~a" idx)))))

(defun clock-button (aclient)
  (multiple-value-bind (s m h) (decode-universal-time (cell :clock (get-universal-time)))
    (declare (ignore s))
    (pine.layout:button :class "clock" :hint "Calendar"
                        :on-click (lambda () (show-panel aclient "calendar"))
      (pine.layout:column :align :center
        (pine.layout:label (format nil "~2,'0d" h) :class "hour")
        (pine.layout:label (format nil "~2,'0d" m) :class "min")))))

(defun bar-top (aclient)
  (declare (ignore aclient))
  (pine.layout:column :align :center :spacing 12
    (pine.layout:column :class "bgroup grp-overview" :align :center
      (ib #xF02C1 "viewer" :hint "Overview" :on-click (sh "niri msg action toggle-overview")))
    (pine.layout:column :class "bgroup grp-nav" :align :center :spacing 6
      (ib #x0F002 "picker" :hint "Search windows"
          :on-click (sh "cd ~ && setsid -f bb ~/.config/eww/niri-window-switch.bb"))
      (apply #'pine.layout:column :class "workspaces" :align :center :spacing 4
             (mapcar #'ws-tile (cell :workspaces nil))))))

(defun bar-middle (aclient)
  (declare (ignore aclient))
  (pine.layout:column :class "bgroup grp-apps" :align :center :spacing 8
    (ib #xF003B "launch" :hint "Applications" :on-click (sh "setsid -f fuzzel"))
    (ib #x0F120 "app term" :hint "Terminal" :on-click (sh "setsid -f alacritty"))
    (ib #x0F268 "app web" :hint "Browser" :on-click (sh "setsid -f google-chrome"))
    (ib #x0F07B "app files" :hint "Files" :on-click (sh "setsid -f nautilus"))
    (ib #x0F121 "app edit" :hint "Editor" :on-click (sh "cd ~ && emacsclient -c -n"))))

(defun bar-bottom (aclient)
  (pine.layout:column :align :center :spacing 12
    (pine.layout:column :class "bgroup grp-tray" :align :center :spacing 10
      (ib #x0F028 "icon" :hint "Volume" :on-click (lambda () (show-panel aclient "audio")))
      (ib #x0F001 "media" :hint "Media" :on-click (lambda () (show-panel aclient "media")))
      (ib #x0F1EB "net" :hint "Network" :on-click (lambda () (show-panel aclient "network"))))
    (clock-button aclient)
    (ib #x0F007 "corner-sq" :hint "System"
        :on-click (lambda () (show-panel aclient "ctl")))))

(defun sidebar-tree (aclient)
  (pine.layout:centerbox :orient :v :class "bar"
    :start (bar-top aclient)
    :center (bar-middle aclient)
    :end (bar-bottom aclient)))

(defun echo-surface (aclient)
  (declare (ignore aclient))
  (let ((hint (cell :hint "")) (title (cell :wintitle ""))
        (user (cell :user "user")) (host (cell :host ""))
        (net (cell :net "")) (vol (cell :vol 0)) (muted (cell :muted nil)))
    (pine.layout:row :class "echo" :align :center
      (pine.layout:column :class "echo-lead")
      (pine.layout:row :class "echo-body" :align :center :expand 1
        (pine.layout:label (cond ((plusp (length hint)) hint)
                                 ((plusp (length title)) title)
                                 (t (format nil "~a@~a" user host)))
                           :class "echo-text" :expand 1)
        (pine.layout:label (format nil "~a   ~a ~d%"
                                   (if (plusp (length net)) net "offline")
                                   (string (code-char (if muted #x0F075F #x0F057E))) vol)
                           :class "echo-stat")))))

;;;; Popup vocabulary (eww nm-* / ctl-*).

(defun nm-head (glyph title sub on)
  (pine.layout:row :class "nm-card nm-head" :align :center
    (pine.layout:icon glyph :class "nm-head-ico")
    (pine.layout:column :expand 1
      (pine.layout:label title :class "nm-title")
      (pine.layout:label sub :class (if on "nm-sub on" "nm-sub")))))

(defun nm-card (&rest nodes)
  (apply #'pine.layout:column :class "nm-card" :align :stretch nodes))

;;;; Calendar.

(defun %days-in-month (mo y)
  (if (= mo 2)
      (if (or (and (zerop (mod y 4)) (plusp (mod y 100))) (zerop (mod y 400))) 29 28)
      (aref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- mo))))

(defun %first-dow (mo y)
  (multiple-value-bind (s m h d dm dow) (decode-universal-time (encode-universal-time 0 0 12 1 mo y))
    (declare (ignore s m h d dm))
    (mod (1+ dow) 7)))

(defun cal-cell (text kind)
  (pine.layout:centered :class kind (pine.layout:label text)))

(defparameter +month-names+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun calendar-panel (aclient)
  (declare (ignore aclient))
  (multiple-value-bind (s m h day mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (pine.layout:column :class "netmenu" :align :stretch
      (pine.layout:column :class "cal-box" :align :stretch
        (pine.layout:cal :year y :month mo :day day)))))

;;;; Audio.

(defun sink-row (s)
  (pine.layout:button :class (if (getf s :default) "nm-row active" "nm-row")
    :on-click (lambda () (pine.source:select-sink! (getf s :name)))
    (pine.layout:row :align :center :spacing 10
      (pine.layout:icon #x0F028 :class "nm-sig")
      (pine.layout:label (getf s :desc) :expand 1
                         :class (if (getf s :default) "nm-name active" "nm-name"))
      (pine.layout:label (if (getf s :default) (string (code-char #xF012C)) "") :class "nm-check"))))

(defun audio-panel (aclient)
  (declare (ignore aclient))
  (let ((vol (cell :vol 0)) (muted (cell :muted nil)) (sinks (cell :sinks nil)))
    (pine.layout:column :class "netmenu" :align :stretch
      (nm-head #x0F028 "Audio" (format nil "~d%~a" vol (if muted "  muted" "")) (not muted))
      (nm-card
       (pine.layout:row :align :center :spacing 12
         (pine.layout:icon (if muted #x0F026 #x0F028) :class "audio-mute"
           :on-click (sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
         (pine.layout:meter :class "menu-slider" :value vol :min 0 :max 100 :expand 1
                            :on-change #'set-volume)
         (pine.layout:label (format nil "~d%" vol) :class "audio-pct")))
      (pine.layout:column :class "nm-card nm-list-card" :align :stretch
        (pine.layout:label "Output" :class "nm-subhead")
        (pine.layout:viewport :height 160 :class "nm-scroll"
          (apply #'pine.layout:column :align :stretch :spacing 2 (mapcar #'sink-row sinks)))))))

;;;; Network.

(defun wifi-row (n netsel)
  (pine.layout:button :class (cond ((getf n :in_use) "nm-row active")
                                   ((equal (getf n :ssid) netsel) "nm-row sel")
                                   (t "nm-row"))
    :on-click (lambda () (pine.source:select! (getf n :ssid)))
    (pine.layout:row :align :center :spacing 10
      (pine.layout:icon #x0F1EB
        :class (cond ((equal (getf n :sig) "hi") "nm-sig hi")
                     ((equal (getf n :sig) "mid") "nm-sig mid")
                     (t "nm-sig lo")))
      (pine.layout:label (getf n :ssid) :expand 1
                         :class (if (getf n :in_use) "nm-name active" "nm-name"))
      (pine.layout:label (if (getf n :secure) (string (code-char #xF0341)) "") :class "nm-lock"))))

(defun act-btn (a)
  (pine.layout:button :class (cond ((equal (getf a :style) "go") "nm-btn go")
                                   ((equal (getf a :style) "no") "nm-btn no")
                                   (t "nm-btn"))
    :on-click (lambda () (pine.source:act! (getf a :kind)))
    (pine.layout:label (getf a :label))))

(defun network-panel (aclient)
  (declare (ignore aclient))
  (let ((net (cell :net "")) (list (cell :netlist nil)) (acts (cell :netactions nil))
        (netsel (cell :netsel "")))
    (pine.layout:column :class "netmenu" :align :stretch
      (nm-head #x0F1EB "Network" (if (plusp (length net)) net "Disconnected") (plusp (length net)))
      (pine.layout:column :class "nm-card nm-list-card" :align :stretch
        (pine.layout:viewport :height 260 :class "nm-scroll"
          (apply #'pine.layout:column :align :stretch :spacing 2
                 (mapcar (lambda (n) (wifi-row n netsel)) list)))
        (apply #'pine.layout:row :class "nm-actions" :align :center :spacing 8
               (mapcar #'act-btn acts))))))

;;;; Media.

(defun media-panel (aclient)
  (declare (ignore aclient))
  (let* ((m (cell :media nil))
         (status (or (getf m :status) "Stopped"))
         (pos (or (getf m :pos) 0))
         (len (max 1 (or (getf m :length) 1))))
    (pine.layout:column :class "netmenu" :align :stretch
      (nm-head #x0F001 "Media" (if (equal status "Stopped") "Idle" status) (equal status "Playing"))
      (if (equal status "Stopped")
          (pine.layout:column :class "nm-card media-none-box" :align :center
            (pine.layout:label "Nothing playing" :class "media-none"))
          (nm-card
           (pine.layout:row :class "media-info" :align :center :spacing 14
             (let ((art (cell :art "")))
               (pine.layout:centered :class "media-art"
                 (if (plusp (length art))
                     (pine.layout:pic art)
                     (pine.layout:icon #x0F075A :class "media-art-ico"))))
             (pine.layout:column :expand 1
               (pine.layout:label (or (getf m :title) "") :class "media-title")
               (pine.layout:label (or (getf m :artist) "") :class "media-artist")))
           (pine.layout:meter :class "menu-slider" :value pos :min 0 :max len :expand 1
                              :on-change #'emms-seek)
           (pine.layout:row :class "media-times" :align :center
             (pine.layout:label (mmss pos) :class "media-time" :expand 1)
             (pine.layout:label (mmss len) :class "media-time"))
           (pine.layout:row :class "media-ctrl" :align :center :spacing 28
             (pine.layout:icon #x0F048 :class "media-btn" :on-click (emms "(emms-previous)"))
             (pine.layout:icon (if (equal status "Playing") #x0F04C #x0F04B) :class "media-btn"
                               :on-click (emms "(emms-pause)"))
             (pine.layout:icon #x0F051 :class "media-btn" :on-click (emms "(emms-next)"))))))))

;;;; Control panel.

(defun ring-face (cls)
  (cond ((equal cls "cpu") :error) ((equal cls "ram") :function-name)
        ((equal cls "disk") :string) (t :variable-param)))

(defun ring-tile (glyph value cls name &optional (unit "%"))
  (pine.layout:column :class (format nil "ctl-ring-box ~a" cls) :align :center
    (pine.layout:ring :class (format nil "ctl-ring ~a" cls)
                      :value value :min 0 :max 100 :thickness 5 :diameter 58
                      :arc-face (ring-face cls) :track-face :comment
      (pine.layout:icon glyph :class "ctl-ring-ico"))
    (pine.layout:label (format nil "~a ~d~a" name value unit) :class "ctl-ring-lbl")))

(defun pw-ask (cmd label)
  (lambda () (pine.source:set! :pwcmd cmd) (pine.source:set! :pwlabel label)))
(defun pw-clear () (pine.source:set! :pwcmd "") (pine.source:set! :pwlabel ""))
(defun pw-run (cmd)
  (lambda () (ignore-errors (uiop:launch-program (list "sh" "-c" cmd))) (pw-clear)))

(defun ctl-power-row ()
  (let ((pw (cell :pwcmd "")))
    (if (plusp (length pw))
        (pine.layout:row :class "ctl-confirm" :align :center :spacing 8
          (pine.layout:label (format nil "~a?" (cell :pwlabel "")) :class "ctl-confirm-lbl" :expand 1)
          (pine.layout:icon #x0F012C :class "ctl-cf yes" :on-click (pw-run pw))
          (pine.layout:icon #x0F0156 :class "ctl-cf no" :on-click (lambda () (pw-clear))))
        (pine.layout:row :class "ctl-power" :align :center :spacing 8
          (pine.layout:icon #x0F023 :class "pw-a lock" :on-click (sh "loginctl lock-session"))
          (pine.layout:icon #x0F08B :class "pw-a logout" :on-click (pw-ask "niri msg action quit" "Log out"))
          (pine.layout:icon #x0F021 :class "pw-a reboot" :on-click (pw-ask "loginctl reboot" "Reboot"))
          (pine.layout:icon #x0F186 :class "pw-a suspend" :on-click (sh "loginctl suspend"))
          (pine.layout:icon #x0F011 :class "pw-a off" :on-click (pw-ask "loginctl poweroff" "Power off"))))))

(defun ctl-panel (aclient)
  (declare (ignore aclient))
  (let ((sys (cell :sys nil)) (vol (cell :vol 0)) (bri (cell :bri 50)))
    (pine.layout:column :class "netmenu ctlpanel-box" :align :stretch
      (pine.layout:column :class "ctl-card" :align :stretch :spacing 12
        (pine.layout:row :class "ctl-profile" :align :center
          (pine.layout:centered :class "ctl-pfp"
            (pine.layout:icon #x0F007 :class "ctl-pfp-ico"))
          (pine.layout:column :expand 1
            (pine.layout:label (cell :user "user") :class "ctl-user")
            (pine.layout:label (cell :uptime "") :class "ctl-uptime")))
        (ctl-power-row))
      (pine.layout:row :class "ctl-card ctl-rings" :align :center
        (ring-tile #x0F2DB (or (getf sys :cpu) 0)  "cpu" "CPU")
        (ring-tile #x0F1C0 (or (getf sys :ram) 0)  "ram" "RAM")
        (ring-tile #x0F02CA (or (getf sys :disk) 0) "disk" "DSK")
        (ring-tile #x0F2C9 (or (getf sys :temp) 0) "temp" "TMP" "C"))
      (pine.layout:column :class "ctl-card" :align :stretch :spacing 16
        (pine.layout:row :class "ctl-srow" :align :center :spacing 14
          (pine.layout:icon #x0F028 :class "ctl-sico vol")
          (pine.layout:meter :class "ctl-scale vol" :value vol :min 0 :max 100 :expand 1
                             :on-change #'set-volume))
        (pine.layout:row :class "ctl-srow" :align :center :spacing 14
          (pine.layout:icon #x0F185 :class "ctl-sico bri")
          (pine.layout:meter :class "ctl-scale bri" :value bri :min 0 :max 100 :expand 1
                             :on-change #'set-brightness))))))

(defun install-desktop-config ()
  "Register the daemon's desktop surfaces (the bar + the toggled panels)."
  (pine.cell:defcell :hint "")
  (pine.cell:defcell :clock (get-universal-time))
  (pine.cell:defcell :pwcmd "")
  (pine.cell:defcell :pwlabel "")
  (defsurface "bar" #'sidebar-tree)
  (defsurface "echo" #'echo-surface)
  (defsurface "calendar" #'calendar-panel)
  (defsurface "audio" #'audio-panel)
  (defsurface "network" #'network-panel)
  (defsurface "media" #'media-panel)
  (defsurface "ctl" #'ctl-panel))
