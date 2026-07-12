(in-package #:pine.desktop)

;;;; One desktop, built on pine.de. This is config, not framework: the specific
;;;; sidebar, panels, glyphs, and shell wiring for this user's niri session. Edit
;;;; and re-eval it live from the editor -- the reactive views re-render, and the
;;;; set-bar! / defpanel registrations at the end are idempotent.

;;;; Nerd-font glyph codepoints (numeric so the source stays ASCII).
(defparameter *g-overview* #xF02C1)
(defparameter *g-search*   #x0F002)
(defparameter *g-apps*     #xF003B)
(defparameter *g-term*     #x0F120)
(defparameter *g-web*      #x0F268)
(defparameter *g-files*    #x0F07B)
(defparameter *g-edit*     #x0F121)
(defparameter *g-vol*      #x0F028)
(defparameter *g-mute*     #x0F026)
(defparameter *g-media*    #x0F001)
(defparameter *g-prev*     #x0F048)
(defparameter *g-pause*    #x0F04C)
(defparameter *g-play*     #x0F04B)
(defparameter *g-next*     #x0F051)
(defparameter *g-net*      #x0F1EB)
(defparameter *g-system*   #x0F007)
(defparameter *g-lock2*    #x0F023)
(defparameter *g-logout*   #x0F08B)
(defparameter *g-reboot*   #x0F021)
(defparameter *g-suspend*  #x0F186)
(defparameter *g-off*      #x0F011)
(defparameter *g-cpu*      #x0F2DB)
(defparameter *g-ram*      #x0F1C0)
(defparameter *g-temp*     #x0F2C9)
(defparameter *g-disk*     #x0F02CA)
(defparameter *g-bri*      #x0F185)

;;;; Shell actions the widgets invoke.
(defun set-volume (v)
  (launch "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" (format nil "~a%" v)))
(defun set-brightness (v)
  (launch "brightnessctl" "set" (format nil "~a%" v)))
(defun emms (form) (sh (format nil "emacsclient -e '~a'" form)))
(defun emms-seek (v) (launch "emacsclient" "-e" (format nil "(emms-seek-to ~a)" v)))
(defun mmss (secs)
  (let ((s (truncate (or secs 0)))) (format nil "~d:~2,'0d" (floor s 60) (mod s 60))))

;;;; The sidebar. Each clickable icon is eww's `ib`: a 28px rounded cell, 8px
;;;; vertical padding, glyph centred, hover highlight.

(defun ib (glyph &key on-click hint (face :variable))
  (icon glyph :on-click on-click :hint hint :face face
              :font-px 15 :min-w 28 :pad-y 8 :radius 8))

(defwidget ws-item (w)
  (let ((idx (princ-to-string (getf w :idx)))
        (click (sh (format nil "niri msg action focus-workspace ~a" (getf w :idx)))))
    (if (getf w :focused)
        (icon idx :on-click click :hint "Workspace" :face :default
                  :font-px 15 :min-w 28 :pad-y 6 :radius 8 :fill "#675072")
        (ib idx :on-click click :hint "Workspace"
                :face (if (getf w :urgent) :constant :variable)))))

(defwidget sidebar-top ()
  (column :align :center :spacing 12
    (ib *g-overview* :on-click (sh "niri msg action toggle-overview") :hint "Overview")
    (column :align :center :spacing 4
      (ib *g-search* :on-click (sh "cd ~ && setsid -f bb ~/.config/eww/niri-window-switch.bb")
                     :hint "Search windows")
      (mapcar #'ws-item (cell :workspaces nil)))))

(defwidget sidebar-apps ()
  (column :align :center :spacing 8
    (ib *g-apps*  :on-click (sh "setsid -f fuzzel")          :hint "Applications")
    (ib *g-term*  :on-click (sh "setsid -f alacritty")       :hint "Terminal")
    (ib *g-web*   :on-click (sh "setsid -f google-chrome")   :hint "Browser")
    (ib *g-files* :on-click (sh "setsid -f nautilus")        :hint "Files")
    (ib *g-edit*  :on-click (sh "cd ~ && emacsclient -c -n") :hint "Editor")))

(defwidget clock ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (button :on-click (toggle "calendar") :hint "Calendar" :min-w 28 :pad-y 6 :radius 8
      (column :align :center
        (label (format nil "~2,'0d" h) :face :default :font-px 15)
        (label (format nil "~2,'0d" m) :face :comment :font-px 15)))))

(defwidget corner ()
  (button :on-click (toggle "ctl") :hint "System" :min-w 28 :min-h 28 :radius 9
          :fill "#675072" :grad "#ffaacf"
    (icon *g-system* :font-px 15 :face :default)))

(defwidget sidebar-tray ()
  (column :align :center :spacing 10
    (ib *g-vol*   :on-click (toggle "audio")   :hint "Volume")
    (ib *g-media* :on-click (toggle "media")   :hint "Media")
    (ib *g-net*   :on-click (toggle "network") :hint "Network" :face :builtin)
    (clock)
    (corner)))

(defwidget sidebar ()
  (column :align :center :pad-y 8
    (sidebar-top)
    (gap)
    (sidebar-apps)
    (gap)
    (sidebar-tray)))

;;;; Calendar: a real month grid, today marked with an accent pill.

(defparameter *month-names*
  #("" "January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))
(defparameter *day-headers* #("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))

(defun days-in-month (mo y)
  (aref #(0 31 28 31 30 31 30 31 31 30 31 30 31)
        (if (and (= mo 2)
                 (or (and (zerop (mod y 4)) (plusp (mod y 100))) (zerop (mod y 400))))
            0 mo)))

(defun month-first-weekday (mo y)
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 12 1 mo y))))

(defwidget cal-cell (day today)
  (if (zerop day)
      (label "" :min-w 30 :pad-y 4)
      (if (= day today)
          (label (princ-to-string day) :face :default :font-px 13
                 :min-w 30 :pad-y 4 :radius 8 :fill "#675072")
          (label (princ-to-string day) :face :default :font-px 13 :min-w 30 :pad-y 4))))

(defwidget calendar-panel ()
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (let* ((lead (month-first-weekday mo y))
           (dim  (days-in-month mo y))
           (cells (append (make-list lead :initial-element 0)
                          (loop for i from 1 to dim collect i)))
           (weeks (loop for row on cells by (lambda (l) (nthcdr 7 l))
                        collect (subseq row 0 (min 7 (length row))))))
      (column :pad 8 :spacing 6
        (card
         (label (format nil "~a ~d" (aref *month-names* mo) y) :face :accent :font-px 17)
         (row :spacing 0
           (map 'list (lambda (hd) (label hd :face :comment :font-px 12 :min-w 30 :pad-y 2))
                *day-headers*))
         (apply #'column :spacing 2
                (mapcar (lambda (week)
                          (apply #'row :spacing 0
                                 (mapcar (lambda (day) (cal-cell day d)) week)))
                        weeks)))))))

;;;; Audio: mute + volume + the output-device list.

(defwidget sink-row (s)
  (pill :on-click (lambda () (pine.source:select-sink! (getf s :name)))
    (row :spacing 10 :align :center
      (icon *g-vol* :font-px 14 :face (if (getf s :default) :string :comment))
      (label (getf s :desc) :expand 1
             :face (if (getf s :default) :string :default))
      (label (if (getf s :default) (string (code-char #xF012C)) "")
             :face :string :font-px 13))))

(defwidget audio-panel ()
  (let ((vol (cell :vol 0)) (muted (cell :muted nil)) (sinks (cell :sinks nil)))
    (column :pad 8 :spacing 6 :min-w 340
      (header *g-vol* "Audio"
              (format nil "~d%~a" vol (if muted "  muted" ""))
              (if muted :comment :string))
      (card
       (row :spacing 12 :align :center
         (button :pad 4 :radius 8 :on-click (sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
           (icon (if muted *g-mute* *g-vol*) :face :builtin :font-px 20))
         (meter :value vol :min 0 :max 100 :expand 1 :on-change #'set-volume)
         (label (format nil "~3d%" vol) :face :comment :min-w 38)))
      (card
       (label "Output" :face :comment :font-px 13)
       (viewport :height 160
         (column :spacing 2 (mapcar #'sink-row sinks)))))))

;;;; Network: status + scrollable wifi list + action buttons.

(defwidget wifi-row (n)
  (pill :on-click (lambda () (pine.source:select! (getf n :ssid)))
    (row :spacing 10 :align :center
      (icon *g-net* :font-px 14
            :face (cond ((equal (getf n :sig) "hi")  :string)
                        ((equal (getf n :sig) "mid") :variable-param)
                        (t :error)))
      (label (getf n :ssid) :expand 1 :face (if (getf n :in_use) :string :default))
      (label (if (getf n :secure) (string (code-char #xF0341)) "") :face :comment :font-px 12)
      (label (if (getf n :in_use) (string (code-char #xF012C)) "") :face :string :font-px 13))))

(defwidget act-btn (a)
  (button :pad-x 20 :pad-y 10 :radius 8
          :fill (if (equal (getf a :style) "go") "#675072" "#3b393e")
          :on-click (lambda () (pine.source:act! (getf a :kind)))
    (label (getf a :label)
           :face (if (equal (getf a :style) "no") :error :default))))

(defwidget network-panel ()
  (let ((net  (cell :net ""))
        (list (cell :netlist nil))
        (acts (cell :netactions nil)))
    (column :pad 8 :spacing 6 :min-w 380
      (header *g-net* "Network"
              (if (plusp (length net)) net "Disconnected")
              (if (plusp (length net)) :string :comment))
      (card
       (viewport :height 260
         (column :spacing 2 (mapcar #'wifi-row list))))
      (row :spacing 8 :align :center (mapcar #'act-btn acts)))))

;;;; Media: title/artist, seek slider with times, transport.

(defwidget media-panel ()
  (let* ((m (cell :media nil))
         (status (or (getf m :status) "Stopped"))
         (pos (or (getf m :pos) 0))
         (len (max 1 (or (getf m :length) 1))))
    (column :pad 8 :spacing 6 :min-w 360
      (header *g-media* "Media"
              (if (equal status "Stopped") "Idle" status)
              (if (equal status "Playing") :string :comment))
      (card
       (label (or (getf m :title) "")  :face :default :font-px 15)
       (label (or (getf m :artist) "") :face :comment :font-px 13)
       (meter :value pos :min 0 :max len :expand 1 :on-change #'emms-seek)
       (row :spacing 0 :align :center
         (label (mmss pos) :face :comment :font-px 12 :expand 1)
         (label (mmss len) :face :comment :font-px 12))
       (row :spacing 28 :align :center
         (button :pad 6 :radius 8 :on-click (emms "(emms-previous)")
           (icon *g-prev* :face :default :font-px 20))
         (button :pad 6 :radius 8 :on-click (emms "(emms-pause)")
           (icon (if (equal status "Playing") *g-pause* *g-play*) :face :default :font-px 20))
         (button :pad 6 :radius 8 :on-click (emms "(emms-next)")
           (icon *g-next* :face :default :font-px 20)))))))

;;;; Control: profile, power, gauges, sliders.

(defwidget gauge (glyph value face)
  (row :spacing 12 :align :center
    (icon glyph :face face :font-px 18)
    (meter :value value :min 0 :max 100 :expand 1)
    (label (format nil "~3d" value) :face :default :min-w 30)))

(defwidget ctl-panel ()
  (let ((sys (cell :sys nil)) (vol (cell :vol 0)) (bri (cell :bri 50)))
    (column :pad 8 :spacing 6 :min-w 360
      (card
       (row :spacing 14 :align :center
         (icon *g-system* :face :accent :font-px 26 :min-w 44 :pad 6 :radius 8 :fill "#3b393e")
         (column :spacing 2
           (label (cell :user "user") :face :default :font-px 17)
           (label (cell :uptime "") :face :comment :font-px 13))))
      (card
       (row :spacing 8 :align :center
         (button :pad 8 :radius 8 :on-click (sh "loginctl lock-session")
           (icon *g-lock2* :face :function-name :font-px 20))
         (button :pad 8 :radius 8 :on-click (sh "niri msg action quit")
           (icon *g-logout* :face :variable-param :font-px 20))
         (button :pad 8 :radius 8 :on-click (sh "loginctl reboot")
           (icon *g-reboot* :face :accent :font-px 20))
         (button :pad 8 :radius 8 :on-click (sh "loginctl suspend")
           (icon *g-suspend* :face :builtin :font-px 20))
         (button :pad 8 :radius 8 :on-click (sh "loginctl poweroff")
           (icon *g-off* :face :error :font-px 20))))
      (card
       (gauge *g-cpu*  (or (getf sys :cpu) 0)  :error)
       (gauge *g-ram*  (or (getf sys :ram) 0)  :function-name)
       (gauge *g-disk* (or (getf sys :disk) 0) :accent)
       (gauge *g-temp* (or (getf sys :temp) 0) :variable-param))
      (card
       (row :spacing 14 :align :center
         (icon *g-vol* :face :accent :font-px 17)
         (meter :value vol :min 0 :max 100 :expand 1 :on-change #'set-volume))
       (row :spacing 14 :align :center
         (icon *g-bri* :face :variable-param :font-px 17)
         (meter :value bri :min 0 :max 100 :expand 1 :on-change #'set-brightness))))))

;;;; Register this desktop with the framework.

(set-bar! #'sidebar :width 44)
(defpanel "calendar" #'calendar-panel :width 264 :height 300)
(defpanel "audio"    #'audio-panel    :width 360 :height 360)
(defpanel "network"  #'network-panel  :width 420 :height 470)
(defpanel "media"    #'media-panel    :width 380 :height 280)
(defpanel "ctl"      #'ctl-panel      :width 400 :height 470)
