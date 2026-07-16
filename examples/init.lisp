;;;; Sample ~/.config/pine/init.lisp -- the desktop defined in the pine.user
;;;; language. The daemon loads this at boot; nothing about the bar is in the
;;;; engine. Copy to ~/.config/pine/init.lisp and edit.

(in-package :pine.user)

(load-theme :ef-dream)

(defsurface bar (:as :bar)
  (centerbox :orient :v :class "bar"
    :start (column :class "bgroup" :align :center
             (icon #xF02C1 :class "viewer"
                   :on-click (launch "niri msg action toggle-overview") :hint "Overview"))
    :center (column :class "bgroup grp-apps" :align :center :spacing 8
              (icon #x0F120 :class "app" :on-click (launch "alacritty") :hint "Terminal")
              (icon #x0F268 :class "app" :on-click (launch "google-chrome") :hint "Browser")
              (icon #x0F07B :class "app" :on-click (launch "nautilus") :hint "Files")
              (icon #x0F121 :class "app" :on-click (launch "emacsclient -c") :hint "Editor"))
    :end (column :align :center :spacing 12
           (icon #x0F028 :class "icon" :on-click (show audio) :hint "Volume")
           (icon #x0F1EB :class "net"  :on-click (show network) :hint "Network")
           (icon #x0F007 :class "corner-sq" :on-click (show ctl) :hint "System"))))
