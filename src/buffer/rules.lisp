(in-package :pine.buffer)

;;;; The stylesheet, authored in Lisp as data: a list of (selector props-plist)
;;;; where props values are CSS strings. pine.style compiles it and paints both
;;;; the cairo panels and the cell render (layout buffers). Colours and
;;;; metrics come from the active theme, so restyling is a theme swap. Users
;;;; append their own rules with ADD-RULES (defrules from init.lisp); user rules
;;;; come after the built-ins, so they win the cascade.

(defvar *user-rules* nil
  "User (selector props-plist) rules, appended after the built-ins. Keyed by
selector string for replace-on-reload.")
(defvar *rules-generation* 0
  "Bumped whenever *user-rules* changes, so pine.style's compiled-rules cache
knows to refresh even when the theme is unchanged.")

(defun selector-string (sel)
  "Canonicalize a selector: a keyword or symbol is one class (.name), a list
of symbols a compound (.a.b), a string is the selector DSL as written."
  (etypecase sel
    (string sel)
    (symbol (format nil ".~(~a~)" (symbol-name sel)))
    (list (format nil "~{.~(~a~)~}" (mapcar #'symbol-name sel)))))

(defun install-rules (rules)
  "Merge RULES (a list of (selector props-plist)) into *user-rules*, replacing
any rule with the same selector, and bump *rules-generation* so the
compiled-rules cache refreshes. Selectors may be keywords, symbol lists, or
selector strings. The local half of ADD-RULES; frontends install rules pushed
from the daemon here."
  (dolist (rule rules)
    (let ((sel (selector-string (first rule))))
      (setf *user-rules*
            (remove sel *user-rules* :key #'first :test #'string=))
      (push (cons sel (rest rule)) *user-rules*)))
  (incf *rules-generation*)
  *user-rules*)

(defun add-rules (rules)
  "Install RULES locally and broadcast the merged set to every attached app,
so the pixel painters in the frontend images restyle too. Reload-safe."
  (install-rules rules)
  (dolist (c pine.attach:*clients*)
    (pine.eval:attempt (lambda () (pine.attach:push-to-app c :rules :rules *user-rules*))
                       "rules broadcast"))
  *user-rules*)

(defun css-color (role) (color role))
(defun css-glass (role &optional (a (metric :opacity 0.4)))
  (multiple-value-bind (r g b) (hex-rgb (color role))
    (format nil "rgba(~d, ~d, ~d, ~a)" r g b a)))
(defun css-rad () (format nil "~apx" (metric :radius 8)))
(defun css-mono () (format nil "~s, monospace" (metric :font "Maple Mono NF")))

(defun theme-rules ()
  (flet ((p (role) (css-color role)) (glass (role) (css-glass role))
         (rad () (css-rad)) (mono () (css-mono)))
    `(;; global reset: every widget starts bare -- no borders, shadows, focus
      ;; rings, or backgrounds; each rule below opts back in.
      ("*" (:border-width "0" :border-style "none" :box-shadow "none" :outline-style "none"
            :background-color "transparent" :background-image "none"))
      ("button" (:min-width "0" :min-height "0" :padding "0"))
      ("button, scale" (:transition "background-color 0.25s, color 0.25s"))
      ("window, .background, .surface, decoration"
       (:background-color "transparent" :padding "0" :margin "0"))
      ("window" (:font-family ,(mono) :font-size "13px" :color ,(p :fg)))
      ;; bar
      (".bar" (:background-color ,(glass :bg) :color ,(p :fg) :padding "8px 0 0 0"))
      (".bgroup" (:border-radius ,(rad) :padding "6px 4px"))
      (".viewer, .picker, .launch, .app, .icon, .net, .media, .ctl, .ws"
       (:color ,(p :fg-alt) :min-width "28px"
        :padding "8px 0" :border-radius ,(rad) :margin "3px 0" :font-size "15px"))
      (".net" (:color ,(p :cyan)))
      (".ctl" (:color ,(p :accent)))
      (".ws-current" (:color ,(p :accent-fg) :background-color ,(p :accent) :min-width "28px"
                      :padding "8px 0" :border-radius ,(rad) :margin "3px 0"))
      (".ws" (:font-size "12px"))
      (".viewer:hover, .picker:hover, .launch:hover, .app:hover, .icon:hover, .net:hover, .media:hover, .ctl:hover, .clock:hover, .ws:hover"
       (:background-color ,(p :bg-active) :color ,(p :accent-fg)))
      (".clock" (:color ,(p :fg) :margin-top "6px"))
      (".clock .hour" (:font-size "15px" :font-weight "bold"))
      (".clock .min" (:color ,(p :fg-dim) :font-size "15px"))
      (".corner-sq" (:background-image ,(format nil "linear-gradient(135deg, ~a, ~a)"
                                                (p :accent) (p :magenta))
                     :color ,(p :accent-fg) :min-width "28px" :min-height "28px"
                     :border-radius "9px" :font-size "15px"))
      ;; per-glyph offsets (nerd-font glyphs are not centred in their em box)
      (".viewer .bar-glyph" (:margin-right "2px"))
      (".picker .bar-glyph" (:margin-right "2px"))
      (".launch .bar-glyph" (:margin-right "0px"))
      (".term .bar-glyph" (:margin-right "0px"))
      (".web .bar-glyph" (:margin-right "0px"))
      (".files .bar-glyph" (:margin-right "0px"))
      (".edit .bar-glyph" (:margin-right "0px"))
      (".icon .bar-glyph" (:margin-right "3px"))
      (".media .bar-glyph" (:margin-right "5px"))
      (".net .bar-glyph" (:margin-right "6px"))
      (".ctl .bar-glyph" (:margin-right "4px"))
      (".workspaces .ws .bar-glyph" (:margin-left "2px"))
      (".corner-sq .bar-glyph" (:color ,(p :accent-fg) :font-size "15px"))
      ;; calendar
      (".cal-box" (:background-color ,(glass :bg-dim) :border-radius ,(rad) :padding "10px"))
      ("calendar" (:color ,(p :fg)))
      ("calendar:selected" (:background-color ,(p :accent) :color ,(p :accent-fg) :border-radius "6px"))
      ;; popup chrome
      (".netmenu" (:background-color ,(glass :bg) :color ,(p :fg)
                   :border-width "1px" :border-style "solid" :border-color ,(p :border)
                   :border-radius ,(rad) :padding "8px" :margin "8px"
                   :box-shadow ,(format nil "0 0 5px 0 ~a" (p :shadow))
                   :min-width "360px"))
      (".nm-card" (:background-color ,(glass :bg-dim) :border-radius ,(rad) :padding "12px" :margin "6px"))
      (".nm-head" (:padding "12px 14px"))
      (".nm-head-ico" (:font-size "22px" :color ,(p :accent) :margin-right "14px"))
      (".nm-title" (:font-size "17px" :font-weight "bold" :color ,(p :fg)))
      (".nm-sub" (:font-size "13px"))
      (".nm-sub.on" (:color ,(p :green)))
      (".nm-sub.off" (:color ,(p :fg-dim)))
      (".nm-subhead" (:font-size "13px" :color ,(p :fg-dim) :margin "2px 4px 8px 4px"))
      (".nm-list-card" (:padding "6px"))
      (".nm-scroll" (:min-height "14rem"))
      (".nm-row" (:border-radius ,(rad) :padding "10px 12px" :margin "2px"
                  :transition "background-color 0.2s"))
      (".nm-row:hover" (:background-color ,(p :bg-active)))
      (".nm-row.active" (:background-color ,(p :bg-alt)))
      (".nm-row.sel" (:background-color ,(p :bg-active) :box-shadow ,(format nil "inset 3px 0 0 0 ~a" (p :accent))))
      (".nm-sig" (:font-size "14px" :color ,(p :fg-dim)))
      (".nm-sig.hi" (:color ,(p :green)))
      (".nm-sig.mid" (:color ,(p :yellow)))
      (".nm-sig.lo" (:color ,(p :red)))
      (".nm-name" (:font-size "14px" :color ,(p :fg)))
      (".nm-name.active" (:color ,(p :green) :font-weight "bold"))
      (".nm-lock" (:font-size "12px" :color ,(p :fg-dim)))
      (".nm-check" (:font-size "13px" :color ,(p :green)))
      (".nm-actions" (:padding "6px 4px 2px 4px"))
      (".nm-btn" (:background-color ,(p :bg-active) :color ,(p :fg) :padding "10px 20px"
                  :border-radius ,(rad) :transition "background-color 0.2s, color 0.2s"))
      (".nm-btn:hover" (:background-color ,(p :bg-alt)))
      (".nm-btn.go" (:background-color ,(p :accent) :color ,(p :accent-fg)))
      (".nm-btn.no" (:color ,(p :red)))
      ;; menu sliders (audio, media) -- the * reset already stripped the Adwaita chrome
      (".menu-slider trough" (:background-color ,(p :bg-alt) :min-height "8px" :border-radius "5px"))
      (".menu-slider trough highlight" (:background-color ,(p :accent) :border-radius "5px"))
      (".menu-slider slider" (:min-width "16px" :min-height "16px" :border-radius "8px" :background-color ,(p :fg)))
      ;; audio
      (".audio-mute" (:color ,(p :cyan) :font-size "20px" :padding "0 4px"))
      (".audio-mute:hover" (:color ,(p :accent)))
      (".audio-pct" (:color ,(p :fg-dim) :min-width "38px"))
      ;; media
      (".media-info" (:padding "2px 2px 12px 2px"))
      (".media-art" (:min-width "64px" :min-height "64px" :border-radius ,(rad) :background-color ,(p :bg-active)))
      (".media-art-ico" (:font-size "26px" :color ,(p :fg-dim)))
      (".media-title" (:font-size "15px" :font-weight "bold" :color ,(p :fg)))
      (".media-artist" (:color ,(p :fg-dim)))
      (".media-times" (:padding "6px 2px 2px 2px"))
      (".media-time" (:font-size "11px" :color ,(p :fg-dim)))
      (".media-ctrl" (:padding "10px 0 2px 0"))
      (".media-btn" (:color ,(p :fg) :font-size "20px" :padding "6px"))
      (".media-btn:hover" (:color ,(p :accent)))
      (".media-none-box" (:padding "8px"))
      (".media-none" (:color ,(p :fg-dim) :padding "14px"))
      ;; control panel
      (".ctlpanel-box" (:padding "4px"))
      (".ctl-card" (:background-color ,(glass :bg-dim) :border-radius ,(rad) :padding "14px" :margin "6px"))
      (".ctl-profile" (:padding "2px"))
      (".ctl-pfp" (:background-color ,(p :accent) :border-radius "100%" :min-width "58px"
                   :min-height "58px" :margin-right "16px"))
      (".ctl-pfp-ico" (:font-size "28px" :color ,(p :accent-fg)))
      (".ctl-user" (:font-size "24px" :font-weight "bold" :color ,(p :accent)))
      (".ctl-uptime" (:color ,(p :fg-dim)))
      (".ctl-power" (:background-color ,(glass :bg) :border-radius ,(rad) :padding "12px 8px" :margin-top "14px"))
      (".ctl-confirm" (:background-color ,(glass :bg) :border-radius ,(rad) :padding "10px 8px" :margin-top "14px"))
      (".ctl-confirm-lbl" (:color ,(p :fg) :font-size "14px" :font-weight "bold"))
      (".ctl-cf" (:font-size "18px" :padding "4px 14px"))
      (".ctl-cf.yes" (:color ,(p :green)))
      (".ctl-cf.no" (:color ,(p :red)))
      (".ctl-cf:hover" (:color ,(p :fg)))
      (".pw-a" (:font-size "22px" :padding "4px 12px"))
      (".pw-a.lock" (:color ,(p :blue)))
      (".pw-a.logout" (:color ,(p :yellow)))
      (".pw-a.reboot" (:color ,(p :magenta)))
      (".pw-a.suspend" (:color ,(p :cyan)))
      (".pw-a.off" (:color ,(p :red)))
      (".pw-a:hover" (:color ,(p :fg)))
      (".ctl-rings" (:padding "4px 0"))
      (".ctl-ring-box" (:background-color ,(glass :bg) :border-radius ,(rad) :padding "10px 8px" :margin "0 4px"))
      (".ctl-ring-ico" (:font-size "18px" :margin "14px"))
      (".ctl-ring-box.cpu .ctl-ring-ico" (:margin-left "11px" :margin-right "17px"))
      (".ctl-ring-box.ram .ctl-ring-ico" (:margin-left "11px" :margin-right "17px"))
      (".ctl-ring-box.disk .ctl-ring-ico" (:margin-left "11px" :margin-right "17px"))
      (".ctl-ring-lbl" (:font-size "11px" :margin-top "6px" :color ,(p :fg-dim)))
      (".ctl-ring-box.cpu .ctl-ring-ico, .ctl-ring-box.cpu .ctl-ring-lbl" (:color ,(p :red)))
      (".ctl-ring-box.ram .ctl-ring-ico, .ctl-ring-box.ram .ctl-ring-lbl" (:color ,(p :blue)))
      (".ctl-ring-box.disk .ctl-ring-ico, .ctl-ring-box.disk .ctl-ring-lbl" (:color ,(p :green)))
      (".ctl-ring-box.temp .ctl-ring-ico, .ctl-ring-box.temp .ctl-ring-lbl" (:color ,(p :yellow)))
      (".ctl-srow" (:padding "4px 8px"))
      (".ctl-sico.vol" (:color ,(p :accent) :font-size "17px"))
      (".ctl-sico.bri" (:color ,(p :yellow) :font-size "17px"))
      (".ctl-scale trough" (:background-color ,(p :bg) :min-height "10px" :min-width "180px" :border-radius "50px"))
      (".ctl-scale.vol trough highlight" (:background-color ,(p :accent) :border-radius "10px"))
      (".ctl-scale.bri trough highlight" (:background-color ,(p :yellow) :border-radius "10px"))
      (".ctl-scale slider" (:min-width "14px" :min-height "14px" :border-radius "7px" :background-color ,(p :fg)))
      ;; layout buffers (the cell render): the completion popup, the debugger,
      ;; jobs, and the help buffers style through the same CSS-as-data as the
      ;; desktop
      (".cand" (:color ,(p :fg)))
      (".cand-annot" (:color ,(p :fg-dim)))
      (".cand-row" (:background-color ,(p :bg-completion)))
      (".cand-row.sel" (:background-color ,(p :bg-active)))
      (".cand-row.sel .cand" (:color ,(p :accent-fg)))
      (".cand-row.sel .cand-annot" (:color ,(p :accent-fg)))
      (".dbg-switch" (:color ,(p :blue-faint)))
      (".dbg-header" (:color ,(p :cyan-warmer) :font-weight "bold"))
      (".dbg-cond" (:color ,(p :red-faint)))
      (".dbg-note" (:color ,(p :blue-faint)))
      (".restart-lbl" (:color ,(p :yellow-cooler)))
      (".restart.sel" (:background-color ,(p :bg-active)))
      (".restart.sel .restart-lbl" (:color ,(p :accent-fg)))
      (".dbg-bt" (:color ,(p :blue-faint)))
      (".eval-result" (:color ,(p :green-cooler) :font-weight "bold"))
      (".job-row.sel" (:background-color ,(p :bg-active)))
      (".help-head" (:color ,(p :cyan-warmer) :font-weight "bold"))
      (".help-entry" (:color ,(p :fg)))
      ;; echo
      (".echo" (:background-color "transparent"))
      (".echo-lead" (:min-width "44px" :background-color "transparent"))
      (".echo-body" (:background-color ,(glass :bg)))
      (".echo-text" (:color ,(p :fg) :font-size "13px" :padding "0 12px"))
      (".echo-stat" (:color ,(p :fg-dim) :font-size "12px" :padding "0 16px 0 8px"))
      ;; user rules last, so they win the cascade
      ,@(reverse *user-rules*))))
