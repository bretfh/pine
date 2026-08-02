(defpackage #:pine.ui.rules
  (:use #:cl #:pine.ui.face)
  (:export #:user-rules #:add-rules #:install-rules #:theme-rules
           #:selector-string
           #:css-color #:css-glass #:css-mono #:css-rad))

(in-package #:pine.ui.rules)
(named-readtables:in-readtable pine.data:syntax)

;;;; The stylesheet, as data: a list of (selector props) where props is a map of
;;;; CSS property to CSS string. Colours and metrics come from the active theme,
;;;; so restyling is a theme swap. What a config wrote lives at /style/?class and
;;;; comes last, so it wins the cascade.

(defun %compound-p (s &optional (from 0))
  "Whether S says more than one class: a descendant, a list, a pseudo, or two
classes on one node."
  (and (find-if (lambda (c) (member c '(#\Space #\, #\: #\.))) s :start from) t))

(defun %segment (sel)
  "The path segment SEL is written at.

One class is its own name, so the doc's (write /style/editor-view ..) is the
path it looks like. Anything more than one class has no name of that kind and
is stored as it was written."
  (let ((s (selector-string sel)))
    (if (and (plusp (length s)) (char= #\. (char s 0)) (not (%compound-p s 1)))
        (subseq s 1)
        s)))

(defun %selector (segment)
  "The selector a segment under /style stands for: the inverse of %SEGMENT."
  (if (%compound-p segment)
      segment
      (format nil ".~a" segment)))

(defun user-rules ()
  "The (SELECTOR PROPS) rules a config wrote, by selector.

The same shape the built-ins are in, since the two are appended into one
stylesheet and whatever compiles it should not have to know which half a rule
came from."
  (let ((held (pine.ns:held (pine.path:parse "/style")))
        (acc nil))
    (when (fset:map? held)
      (fset:do-map (key props held)
        (when (fset:map? props)
          (push (list (%selector (pine.path:name key)) props) acc))))
    (sort acc #'string< :key #'first)))

(defgeneric selector-string (sel)
  (:documentation "SEL as a selector: a symbol is one class (.name), a list of
symbols a compound (.a.b), a string the selector DSL as written."))

(defmethod selector-string ((sel string)) sel)

(defmethod selector-string ((sel symbol))
  (format nil ".~(~a~)" (symbol-name sel)))

(defmethod selector-string ((sel cons))
  (format nil "~{.~(~a~)~}" (mapcar #'symbol-name sel)))

(defun install-rules (rules)
  "Write RULES (a list of (selector props)) at /style/?class, replacing any
rule with the same selector.

Selectors may be keywords, symbol lists, or selector strings. The local half of
ADD-RULES; a frontend installs the rules the daemon pushed it here. Reload-safe
because a selector names its own path."
  (dolist (rule rules)
    (pine.ns:write (pine.path:child (pine.path:parse "/style")
                                    (%segment (first rule)))
                   (first (rest rule))))
  (user-rules))

(defun add-rules (rules)
  "Install RULES here and broadcast the merged set to every attached app, so
the pixel painters in the frontend images restyle too."
  (install-rules rules)
  (let ((all (user-rules)))
    (dolist (c pine.core.attach:*clients*)
      (pine.err:attempt (lambda () (pine.core.attach:push-to-app c :rules :rules all))
                        "rules broadcast"))
    all))

(defun css-color (role) (color role))
(defun css-glass (role &optional (a (metric :opacity 0.4)))
  (multiple-value-bind (r g b) (hex-rgb (color role))
    (format nil "rgba(~d, ~d, ~d, ~a)" r g b a)))
(defun css-rad () (format nil "~apx" (metric :radius 8)))
(defun css-mono () (format nil "~s, monospace" (metric :font "Maple Mono NF")))

(defun theme-rules ()
  (flet ((p (role) (css-color role)) (glass (role) (css-glass role))
         (rad () (css-rad)) (mono () (css-mono)))
    (append
     (list
      ;; global reset: every widget starts bare -- no borders, shadows, focus
      ;; rings, or backgrounds; each rule below opts back in.
      (list "*" {:border-width "0" :border-style "none" :box-shadow "none" :outline-style "none"
            :background-color "transparent" :background-image "none"})
      (list "button" {:min-width "0" :min-height "0" :padding "0"})
      (list "button, scale" {:transition "background-color 0.25s, color 0.25s"})
      (list "window, .background, .surface, decoration"
       {:background-color "transparent" :padding "0" :margin "0"})
      (list "window" {:font-family (mono) :font-size "13px" :color (p :fg)})
      ;; bar
      (list ".bar" {:background-color (glass :bg) :color (p :fg) :padding "8px 0 0 0"})
      (list ".bgroup" {:border-radius (rad) :padding "6px 4px"})
      (list ".viewer, .picker, .launch, .app, .icon, .net, .media, .ctl, .ws"
       {:color (p :fg-alt) :min-width "28px"
        :padding "8px 0" :border-radius (rad) :margin "3px 0" :font-size "15px"})
      (list ".net" {:color (p :cyan)})
      (list ".ctl" {:color (p :accent)})
      (list ".ws-current" {:color (p :accent-fg) :background-color (p :accent) :min-width "28px"
                      :padding "8px 0" :border-radius (rad) :margin "3px 0"})
      (list ".ws" {:font-size "12px"})
      (list ".viewer:hover, .picker:hover, .launch:hover, .app:hover, .icon:hover, .net:hover, .media:hover, .ctl:hover, .clock:hover, .ws:hover"
       {:background-color (p :bg-active) :color (p :accent-fg)})
      (list ".clock" {:color (p :fg) :margin-top "6px"})
      (list ".clock .hour" {:font-size "15px" :font-weight "bold"})
      (list ".clock .min" {:color (p :fg-dim) :font-size "15px"})
      (list ".corner-sq" {:background-image (format nil "linear-gradient(135deg, ~a, ~a)"
                                                (p :accent) (p :magenta))
                     :color (p :accent-fg) :min-width "28px" :min-height "28px"
                     :border-radius "9px" :font-size "15px"})
      ;; per-glyph offsets (nerd-font glyphs are not centred in their em box)
      (list ".viewer .bar-glyph" {:margin-right "2px"})
      (list ".picker .bar-glyph" {:margin-right "2px"})
      (list ".launch .bar-glyph" {:margin-right "0px"})
      (list ".term .bar-glyph" {:margin-right "0px"})
      (list ".web .bar-glyph" {:margin-right "0px"})
      (list ".files .bar-glyph" {:margin-right "0px"})
      (list ".edit .bar-glyph" {:margin-right "0px"})
      (list ".icon .bar-glyph" {:margin-right "3px"})
      (list ".media .bar-glyph" {:margin-right "5px"})
      (list ".net .bar-glyph" {:margin-right "6px"})
      (list ".ctl .bar-glyph" {:margin-right "4px"})
      (list ".workspaces .ws .bar-glyph" {:margin-left "2px"})
      (list ".corner-sq .bar-glyph" {:color (p :accent-fg) :font-size "15px"})
      ;; calendar
      (list ".cal-box" {:background-color (glass :bg-dim) :border-radius (rad) :padding "10px"})
      (list "calendar" {:color (p :fg)})
      (list "calendar:selected" {:background-color (p :accent) :color (p :accent-fg) :border-radius "6px"})
      ;; popup chrome
      (list ".netmenu" {:background-color (glass :bg) :color (p :fg)
                   :border-width "1px" :border-style "solid" :border-color (p :border)
                   :border-radius (rad) :padding "8px" :margin "8px"
                   :box-shadow (format nil "0 0 5px 0 ~a" (p :shadow))
                   :min-width "360px"})
      (list ".nm-card" {:background-color (glass :bg-dim) :border-radius (rad) :padding "12px" :margin "6px"})
      (list ".nm-head" {:padding "12px 14px"})
      (list ".nm-head-ico" {:font-size "22px" :color (p :accent) :margin-right "14px"})
      (list ".nm-title" {:font-size "17px" :font-weight "bold" :color (p :fg)})
      (list ".nm-sub" {:font-size "13px"})
      (list ".nm-sub.on" {:color (p :green)})
      (list ".nm-sub.off" {:color (p :fg-dim)})
      (list ".nm-subhead" {:font-size "13px" :color (p :fg-dim) :margin "2px 4px 8px 4px"})
      (list ".nm-list-card" {:padding "6px"})
      (list ".nm-scroll" {:min-height "14rem"})
      (list ".nm-row" {:border-radius (rad) :padding "10px 12px" :margin "2px"
                  :transition "background-color 0.2s"})
      (list ".nm-row:hover" {:background-color (p :bg-active)})
      (list ".nm-row.active" {:background-color (p :bg-alt)})
      (list ".nm-row.sel" {:background-color (p :bg-active) :box-shadow (format nil "inset 3px 0 0 0 ~a" (p :accent))})
      (list ".nm-sig" {:font-size "14px" :color (p :fg-dim)})
      (list ".nm-sig.hi" {:color (p :green)})
      (list ".nm-sig.mid" {:color (p :yellow)})
      (list ".nm-sig.lo" {:color (p :red)})
      (list ".nm-name" {:font-size "14px" :color (p :fg)})
      (list ".nm-name.active" {:color (p :green) :font-weight "bold"})
      (list ".nm-lock" {:font-size "12px" :color (p :fg-dim)})
      (list ".nm-check" {:font-size "13px" :color (p :green)})
      (list ".nm-actions" {:padding "6px 4px 2px 4px"})
      (list ".nm-btn" {:background-color (p :bg-active) :color (p :fg) :padding "10px 20px"
                  :border-radius (rad) :transition "background-color 0.2s, color 0.2s"})
      (list ".nm-btn:hover" {:background-color (p :bg-alt)})
      (list ".nm-btn.go" {:background-color (p :accent) :color (p :accent-fg)})
      (list ".nm-btn.no" {:color (p :red)})
      ;; menu sliders (audio, media) -- the * reset already stripped the Adwaita chrome
      (list ".menu-slider trough" {:background-color (p :bg-alt) :min-height "8px" :border-radius "5px"})
      (list ".menu-slider trough highlight" {:background-color (p :accent) :border-radius "5px"})
      (list ".menu-slider slider" {:min-width "16px" :min-height "16px" :border-radius "8px" :background-color (p :fg)})
      ;; audio
      (list ".audio-mute" {:color (p :cyan) :font-size "20px" :padding "0 4px"})
      (list ".audio-mute:hover" {:color (p :accent)})
      (list ".audio-pct" {:color (p :fg-dim) :min-width "38px"})
      ;; media
      (list ".media-info" {:padding "2px 2px 12px 2px"})
      (list ".media-art" {:min-width "64px" :min-height "64px" :border-radius (rad) :background-color (p :bg-active)})
      (list ".media-art-ico" {:font-size "26px" :color (p :fg-dim)})
      (list ".media-title" {:font-size "15px" :font-weight "bold" :color (p :fg)})
      (list ".media-artist" {:color (p :fg-dim)})
      (list ".media-times" {:padding "6px 2px 2px 2px"})
      (list ".media-time" {:font-size "11px" :color (p :fg-dim)})
      (list ".media-ctrl" {:padding "10px 0 2px 0"})
      (list ".media-btn" {:color (p :fg) :font-size "20px" :padding "6px"})
      (list ".media-btn:hover" {:color (p :accent)})
      (list ".media-none-box" {:padding "8px"})
      (list ".media-none" {:color (p :fg-dim) :padding "14px"})
      ;; control panel
      (list ".ctlpanel-box" {:padding "4px"})
      (list ".ctl-card" {:background-color (glass :bg-dim) :border-radius (rad) :padding "14px" :margin "6px"})
      (list ".ctl-profile" {:padding "2px"})
      (list ".ctl-pfp" {:background-color (p :accent) :border-radius "100%" :min-width "58px"
                   :min-height "58px" :margin-right "16px"})
      (list ".ctl-pfp-ico" {:font-size "28px" :color (p :accent-fg)})
      (list ".ctl-user" {:font-size "24px" :font-weight "bold" :color (p :accent)})
      (list ".ctl-uptime" {:color (p :fg-dim)})
      (list ".ctl-power" {:background-color (glass :bg) :border-radius (rad) :padding "12px 8px" :margin-top "14px"})
      (list ".ctl-confirm" {:background-color (glass :bg) :border-radius (rad) :padding "10px 8px" :margin-top "14px"})
      (list ".ctl-confirm-lbl" {:color (p :fg) :font-size "14px" :font-weight "bold"})
      (list ".ctl-cf" {:font-size "18px" :padding "4px 14px"})
      (list ".ctl-cf.yes" {:color (p :green)})
      (list ".ctl-cf.no" {:color (p :red)})
      (list ".ctl-cf:hover" {:color (p :fg)})
      (list ".pw-a" {:font-size "22px" :padding "4px 12px"})
      (list ".pw-a.lock" {:color (p :blue)})
      (list ".pw-a.logout" {:color (p :yellow)})
      (list ".pw-a.reboot" {:color (p :magenta)})
      (list ".pw-a.suspend" {:color (p :cyan)})
      (list ".pw-a.off" {:color (p :red)})
      (list ".pw-a:hover" {:color (p :fg)})
      (list ".ctl-rings" {:padding "4px 0"})
      (list ".ctl-ring-box" {:background-color (glass :bg) :border-radius (rad) :padding "10px 8px" :margin "0 4px"})
      (list ".ctl-ring-ico" {:font-size "18px" :margin "14px"})
      (list ".ctl-ring-box.cpu .ctl-ring-ico" {:margin-left "11px" :margin-right "17px"})
      (list ".ctl-ring-box.ram .ctl-ring-ico" {:margin-left "11px" :margin-right "17px"})
      (list ".ctl-ring-box.disk .ctl-ring-ico" {:margin-left "11px" :margin-right "17px"})
      (list ".ctl-ring-lbl" {:font-size "11px" :margin-top "6px" :color (p :fg-dim)})
      (list ".ctl-ring-box.cpu .ctl-ring-ico, .ctl-ring-box.cpu .ctl-ring-lbl" {:color (p :red)})
      (list ".ctl-ring-box.ram .ctl-ring-ico, .ctl-ring-box.ram .ctl-ring-lbl" {:color (p :blue)})
      (list ".ctl-ring-box.disk .ctl-ring-ico, .ctl-ring-box.disk .ctl-ring-lbl" {:color (p :green)})
      (list ".ctl-ring-box.temp .ctl-ring-ico, .ctl-ring-box.temp .ctl-ring-lbl" {:color (p :yellow)})
      (list ".ctl-srow" {:padding "4px 8px"})
      (list ".ctl-sico.vol" {:color (p :accent) :font-size "17px"})
      (list ".ctl-sico.bri" {:color (p :yellow) :font-size "17px"})
      (list ".ctl-scale trough" {:background-color (p :bg) :min-height "10px" :min-width "180px" :border-radius "50px"})
      (list ".ctl-scale.vol trough highlight" {:background-color (p :accent) :border-radius "10px"})
      (list ".ctl-scale.bri trough highlight" {:background-color (p :yellow) :border-radius "10px"})
      (list ".ctl-scale slider" {:min-width "14px" :min-height "14px" :border-radius "7px" :background-color (p :fg)})
      ;; layout buffers (the cell render): the completion popup, the debugger,
      ;; jobs, and the help buffers style through the same CSS-as-data as the
      ;; desktop
      (list ".cand" {:color (p :fg)})
      (list ".cand-annot" {:color (p :fg-dim)})
      (list ".cand-row" {:background-color (p :bg-completion)})
      (list ".cand-row.sel" {:background-color (p :bg-active)})
      (list ".cand-row.sel .cand" {:color (p :accent-fg)})
      (list ".cand-row.sel .cand-annot" {:color (p :accent-fg)})
      (list ".dbg-switch" {:color (p :blue-faint)})
      (list ".dbg-header" {:color (p :cyan-warmer) :font-weight "bold"})
      (list ".dbg-cond" {:color (p :red-faint)})
      (list ".dbg-note" {:color (p :blue-faint)})
      (list ".restart-lbl" {:color (p :yellow-cooler)})
      (list ".restart.sel" {:background-color (p :bg-active)})
      (list ".restart.sel .restart-lbl" {:color (p :accent-fg)})
      (list ".dbg-bt" {:color (p :blue-faint)})
      (list ".eval-result" {:color (p :green-cooler) :font-weight "bold"})
      (list ".job-row.sel" {:background-color (p :bg-active)})
      (list ".help-head" {:color (p :cyan-warmer) :font-weight "bold"})
      (list ".help-entry" {:color (p :fg)})
      ;; echo
      (list ".echo" {:background-color "transparent"})
      (list ".echo-lead" {:min-width "44px" :background-color "transparent"})
      (list ".echo-body" {:background-color (glass :bg)})
      (list ".echo-text" {:color (p :fg) :font-size "13px" :padding "0 12px"})
      (list ".echo-stat" {:color (p :fg-dim) :font-size "12px" :padding "0 16px 0 8px"}))
     ;; user rules last, so they win the cascade
     (user-rules))))
