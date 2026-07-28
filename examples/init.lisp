;;;; Sample ~/.config/pine/init.lisp -- the desktop defined in the pine.user
;;;; language. The daemon loads this at boot; nothing about the bar is in the
;;;; engine. Copy to ~/.config/pine/init.lisp and edit.

(in-package :pine.user)
(named-readtables:in-readtable pine.path:syntax)

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


;;;; ------------------------------------------------------------------
;;;; Editor authorship: keys, a mode, a variable, a styled tool buffer.
;;;; The same node language as the bar above -- one layer, any surface.
;;;; ------------------------------------------------------------------

;; a command and a global binding for it
(defcommand reload-init ()
  (call-command 'reload-desktop)
  (message "init reloaded"))
(global-set-key (kbd "C-c r") 'reload-init)

;; an editor variable, read by a command
(defonce :greeting :default "hi" :documentation "what greet echoes")
(defcommand greet () (message (var :greeting)))
(global-set-key (kbd "C-c g") 'greet)

;; a minor mode with its own key, toggled by a command. A mode is a map, so
;; writing it is the whole of defining it: no class, no registration step.
(write /minor/focus {:precedence 12 :indicator "Focus"})
(define-key (keymap :focus) (kbd "C-c f") 'greet)
(defcommand toggle-focus () (toggle-minor-mode :focus))
(global-set-key (kbd "C-c t") 'toggle-focus)

;; a major mode for .todo files
(write /mode/todo {:parent :text :indicator "TODO" :files ["*.todo"]})

;; a styled tool buffer: selectable rows whose Return runs a thunk
(defrules (:tool-title :color (color :accent) :font-weight "bold")
          ((:tool-row :sel) :background-color (color :bg-active)))

(defcommand scratchpad ()
  (show-layout "*scratchpad*"
    (lambda (state) (declare (ignore state))
      (column :align :stretch
        (label "scratchpad" :class :tool-title)
        (choice :class :tool-row :data (lambda () (call-command 'greet))
          (label "greet"))
        (choice :class :tool-row :data (lambda () (call-command 'open-repl))
          (label "open a repl"))))))
(global-set-key (kbd "C-c s") 'scratchpad)
