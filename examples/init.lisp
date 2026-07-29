;;;; Sample ~/.config/pine/init.lisp -- what this machine runs and what it
;;;; shows, said with read, write and watch. The daemon loads this at boot;
;;;; nothing about the bar is in the engine. Copy to ~/.config/pine/init.lisp
;;;; and edit.

(in-package :pine.user)
(named-readtables:in-readtable pine.path:syntax)

;;;; What this machine has. A driver is a provider written to a path, and
;;;; nothing above one learns that there is a subprocess, a poll or a socket
;;;; behind it.

(write /sys    (procfs))
(write /audio  (pipewire))
(write /screen (backlight))
(write /power  (logind))
(write /net    (networkmanager))
(write /media  (mpris))
(write /wm     (niri))

(load-theme :ef-dream)

;;;; The bar. A click is a write, so a launcher holds no closure.

(defun launch (&rest argv)
  (fset:map (/sh (fset:convert 'fset:seq (cons :run argv)))))

(defun panel (name)
  (fset:map ((pine.path:path /surface name "shown") (fset:seq :toggle))))

(defsurface bar (:as :bar)
  (centerbox :orient :v :class "bar"
    :start (column :class "bgroup" :align :center
             (icon #xF02C1 :class "viewer" :hint "Overview"
                   :on-click {/wm [:overview]}))
    :center (column :class "bgroup grp-apps" :align :center :spacing 8
              (icon #x0F120 :class "app" :hint "Terminal"
                    :on-click (launch "alacritty"))
              (icon #x0F268 :class "app" :hint "Browser"
                    :on-click (launch "google-chrome"))
              (icon #x0F07B :class "app" :hint "Files"
                    :on-click (launch "nautilus"))
              (icon #x0F121 :class "app" :hint "Editor"
                    :on-click (launch "emacsclient" "-c")))
    :end (column :align :center :spacing 12
           (icon #x0F028 :class "icon" :hint "Volume"
                 :on-click (panel "audio"))
           (icon #x0F1EB :class "net" :hint "Network"
                 :on-click (panel "network"))
           (icon #x0F007 :class "corner-sq" :hint "System"
                 :on-click (panel "ctl")))))

;;;; ------------------------------------------------------------------
;;;; Editor authorship: keys, a mode, a variable, a styled tool buffer.
;;;; The same node language as the bar above -- one layer, any surface.
;;;; ------------------------------------------------------------------

;; a command and a global binding for it
(defcommand reload-init ()
  (call-command 'reload-desktop)
  (message "init reloaded"))
(global-set-key (kbd "C-c r") 'reload-init)

;; an editor variable is a path. Buffer-local is not a mechanism: a leaf under
;; a buffer with no value there reads the same leaf at the root
(write /greeting "hi")                              ; everywhere
(write /buf/*scratchpad*/greeting "hello there")    ; here
(defcommand greet ()
  (message (read (pine.path:path /buf (current-buffer) "greeting"))))
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

;; a mode with a :view is a tool buffer, and that is the whole of one: the
;; view is an expression, so it re-renders when anything it read moves
(write /mode/scratchpad
  {:indicator "Scratch"
   :view (lambda (buf)
           (declare (ignore buf))
           (column :align :stretch
             (label "scratchpad" :class :tool-title)
             (choice :class :tool-row :data (lambda () (call-command 'greet))
               (label "greet"))
             (choice :class :tool-row :data (lambda () (call-command 'open-repl))
               (label "open a repl"))))})

(defcommand scratchpad ()
  (buffer "*scratchpad*")
  (write /buf/*scratchpad* {:mode :scratchpad})
  (switch-buffer "*scratchpad*"))
(global-set-key (kbd "C-c s") 'scratchpad)
