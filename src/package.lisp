

(defpackage :pine.buffer
  (:use :cl)
  (:export
   ;; buffer-state
   #:buffer-state #:lines #:marks #:meta #:tick
   ;; snapshot
   #:snapshot #:name #:line-count #:point-line #:point-col #:highlights
   ;; state ops
   #:make-empty-state #:state->snapshot #:state->snapshot-with-hl #:state->string
   #:insert-char #:insert-string #:insert-newline
   #:delete-char #:delete-region
   #:move-mark #:set-meta #:region-bounds
   #:buffer-local
   #:refresh-highlights #:point-after-move
   #:line-indent-width #:previous-line-indent #:reindent-line
   #:load-content #:notify-subscribers #:split-lines
   #:line-count-of #:line-at #:region-string #:buffer-table
   ;; faces
   #:face #:fg #:bg #:bold #:italic #:underline
   #:defface #:find-face #:face-attr-bits
   #:deftheme #:load-theme #:find-theme #:theme-color #:color #:*active-theme*
   #:hex-rgb #:face-fg #:face-bg #:metric #:theme-metric #:theme-rules
   #:add-rules #:install-rules #:*user-rules* #:*rules-generation*
   #:theme #:theme-name #:theme-palette #:theme-metrics #:theme-faces
   #:face-run #:run-start #:run-end #:run-face
   #:display-line #:display-text #:display-runs
   #:make-display-line
   ;; windows
   #:window #:buffer-ref #:window-name #:row #:col #:win-width #:win-height
   #:scroll-top #:focusedp #:snap #:win-display
   #:frame #:windows #:frame-cols #:frame-rows #:bg-face
   #:frame-cells #:frame-cell-count #:frame-cursor-row #:frame-cursor-col
   #:frame-scroll-pixel #:frame-dirtyp #:ensure-frame-cells
   #:window-display-lines #:ensure-point-visible #:ensure-col-visible
   ;; buffer actor
   #:make-buffer-actor #:notify-subscribers #:load-content
   ;; registry
   #:start-buffer-registry))

(defpackage :pine.echo
  (:use :cl)
  (:export #:message #:current-message
           #:show-prompt #:hide-prompt
           #:prompt-active-p #:prompt-text))

(defpackage :pine.file
  (:use :cl)
  (:export
   #:read-file
   #:write-file
   #:find-file
   #:save-current-buffer
   #:record-places))

(defpackage :pine.render
  (:use :cl)
  (:export
   #:start-renderer
   #:subscribe-to-buffer
   #:unsubscribe-from-buffer
   #:render-window-rows
   #:modeline-rows
   #:echo-rows
   #:arrange-editor-tree
   #:refresh-editor-tree
   #:frame->rows
   #:relayout))

(defpackage #:pine.target
  (:use #:cl)
  (:export #:*eval-target* #:*eval-target-saved* #:eval-in-target)
  (:documentation "Which image an evaluation runs in. C-x C-e, eval-defun and
the repl share one path through here, so redirecting the target redirects all
of them."))

(defpackage :pine.repl
  (:use :cl)
  (:export
   #:start-repl
   #:repl-eval
   #:repl-submit
   #:repl-buffer))

(defpackage :pine.ts
  (:use :cl)
  (:export
   #:ts-runtime
   #:make-ts-runtime
   #:ts-loaded-p
   #:ensure-ts
   #:ensure-language
   #:compute-highlights
   #:forward-sexp-pos
   #:backward-sexp-pos
   #:defun-bounds-pos
   ;; per-buffer incremental parse state
   #:parse-state #:make-parse-state #:free-parse-state
   #:reparse! #:parse-full! #:parse-highlights #:parse-motion #:parse-indent
   ;; highlight harness
   #:walk-highlights #:hl-dump #:hl-dump-file))

(defpackage :pine.term
  (:use :cl)
  (:export
   #:terminal #:terminal-term #:terminal-fd #:terminal-pid #:terminal-buffer
   #:open-terminal
   #:terminal-for-buffer
   #:term-write
   #:gterm-text
   #:drain-terminals
   #:resize-active-terminal
   #:terminal-dispatch))

(defpackage #:pine.edit
  (:use #:cl)
  (:documentation "The dispatch-message methods for base-mode and text-mode:
what every buffer does with a verb, and what a text buffer layers on top. Adds
methods only; it exports nothing."))

(defpackage :pine.mode
  (:use :cl)
  (:export
   #:mode #:major-mode #:minor-mode
   #:base-mode #:text-mode #:lisp-mode #:repl-mode #:terminal-mode
   #:debugger-mode #:minibuffer-mode #:layout-mode #:overwrite-mode
   #:mode-name #:mode-keymap #:mode-indicator #:parent-mode #:ts-language
   #:precedence #:transparent
   #:register-mode #:find-mode #:all-mode-names #:global-keymap
   #:mode-for-file #:auto-mode #:*auto-modes*
   #:modes-dispatch-class
   #:defmode #:defminor
   #:dispatch-message))

(defpackage #:pine.wm
  (:use #:cl)
  (:export #:wm-keymap #:binding-table #:push-bindings #:run-binding
           #:attached-p #:leaves #:focused-leaf
           #:spawn #:close-window #:focus-step #:split #:exit-session)
  (:documentation "Window management policy: the keymap whose chords are
registered with the compositor, the commands they run, and the actions sent
to the wm frontend. design/wm.org is the contract."))

(defpackage :pine.layout
  (:use :cl)
  (:export
   ;; nodes
   #:node #:key-of #:parent #:face #:hint #:expand-of #:css-class
   #:radius #:fill-of #:grad #:font-px #:hovered #:nodes-of
   #:*text-size* #:*default-font-px*
   #:start-line #:start-col #:end-line #:end-col
   #:text-node #:content
   #:separator #:sep-char
   #:spacer #:center
   #:scroll #:scroll-offset #:vheight
   #:vstack #:nodes #:spacing #:align
   #:hstack
   #:box #:width-of #:pad-char
   #:selectable #:data #:selectedp #:prefix-selected #:prefix-unselected
   #:action #:callback
   #:list-node #:items #:item-fn #:max-visible
   #:grid #:cells #:col-widths
   #:slider #:value #:min-of #:max-of #:track #:on-change #:filled-face #:empty-face
   #:slider-fraction
   #:ring #:thickness #:diameter #:arc-face #:track-face #:ring-fraction
   #:calendar #:cal-year #:cal-month #:cal-day #:picture #:pic-path
   #:window #:window-node #:window-rows #:window-crow #:window-ccol
   #:window-opacity #:window-of #:window-kind #:blit-row
   ;; constructor DSL
   #:label #:icon #:column #:row #:button #:boxed #:centered #:viewport
   #:gap #:rule #:meter #:rows #:choice #:cal #:pic #:centerbox
   ;; layout protocol
   #:measure #:arrange #:paint #:*hover-face*
   ;; layout -> cell rows (layout buffers + the chrome popup)
   #:render #:resolve-styles! #:raster->rows #:class-names
   #:defwidget
   #:node->wire #:wire->node
           #:rows-patch #:apply-rows-patch #:wire-shape #:wire-windows #:arranged-p
   ;; live-tree surgery, shared by every arranged tree
   #:node-parent #:replace-child #:remove-with-divider
   #:split-node #:remove-node
   ;; hit-testing + selection
   #:node-at #:action-at #:click-thunk #:slider-value-at #:hint-at
   #:collect-selectables
   #:scroll-to-selection))

(defpackage #:pine.source
  (:use #:cl)
  (:export #:start-sources #:stop-sources #:source-status
           #:defsource #:defpoll #:start-stream #:start-poll #:ref-of
           ;; helpers a declared source needs
           #:sh #:split #:lines #:starts-with #:first-number
           #:read-int-file #:json))

(defpackage #:pine.style
  (:use #:cl)
  (:export #:style #:st-bg #:st-gradient #:st-fg #:st-border-w #:st-border-color
           #:st-radius #:st-pad-x #:st-pad-y #:st-min-w #:st-min-h
           #:st-font-px #:st-bold #:st-inset #:st-margin #:st-shadow
           #:st-opacity
           #:resolve #:reset-rules))

(defpackage #:pine.desktop
  (:use #:cl)
  (:export #:defsurface #:set-surface-role #:push-surface #:show-panel #:hide-panel
           #:refresh-all #:*surface-client*))

(defpackage :pine.client
  (:use :cl)
  (:export
   #:client
   #:completion
   #:actor
   #:renderer
   #:paint-sink
   #:ts-actor
   #:render-state
   #:frame
   #:px-width
   #:px-height
   #:cell-w
   #:cell-h
   #:windows
   #:arrangement
   #:focused-window
   #:pending-keys
   #:prefix-arg
   #:this-command-key
   #:pending-key-reader
   #:mode-stack
   #:completion-state
   #:current-buffer
   #:buffer-modes
   #:buffer-minor-modes
   #:kill-ring
   #:kill-ring-max
   #:last-command
   #:server-of
   #:terminals
   #:terminal-map
   #:terminal-wake
   #:repl-buffer
   #:prompt-callback
   #:prompt-active
   #:prompt-history
   #:prompt-history-pos
   #:prompt-history-items
   #:minibuffer-buffer #:saved-buffer #:minibuffer-snap #:minibuffer-controller
   #:active-p
   #:candidates
   #:filtered
   #:index
   #:input
   #:callback
   #:prompt
   #:dynamic-fn
   #:popup-rows
   #:popup-tree
   #:*client*
   #:current-client
   #:buffer-in-scope
   #:make-window #:remove-window #:focus-window
   #:buffer-mode #:current-buffer-mode #:set-buffer-mode
   #:buffer-active-modes #:active-minor-modes #:active-keymaps
   #:active-modes-instance
   #:minor-mode-enabled-p #:enable-minor-mode #:disable-minor-mode
   #:toggle-minor-mode #:active-minor-mode-indicators
   #:buffer #:make-buffer #:kill-buffer #:switch-buffer
   #:list-buffers #:buffer-count
   #:current-buffer-text #:current-buffer-snapshot
   #:start-client
   #:stop-client))

(defpackage #:pine.ask
  (:use #:cl)
  (:export #:ask #:tell)
  (:documentation "Ask and tell: the scripting surface over the live system.
ASK queries the server, the client or a buffer; TELL messages a buffer. Above
modes and commands, so it can read the registries that hold them."))

(defpackage :pine.editor
  (:use :cl)
  (:export
   #:start-editor
   #:make-editor-session
   #:session-feed
   #:reseed-editor-sessions
   ;; the editor's live tree: view leaves the render walk refreshes
   #:editor-window-node
   #:editor-terminal-node
   #:editor-modeline-node
   #:editor-echo-node
   #:focused-snap
   #:scroll-window
   #:eval-last-sexp
   #:eval-buffer
   
   ;; layout buffers (authorable tool buffers)
   #:show-layout #:layout-node-at-point #:layout-select #:layout-activate
   ;; prompt
   #:prompt
   #:cancel-prompt
   ;; kill ring
   #:kill-ring-push
   #:kill-ring-top
   #:set-mark
   #:kill-region-cmd
   #:kill-line-cmd
   #:copy-region-cmd
   #:yank-cmd
   #:yank-pop-cmd
   ;; the completion facility: candidates, sources, actions, builders
   #:candidate #:to-candidate
   #:candidate-string #:candidate-annotation #:candidate-value
   #:candidate-category #:candidate-source
   #:register-source #:source-table
   #:register-actions #:candidate-actions
   #:completion-popup #:completion-widget
   ;; completing-read
   #:completing-read
   #:read-file-name
   #:file-completion-active-p
   #:file-name-complete
   #:file-name-accept
   #:completion-next
   #:completion-prev
   #:completion-update-input
   #:completing-read-active-p))

(defpackage :pine
  (:use :cl)
  (:export
   #:main
   #:start-daemon
   #:run-daemon
   #:stop
   ;; which frontends the daemon spawns and keeps alive; a config sets it
   #:*frontends*
   #:+frontend-unavailable+))

;;;; The client side: what a frontend is. Its backings declare their own
;;;; packages, since those rest on libraries this system does not load.

(defpackage #:pine.frontend
  (:use #:cl)
  (:export #:pump #:make-pump #:close-pump #:pump-wake-in
           #:enqueue #:wake #:pump-queued-p #:drain #:drain-wake
           #:backing #:wait-for-work #:dispatch-pending #:shutdown #:run
           #:attach)
  (:documentation "The client interface: what every frontend is, with no
platform in it. Bootstraps the actor system and the daemon attachment, and
owns the queue the daemon's threads hand work across."))
