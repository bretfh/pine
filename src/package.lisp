(defpackage :pine.server
  (:use :cl)
  (:export
   #:server
   #:*server*
   #:actor-system
   #:event-bus
   #:agent-registry
   #:buffer-registry
   #:buffer-table
   #:commands
   #:global-keymap
   #:faces
   #:ts-runtime
   #:modes
   #:clients
   #:remoting-port
   #:start-server
   #:stop-server
   #:*host*
   #:*port*
   #:daemon-uri
   #:local-uri))

(defpackage :pine.actor
  (:use :cl :ac :act :asys :rem)
  (:export
   #:agent-info
   #:agent-info-name #:agent-info-type #:agent-info-actor
   #:agent-info-meta #:agent-info-port
   #:start-agent-registry
   #:register-agent #:unregister-agent #:find-agent #:list-agents
   #:agent-eval #:agent-compile #:agent-run #:*local-agent*
   #:start-local-agent #:start-agent-debug #:*agent-debug-hook*
   #:start-agent-supervisor #:supervise-agent #:unsupervise-agent
   #:agent-alive-p #:request
   #:spawn-agent #:kill-agent))

(defpackage :pine.event
  (:use :cl)
  (:export
   #:make-event-bus
   #:publish
   #:subscribe
   #:unsubscribe))

(defpackage :pine.hooks
  (:use :cl)
  (:export
   #:add-init-hook
   #:add-shutdown-hook
   #:run-init-hooks
   #:run-shutdown-hooks))

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
   #:move-mark #:set-meta
   #:buffer-local
   #:refresh-highlights #:point-after-move
   #:line-indent-width #:previous-line-indent #:reindent-line
   #:load-content #:notify-subscribers #:split-lines
   #:line-count-of #:line-at #:region-string
   ;; faces
   #:face #:fg #:bg #:bold #:italic #:underline
   #:defface #:find-face #:face-attr-bits #:install-default-faces
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
   #:make-window #:remove-window #:focus-window
   #:window-display-lines #:ensure-point-visible #:ensure-col-visible
   ;; buffer actor
   #:make-buffer-actor #:notify-subscribers #:load-content
   ;; registry
   #:start-buffer-registry
   #:make-buffer #:kill-buffer #:switch-buffer
   #:list-buffers #:buffer-count
   #:current-buffer-text #:current-buffer-snapshot
   ;; high-level API
   #:buffer #:ask #:tell))

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

(defpackage #:pine.key
  (:use #:cl)
  (:export #:key #:key-p #:make-key
           #:key-sym #:key-ctrl #:key-meta #:key-shift #:key-super
           #:key= #:parse-key #:key->string))

(defpackage #:pine.keymap
  (:use #:cl)
  (:export #:keymap #:keymap-p #:make-keymap #:keymap-name #:keymap-parent
           #:define-key #:keymap-lookup #:prefix-p #:keymap-tables
           #:keymap-bindings))

(defpackage #:pine.command
  (:use #:cl)
  (:export #:command #:command-name #:command-fn #:command-arguments #:command-prefix-p
           #:command-key
           #:define-command #:register-command #:find-command #:all-command-names
           #:execute #:call-command #:dispatch #:command-error
           #:self-insert #:self-insert-key-p
           #:prefix-numeric-value #:this-command-key
           #:key-binding #:read-next-key
           #:*terminal-handler*))

(defpackage :pine.mode
  (:use :cl)
  (:export
   #:mode #:major-mode #:minor-mode
   #:base-mode #:text-mode #:lisp-mode #:repl-mode #:terminal-mode
   #:overwrite-mode
   #:mode-name #:mode-keymap #:mode-indicator #:parent-mode #:ts-language
   #:precedence #:transparent
   #:register-mode #:find-mode #:global-keymap
   #:buffer-mode #:current-buffer-mode #:set-buffer-mode #:mode-for-file
   #:auto-mode #:*auto-modes*
   #:buffer-active-modes #:buffer-minor-modes #:active-keymaps
   #:active-modes-instance
   #:minor-mode-enabled-p #:enable-minor-mode #:disable-minor-mode
   #:toggle-minor-mode #:active-minor-mode-indicators
   #:dispatch-message #:install-default-modes))

(defpackage #:pine.store
  (:use #:cl)
  (:export #:store #:store-push #:store-items #:store-clear #:store-forget
           #:open-store #:close-store)
  (:documentation "The persistence facility: one SQLite store the daemon
owns. (store K) reads a durable value, (setf (store K) V) writes;
store-push/store-items keep bounded lists (histories, recents). Modes
declare what persists -- via defonce/defref :persist or their own store
keys -- and never touch files."))

(defpackage #:pine.wm
  (:use #:cl)
  (:export #:wm-keymap #:binding-table #:push-bindings #:run-binding
           #:attached-p #:install-wm-sessions #:leaves #:focused-leaf
           #:spawn #:close-window #:focus-step #:split #:exit-session)
  (:documentation "Window management policy: the keymap whose chords are
registered with the compositor, the commands they run, and the actions sent
to the wm frontend. design/wm.org is the contract."))

(defpackage #:pine.world
  (:use #:cl)
  (:export #:register #:save-world #:restore-world)
  (:documentation "What comes back after a restart. Subsystems register
:save/:restore function pairs; the data rides (:world NAME) store keys.
Gated by the :world-save editor variable."))

(defpackage :pine.var
  (:use :cl)
  (:export
   ;; the API: declare once, one setf-able accessor
   #:defonce #:var
   ;; introspection (describe-variables)
   #:find-variable #:all-variable-names #:variable-scope
   #:evar-default #:evar-documentation))

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
   #:node->wire #:wire->node #:arranged-p
   ;; live-tree surgery, shared by every arranged tree
   #:node-parent #:replace-child #:remove-with-divider
   #:split-node #:remove-node
   ;; hit-testing + selection
   #:node-at #:action-at #:click-thunk #:slider-value-at #:hint-at
   #:collect-selectables
   #:scroll-to-selection))

(defpackage #:pine.attach
  (:use #:cl)
  (:export #:start-attach-listener #:attach-listener
           #:attach-to-daemon #:register-app-kind
           #:attached-client #:attached-client-id #:attached-client-kind
           #:attached-client-display #:attached-client-input #:attached-client-session
           #:*clients* #:push-to-app))

(defpackage #:pine.agent
  (:use #:cl)
  (:export #:connect #:serve #:*agent-system* #:*name*))

(defpackage #:pine.jobs
  (:use #:cl)
  (:export #:install-jobs #:list-jobs #:*agent-jobs*))

(defpackage #:pine.ref
  (:use #:cl)
  (:export #:ref #:make-ref #:defref #:find-ref #:ref-name
           #:ref-value #:deref #:set-ref #:update-ref
           #:ref-subscribe #:ref-unsubscribe
           #:reactive-view #:make-view #:render-view #:dispose-view)
  (:documentation "Reactive named values: (ref NAME) reads (tracked),
(setf (ref NAME) V) writes (deduped, notifying)."))

(defpackage #:pine.source
  (:use #:cl)
  (:export #:start-sources #:stop-sources #:workspaces
           #:defsource #:defpoll
           #:select! #:act! #:select-sink!))

(defpackage #:pine.style
  (:use #:cl)
  (:export #:style #:st-bg #:st-gradient #:st-fg #:st-border-w #:st-border-color
           #:st-radius #:st-pad-x #:st-pad-y #:st-min-w #:st-min-h
           #:st-font-px #:st-bold #:st-inset #:st-margin #:st-shadow
           #:st-opacity
           #:resolve #:reset-rules))

(defpackage #:pine.desktop
  (:use #:cl)
  (:export #:install-desktop-sessions
           #:defsurface #:set-surface-role #:push-surface #:show-panel #:hide-panel
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
   #:start-client
   #:stop-client))

(defpackage :pine.editor
  (:use :cl)
  (:export
   #:start-editor
   #:install-commands
   #:install-bindings
   #:install-editor-sessions
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
   #:eval-in-target
   ;; layout buffers (authorable tool buffers)
   #:show-layout #:layout-node-at-point #:layout-select #:layout-activate
   ;; prompt
   #:prompt
   #:cancel-prompt
   ;; kill ring
   #:kill-ring-push
   #:kill-ring-top
   #:set-mark
   #:region-bounds
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
   #:stop))

(defpackage :pine.user
  (:nicknames :pine-user)
  (:use :cl)
  (:import-from :pine.layout
                #:column #:row #:centerbox #:label #:icon #:button #:ring
                #:gap #:rule #:rows #:choice)
  (:import-from :pine.editor
                #:candidate #:register-source #:register-actions
                #:candidate-actions #:completion-widget)
  (:import-from :pine.buffer
                #:deftheme #:defface #:load-theme #:color #:metric #:face-fg
                #:make-buffer #:kill-buffer #:switch-buffer #:list-buffers
                #:ask #:tell)
  (:import-from :pine.ref #:defref #:ref)
  (:import-from :pine.store #:store #:store-push #:store-items #:store-clear)
  (:import-from :pine.world #:register)
  (:import-from :pine.source #:defsource #:defpoll)
  (:import-from :pine.command #:call-command #:execute)
  (:import-from :pine.mode #:find-mode #:current-buffer-mode #:set-buffer-mode
                #:dispatch-message #:auto-mode)
  (:import-from :pine.var #:defonce #:var)
  (:import-from :pine.client #:current-client #:current-buffer)
  (:import-from :pine.editor #:eval-last-sexp #:eval-buffer
                #:completing-read #:read-file-name #:prompt
                #:show-layout #:layout-node-at-point #:layout-select
                #:layout-activate)
  (:import-from :pine.echo #:message)
  (:export
   ;; surfaces
   #:defsurface #:show #:hide #:toggle
   ;; widgets: containers
   #:column #:row #:centerbox #:center #:box #:scroll
   ;; widgets: content
   #:label #:icon #:image #:rule #:gap
   ;; widgets: controls
   #:button #:slider #:ring #:choice #:rows
   ;; widgets: views -- a window is a live view of the buffer you name
   #:calendar #:window #:buffer #:terminal #:modeline #:echo #:minibuffer
   ;; completion facility
   #:candidate #:register-source #:register-actions #:candidate-actions
   #:completion-widget
   ;; style
   #:deftheme #:defface #:load-theme #:color #:metric #:face-fg
   ;; data
   #:defref #:ref #:defpoll #:defsource #:sh #:launch
   ;; persistence
   #:store #:store-push #:store-items #:store-clear #:register
   ;; style rules
   #:defrules
   ;; behavior
   #:defcommand #:call-command
   ;; keys
   #:kbd #:keymap #:define-key #:global-set-key
   ;; modes -- defmode/defminor make real classes; execute/dispatch-message are
   ;; the CLOS behaviour hooks users specialise
   #:defmode #:defminor #:execute #:dispatch-message #:auto-mode
   #:enable-minor-mode #:disable-minor-mode #:toggle-minor-mode
   ;; editor variables
   #:defonce #:var
   ;; prompts + echo
   #:completing-read #:read-file-name #:prompt #:message
   ;; layout buffers (authorable tool buffers)
   #:show-layout #:layout-node-at-point #:layout-select #:layout-activate
   ;; processes
   #:defagent #:spawn #:supervise #:kill
   ;; buffers / editor
   #:make-buffer #:kill-buffer #:switch-buffer #:list-buffers
   #:ask #:tell #:current-client #:current-buffer
   #:find-mode #:current-buffer-mode #:set-buffer-mode
   #:eval-last-sexp #:eval-buffer))
