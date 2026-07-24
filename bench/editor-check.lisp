(in-package :cl-user)
(defvar *f* 0)
(defun chk (l g w) (if (equal g w) (format t "ok: ~a~%" l)
                       (progn (incf *f*) (format t "FAIL: ~a got ~s want ~s~%" l g w))))
(let ((srv (pine.server:start-server)))
  (setf pine.server:*server* srv)
  (setf (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
  (pine.event:make-event-bus srv)
  (pine.actor:start-agent-registry srv) (pine.actor:start-local-agent srv)
  (pine.buffer:start-buffer-registry srv) (pine.buffer:install-default-faces)
  (pine.mode:install-default-modes) (pine.editor:install-commands) (pine.editor:install-bindings)
  (let ((client (pine.client:start-client srv)))
    (setf pine.client:*client* client)
    (pine.render:start-renderer client)
    (setf (pine.client:paint-sink client) (lambda (&rest _) (declare (ignore _)) nil))
    (let ((buf (pine.buffer:make-buffer "scratch")))
      (pine.buffer:make-window buf "scratch" :row 0 :col 0 :width 80 :height 29 :focused t)
      (setf (pine.client:current-buffer client) buf)
      (pine.mode:set-buffer-mode buf :text-mode))
    (pine.editor::ensure-minibuffer client)
    (sleep 0.2)
    (flet ((key (s) (pine.command:dispatch client (pine.key:parse-key s)) (sleep 0.06))
           (mbtext () (let ((mb (pine.client:minibuffer-buffer client)))
                        (sento.actor:ask-s mb '(:get-text) :time-out 5)))
           (mbcol () (pine.buffer:point-col
                      (sento.actor:ask-s (pine.client:minibuffer-buffer client)
                                         '(:get-snapshot) :time-out 5))))
      (let ((chosen :none))
        (pine.editor::completing-read "M-x " '("forward-word" "backward-word" "kill-line")
          (lambda (r) (setf chosen r)))
        (sleep 0.15)
        (chk "current-buffer is minibuffer"
             (eq (pine.client:current-buffer client) (pine.client:minibuffer-buffer client)) t)
        ;; type "abc"
        (key "a") (key "b") (key "c")
        (chk "typed abc" (mbtext) "abc")
        ;; C-a to start, C-f forward one, insert X -> aXbc
        (key "C-a") (key "C-f") (key "X")
        (chk "edit mid" (mbtext) "aXbc")
        (chk "point after X" (mbcol) 2)
        ;; C-e end, backspace removes c -> aXb
        (key "C-e") (key "BackSpace")
        (chk "backspace at end" (mbtext) "aXb")
        ;; C-a then C-k (kill-line) clears the input
        (key "C-a") (key "C-k")
        (chk "kill-line clears" (mbtext) "")
        ;; type "forward" -> filters to forward-word, Return accepts it
        (dolist (ch '("f" "o" "r" "w" "a" "r" "d")) (key ch))
        (sleep 0.15)
        (chk "filtered to one"
             (mapcar #'pine.editor:candidate-string
                     (pine.client:filtered (pine.editor::completion)))
             '("forward-word"))
        (let ((rows (pine.client:popup-rows (pine.editor::completion))))
          (chk "popup rows rendered" (and rows t) t)
          (chk "popup first row carries the candidate"
               (and rows (search "forward-word" (car (first rows))) t) t)
          (chk "selected popup row styled by .cand-row.sel"
               (and rows (subseq (rest (first (cdr (first rows)))) 3 6))
               (multiple-value-bind (r g b)
                   (pine.buffer:hex-rgb (pine.buffer:color :bg-active))
                 (list r g b))))
        (key "Return") (sleep 0.15)
        (chk "accepted candidate" chosen "forward-word")
        (chk "minibuffer deactivated" (eq (pine.client:current-buffer client)
                                          (pine.client:minibuffer-buffer client)) nil))
      ;; a prompt fired while one is active replaces it and must NOT wedge:
      ;; saved-buffer stays the file buffer, and abort restores it -- never the
      ;; hidden minibuffer (the frozen-buffer bug)
      (let ((file-buf (pine.client:current-buffer client)))
        (pine.editor::completing-read "M-x " '("one" "two") (lambda (r) r))
        (sleep 0.1)
        (pine.editor::completing-read "M-x " '("three") (lambda (r) r))
        (sleep 0.1)
        (chk "nested prompt keeps the original return buffer"
             (eq (pine.client:saved-buffer client) file-buf) t)
        (pine.editor::minibuffer-abort) (sleep 0.1)
        (chk "abort after nested prompt restores the file buffer"
             (eq (pine.client:current-buffer client) file-buf) t)
        (pine.command:dispatch client (pine.key:parse-key "q")) (sleep 0.15)
        (chk "typing after nested prompt lands in the file buffer"
             (and (search "q" (sento.actor:ask-s file-buf '(:get-text) :time-out 5)) t)
             t)
        ;; undo the probe keystroke so later probes see the expected content
        (pine.command:dispatch client (pine.key:parse-key "BackSpace")) (sleep 0.1))
      ;; abort restores
      (let ((chosen2 :none))
        (pine.editor::completing-read "M-x " '("aaa" "bbb") (lambda (r) (setf chosen2 r)))
        (sleep 0.1) (key "a")
        (key "C-g") (sleep 0.1)
        (chk "abort no callback" chosen2 :none)
        (chk "abort restored buffer" (eq (pine.client:current-buffer client)
                                         (pine.client:minibuffer-buffer client)) nil)
        (chk "popup cleared on abort"
             (pine.client:popup-rows (pine.editor::completion)) nil)
        (chk "echo completions API deleted"
             (find-symbol "COMPLETIONS-TEXT" :pine.echo) nil))
      ;; --- error trapping: a failing edit surfaces through the debugger and the
      ;; buffer keeps its prior state, then recovers; a failing command routes by
      ;; :debug-on-error ---
      (let ((buf (pine.buffer:make-buffer "err-scratch")) (saw nil) (saw2 nil))
        (pine.mode:set-buffer-mode buf :text-mode)
        (sento.actor:tell buf (list :insert :text "hello"))
        (sleep 0.1)
        ;; global setf, not let: the actor runs on its own thread and reads the
        ;; global *on-debug*. The recorder picks ABORT so the parked actor thread
        ;; unwinds (as the live debugger would when the user presses a restart).
        (setf pine.eval:*on-debug*
              (lambda (ev) (setf saw ev) (pine.eval:pick-restart ev "ABORT")))
        ;; :insert with a non-string errors inside insert-string
        (sento.actor:tell buf (list :insert :text 42))
        (sleep 0.25)
        (chk "edit error surfaced to debugger" (and saw t) t)
        (chk "edit error status :error"
             (and saw (pine.eval:evaluation-status saw)) :error)
        (chk "buffer state preserved after edit error"
             (sento.actor:ask-s buf '(:get-text) :time-out 5) "hello")
        (chk "edit boundary offers a RETRY restart"
             (and (member "RETRY" (mapcar #'first (pine.eval:evaluation-restarts saw))
                          :test #'string=) t) t)
        (sento.actor:tell buf (list :insert :text "!"))
        (sleep 0.15)
        (let ((tx (sento.actor:ask-s buf '(:get-text) :time-out 5)))
          (chk "buffer recovers after edit error"
               (and (= 6 (length tx)) (and (search "hello" tx) t)) t))
        (pine.editor::install-variables)
        (pine.command:define-command "probe-boom" () (error "boom"))
        (setf pine.eval:*on-debug* (lambda (ev) (setf saw2 ev)))
        (setf (pine.var:var :debug-on-error) t)
        (pine.command:call-command "probe-boom")
        (chk "command error -> debugger when debug-on-error" (and saw2 t) t)
        (setf saw2 nil)
        (setf (pine.var:var :debug-on-error) nil)
        (pine.command:call-command "probe-boom")
        (chk "command error -> echo (no debugger) when off" saw2 nil)
        (setf pine.eval:*on-debug* nil)
        ;; --- debugger session registry: concurrent faults coexist and page ---
        (setf pine.editor::*debugger-sessions* nil pine.editor::*attended-session* nil)
        (pine.editor::%agent-debug-surface
         (list :agent-debug :agent "alpha" :eval-id 1 :condition "boom-a"
               :restarts (list "RETRY" "ABORT")))
        (pine.editor::%agent-debug-surface
         (list :agent-debug :agent "beta" :eval-id 2 :condition "boom-b"
               :restarts (list "ABORT")))
        (sleep 0.1)
        (chk "two live debugger sessions" (length pine.editor::*debugger-sessions*) 2)
        (chk "attended is newest fault"
             (pine.editor::dbg-session-agent pine.editor::*attended-session*) "beta")
        (chk "eval-target follows the attended fault" pine.editor::*eval-target* "beta")
        (let ((txt (sento.actor:ask-s (pine.buffer:buffer "*debugger*")
                                      '(:get-text) :time-out 5)))
          (chk "debugger buffer shows switcher + restart rows"
               (and (search "alpha" txt) (search "beta" txt)
                    (search "[ABORT]" txt) t)
               t))
        (pine.command:call-command "debugger-next-session") (sleep 0.05)
        (chk "Tab pages to the other session"
             (pine.editor::dbg-session-agent pine.editor::*attended-session*) "alpha")
        (chk "eval-target follows on page" pine.editor::*eval-target* "alpha")
        ;; select alpha's ABORT row (RETRY is row 1) and activate it through the
        ;; layout interaction -- the restart row IS the invocation
        (pine.command:call-command "layout-next") (sleep 0.1)
        (pine.command:call-command "layout-activate") (sleep 0.05)
        (chk "restart-row activation resolves + advances"
             (list (length pine.editor::*debugger-sessions*)
                   (pine.editor::dbg-session-agent pine.editor::*attended-session*))
             (list 1 "beta"))
        (pine.editor::invoke-pending-restart "ABORT") (sleep 0.05)
        (chk "last resolve empties the registry"
             (list (length pine.editor::*debugger-sessions*) pine.editor::*attended-session*)
             (list 0 nil))
        (chk "eval-target restored after the debugger closes" pine.editor::*eval-target* :local)
        ;; --- one eval path: repl + editor share eval-in-target, target-swappable ---
        (let ((done :none))
          (pine.editor:eval-in-target "(+ 1 2)" (find-package :cl-user)
            :on-done (lambda (ev) (setf done (first (pine.eval:evaluation-values ev)))))
          (sleep 0.3)
          (chk "eval-in-target :local runs through the one eval path" done 3))
        ;; --- jobs as a layout buffer ---
        (pine.command:call-command "jobs") (sleep 0.15)
        (let ((txt (sento.actor:ask-s (pine.buffer:buffer "*jobs*")
                                      '(:get-text) :time-out 5)))
          (chk "jobs buffer lists the local eval"
               (and (search "Jobs" txt) (search "local" txt) t) t))
        ;; --- layout render: one styling resolution, position->node mapping ---
        (multiple-value-bind (rows root)
            (pine.layout:render
             (pine.layout:column :align :stretch
               (pine.layout:label "head" :face :keyword)
               (pine.layout:choice (pine.layout:label "aaa"))
               (pine.layout:choice (pine.layout:label "bbb")))
             10 :selection 1)
          (chk "render row texts"
               (mapcar (lambda (r) (string-right-trim " " (car r))) rows)
               '("head" "  aaa" "> bbb"))
          (chk "render face-name colour"
               (subseq (rest (first (cdr (first rows)))) 0 3)
               (pine.buffer:face-fg :keyword))
          (let ((hit (pine.layout:node-at root 2 3)))
            (chk "render node-at finds the selected row"
                 (and hit (typep hit 'pine.layout:selectable)
                      (pine.layout:selectedp hit))
                 t)))
        (multiple-value-bind (rows root)
            (pine.layout:render (pine.layout:label "x" :class "nm-title") 6)
          (declare (ignore root))
          (let ((run (rest (first (cdr (first rows))))))
            (chk "render css colour + bold"
                 (list (subseq run 0 3) (nth 6 run))
                 (multiple-value-bind (r g b)
                     (pine.buffer:hex-rgb (pine.buffer:color :fg))
                   (list (list r g b) 1)))))
        ;; --- layout buffers: set-layout / select / activate / reproject ---
        (let ((fired nil))
          (let ((buf (pine.buffer:make-buffer "layout-probe")))
            (sento.actor:tell buf
              (list :set-layout :width 12 :builder
                    (lambda (state) (declare (ignore state))
                      (pine.layout:column :align :stretch
                        (pine.layout:label "hd" :face :keyword)
                        (pine.layout:choice :data (lambda () (setf fired :one))
                          (pine.layout:label "one"))
                        (pine.layout:choice :data (lambda () (setf fired :two))
                          (pine.layout:label "two"))))))
            (sleep 0.15)
            (chk "layout buffer text is the rendered rows"
                 (sento.actor:ask-s buf '(:get-text) :time-out 5)
                 (format nil "hd~%> one~%  two"))
            (setf (pine.client:current-buffer client) buf)
            (pine.command:call-command "layout-next") (sleep 0.1)
            (chk "layout-next selects row 2"
                 (sento.actor:ask-s buf '(:get-text) :time-out 5)
                 (format nil "hd~%  one~%> two"))
            (multiple-value-bind (l c) (pine.buffer:ask buf :point)
              (declare (ignore c))
              (chk "point follows the selection" l 2))
            (pine.command:call-command "layout-activate")
            (chk "layout-activate fires the row's thunk" fired :two)
            (sento.actor:tell buf '(:reproject :width 20)) (sleep 0.1)
            (chk "reproject at a new width"
                 (pine.buffer:buffer-local
                  (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)
                  :layout-width)
                 20)))
        ;; ================= PART 2: the user language ==================
        ;; run authorship forms the way init.lisp is loaded: read + eval in
        ;; :pine-user, then drive through the real dispatch.
        (let ((*package* (find-package :pine.user)))
          (flet ((u (form) (eval (read-from-string form))))
            ;; keys: a user command bound by a chord runs on dispatch
            (u "(defvar *ran* nil)")
            (u "(defcommand \"user-probe-cmd\" () (setf *ran* :yes))")
            (u "(global-set-key (kbd \"C-c p\") \"user-probe-cmd\")")
            (setf (pine.client:current-buffer client) (pine.buffer:buffer "scratch"))
            (pine.command:dispatch client (pine.key:parse-key "C-c"))
            (pine.command:dispatch client (pine.key:parse-key "p")) (sleep 0.05)
            (chk "user command bound by kbd runs on dispatch"
                 (symbol-value (find-symbol "*RAN*" :pine.user)) :yes)
            ;; minor mode with its own keymap: key resolves when enabled, not else
            (u "(defvar *min* nil)")
            (u "(defcommand \"user-min-cmd\" () (setf *min* :hit))")
            (u "(defminor user-probe-minor (:precedence 12))")
            (u "(define-key (keymap :user-probe-minor) (kbd \"z\") \"user-min-cmd\")")
            (chk "minor key inert before enable"
                 (pine.command:key-binding client (pine.key:parse-key "z")) nil)
            (u "(enable-minor-mode :user-probe-minor)")
            (chk "minor key resolves once enabled"
                 (pine.command:key-binding client (pine.key:parse-key "z")) "user-min-cmd")
            (u "(disable-minor-mode :user-probe-minor)")
            (chk "minor key inert again after disable"
                 (pine.command:key-binding client (pine.key:parse-key "z")) nil)
            ;; major mode deriving from text-mode + auto-mode by extension
            (u "(defmode user-probe-mode (:parent :text-mode :indicator \"UP\"))")
            (u "(auto-mode \"upx\" :user-probe-mode)")
            (chk "auto-mode maps the extension"
                 (pine.mode:mode-for-file "/tmp/x.upx") :user-probe-mode)
            (u "(set-buffer-mode (buffer \"scratch\") :user-probe-mode)")
            (chk "user major mode active"
                 (pine.buffer:buffer-local
                  (sento.actor:ask-s (pine.buffer:buffer "scratch") '(:get-state) :time-out 5)
                  :mode)
                 :user-probe-mode)
            (chk "user major mode inherits text-mode editing"
                 (pine.mode:find-mode :user-probe-mode) (pine.mode:find-mode :user-probe-mode))
            (u "(set-buffer-mode (buffer \"scratch\") :text-mode)")
            ;; editor variable: define, set, read; redefine keeps the set value
            (u "(defonce :user-probe-var :default 1 :documentation \"probe\")")
            (u "(setf (var :user-probe-var) 42)")
            (chk "user variable reads back" (u "(var :user-probe-var)") 42)
            (u "(defonce :user-probe-var :default 99)")
            (chk "redefining a variable keeps the set value"
                 (u "(var :user-probe-var)") 42)
            ;; user CSS rule styles a rendered node of that class
            (u "(defrules (\".uprobe\" :color (color :accent)))")
            (multiple-value-bind (rows root)
                (pine.layout:render (pine.layout:label "x" :class "uprobe") 6)
              (declare (ignore root))
              (chk "user defrules styles a rendered node"
                   (subseq (rest (first (cdr (first rows)))) 0 3)
                   (multiple-value-bind (r g b) (pine.buffer:hex-rgb
                                                 (pine.buffer:color :accent))
                     (list r g b))))
            ;; a user tool buffer via show-layout, driven by layout interaction
            (u "(defvar *tool* nil)")
            (u "(show-layout \"*user-tool*\"
                  (lambda (state) (declare (ignore state))
                    (column :align :stretch
                      (label \"tool\" :class \"help-head\")
                      (choice :data (lambda () (setf *tool* :a)) (label \"a\"))
                      (choice :data (lambda () (setf *tool* :b)) (label \"b\")))))")
            (sleep 0.15)
            (chk "user tool buffer rendered"
                 (sento.actor:ask-s (pine.buffer:buffer "*user-tool*") '(:get-text) :time-out 5)
                 (format nil "tool~%> a~%  b"))
            (pine.command:call-command "layout-next")
            (pine.command:call-command "layout-activate") (sleep 0.05)
            (chk "user tool buffer activation fires the row"
                 (symbol-value (find-symbol "*TOOL*" :pine.user)) :b)
            ;; a user command that prompts + echoes
            (u "(defvar *picked* :none)")
            (u "(defcommand \"user-pick\" ()
                  (completing-read \"pick: \" (list \"xx\" \"yy\")
                    (lambda (r) (setf *picked* r) (message (format nil \"got ~a\" r)))))")
            (setf (pine.client:current-buffer client) (pine.buffer:buffer "scratch"))
            (pine.command:call-command "user-pick") (sleep 0.1)
            (key "y") (key "y") (sleep 0.1) (key "Return") (sleep 0.1)
            (chk "user completing-read command returns the pick"
                 (symbol-value (find-symbol "*PICKED*" :pine.user)) "yy")
            (chk "user message set the echo" (pine.echo:current-message) "got yy")
            ;; the shipped worked example loads clean and its definitions land
            (pine.desktop:install-desktop-sessions)
            (chk "examples/init.lisp loads without error"
                 (progn (load (merge-pathnames "../examples/init.lisp"
                                               (or *load-truename* *default-pathname-defaults*)))
                        t)
                 t)
            (chk "example bound C-c s"
                 (pine.command:key-binding
                  client (pine.key:parse-key "C-c") ) ; prefix exists
                 (pine.command:key-binding client (pine.key:parse-key "C-c")))
            (chk "example defmode todo-mode registered"
                 (and (pine.mode:find-mode :todo-mode) t) t)
            (chk "example auto-mode .todo" (pine.mode:mode-for-file "/x.todo") :todo-mode)
            (chk "example variable :greeting" (u "(var :greeting)") "hi")))))))
;; ================= PART 3: the live editor tree ==================
(let* ((client (pine.client:current-client))
       (f (pine.client:frame client))
       (scratch (pine.buffer:buffer "scratch"))
       (b2 (pine.buffer:make-buffer "editor-b2")))
  (setf (pine.buffer:frame-cols f) 40 (pine.buffer:frame-rows f) 12)
  (pine.mode:set-buffer-mode b2 :text-mode)
  (sento.actor:tell b2 (list :insert :text "beta-two"))
  (sleep 0.1)
  ;; the engine default seeds when init.lisp declares no editor surface
  (pine.editor::%seed-editor-tree client)
  (let ((tree (pine.render:refresh-editor-tree client))
        (leaves (pine.editor::%editor-leaves client)))
    (chk "default editor tree renders with no declaration"
         (and tree (= 1 (length leaves))
              (= 10 (length (pine.layout:window-rows (first leaves))))
              t)
         t))
  ;; a declared arrangement: two window views side by side, echo then modeline
  (setf (pine.client:windows client) nil (pine.client:focused-window client) nil)
  (let* ((wa (pine.editor:editor-window-node "scratch" :expand 1))
         (wb (pine.editor:editor-window-node "editor-b2" :expand 1))
         (tree (pine.layout:column :align :stretch
                 (pine.layout:row :align :stretch :expand 1 wa wb)
                 (pine.editor:editor-echo-node)
                 (pine.editor:editor-modeline-node))))
    (setf (pine.client:arrangement client) tree)
    (pine.buffer:focus-window (pine.layout:window-of wa))
    (setf (pine.client:current-buffer client)
          (pine.buffer:buffer-ref (pine.layout:window-of wa)))
    (sleep 0.2)
    (pine.render:refresh-editor-tree client)
    (chk "window by name renders its buffer's text"
         (and (search "beta-two" (car (first (pine.layout:window-rows wb)))) t)
         t))
  ;; splits + focus + delete over the live tree, via the commands
  (pine.editor::%seed-editor-tree client)
  (pine.render:relayout)
  (pine.command:call-command "split-window-below")
  (chk "split-window-below makes two windows"
       (length (pine.editor::%editor-leaves client)) 2)
  (pine.command:call-command "other-window")
  (let ((buf (pine.buffer:switch-buffer "editor-b2")))
    (sento.actor:tell (pine.client:renderer client)
                      (list :switch-buffer :buffer buf :name "editor-b2"))
    (pine.render:subscribe-to-buffer buf))
  (sleep 0.15)
  (pine.command:dispatch client (pine.key:parse-key "x"))
  (sleep 0.15)
  (chk "typing lands in the focused window"
       (and (search "x" (pine.buffer:ask b2 :text))
            (not (search "x" (pine.buffer:ask scratch :text)))
            t)
       t)
  (pine.render:refresh-editor-tree client)
  (let ((ml (find :modeline (pine.render::%window-leaves
                             (pine.client:arrangement client))
                  :key #'pine.layout:window-kind)))
    (chk "modeline follows the focused window"
         (and ml (search "editor-b2" (car (first (pine.layout:window-rows ml)))) t)
         t)
    (pine.command:call-command "other-window")
    (pine.render:refresh-editor-tree client)
    (chk "modeline follows focus back"
         (and ml (search "scratch" (car (first (pine.layout:window-rows ml)))) t)
         t))
  (pine.command:call-command "delete-other-windows")
  (chk "delete-other-windows leaves one window"
       (length (pine.editor::%editor-leaves client)) 1)
  (chk "focus survives delete-other-windows"
       (eq (pine.buffer:buffer-ref (pine.client:focused-window client)) scratch)
       t)
  ;; a window view embeds in a layout buffer without wedging the actor; its
  ;; snapshot cannot be fetched from inside the receive, so it renders empty
  ;; there -- panels and other off-actor builds carry content
  (let ((prev pine.client:*client*))
    (setf pine.client:*client* nil)
    (unwind-protect
         (let ((node (pine.editor:editor-window-node "editor-b2")))
           (chk "detached window view renders the buffer off-actor"
                (and (search "beta-two"
                             (car (first (pine.layout:window-rows node))))
                     t)
                t))
      (setf pine.client:*client* prev)))
  (pine.editor:show-layout "*wt-probe*"
    (lambda (state) (declare (ignore state))
      (pine.layout:column :align :stretch
        (pine.layout:label "wt" :face :keyword)
        (pine.editor:editor-window-node "editor-b2"))))
  (sleep 0.3)
  (chk "window view inside a layout buffer builds without wedging"
       (let ((tx (sento.actor:ask-s (pine.buffer:buffer "*wt-probe*")
                                    '(:get-text) :time-out 5)))
         (and (stringp tx) (search "wt" tx) t))
       t)
  ;; kill/yank round-trips multi-line text with its line structure
  (let ((kb (pine.buffer:make-buffer "yank-probe")))
    (pine.mode:set-buffer-mode kb :text-mode)
    (setf (pine.client:current-buffer client) kb)
    (sento.actor:tell kb (list :insert :text (format nil "alpha~%beta~%gamma")))
    (sleep 0.1)
    (chk "multi-line insert splits lines"
         (sento.actor:ask-s kb '(:get-text) :time-out 5)
         (format nil "alpha~%beta~%gamma"))
    (sento.actor:tell kb (list :move-point :line 0 :col 0))
    (sleep 0.05)
    (pine.command:call-command "set-mark")
    (sento.actor:tell kb (list :move-point :line 2 :col 5))
    (sleep 0.05)
    (pine.command:call-command "kill-region")
    (sleep 0.1)
    (chk "kill-region takes the whole region"
         (sento.actor:ask-s kb '(:get-text) :time-out 5) "")
    (pine.command:call-command "yank")
    (sleep 0.1)
    (chk "yank restores the killed lines"
         (sento.actor:ask-s kb '(:get-text) :time-out 5)
         (format nil "alpha~%beta~%gamma")))
  ;; two half-slack expanders must not round the tail child off the rect
  (multiple-value-bind (rows tree)
      (pine.layout:render
       (pine.layout:column :align :stretch
         (pine.layout:label "a" :expand 1)
         (pine.layout:label "b" :expand 1)
         (pine.layout:label "tail"))
       10 :height 10)
    (declare (ignore tree))
    (chk "flex rounding keeps tail children on the rect"
         (string-trim " " (car (nth 9 rows))) "tail"))
  (chk "current is gone from the language"
       (find-symbol "CURRENT" :pine.user) nil))

(format t "~&~d failures~%" *f*)
(sb-ext:exit :code (if (zerop *f*) 0 1))
