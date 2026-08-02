(in-package :pine.test)
(named-readtables:in-readtable pine.data:syntax)

(def-suite* :pine.editor :in :pine)

;;;; The editor over a real server and real buffer actors. Everything here
;;;; runs against the shared substrate through the SUBSTRATE fixture, which
;;;; puts the session back afterwards.

(test the-minibuffer-is-a-real-buffer
  (with-fixture substrate ()
    (let ((chosen :none))
      (pine.ns:write (pine.path:parse "/echo")
                     {:prompt "M-x "
                      :complete '("forward-word" "backward-word" "kill-line")
                      :then (lambda (r) (setf chosen r))})
      (sleep 0.15)
      (is (string= pine.echo:+buffer+ (pine.buf:current-name)))
      (type-text "abc")
      (is (string= "abc" (minibuffer-input)))
      (press* "C-a" "C-f" "X")
      (is (string= "aXbc" (minibuffer-input)))
      (is (= 2 (minibuffer-column)))
      (press* "C-e" "BackSpace")
      (is (string= "aXb" (minibuffer-input)))
      (press* "C-a" "C-k")
      (is (string= "" (minibuffer-input)))
      (type-text "forward")
      (sleep 0.15)
      (is (equal '("forward-word")
                 (mapcar #'pine.echo:candidate-string
                         (pine.echo:said :filtered))))
      (press "Return")
      (sleep 0.15)
      (is (string= "forward-word" chosen))
      (is (not (equal pine.echo:+buffer+ (pine.buf:current-name)))))))

(test the-completion-popup-styles-the-selected-row
  (with-fixture substrate ()
    (pine.ns:write (pine.path:parse "/echo")
                   {:prompt "M-x " :complete '("aaa" "bbb") :then (lambda (r) r)})
    (sleep 0.15)
    (let ((rows (pine.echo:popup-rows)))
      (is (not (null rows)))
      (is (search "aaa" (car (first rows))))
      (is (equal (multiple-value-list
                  (pine.ui.face:hex-rgb (pine.ui.face:color :bg-active)))
                 (subseq (rest (first (cdr (first rows)))) 3 6))))
    (press "C-g")))

(test a-prompt-fired-over-a-prompt-keeps-the-original-return-buffer
  (with-fixture substrate ()
    (let ((file-buf (pine.editor.frame::current-buffer)))
      (pine.ns:write (pine.path:parse "/echo")
                     {:prompt "M-x " :complete '("one" "two") :then (lambda (r) r)})
      (sleep 0.1)
      (pine.ns:write (pine.path:parse "/echo")
                     {:prompt "M-x " :complete '("three") :then (lambda (r) r)})
      (sleep 0.1)
      (is (fset:equal? (pine.buf:at file-buf) (pine.echo:said :back)))
      (pine.echo:abort)
      (sleep 0.1)
      (is (eq file-buf (pine.editor.frame::current-buffer)))
      (press "q")
      (sleep 0.15)
      (is (search "q" (btext file-buf)))
      (press "BackSpace"))))

(test c-g-aborts-a-prompt-without-firing-its-callback
  (with-fixture substrate ()
    (let ((chosen :none))
      (pine.ns:write (pine.path:parse "/echo")
                     {:prompt "M-x " :complete '("aaa" "bbb")
                      :then (lambda (r) (setf chosen r))})
      (sleep 0.1)
      (press* "a" "C-g")
      (sleep 0.1)
      (is (eq :none chosen))
      (is (not (equal pine.echo:+buffer+ (pine.buf:current-name))))
      (is (null (pine.echo:popup-rows))))))

(test a-failing-edit-leaves-the-buffer-as-it-was
  "An edit is a write, so a bad one signals where it was written and lands
nothing. There is no half-edited buffer to recover: the write either produced a
new value or it did not."
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "err-scratch")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (pine.buf:edit buf (fset:seq :insert "hello"))
      (sleep 0.1)
      (signals error (pine.buf:edit buf (fset:seq :insert 42)))
      (is (string= "hello" (btext buf)))
      (pine.buf:edit buf (fset:seq :insert "!"))
      (is (string= "hello!" (btext buf))))))

(test a-failing-command-routes-by-debug-on-error
  (with-fixture substrate ()
    (pine.editor.debugger:install)
    (pine.ns:write (pine.cmd:at "probe-boom") (lambda () (error "boom")))
    (pine.ns:write (pine.path:parse "/debug-on-error") t)
    (pine.key::call-command "probe-boom")
    (is (not (null (pine.editor.debugger:attended)))
        "a failing command left no fault at /err")
    (reset-faults)
    (pine.ns:write (pine.path:parse "/debug-on-error") nil)
    (pine.key::call-command "probe-boom")
    (is (null (pine.editor.debugger:ids)))
    (is (search "boom" (pine.echo:current-message)))))

(defun probe-agent-fault (agent id condition restarts)
  "A fault from another image, the way the agent-debug receiver puts one at
/err."
  (pine.err:note
   (make-instance 'pine.core.actor:remote
                  :server *server* :agent agent :eval-id id
                  :label (format nil "agent ~a" agent)
                  :condition condition
                  :offered (mapcar (lambda (r) (list r nil)) restarts))))

(test concurrent-faults-coexist-and-tab-pages-between-them
  (with-fixture substrate ()
    (pine.editor.debugger:install)
    (probe-agent-fault "alpha" 1 "boom-a" (list "RETRY" "ABORT"))
    (probe-agent-fault "beta" 2 "boom-b" (list "ABORT"))
    (sleep 0.1)
    (is (= 2 (length (pine.editor.debugger:ids))))
    (is (equal "beta" (pine.eval:target))
        "the eval target follows the fault, so a fix lands in the image that broke")
    (let ((text (btext "*debugger*")))
      (is (search "alpha" text))
      (is (search "beta" text))
      (is (search "[ABORT]" text)))
    (pine.key::call-command "debugger-next-session")
    (sleep 0.05)
    (is (equal "alpha" (pine.eval:target)))
    (pine.key::call-command "view-next")
    (sleep 0.1)
    (pine.key::call-command "view-activate")
    (sleep 0.05)
    (is (= 1 (length (pine.editor.debugger:ids))))
    (pine.editor.debugger:take "ABORT")
    (sleep 0.05)
    (is (= 0 (length (pine.editor.debugger:ids))))
    (is (null (pine.editor.debugger:attended)))
    (is (eq :local (pine.eval:target)))))

(test a-watch-that-opens-a-view-is-not-a-cycle
  "A view's rows are an expression over the tree the watch just wrote, so one
expression is computed again for each of several paths moving in one wave. That
is a wave settling, not a loop, and calling it a loop made it impossible to
show a fault from the watch that heard about it."
  (with-fixture substrate ()
    (pine.ns:watch (pine.path:parse "/probe-open")
                   (lambda (v)
                     (when v
                       (pine.view:show "*probe-view*"
                                       (pine.editor.debugger:text-layout
                                        (format nil "one~%two"))))
                     nil)
                   :as :probe)
    (unwind-protect
         (progn
           (finishes (pine.ns:write (pine.path:parse "/probe-open") t))
           (is (search "one" (btext "*probe-view*"))))
      (pine.ns:watch (pine.path:parse "/probe-open") nil :as :probe))))

(test eval-in-target-local-runs-through-the-one-eval-path
  (with-fixture substrate ()
    (let ((done :none))
      (pine.eval:in-target
       "(+ 1 2)" (find-package :cl-user)
       :on-done (lambda (ev) (setf done (first (pine.err:evaluation-values ev)))))
      (sleep 0.3)
      (is (= 3 done)))))

(test a-tool-buffer-renders-selects-and-activates
  "A tool buffer is a view: the widget tree is at /buf/?name/view, the buffer
reads as its rows, and the selection moves and acts as verbs on the buffer."
  (with-fixture substrate ()
    (let ((fired nil)
          (buf (pine.editor.frame::make-buffer "view-probe")))
      (pine.view:show
       "view-probe"
       (lambda (name)
         (declare (ignore name))
         (pine.ui.build:column
          :align :stretch
          (pine.ui.build:label "hd" :face :keyword)
          (pine.ui.build:choice :data (lambda () (setf fired :one))
                                (pine.ui.build:label "one"))
          (pine.ui.build:choice :data (lambda () (setf fired :two))
                                (pine.ui.build:label "two")))))
      (sleep 0.15)
      (is (string= (format nil "hd~%> one~%  two") (btext buf)))
      (setf (pine.editor.frame::current-buffer) buf)
      (pine.key::call-command "view-next")
      (sleep 0.1)
      (is (string= (format nil "hd~%  one~%> two") (btext buf)))
      (pine.key::call-command "view-activate")
      (is (eq :two fired)))))

(test the-user-language-drives-the-real-dispatch
  (with-fixture substrate ()
    (in-user "(defvar *ran* nil)")
    (in-user "(write /cmd/user-probe-cmd (fn () (setf *ran* :yes) {}))")
    (in-user "(write /key/global/C-c/p /cmd/user-probe-cmd)")
    (press* "C-c" "p")
    (is (eq :yes (user-value "*RAN*")))
    (in-user "(defvar *min* nil)")
    (in-user "(write /cmd/user-min-cmd (fn () (setf *min* :hit) {}))")
    (in-user "(write /minor/user-probe {:precedence 12})")
    (in-user "(write /key/mode/user-probe/z /cmd/user-min-cmd)")
    (is (null (pine.key:key-binding (pine.key:parse-key "z"))))
    (pine.ns:write (pine.buf:at "scratch" :minor) (fset:seq :conj :user-probe))
    (is (fset:equal? (pine.cmd:at "user-min-cmd")
                     (pine.key:key-binding (pine.key:parse-key "z"))))
    (pine.ns:write (pine.buf:at "scratch" :minor) (fset:seq :disj :user-probe))
    (is (null (pine.key:key-binding (pine.key:parse-key "z"))))
    (in-user "(write /mode/user-probe {:parent :text :indicator \"UP\" :files [\"*.upx\"]})")
    (is (eq :user-probe (pine.mode:for-file "/tmp/x.upx")))
    (in-user "(write /buf/scratch/mode :user-probe)")
    (is (eq :user-probe (pine.ns:read (pine.buf:at "scratch" :mode))))
    (in-user "(write /buf/scratch/mode :text)")
    (in-user "(write /user-probe-setting 42)")
    (is (= 42 (in-user "(read /buf/scratch/user-probe-setting)"))
        "a leaf a buffer does not have reads the root")))

(test a-user-rule-styles-a-rendered-label
  (with-fixture substrate ()
    (in-user "(write /style/uprobe {:color (color :accent)})")
    (let ((rows (pine.ui.cells:render
                 (pine.ui.build:label "x" :class "uprobe") 6)))
      (is (equal (multiple-value-list
                  (pine.ui.face:hex-rgb (pine.ui.face:color :accent)))
                 (subseq (rest (first (cdr (first rows)))) 0 3))))))

(test a-user-tool-buffer-selects-and-activates
  (with-fixture substrate ()
    (in-user "(defvar *tool* nil)")
    (in-user "(write /mode/user-tool
        {:view (lambda (buf) (declare (ignore buf))
                 (column :align :stretch
                   (label \"tool\" :class \"help-head\")
                   (choice :data (lambda () (setf *tool* :a)) (label \"a\"))
                   (choice :data (lambda () (setf *tool* :b)) (label \"b\"))))})")
    (in-user "(write /buf/*user-tool*/text \"\")")
    (in-user "(write /buf/*user-tool* {:mode :user-tool})")
    (in-user "(write /buf/current /buf/*user-tool*)")
    (sleep 0.15)
    (is (string= (format nil "tool~%> a~%  b") (btext "*user-tool*")))
    (pine.key::call-command "view-next")
    (pine.key::call-command "view-activate")
    (sleep 0.05)
    (is (eq :b (user-value "*TOOL*")))))

(test a-user-command-can-prompt-and-echo-its-answer
  (with-fixture substrate ()
    (in-user "(defvar *picked* :none)")
    (in-user "(write /cmd/user-pick
        {/echo {:prompt \"pick: \" :complete (list \"xx\" \"yy\")
                :then (lambda (r) (setf *picked* r)
                        (message (format nil \"got ~a\" r)))}})")
    (pine.key::call-command "user-pick")
    (sleep 0.1)
    (type-text "yy")
    (sleep 0.1)
    (press "Return")
    (sleep 0.1)
    (is (string= "yy" (user-value "*PICKED*")))
    (is (string= "got yy" (pine.echo:current-message)))))

(test a-command-answers-to-a-symbol-in-definition-binding-and-call
  (with-fixture substrate ()
    (in-user "(defvar *sym-ran* nil)")
    (in-user "(write /cmd/sym-probe (fn () (setf *sym-ran* :sym) {}))")
    (in-user "(write /key/global/C-c/y /cmd/sym-probe)")
    (press* "C-c" "y")
    (is (eq :sym (user-value "*SYM-RAN*")))
    (in-user "(setf *sym-ran* nil)")
    (pine.key::call-command 'sym-probe)
    (is (eq :sym (user-value "*SYM-RAN*")))))

(test the-language-is-the-verbs-the-widgets-and-the-drivers
  "Everything the doc says is gone, is gone from what a config can name."
  (dolist (name '("DEFREF" "REF" "DEFONCE" "VAR" "DEFPOLL" "DEFSOURCE"
                  "DEFSURFACE" "SHOW" "HIDE" "TOGGLE" "DEFCOMMAND" "DEFMODE"
                  "KBD" "KEYMAP" "DEFINE-KEY" "GLOBAL-SET-KEY" "DEFFACE"
                  "DEFRULES" "LOAD-THEME" "ASK" "TELL" "BUFFER" "DEFAGENT"
                  "SPAWN" "KILL" "SH" "LAUNCH" "READ-INT-FILE"))
    (is (null (find-symbol name :pine.user))
        "pine.user still names ~a" name)))

(test add-rules-broadcasts-the-merged-set-to-every-attached-app
  (with-fixture substrate ()
    (let* ((got nil)
           (system (pine.core.server:actor-system *server*))
           (display (sento.actor-context:actor-of
                     system
                     :name (format nil "rules-probe-~a" (gensym))
                     :receive (lambda (msg)
                                (when (eq (first msg) :rules)
                                  (setf got (getf (rest msg) :rules)))
                                nil)))
           (client (pine.core.attach::make-attached-client
                    :id 999 :kind :probe :display display)))
      (push client pine.core.attach:*clients*)
      (unwind-protect
           (progn
             (pine.ui.rules:add-rules (list (list ".wire-probe" {:opacity "0.5"})))
             (sleep 0.2)
             (is (find ".wire-probe" got :key #'first :test #'string=)))
        (setf pine.core.attach:*clients*
              (remove client pine.core.attach:*clients*))))))

(test a-kill-is-a-write-to-the-ring
  "The kill ring is /kill: pushing is a write, the newest is a read, and it is
a held path, so it is what the store keeps."
  (with-fixture substrate ()
    (pine.kill:push-text "persist-kill-probe")
    (is (string= "persist-kill-probe"
                 (pine.ns:read (pine.path:parse "/kill"))))
    (is (eq :held (pine.ns:kind (pine.path:parse "/kill"))))))

(test find-file-records-a-recent-and-restores-the-saved-place
  (with-fixture substrate ()
    (let ((path "/tmp/pine-place-probe.txt"))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "one~%two~%three~%"))
      (pine.buf::find-file path)
      (sleep 0.15)
      (is (member path (pine.data:vals
                        (pine.ns:read (pine.path:path (pine.path:parse "/recent") (pine.path:any))
                                      (fset:empty-map)))
                  :test #'equal))
      (let ((buf (pine.buf:live "pine-place-probe.txt")))
        (pine.buf:put-point buf 2 1)
        (sleep 0.1)
        (setf (pine.editor.frame::current-buffer) buf)
        (pine.buf:save-current)
        (sleep 0.1)
        (is (fset:equal? (fset:seq 2 1)
                         (pine.ns:read (pine.buf:at "pine-place-probe.txt" :point)))
            "where point was left is the buffer's own leaf, and it is kept"))
      (pine.editor.frame::kill-buffer "pine-place-probe.txt")
      (sleep 0.1)
      (pine.buf::find-file path)
      (sleep 0.15)
      (multiple-value-bind (l c) (pine.buf:point "pine-place-probe.txt")
        (is (equal '(2 1) (list l c))))
      (pine.editor.frame::kill-buffer "pine-place-probe.txt")
      (sleep 0.1))))

(test prompt-history-recalls-with-meta-p
  (with-fixture substrate ()
    (pine.key::call-command "execute-command")
    (sleep 0.1)
    (type-text "list-buffers")
    (press "Return")
    (sleep 0.1)
    (is (member "list-buffers"
                (pine.data:vals (pine.ns:read (pine.path:path (pine.path:parse "/history/commands") (pine.path:any))
                                                (fset:empty-map)))
                :test #'equal))
    (pine.key::call-command "execute-command")
    (sleep 0.1)
    (press "M-p")
    (sleep 0.1)
    (is (string= "list-buffers" (minibuffer-input)))
    (press "Escape")
    (sleep 0.1)))

(test an-open-file-and-the-arrangement-are-both-just-held-paths
  "A buffer comes back because its file, its point and its mode are held and
its text follows from the file. The arrangement comes back the same way. There
is no snapshot of either."
  (with-fixture substrate ()
    (pine.editor.session::%seed-editor-tree *client*)
    (pine.editor.render:relayout)
    (let ((path "/tmp/pine-world-probe.txt"))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "alpha~%beta~%"))
      (pine.key::call-command "split-window-below")
      (sleep 0.1)
      (pine.buf::find-file path)
      (sleep 0.15)
      (pine.key::call-command "split-window-right")
      (sleep 0.1)
      (is (= 3 (length (pine.win:windows))))
      (is (equal "pine-world-probe.txt"
                 (pine.path:leaf (pine.pane:subject (pine.win:focused)))))
      (is (eq :held (pine.ns:kind (pine.path:child (pine.win:focused) "buf")))
          "the arrangement is held, so it comes back with the store")
      (is (eq :held (pine.ns:kind (pine.buf:at "pine-world-probe.txt" :file)))
          "and so is the file the buffer visits")
      (pine.key::call-command "delete-other-windows")
      (pine.editor.frame::kill-buffer "pine-world-probe.txt"))))

(test the-arrangement-is-paths-so-there-is-nothing-to-serialize
  "A split is a write to /win, and /win is held: it comes back with the store
like any other path, so no tree is ever turned into a form."
  (with-fixture substrate ()
    (pine.editor.session::%seed-editor-tree *client*)
    (pine.key::call-command "split-window-below")
    (is (= 2 (length (pine.win:windows))))
    (is (eq :column (pine.ns:read (pine.path:parse "/win/0/runs"))))
    (is (eq :held (pine.ns:kind (pine.path:parse "/win/0/0/buf"))))
    (pine.key::call-command "delete-other-windows")))

(test the-shipped-example-loads-clean-and-its-definitions-land
  (with-fixture substrate ()
    (finishes
      (pine:load-config (merge-pathnames "../examples/init.lisp"
                             #.(or *compile-file-truename* *load-truename*))))
    (is (= 2 (pine.ns:read (pine.path:parse "/tab-width"))))
    (is (eq :bar (pine.ns:read (pine.path:parse "/surface/bar/as"))))
    (is (string= "alacritty" (pine.ns:read (pine.path:parse "/wm-terminal"))))
    (is (not (null (pine.ns:read (pine.path:parse "/key/wm/s-2")))))
    (is (not (null (pine.ns:read (pine.path:parse "/key/wm/s-5"))))
        "a chord written over a range is every chord in it")))

(test the-live-tree-seeds-splits-focuses-and-follows-the-modeline
  (with-fixture substrate ()
    (let* ((frame (pine.editor.frame::frame *client*))
           (scratch (pine.buf:live "scratch"))
           (other (pine.editor.frame::make-buffer "editor-b2")))
      (setf (pine.editor.view-state:frame-cols frame) 40
            (pine.editor.view-state:frame-rows frame) 12)
      (pine.editor.frame::set-buffer-mode other :text)
      (pine.buf:edit other (fset:seq :insert "beta-two"))
      (sleep 0.1)
      (pine.editor.session::%seed-editor-tree *client*)
      (let* ((tree (pine.editor.render:refresh-editor-tree *client*))
             (leaves (remove-if-not (lambda (n)
                                      (typep n 'pine.ui.node:buffer-view))
                                    (pine.editor.render:window-leaves tree))))
        (is (not (null tree)))
        (is (= 1 (length (pine.win:windows))))
        (is (= 10 (length (pine.ui.node:window-rows (first leaves))))))
      (pine.editor.session::%seed-editor-tree *client*)
      (pine.editor.render:relayout)
      (pine.key::call-command "split-window-below")
      (is (= 2 (length (pine.win:windows))))
      (pine.key::call-command "other-window")
      (let ((buf (pine.editor.frame::switch-buffer "editor-b2")))
        (sento.actor:tell (pine.editor.frame::renderer *client*)
                          (list :switch-buffer :buffer buf :name "editor-b2"))
        )
      (sleep 0.15)
      (press "x")
      (sleep 0.15)
      (is (search "x" (btext other)))
      (is (not (search "x" (btext scratch))))
      (flet ((modeline-text ()
               (pine.editor.render:refresh-editor-tree *client*)
               (let ((n (find-if (lambda (x)
                                   (typep x 'pine.ui.node:modeline-view))
                                 (pine.editor.render:window-leaves
                                  (pine.editor.frame::tree *client*)))))
                 (car (first (pine.ui.node:window-rows n))))))
        ;; the tree is built again from /win after every command, so the node
        ;; is looked up again rather than held
        (is (search "editor-b2" (modeline-text)))
        (pine.key::call-command "other-window")
        (is (search "scratch" (modeline-text))))
      (pine.key::call-command "delete-other-windows")
      (is (= 1 (length (pine.win:windows))))
      (is (equal (pine.buf:name-of scratch)
                 (pine.path:leaf (pine.pane:subject (pine.win:focused))))))))

(test a-pane-shows-its-content-with-no-client-at-all
  "A pane holds content, and what content looks like is the content type's own
answer. So a pane a config named before any frontend existed renders the moment
anything asks -- which is the whole reason the editor is nothing special."
  (with-fixture substrate ()
    (let ((other (pine.editor.frame::make-buffer "editor-b3")))
      (pine.editor.frame::set-buffer-mode other :text)
      (pine.buf:edit other (fset:seq :insert "beta-three"))
      (sleep 0.1)
      (let ((pine.editor.frame::*client* nil))
        (is (search "beta-three"
                    (car (first (pine.pane:rows "editor-b3" 40 5)))))))))

(test an-overlay-renders-after-the-line-and-dies-on-the-next-edit
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "overlay-probe")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (pine.buf:edit buf (fset:seq :insert "abc"))
      (sleep 0.1)
      (pine.buf:overlay (pine.buf:name-of buf) 0 "=> 3")
      (sleep 0.1)
      (pine.ns:write (pine.path:parse "/pane/overlay/buf") (pine.buf:at "overlay-probe"))
      (let* ((node (pine.pane:window (pine.path:parse "/pane/overlay") :expand 1))
             (tree (pine.ui.build:column :align :stretch
                                         node
                                         (pine.pane:echo)
                                         (pine.pane:modeline "overlay-probe"))))
        (setf (pine.editor.frame::tree *client*) tree)
        (setf (pine.editor.frame::current-buffer) buf)
        (sleep 0.2)
        (pine.editor.render:refresh-editor-tree *client*)
        (is (search "=> 3" (car (first (pine.ui.node:window-rows node)))))
        (pine.buf:edit buf (fset:seq :insert "d"))
        (sleep 0.15)
        (pine.editor.render:refresh-editor-tree *client*)
        (is (not (search "=>" (car (first (pine.ui.node:window-rows node))))))))
    (pine.editor.session::%seed-editor-tree *client*)
    (pine.editor.render:relayout)))

(test eval-last-sexp-leaves-its-result-inline
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "eval-probe")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer) buf)
      (pine.buf:edit buf (fset:seq :insert "(+ 1 2)"))
      (sleep 0.1)
      (pine.eval:last-sexp)
      (sleep 0.5)
      (let ((overlays (pine.buf:local buf :overlays)))
        (is (not (null overlays)))
        (is (search "=> 3" (first (fset:@ overlays 0))))))))

(test kill-and-yank-round-trip-multi-line-text
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "yank-probe")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer) buf)
      (pine.buf:edit buf (fset:seq :insert (format nil "alpha~%beta~%gamma")))
      (sleep 0.1)
      (is (string= (format nil "alpha~%beta~%gamma") (btext buf)))
      (pine.buf:put-point buf 0 0)
      (sleep 0.05)
      (pine.key::call-command "set-mark")
      (pine.buf:put-point buf 2 5)
      (sleep 0.05)
      (pine.key::call-command "kill-region")
      (sleep 0.1)
      (is (string= "" (btext buf)))
      (pine.key::call-command "yank")
      (sleep 0.1)
      (is (string= (format nil "alpha~%beta~%gamma") (btext buf))))))

(test undo-and-redo-walk-the-edit-history
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "undo-probe")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (pine.buf:edit buf (fset:seq :insert "one"))
      (sleep 0.1)
      (pine.buf:edit buf (fset:seq :insert "-two"))
      (sleep 0.1)
      (is (string= "one-two" (btext buf)))
      (pine.buf:edit buf (fset:seq :undo))
      (sleep 0.1)
      (is (string= "one" (btext buf)))
      (pine.buf:edit buf (fset:seq :redo))
      (sleep 0.1)
      (is (string= "one-two" (btext buf))))))

(test a-lisp-buffer-indents-electrically-on-a-newline
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "indent-probe")))
      (pine.editor.frame::set-buffer-mode buf :lisp)
      (setf (pine.editor.frame::current-buffer) buf)
      (pine.ns:write (pine.buf:at buf :text) (format nil "(defun f ()~%(bar))"))
      (sleep 0.15)
      (pine.buf:put-point buf 0 11)
      (sleep 0.05)
      (pine.ns:write (pine.buf:at (pine.buf:name-of buf) :text)
                     (fset:seq :newline))
      ;; the line lands at once and the parser answers with its column, so the
      ;; indent is waited for rather than assumed to have already happened
      (is (wait-for (lambda ()
                      (multiple-value-bind (l c) (pine.buf:point (pine.buf:name-of buf))
                        (and (= 1 l) (= 2 c))))
                    :seconds 10)
          "the new line is indented into the defun body; point is at ~a"
          (multiple-value-list (pine.buf:point (pine.buf:name-of buf)))))))
