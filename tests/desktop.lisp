(in-package :pine.test)

(def-suite* :pine.desktop :in :pine)

(defmacro with-desktop (&body body)
  `(unwind-protect (progn (pine:start) ,@body) (pine:stop)))

(defun surface-of (name) (pine.app.surface:surface-named name))

(test a-surface-is-a-node-and-its-contents-is-the-tree-it-builds
  (with-desktop
    (pine.app.surface:surface "probe"
                              (lambda () (pine.ui.build:label "hello"))
                              :as :bar)
    (let ((s (surface-of "probe")))
      (is-true s)
      (is (eq :bar (pine.app.surface:as s)))
      (is (typep (pine.fs.node:contents s) 'pine.ui.node:node))
      (is (equal '("hello")
                 (mapcar (lambda (row) (string-right-trim " " (car row)))
                         (pine.ui.cells:render (pine.fs.node:contents s) 20)))))))

(test what-a-surface-reads-is-what-rebuilds-it
  (with-desktop
    (let ((where (pine.world.world:ensure pine.world.world:*world* "probe")))
      (setf (pine.fs.node:contents where) "one")
      (pine.app.surface:surface
       "reading" (lambda () (pine.ui.build:label (pine.fs.node:contents where))))
      (let ((s (surface-of "reading")))
        (is (equal "one" (pine.ui.node:content (pine.fs.node:contents s))))
        (setf (pine.fs.node:contents where) "two")
        (is (equal "two" (pine.ui.node:content (pine.fs.node:contents s)))
            "nothing subscribed: the surface read it, so the write invalidated it")))))

(test a-surface-that-moves-tells-whoever-is-watching-it
  (with-desktop
    (let ((where (pine.world.world:ensure pine.world.world:*world* "probe"))
          (told 0))
      (setf (pine.fs.node:contents where) "one")
      (pine.app.surface:surface
       "reading" (lambda () (pine.ui.build:label (pine.fs.node:contents where))))
      (pine.fs.node:contents (surface-of "reading"))
      (pine.fs.watch:watch (surface-of "reading")
                           (lambda (of value) (declare (ignore of value)) (incf told)))
      (setf (pine.fs.node:contents where) "two")
      (is (= 1 told) "the desktop is pushed a fresh tree without polling for one"))))

(test the-placement-is-wayland-vocabulary-and-the-content-is-not
  (with-desktop
    (pine.app.surface:surface "bar" (lambda () (pine.ui.build:label "")) :as :bar)
    (pine.app.surface:surface "audio" (lambda () (pine.ui.build:label "")) :as :panel)
    (is-true (pine.app.surface:shownp (surface-of "bar"))
             "furniture is up as soon as it is declared")
    (is-false (pine.app.surface:shownp (surface-of "audio"))
              "a panel waits to be asked for")
    (is-true (pine.app.surface:panelp (surface-of "audio")))
    (is-false (pine.app.surface:panelp (surface-of "bar")))))

(test toggling-a-panel-puts-the-other-one-away
  (with-desktop
    (dolist (name '("audio" "network" "media"))
      (pine.app.surface:surface name (lambda () (pine.ui.build:label name))
                                :as :panel))
    (pine.repl.command:run "toggle-surface" (list "audio"))
    (is-true (pine.app.surface:shownp (surface-of "audio")))
    (pine.repl.command:run "toggle-surface" (list "network"))
    (is-true (pine.app.surface:shownp (surface-of "network")))
    (is-false (pine.app.surface:shownp (surface-of "audio"))
              "two panels over each other is not a desktop")
    (pine.repl.command:run "toggle-surface" (list "network"))
    (is-false (pine.app.surface:shownp (surface-of "network")))))

(test a-surface-slot-is-a-node-so-a-config-can-write-it
  (with-desktop
    (pine.app.surface:surface "probe" (lambda () (pine.ui.build:label "")) :as :panel)
    (let ((shown (pine.fs.tree:at (pine.app.surface:root) "probe/shown")))
      (is-true shown)
      (setf (pine.fs.node:contents shown) t)
      (is-true (pine.app.surface:shownp (surface-of "probe")))
      (is (eq :panel (pine.fs.node:contents
                      (pine.fs.tree:at (pine.app.surface:root) "probe/as")))))))

(test the-desktop-verbs-are-commands-like-any-other
  (with-desktop
    (pine.app.surface:surface "probe" (lambda () (pine.ui.build:label "")) :as :panel)
    (is (equal '(("probe" :panel nil)) (pine.repl.command:run "surfaces")))
    (pine.repl.command:run "show-surface" (list "probe"))
    (is (equal '(("probe" :panel t)) (pine.repl.command:run "surfaces")))))

(def-suite* :pine.wm :in :pine)

(test the-compositor-is-a-subtree-and-its-verbs-are-nodes
  (with-desktop
    (pine:niri)
    (let ((wm (pine.app.wm:root)))
      (is-true wm)
      (is-true (pine.fs.node:livep wm)
               "the compositor answers from itself, so no snapshot walks into it")
      (is-true (pine.fs.tree:at wm "windows"))
      (is-true (pine.fs.tree:at wm "workspaces"))
      (is-true (pine.fs.node:resolve wm "close")
               "what it can be told is a node, not a special message"))))

(test the-wm-commands-read-the-compositor-rather-than-a-copy
  (with-desktop
    (pine:niri)
    (is (equal (pine.app.wm:windows)
               (pine.fs.node:contents (pine.fs.tree:at (pine.app.wm:root) "windows")))
        "the command and the path answer the same thing")
    (is-true (member "wm-focus-next" (mapcar #'pine.repl.command:name
                                             (pine.repl.command:commands))
                     :test #'equal))))

(test the-terminal-this-machine-uses-is-a-node-a-config-writes
  (with-desktop
    (setf (pine.fs.node:contents
           (pine.world.world:ensure pine.world.world:*world* "wm-terminal"))
          "foot")
    (is (equal "foot" (pine.app.wm:terminal)))))

(test the-editor-window-is-a-surface-like-any-other
  (with-desktop
    (let ((s (pine.edit.session:surface)))
      (is-true s "nothing declared it, so pine did")
      (is (eq :toplevel (pine.app.surface:as s)))
      (is (eq s (pine.app.surface:surface-named "editor"))))))

(test a-config-can-say-what-the-editor-window-is
  (with-desktop
    (pine.app.surface:surface
     "editor"
     (lambda () (pine.ui.build:column :class "chrome"
                                      (pine.ui.build:label "pine")
                                      (pine.edit.render:frame-tree)))
     :as :toplevel)
    (let ((tree (pine.fs.node:contents (pine.edit.session:surface))))
      (is (equal "chrome" (pine.ui.node:css-class tree))
          "the editor pushes the surface, so a config replacing it is what ships")
      (is-true (%classed tree "editor-view")
               "and the frame is still in there, because the config put it there"))))

(defun told (message) (pine.app.compositor:received nil message))

(defmacro with-compositor (&body body)
  `(unwind-protect
        (progn (pine:start) (pine:compositor)
               (told '(:output :x 0 :y 0 :width 1920 :height 1080))
               ,@body)
     (pine:stop)))

(test pine-as-the-window-manager-arranges-what-the-compositor-reports
  (with-compositor
    (told '(:window-added :id "a" :title "one" :app-id "probe"))
    (is (equal '("a") (pine.app.wm:windows)))
    (told '(:window-added :id "b" :title "two" :app-id "probe"))
    (is (equal '("a" "b") (sort (copy-list (pine.app.wm:windows)) #'string<)))
    (let ((rects (pine.app.compositor:rects (pine.app.wm:current))))
      (is (= 2 (length rects)))
      (is (equal '("a" "b") (sort (mapcar #'first rects) #'string<)))
      (is (apply #'= (mapcar #'second rects))
          "below means they share an x")
      (is (/= (third (first rects)) (third (second rects)))
          "and differ in y")
      (is (>= 1920 (reduce #'max rects :key (lambda (r) (+ (second r) (fourth r)))))
          "and neither runs off the output the compositor reported"))))

(test a-window-that-closes-leaves-the-arrangement
  (with-compositor
    (told '(:window-added :id "a" :title "one" :app-id "probe"))
    (told '(:window-added :id "b" :title "two" :app-id "probe"))
    (told '(:window-closed :id "b"))
    (is (equal '("a") (pine.app.wm:windows)))
    (is (= 1 (length (pine.app.compositor:rects (pine.app.wm:current)))))))

(test the-compositor-and-niri-answer-the-same-commands
  (with-compositor
    (told '(:window-added :id "a" :title "one" :app-id "probe"))
    (told '(:window-added :id "b" :title "two" :app-id "probe"))
    (told '(:window-focused :id "a"))
    (is (equal "a" (pine.app.wm:focused)))
    (is (equal "one" (pine.app.wm:title "a"))
        "the same generic answers, whichever compositor is at /wm")
    (pine.repl.command:run "wm-focus-next")
    (is (equal "b" (pine.app.wm:focused)))
    (pine.repl.command:run "wm-focus-previous")
    (is (equal "a" (pine.app.wm:focused)))))

(test the-chords-the-frontend-registers-are-an-ordinary-mode
  (with-compositor
    (let ((table (pine.app.compositor:bindings)))
      (is (equal "wm-terminal" (cdr (assoc "s-Return" table :test #'equal))))
      (is (equal "wm-focus-next" (cdr (assoc "s-j" table :test #'equal))))
      (pine.repl.mode:bind "wm" "s-o" "wm-overview")
      (is (equal "wm-overview"
                 (cdr (assoc "s-o" (pine.app.compositor:bindings) :test #'equal)))
          "a config binds a wm chord the way it binds any other"))))

(test splitting-says-where-the-next-window-lands
  (with-compositor
    (told '(:window-added :id "a" :title "one" :app-id "probe"))
    (pine.repl.command:run "wm-split-beside")
    (told '(:window-added :id "b" :title "two" :app-id "probe"))
    (let ((rects (pine.app.compositor:rects (pine.app.wm:current))))
      (is (= 2 (length rects)))
      (is (/= (second (first rects)) (second (second rects)))
          "beside means they differ in x, not in y"))))

(defclass probe-client (pine.net.attach:client)
  ((sent :initform nil :accessor sent))
  (:default-initargs :id 99 :kind :desktop))

(defmethod pine.net.attach:push-to ((c probe-client) &rest message)
  (push (cons (first message) (rest message)) (sent c))
  message)

(defun acted (client id)
  "A widget's action is not run on the thread that told the daemon about it, so
a test waits for the thread it did run on."
  (let ((tk (pine.app.desktop:received client (list :widget-action :id id :args nil))))
    (when (typep tk 'pine/run/task:task) (pine/run/task:join tk))
    tk))

(test a-click-crosses-as-an-id-and-runs-the-thunk-it-stands-for
  (with-desktop
    (let ((client (make-instance 'probe-client))
          (ran nil))
      (pine.app.surface:surface
       "probe"
       (lambda () (pine.ui.build:button :on-click (lambda () (setf ran t))
                                        (pine.ui.build:label "go")))
       :as :bar)
      (let ((s (pine.app.desktop::%attached client)))
        (let* ((push (find :widgets (sent client) :key #'first))
               (wire (getf (rest push) :tree)))
          (is-true push "the surface was pushed")
          (is (search "ACTION" (princ-to-string wire))
              "a clickable node crosses the wire as an action with an id")
          (acted client "probe/0")
          (is-true ran "the id the frontend sent back ran the thunk the daemon kept"))
        (is-true s)))))

(test the-editor-is-not-the-desktops-to-draw
  (with-desktop
    (pine.edit.session:surface)
    (pine.app.surface:surface "bar" (lambda () (pine.ui.build:label "")) :as :bar)
    (let ((client (make-instance 'probe-client)))
      (pine.app.desktop::%attached client)
      (let ((surfaces (loop :for (verb . message) :in (sent client)
                            :when (eq :widgets verb) :collect (getf message :surface))))
        (is (member "bar" surfaces :test #'equal))
        (is (null (member "editor" surfaces :test #'equal))
            "a toplevel is the editor frontend's, not a layer surface")))))

(test a-provider-hands-out-the-same-node-every-time
  (with-desktop
    (pine:niri)
    (let ((wm (pine.app.wm:root)))
      (is (eq (pine.fs.tree:at wm "focused") (pine.fs.tree:at wm "focused"))
          "a surface can only depend on a child that is the same object twice")
      (is (eq (first (pine.fs.node:nodes wm)) (first (pine.fs.node:nodes wm)))))))

(test stirring-a-provider-recomputes-what-read-it
  (with-desktop
    (pine:niri)
    (let ((told 0)
          (s (pine.app.surface:surface
              "live"
              (lambda () (pine.ui.build:label
                          (princ-to-string
                           (pine.fs.node:contents
                            (pine.fs.tree:at (pine.app.wm:root) "focused")))))
              :as :bar)))
      (pine.fs.node:contents s)
      (pine.fs.watch:watch s (lambda (of value) (declare (ignore of value)) (incf told)))
      (pine.fs.node:stir (pine.app.wm:root))
      (is (= 1 told)
          "the compositor said it moved, so the bar that read it was pushed again"))))

(test a-stream-says-a-provider-moved
  (with-desktop
    (let* ((root (pine.world.world:root pine.world.world:*world*))
           (probe (pine.fs.tree:ensure root "probe"))
           (told 0))
      (declare (ignorable probe))
      (let ((stream (pine.provider.sh:streaming
                     "for i in 1 2 3; do echo tick; sleep 0.2; done")))
        (is-true stream "a command that streams is a node under /sh")
        (pine.fs.watch:watch stream
                             (lambda (of said) (declare (ignore of said)) (incf told))
                             :only nil :poll nil)
        (loop :repeat 60 :until (>= told 2) :do (sleep 0.05))
        (is (>= told 2) "each line the process said reached the tree")
        (pine.provider.sh:quiet! stream)))))

(test a-click-writes-and-the-write-is-what-pushes
  (with-desktop
    (let* ((client (make-instance 'probe-client))
           (where (pine.world.world:ensure pine.world.world:*world* "probe")))
      (setf (pine.fs.node:contents where) nil)
      (pine.app.surface:surface
       "probe"
       (lambda ()
         (pine.ui.build:button
          :on-click (lambda () (setf (pine.fs.node:contents where) t))
          (pine.ui.build:label (if (pine.fs.node:contents where) "on" "off"))))
       :as :bar)
      (pine.app.desktop::%attached client)
      (setf (sent client) nil)
      (acted client "probe/0")
      (pine.app.desktop:flush (pine.app.desktop::%for client))
      (let ((push (find :widgets (sent client) :key #'first)))
        (is-true push "the write the click made is what pushed the surface")
        (is (search "on" (princ-to-string (getf (rest push) :tree)))
            "and what it says is what the click made true")))))

(test a-click-can-be-a-write-a-path-or-a-command
  (with-desktop
    (let ((where (pine.world.world:ensure pine.world.world:*world* "probe")))
      (setf (pine.fs.node:contents where) nil)
      (let ((by-map (pine.ui.build:acting
                     (pine/data:map (pine.path.path:parse "/probe") 41))))
        (funcall by-map)
        (is (eql 41 (pine.fs.node:contents where))
            "a write-map is a click, so a config declares one without a closure"))
      (let ((by-path (pine.ui.build:acting (pine.path.path:parse "/probe"))))
        (funcall by-path)
        (is (eq t (pine.fs.node:contents where))))
      (let ((by-name (pine.ui.build:acting "pwd")))
        (is (equal "/" (funcall by-name))
            "a command's name is a click too")))))

(test a-verb-is-applied-against-what-the-node-holds
  (with-desktop
    (let ((where (pine.world.world:ensure pine.world.world:*world* "probe")))
      (setf (pine.fs.node:contents where) nil)
      (setf (pine.fs.node:contents where) (pine/data:seq :toggle))
      (is (eq t (pine.fs.node:contents where)))
      (setf (pine.fs.node:contents where) (pine/data:seq :toggle))
      (is (null (pine.fs.node:contents where)))
      (setf (pine.fs.node:contents where) (pine/data:seq :set 7))
      (is (eql 7 (pine.fs.node:contents where)))
      (setf (pine.fs.node:contents where) (pine/data:no-set))
      (setf (pine.fs.node:contents where) (pine/data:seq :conj :probe))
      (is-true (pine/data:contains (pine.fs.node:contents where) :probe)))))

(test asking-before-a-dangerous-click-goes-through-the-prompt
  (with-desktop
    (let ((ran nil))
      (let ((thunk (pine.ui.build::%click
                    (list :on-click (lambda () (setf ran t)) :confirm "Really?"))))
        (funcall thunk)
        (is-true (pine.edit.prompt:asking-p) "it asked instead of acting")
        (pine.edit.prompt:answer! "no")
        (is-false ran)
        (funcall thunk)
        (pine.edit.prompt:answer! "yes")
        (is-true ran)))))

(test a-surface-declared-after-a-frontend-attached-still-reaches-it
  (with-desktop
    (let ((client (make-instance 'probe-client)))
      (pine.app.desktop::%attached client)
      (setf (sent client) nil)
      (pine.app.surface:surface "late" (lambda () (pine.ui.build:label "late"))
                                :as :bar)
      (let ((push (find :widgets (sent client) :key #'first)))
        (is-true push "a surface a config declares later is pushed, not stranded")
        (is (equal "late" (getf (rest push) :surface)))))))

(test restyling-reaches-the-frontends
  (with-desktop
    (let ((client (make-instance 'probe-client)))
      (pine.app.desktop::%attached client)
      (setf (sent client) nil)
      (pine:style ".probe" (list :color "#ff0000"))
      (let ((push (find :style (sent client) :key #'first)))
        (is-true push "a style written at runtime is broadcast")
        (is (search "probe" (princ-to-string (getf (rest push) :styles))))))))

(test a-field-asks-for-its-new-value
  (with-desktop
    (let* ((where (pine.world.world:ensure pine.world.world:*world* "probe"))
           (field (pine.ui.build:field (pine.path.path:parse "/probe")
                                       :hint "Probe")))
      (setf (pine.fs.node:contents where) "was")
      (let ((thunk (pine.ui.layout:clicked field 0)))
        (is-true thunk "a field is clickable")
        (funcall thunk)
        (is-true (pine.edit.prompt:asking-p) "and asks rather than doing nothing")
        (pine.edit.prompt:answer! "now")
        (is (equal "now" (pine.fs.node:contents where)))))))

(test a-keystroke-ships-the-lines-that-moved-and-not-the-whole-frame
  (with-desktop
    (let* ((client (make-instance 'probe-client))
           (s (pine.edit.session::%attached client)))
      (is-true (wait-until (lambda () (find :widgets (sent client) :key #'first)))
               "the first frame is the whole tree")
      (setf (sent client) nil)
      (pine.edit.buffer:insert! (pine.edit.buffer:current) "x")
      (pine.edit.session:push-frame s)
      (let ((push (first (sent client))))
        (is (eq :rows-patch (first push))
            "the second frame is the lines that moved")
        (is (< (length (princ-to-string push)) 600)
            "which is a fraction of what the whole tree costs"))
      (setf (sent client) nil)
      (pine.edit.session:push-frame s :whole t)
      (is (eq :widgets (first (first (sent client))))
          "and a whole one is still there when it is asked for"))))

(test a-widget-that-blocks-holds-up-nothing-else
  (with-desktop
    (let ((client (make-instance 'probe-client))
          (held (bordeaux-threads:make-semaphore))
          (ran nil))
      (pine.app.surface:surface
       "probe"
       (lambda ()
         (pine.ui.build:button
          :on-click (lambda ()
                      (bordeaux-threads:wait-on-semaphore held :timeout 5)
                      (setf ran t))
          (pine.ui.build:label "go")))
       :as :bar)
      (pine.app.desktop::%attached client)
      (let ((tk (pine.app.desktop:received client
                                           (list :widget-action :id "probe/0" :args nil))))
        (setf (sent client) nil)
        (pine.app.desktop:received client (list :refresh))
        (is-true (find :widgets (sent client) :key #'first)
                 "the next message was answered while the click was still in it")
        (is (null ran))
        (bordeaux-threads:signal-semaphore held)
        (pine/run/task:join tk)
        (is-true ran)
        (is (null (pine/run/task:task-named (pine/run/task:name tk)))
            "and the task it ran on is gone")))))

(test a-slow-command-does-not-hold-up-the-loop-that-reads-keys
  (with-desktop
    (let* ((client (make-instance 'probe-client))
           (s (pine.edit.session::%attached client))
           (held (bordeaux-threads:make-semaphore)))
      (is-true (wait-until (lambda () (find :widgets (sent client) :key #'first))))
      (pine.repl.command:command "probe-slow"
                                 (lambda ()
                                   (bordeaux-threads:wait-on-semaphore held
                                                                       :timeout 5))
                                 :describes "waits")
      (pine.repl.mode:bind "text" "C-c C-w" "probe-slow")
      (unwind-protect
           (let ((was (get-internal-real-time)))
             (pine.edit.session:received client (list :key :key-str "c" :ctrl t))
             (pine.edit.session:received client (list :key :key-str "w" :ctrl t))
             (pine.edit.session:received client (list :key :key-str "x" :ctrl nil))
             (is (< (- (get-internal-real-time) was)
                    internal-time-units-per-second)
                 "handing the keys over did not wait for the command")
             (bordeaux-threads:signal-semaphore held)
             (is-true (wait-until
                       (lambda ()
                         (search "x" (pine.fs.node:contents
                                      (pine.edit.buffer:current)))))
                      "and the keys behind it landed once it was done, in order"))
        (pine.repl.command:forget "probe-slow")))))

(test the-frame-is-arranged-in-pixels-once-the-frontend-says-what-a-cell-is
  (with-desktop
    (let* ((client (make-instance 'probe-client))
           (s (pine.edit.session::%attached client)))
      (is-true (wait-until (lambda () (find :widgets (sent client) :key #'first))))
      (pine.edit.session::%work s (list :resize :cols 80 :rows 24
                                        :width 720 :height 432
                                        :cell-w 9 :cell-h 18))
      (let* ((push (find :widgets (sent client) :key #'first))
             (form (getf (rest push) :tree))
             (view (first (pine.ui.wire:wire-views form))))
        (is-true view)
        (destructuring-bind (sl sc el ec) (getf (second view) :rect)
          (declare (ignore sl sc))
          (is (= 720 ec) "the frame was arranged in pixels, not in cells")
          (is (> el 24) "and so was its height"))))))

(test a-window-that-grew-by-less-than-a-cell-is-still-laid-out-for-its-size
  "The frame is arranged in pixels, so the size it was laid out for has to be
the size it lands in: a frame drawn for a smaller window is cut off by the one
it is painted into."
  (with-desktop
    (let* ((client (make-instance 'probe-client))
           (s (pine.edit.session::%attached client)))
      (is-true (wait-until (lambda () (find :widgets (sent client) :key #'first))))
      (flet ((frame-at (w h)
               (pine.edit.session::%work
                s (list :resize :cols (floor w 9) :rows (floor h 18)
                        :width w :height h :cell-w 9 :cell-h 18))
               (setf (sent client) nil)
               (pine.edit.session::%work s (list :refresh))
               (let ((tree (pine.ui.wire:wire->node
                            (getf (rest (find :widgets (sent client) :key #'first))
                                  :tree))))
                 (list (pine.ui.node:end-col tree)
                       (1+ (pine.ui.node:end-line tree))))))
        (is (equal '(900 540) (frame-at 900 540)))
        (is (equal '(900 545) (frame-at 900 545))
            "eight more pixels is the same 30 rows and a different frame")))))

(test a-click-lands-even-though-the-surface-was-pushed-again
  "A bar is pushed several times a second because it reads a clock. The id a
pointer sends back names where the widget is, not which push it came from, so
a click that crossed during a repaint still means what it looks like it means."
  (with-desktop
    (let ((client (make-instance 'probe-client))
          (where (pine.world.world:ensure pine.world.world:*world* "probe"))
          (ticks (pine.world.world:ensure pine.world.world:*world* "probe-tick")))
      (setf (pine.fs.node:contents where) nil
            (pine.fs.node:contents ticks) 0)
      (pine.app.surface:surface
       "probe"
       (lambda ()
         (pine.ui.build:row
          (pine.ui.build:label (format nil "~a" (pine.fs.node:contents ticks)))
          (pine.ui.build:button
           :on-click (lambda () (setf (pine.fs.node:contents where) t))
           (pine.ui.build:label "go"))))
       :as :bar)
      (let ((s (pine.app.desktop::%attached client)))
        (let ((id (block found
                    (dolist (each (pine/data:keys
                                   (pine/data:all (pine.app.desktop::acting s))))
                      (return-from found each)))))
          (is-true id "the surface registered a click")
          (dotimes (n 20)
            (setf (pine.fs.node:contents ticks) n)
            (pine.app.desktop:push-surface s (pine.app.surface:surface-named "probe")))
          (acted client id)
          (is (eq t (pine.fs.node:contents where))
              "the id from twenty pushes ago still names the same button"))))))

(test showing-a-panel-tells-the-frontend-when-the-write-is-to-its-slot
  "A config toggles a panel by writing /surface/ctl/shown. What is watching is
the surface, so the slot's write has to move it, or the panel opens whenever
something else happens to rebuild it."
  (with-desktop
    (let ((client (make-instance 'probe-client)))
      (pine.app.surface:surface "probe-panel"
                                (lambda () (pine.ui.build:label "here"))
                                :as :panel)
      (pine.app.desktop::%attached client)
      (setf (sent client) nil)
      (let ((shown (pine.fs.tree:at (pine.app.surface:root) "probe-panel/shown")))
        (is-true shown "a surface says whether it is shown at a path")
        (setf (pine.fs.node:contents shown) t)
        (pine.app.desktop:flush (pine.app.desktop::%for client))
        (let ((told (find-if (lambda (m)
                               (and (eq :panel (first m))
                                    (equal "probe-panel" (getf (rest m) :name))))
                             (sent client))))
          (is-true told "the frontend was told to put the panel up")
          (is (eq t (getf (rest told) :show)))
          (is-true (find-if (lambda (m)
                              (and (eq :widgets (first m))
                                   (equal "probe-panel" (getf (rest m) :surface))))
                            (sent client))
                   "and it has the tree to put in it"))))))

(test what-the-pointer-is-over-survives-the-surface-being-pushed-again
  "A bar reading a clock rebuilds its tree several times a second. The pointer
has not moved, so what it is over is found again on the new tree; dropping it
is why a hint lit up only while the pointer was moving."
  (let* ((built 0)
         (tree-fn (lambda ()
                    (incf built)
                    (pine.ui.build:column
                     (pine.ui.build:button :hint "one" :on-click (lambda () nil)
                                           (pine.ui.build:label "one"))
                     (pine.ui.build:button :hint "two" :on-click (lambda () nil)
                                           (pine.ui.build:label "two")))))
         (tree (funcall tree-fn)))
    (pine.ui.layout:measure tree 20 4)
    (pine.ui.layout:arrange tree 0 0 20 4)
    (let ((over (pine.ui.layout:node-at tree 1 1)))
      (is-true over "the pointer is over something")
      (setf (pine.ui.node:hovered over) t)
      (let ((again (funcall tree-fn)))
        (pine.ui.layout:measure again 20 4)
        (pine.ui.layout:arrange again 0 0 20 4)
        (let ((now (pine.ui.layout:node-at again 1 1)))
          (is (not (eq now over)) "the tree was built again, so the node is new")
          (is (equal (pine.ui.node:hint over) (pine.ui.node:hint now))
              "and the same place under the pointer means the same thing")
          (is (null (pine.ui.node:hovered now))
              "which is why the frontend puts the flag back rather than waiting
for the pointer to move"))))))

(test a-surface-that-says-what-it-said-is-not-pushed-again
  "The world behind a bar moves dozens of times a second and the bar reads the
same as before. Pushing that is what made hovering flicker and a hint cost
twenty frames."
  (with-desktop
    (let ((client (make-instance 'probe-client))
          (where (pine.world.world:ensure pine.world.world:*world* "probe")))
      (setf (pine.fs.node:contents where) "one")
      (pine.app.surface:surface "probe-bar"
                                (lambda () (pine.ui.build:label
                                            (pine.fs.node:contents where)))
                                :as :bar)
      (let ((s (pine.app.desktop::%attached client)))
        (setf (sent client) nil)
        (dotimes (n 20) (pine.app.desktop:push-surface s (pine.app.surface:surface-named "probe-bar")))
        (is (zerop (count :widgets (sent client) :key #'first))
            "twenty pushes of a surface that did not move are none")
        (setf (pine.fs.node:contents where) "two")
        (pine.app.desktop:flush s)
        (is (= 1 (count :widgets (sent client) :key #'first))
            "and one that did is one")
        (is (search "two" (princ-to-string
                           (getf (rest (find :widgets (sent client) :key #'first))
                                 :tree))))))))
