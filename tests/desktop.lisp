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
          (pine.app.desktop:received client (list :widget-action :id 1 :args nil))
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

(test a-surface-that-reads-the-machine-is-repainted-without-being-invalidated
  (with-desktop
    (pine:niri)
    (pine.app.surface:surface
     "live" (lambda () (pine.ui.build:label
                        (princ-to-string (pine.fs.node:contents
                                          (pine.fs.tree:at (pine.app.wm:root) "focused")))))
     :as :bar)
    (let ((s (pine.app.surface:surface-named "live")))
      (pine.fs.node:contents s)
      (is-true (pine.app.desktop:live-reader-p s)
               "nothing writes a compositor, so nothing invalidates a surface reading one")
      (pine.app.surface:surface "still" (lambda () (pine.ui.build:label "x")) :as :bar)
      (let ((other (pine.app.surface:surface-named "still")))
        (pine.fs.node:contents other)
        (is-false (pine.app.desktop:live-reader-p other)
                  "one that reads only the tree is repainted by the write instead")))))

(test a-click-is-followed-by-a-fresh-tree
  (with-desktop
    (let ((client (make-instance 'probe-client))
          (shown nil))
      (pine.app.surface:surface
       "probe"
       (lambda () (pine.ui.build:button :on-click (lambda () (setf shown t))
                                        (pine.ui.build:label (if shown "on" "off"))))
       :as :bar)
      (pine.app.desktop::%attached client)
      (setf (sent client) nil)
      (pine.app.desktop:received client (list :widget-action :id 1 :args nil))
      (let ((push (find :widgets (sent client) :key #'first)))
        (is-true push "the surface is pushed again after the click that changed it")
        (is (search "on" (princ-to-string (getf (rest push) :tree)))
            "and what it says is what the click made true")))))
