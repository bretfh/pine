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
