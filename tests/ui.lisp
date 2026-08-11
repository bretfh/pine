(in-package :pine.test)

(def-suite* :pine.ui :in :pine)

(test a-widget-tree-measures-arranges-and-renders-to-cells
  (let* ((tree (pine.ui.build:column
                :align :stretch
                (pine.ui.build:label "hello")
                (pine.ui.build:label "there")))
         (rows (pine.ui.cells:render tree 20)))
    (is (= 2 (length rows)))
    (is (equal "hello" (string-right-trim " " (car (first rows)))))
    (is (equal "there" (string-right-trim " " (car (second rows)))))))

(test a-row-lays-its-widgets-out-side-by-side
  (let* ((tree (pine.ui.build:row (pine.ui.build:label "ab")
                                  (pine.ui.build:label "cd")))
         (rows (pine.ui.cells:render tree 20)))
    (is (= 1 (length rows)))
    (is (search "ab" (car (first rows))))
    (is (search "cd" (car (first rows))))))

(test a-node-carries-where-it-was-arranged
  (let ((tree (pine.ui.build:column (pine.ui.build:label "one")
                                    (pine.ui.build:label "two"))))
    (pine.ui.layout:measure tree 20 10)
    (pine.ui.layout:arrange tree 0 0 20 10)
    (let ((second (second (pine.ui.layout:nodes-of tree))))
      (is (= 1 (pine.ui.node:start-line second))))))

(test the-theme-in-force-resolves-a-face
  (let ((f (pine.ui.face:find-face :default)))
    (is (typep f 'pine.ui.face:face))
    (is (stringp (pine.ui.face:fg f)))))

(test a-selector-resolves-through-the-stylesheet
  (let ((st (pine.ui.style:resolve '(nil ("field")))))
    (is (typep st 'pine.ui.style:style))
    (is (pine.ui.style:st-fg st) "the .field rule pine ships gives it a colour")))

(test a-widget-crosses-the-wire-and-comes-back
  (let* ((tree (pine.ui.build:column (pine.ui.build:label "hello")))
         (data (pine.ui.wire:node->wire tree))
         (back (pine.ui.wire:wire->node data)))
    (is (consp data))
    (is (typep back 'pine.ui.node:node))
    (is (equal (pine.ui.cells:render tree 20)
               (pine.ui.cells:render back 20))
        "what came back renders to the same cells")))

(test the-themes-and-faces-are-nodes-in-the-tree
  (unwind-protect
       (progn
         (pine:start)
         (pine.ui.paths:install)
         (let ((themes (pine.world.world:at pine.world.world:*world* "theme"))
               (faces (pine.world.world:at pine.world.world:*world* "faces")))
           (is (member "ef-dream" (pine.fs.tree:listing themes) :test #'equal))
           (is (member "default" (pine.fs.tree:listing faces) :test #'equal))
           (is (getf (pine.fs.node:contents
                      (pine.fs.tree:at faces "default"))
                     :fg)
               "a face reads as what it is, through the same generic")))
    (pine:stop)))

(test a-face-written-into-the-tree-wins-over-the-theme
  (unwind-protect
       (progn
         (pine:start)
         (pine.ui.paths:install)
         (setf (pine.fs.node:contents
                (pine.world.world:ensure pine.world.world:*world* "face" "probe-face"))
               (list :fg "#ff0000"))
         (let ((f (pine.ui.face:find-face :probe-face)))
           (is (equal "#ff0000" (pine.ui.face:fg f)))))
    (pine:stop)))

(test a-frontend-has-no-world-and-still-resolves-a-background
  "The frontend image never calls pine:start, so it has no tree. Installing the
styles the daemon sent used to signal there, which killed the display actor and
left every surface transparent."
  (let ((pine.world.world:*world* nil)
        (pine.ui.css:*given* nil))
    (finishes (pine.ui.css:install (pine.ui.css:stylesheet)))
    (dolist (class '("editor-view" "modeline" "echo"))
      (let ((st (pine.ui.style:resolve (list nil '("editor") (list class)))))
        (is (pine.ui.style:st-bg st) "~a resolves to no background" class)
        (is (pine.ui.style:st-fg st) "~a resolves to no colour" class)))))

(test the-editor-frame-carries-the-classes-its-styles-are-written-against
  (unwind-protect
       (progn
         (pine:start)
         (setf (pine.fs.node:contents (pine.edit.buffer:current)) "hello")
         (let ((wire (pine.ui.wire:node->wire (pine.edit.render:frame-tree))))
           (let ((text (princ-to-string wire)))
             (dolist (class '("editor" "editor-view" "modeline" "echo"))
               (is (search class text) "the frame has no ~a" class)))))
    (pine:stop)))

(test a-centerbox-keeps-its-middle-out-of-its-ends
  "A bar's start grows with the workspaces there are. The middle sits in what
is left, not in the middle of the whole box, or the two draw on each other."
  (let* ((start (pine.ui.build:column
                 (pine.ui.build:label "a") (pine.ui.build:label "b")
                 (pine.ui.build:label "c") (pine.ui.build:label "d")))
         (middle (pine.ui.build:column (pine.ui.build:label "m")))
         (end (pine.ui.build:column (pine.ui.build:label "z")))
         (box (pine.ui.build:centerbox :orient :v :start start
                                       :center middle :end end)))
    (pine.ui.layout:measure box 10 10)
    (pine.ui.layout:arrange box 0 0 10 10)
    (is (< (pine.ui.node:end-line start) (pine.ui.node:start-line middle))
        "the middle begins after the start ends")
    (is (< (pine.ui.node:end-line middle) (pine.ui.node:start-line end))
        "and ends before the end begins")))

(test a-node-a-config-holds-is-written-by-clicking-it
  "(at (root *world*) \"wm\" \"workspaces\" n) is a node, and clicking a widget
that carries one writes it."
  (let* ((w (pine.world.world:make-world))
         (n (pine.world.world:ensure w "probe"))
         (thunk (pine.ui.build:acting n)))
    (setf (pine.fs.node:contents n) nil)
    (is-true thunk "a node is something a click can mean")
    (funcall thunk)
    (is (eq t (pine.fs.node:contents n)))))
