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
