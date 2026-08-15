(in-package :pine.test)

(def-suite* :pine.layout :in :pine)

(defun rendered-rows (node width &key height selection)
  (mapcar (lambda (row) (string-right-trim " " (car row)))
          (pine.ui.cells:render node width :height height :selection selection)))

(defun rect-of (node)
  (list (pine.ui.node:start-line node) (pine.ui.node:start-col node)
        (pine.ui.node:end-line node) (pine.ui.node:end-col node)))

(defun laid-out (node width height)
  (pine.ui.layout:measure node width height)
  (pine.ui.layout:arrange node 0 0 width height)
  node)

(test a-label-measures-its-text
  (multiple-value-bind (w h) (pine.ui.layout:measure (pine.ui.build:label "hello") 40 10)
    (is (= 5 w))
    (is (= 1 h))))

(test a-column-stacks-and-a-row-abuts
  (let ((column (pine.ui.build:column (pine.ui.build:label "aaa")
                                      (pine.ui.build:label "b")))
        (row (pine.ui.build:row :spacing 0
                                (pine.ui.build:label "aaa")
                                (pine.ui.build:label "b"))))
    (multiple-value-bind (w h) (pine.ui.layout:measure column 40 10)
      (is (equal '(3 2) (list w h))))
    (multiple-value-bind (w h) (pine.ui.layout:measure row 40 10)
      (is (equal '(4 1) (list w h))))))

(test spacing-counts-only-between-nodes
  (let ((column (pine.ui.build:column :spacing 2
                                      (pine.ui.build:label "a")
                                      (pine.ui.build:label "b")
                                      (pine.ui.build:label "c"))))
    (is (= 7 (nth-value 1 (pine.ui.layout:measure column 40 40))))))

(test stretch-gives-every-node-the-cross-axis
  (let ((column (pine.ui.build:column :align :stretch
                                      (pine.ui.build:label "a")
                                      (pine.ui.build:label "bb"))))
    (laid-out column 10 4)
    (is (= 10 (pine.ui.node:end-col (first (pine.ui.node:nodes column)))))))

(test two-half-slack-expanders-do-not-round-the-tail-off-the-rect
  (is (string= "tail"
               (string-trim " "
                            (nth 9 (rendered-rows
                                    (pine.ui.build:column
                                     :align :stretch
                                     (pine.ui.build:label "a" :expand 1)
                                     (pine.ui.build:label "b" :expand 1)
                                     (pine.ui.build:label "tail"))
                                    10 :height 10))))))

(test slack-is-shared-by-weight
  (let ((column (pine.ui.build:column
                 (pine.ui.build:label "a" :expand 1)
                 (pine.ui.build:label "b" :expand 3))))
    (laid-out column 10 10)
    (destructuring-bind (a b) (pine.ui.node:nodes column)
      (is (= 3 (1+ (- (pine.ui.node:end-line a) (pine.ui.node:start-line a)))))
      (is (= 7 (1+ (- (pine.ui.node:end-line b) (pine.ui.node:start-line b))))))))

(test padding-grows-the-border-box-and-insets-the-content
  (let ((column (pine.ui.build:column :pad 2 (pine.ui.build:label "ab"))))
    (multiple-value-bind (w h) (pine.ui.layout:measure column 40 40)
      (is (equal '(6 5) (list w h))))
    (laid-out column 40 40)
    (is (equal '(2 2) (subseq (rect-of (first (pine.ui.node:nodes column))) 0 2)))))

(test min-width-is-a-floor-not-a-size
  (let ((wide (pine.ui.build:label "abcdef" :min-w 3))
        (narrow (pine.ui.build:label "ab" :min-w 8)))
    (is (= 6 (nth-value 0 (pine.ui.layout:measure wide 40 4))))
    (is (= 8 (nth-value 0 (pine.ui.layout:measure narrow 40 4))))))

(test a-margin-insets-the-node-inside-its-allocation
  (let ((label (pine.ui.build:label "ab" :margin '(1 2 1 2))))
    (multiple-value-bind (w h) (pine.ui.layout:measure label 40 40)
      (is (equal '(6 3) (list w h))))
    (laid-out label 40 40)
    (is (equal '(1 2) (subseq (rect-of label) 0 2)))))

(test a-centerbox-pins-the-ends-and-floats-the-centre
  (let ((box (pine.ui.build:centerbox
              :orient :v
              :start (pine.ui.build:label "s")
              :center (pine.ui.build:label "c")
              :end (pine.ui.build:label "e"))))
    (laid-out box 10 9)
    (destructuring-bind (s c e) (pine.ui.layout:centerbox-parts box)
      (is (= 0 (pine.ui.node:start-line s)))
      (is (= 4 (pine.ui.node:start-line c)))
      (is (= 8 (pine.ui.node:start-line e))))))

(test a-viewport-clips-to-its-height-and-scrolls-its-content
  (let ((rows (rendered-rows
               (pine.ui.build:scroll
                :height 2 :offset 1
                (pine.ui.build:column (pine.ui.build:label "one")
                                      (pine.ui.build:label "two")
                                      (pine.ui.build:label "three")))
               10 :height 2)))
    (is (equal '("two" "three") rows))))

(test a-separator-takes-one-cell-and-fills-the-axis
  (let ((column (pine.ui.build:column :align :stretch
                                      (pine.ui.build:label "ab")
                                      (pine.ui.build:rule :char #\-))))
    (is (equal '("ab" "----------") (rendered-rows column 10)))))

(test a-view-node-measures-its-rows
  (let ((node (pine.ui.build:cells (list (cons "abcd" nil) (cons "ef" nil)))))
    (multiple-value-bind (w h) (pine.ui.layout:measure node 40 40)
      (is (equal '(4 2) (list w h))))))

(test overlay-rows-sit-outside-the-measured-height
  (let ((node (pine.ui.build:cells (list (cons "pop" nil) (cons "line" nil))
                                    :base 1)))
    (is (= 1 (pine.ui.layout:view-overlay-count node)))
    (is (= 1 (nth-value 1 (pine.ui.layout:measure node 40 40))))))

(test render-reports-selection-prefixes-and-the-arranged-tree
  (multiple-value-bind (rows root)
      (pine.ui.cells:render
       (pine.ui.build:column :align :stretch
                             (pine.ui.build:label "head" :face :keyword)
                             (pine.ui.build:choice (pine.ui.build:label "aaa"))
                             (pine.ui.build:choice (pine.ui.build:label "bbb")))
       10 :selection 1)
    (is (equal '("head" "  aaa" "> bbb")
               (mapcar (lambda (r) (string-right-trim " " (car r))) rows)))
    (is (equal (pine.ui.face:face-fg :keyword)
               (subseq (rest (first (cdr (first rows)))) 0 3)))
    (let ((hit (pine.ui.layout:node-at root 2 3)))
      (is (typep hit 'pine.ui.node:selectable))
      (is-true (pine.ui.node:selectedp hit)))))

(test collect-selectables-reads-them-in-tree-order-and-stops-at-a-choice
  (let* ((inner (pine.ui.build:choice (pine.ui.build:label "in")))
         (tree (pine.ui.build:column
                (pine.ui.build:choice (pine.ui.build:column
                                       (pine.ui.build:label "a") inner))
                (pine.ui.build:choice (pine.ui.build:label "b")))))
    (is (= 2 (length (pine.ui.layout:collect-selectables tree))))
    (is (not (member inner (pine.ui.layout:collect-selectables tree))))))

(test node-at-answers-only-for-interactive-nodes
  (let ((tree (pine.ui.build:column :align :stretch
                                    (pine.ui.build:label "plain")
                                    (pine.ui.build:choice (pine.ui.build:label "pick")))))
    (laid-out tree 10 2)
    (is (null (pine.ui.layout:node-at tree 0 1)))
    (is (typep (pine.ui.layout:node-at tree 1 1) 'pine.ui.node:selectable))
    (is (null (pine.ui.layout:node-at tree 9 9)))))

(test a-click-on-an-action-yields-its-callback
  (let* ((fired nil)
         (tree (pine.ui.build:column
                (pine.ui.build:icon #\x :on-click (lambda () (setf fired :yes))))))
    (laid-out tree 10 1)
    (let ((thunk (pine.ui.layout:click-thunk tree 0 0)))
      (is (not (null thunk)))
      (funcall thunk)
      (is (eq :yes fired)))))

(test a-click-on-a-slider-yields-its-value
  (let* ((got nil)
         (meter (pine.ui.build:slider :value 0 :min 0 :max 100 :track 10
                                     :on-change (lambda (v) (setf got v)))))
    (laid-out meter 10 1)
    (is (= 0 (pine.ui.layout:slider-value-at meter 0)))
    (is (= 50 (pine.ui.layout:slider-value-at meter 5)))
    (is (= 100 (pine.ui.layout:slider-value-at meter 20)))
    (funcall (pine.ui.layout:click-thunk meter 0 5))
    (is (= 50 got))))

(test a-split-joins-a-matching-parent-flat
  (let* ((a (pine.ui.build:label "a"))
         (b (pine.ui.build:label "b"))
         (n (pine.ui.build:label "n"))
         (div (pine.ui.build:rule))
         (root (pine.ui.build:column :align :stretch a b))
         (result (pine.ui.layout:split-node root a n :column :divider div)))
    (is (eq root result))
    (is (equal (list a div n b) (pine.ui.node:nodes root)))))

(test a-split-across-the-grain-wraps-the-leaf
  (let* ((a (pine.ui.build:label "a"))
         (b (pine.ui.build:label "b"))
         (n (pine.ui.build:label "n"))
         (root (pine.ui.build:column :align :stretch a b))
         (result (pine.ui.layout:split-node root a n :row)))
    (is (eq root result))
    (let ((wrapper (first (pine.ui.node:nodes root))))
      (is (typep wrapper 'pine.ui.node:hstack))
      (is (equal (list a n) (pine.ui.node:nodes wrapper))))))

(test splitting-the-root-returns-the-new-container
  (let* ((leaf (pine.ui.build:label "only"))
         (n (pine.ui.build:label "n"))
         (result (pine.ui.layout:split-node leaf leaf n :column)))
    (is (typep result 'pine.ui.node:vstack))
    (is (equal (list leaf n) (pine.ui.node:nodes result)))))

(test a-split-carries-the-leaf-weight-to-the-sibling
  (let* ((a (pine.ui.build:label "a" :expand 4))
         (n (pine.ui.build:label "n"))
         (root (pine.ui.build:column a (pine.ui.build:label "b"))))
    (pine.ui.layout:split-node root a n :column)
    (is (= 4 (pine.ui.node:expand-of n)))))

(test removing-a-leaf-takes-its-divider-with-it
  (let* ((a (pine.ui.build:label "a"))
         (b (pine.ui.build:label "b"))
         (c (pine.ui.build:label "c"))
         (div (pine.ui.build:rule))
         (root (pine.ui.build:column a div b c)))
    (pine.ui.layout:remove-node root b)
    (is (equal (list a c) (pine.ui.node:nodes root)))))

(test removing-down-to-one-node-splices-the-container-out
  (let* ((a (pine.ui.build:label "a"))
         (b (pine.ui.build:label "b"))
         (inner (pine.ui.build:row a b))
         (outer (pine.ui.build:column inner (pine.ui.build:label "c")))
         (result (pine.ui.layout:remove-node outer b)))
    (is (eq outer result))
    (is (eq a (first (pine.ui.node:nodes outer))))))

(test removing-the-last-sibling-under-the-root-returns-the-survivor
  (let* ((a (pine.ui.build:label "a"))
         (b (pine.ui.build:label "b"))
         (root (pine.ui.build:column a b)))
    (is (eq a (pine.ui.layout:remove-node root b)))))

(test class-names-normalize-from-keywords-lists-and-strings
  (is (equal '("a") (pine.ui.cells:class-names :a)))
  (is (equal '("nm-row" "sel") (pine.ui.cells:class-names '(:nm-row :sel))))
  (is (equal '("nm-row" "sel") (pine.ui.cells:class-names "nm-row sel")))
  (is (null (pine.ui.cells:class-names nil))))

(test a-widget-takes-a-node-in-place-of-a-value
  (let* ((w (pine.world.world:make-world))
         (title (pine.world.world:ensure w "media" "title")))
    (setf (pine/fs/node:contents title) "Ligeia")
    (is (string= "Ligeia" (pine.ui.node:content (pine.ui.build:label title))))
    (let ((nothing (pine.world.world:ensure w "media" "nothing")))
      (is (string= "" (pine.ui.node:content (pine.ui.build:label nothing)))
          "a node holding nothing shows nothing, not the word NIL"))))

(test a-control-takes-the-node-it-edits-as-its-subject
  "No :value and no :on-change: the display and the edit are one node."
  (let* ((w (pine.world.world:make-world))
         (volume (pine.world.world:ensure w "audio" "volume")))
    (setf (pine/fs/node:contents volume) 40)
    (let ((slider (pine.ui.build:slider volume :min 0 :max 100)))
      (is (= 40 (pine.ui.node:value slider)))
      (funcall (pine.ui.node:on-change slider) 75)
      (is (= 75 (pine/fs/node:contents volume))
          "dragging it did not write the node it shows"))))

(test a-field-shows-a-node-and-writes-it-back
  (let* ((w (pine.world.world:make-world))
         (input (pine.world.world:ensure w "echo" "input")))
    (setf (pine/fs/node:contents input) "hel")
    (let ((f (pine.ui.build:field input)))
      (is (string= "hel" (pine.ui.node:content f)))
      (funcall (pine.ui.node:on-change f) "hello")
      (is (string= "hello" (pine/fs/node:contents input))))))

(test something-that-acts-takes-writes
  "A click is a node, a command name, a write-map or a function."
  (let* ((w (pine.world.world:make-world))
         (muted (pine.world.world:ensure w "audio" "muted")))
    (setf (pine/fs/node:contents muted) nil)
    (let ((b (pine.ui.build:button :click muted (pine.ui.build:label "mute"))))
      (funcall (pine.ui.node:callback b))
      (is (eq t (pine/fs/node:contents muted))))
    (let ((ran nil))
      (pine.repl.command:command "probe-click" (lambda () (setf ran t)))
      (unwind-protect
           (progn
             (funcall (pine.ui.node:callback
                       (pine.ui.build:button :click "probe-click"
                                             (pine.ui.build:label "go"))))
             (is (eq t ran) "a command name did not run"))
        (pine.repl.command:forget "probe-click")))))

(test a-grid-is-rows-of-columns
  (let ((g (pine.ui.build:grid :columns 2
                               (pine.ui.build:label "a") (pine.ui.build:label "b")
                               (pine.ui.build:label "c"))))
    (is (= 2 (length (pine.ui.node:nodes g))) "three cells two wide are two rows")
    (is (= 2 (length (pine.ui.node:nodes (first (pine.ui.node:nodes g))))))))

(test a-field-is-the-node-it-shows
  "The display and the edit are one node: a field shows what its node holds,
and what is typed into the prompt it opens is written back there. No :value and
no :on-change anywhere in a config."
  (unwind-protect
       (progn
         (pine:start)
         (let ((at (pine.world.world:ensure pine.world.world:*world* "probe-field")))
           (setf (pine/fs/node:contents at) "foot")
           (let ((f (pine.ui.build:field at :hint "Terminal:")))
             (is (equal "foot" (pine.ui.node:content f))
                 "a field did not show what its node holds")
             (pine.ui.layout:arrange f 0 0 10 1)
             (let ((thunk (pine.ui.layout:click-thunk f 0 0)))
               (is (not (null thunk))
                   "a field answered no click, so it cannot be edited")
               (funcall thunk)
               (is-true (pine.edit.prompt:asking-p)
                        "clicking a field did not ask for a value")
               (is (equal "foot" (pine.edit.prompt:said))
                   "the prompt did not start from the value it is showing")
               (pine.edit.prompt:answer! "alacritty")
               (is (equal "alacritty" (pine/fs/node:contents at))
                   "what was typed did not reach the node the field shows")))))
    (pine:stop)))

(test a-stack-puts-every-node-in-one-place
  "A stack is the one container that does not divide its space: each node gets
the whole rect, and the order they were written is what puts one over another."
  (let* ((under (pine.ui.build:label "under"))
         (over (pine.ui.build:label "over"))
         (s (pine.ui.build:stack under over)))
    (pine.ui.layout:arrange s 0 0 10 4)
    (is (= 2 (length (pine.ui.node:nodes s))))
    (is (and (= (pine.ui.node:start-col under) (pine.ui.node:start-col over))
             (= (pine.ui.node:start-line under) (pine.ui.node:start-line over))
             (= (pine.ui.node:end-col under) (pine.ui.node:end-col over)))
        "the two nodes were laid out side by side rather than one over the other")
    (let ((rows (pine.ui.cells:render s 10 :height 4)))
      (is (equal "over" (subseq (car (first rows)) 0 4))
          "the last node written is not the one on top"))))

(test a-label-is-not-a-place-and-takes-no-click
  "A run of text that answered every hit over itself would swallow the clicks of
whatever it sits inside."
  (let ((l (pine.ui.build:label "just words")))
    (pine.ui.layout:arrange l 0 0 10 1)
    (is (null (pine.ui.layout:node-at l 0 0)))))
