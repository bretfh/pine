(in-package :pine.test)

(def-suite* :pine.wire :in :pine)

;;;; Two properties. A tree survives the crossing: node->wire then wire->node
;;;; gives a form equal to the first. And a patch applied to the form the
;;;; frontend holds yields exactly the form the daemon built, or refuses.

(defun crosses-unchanged-p (node)
  (let ((wire (pine.ui.wire:node->wire node)))
    (equal wire (pine.ui.wire:node->wire (pine.ui.wire:wire->node wire)))))

(defun bench-row (cols text)
  (cons (let ((s (make-string cols :initial-element #\Space)))
          (replace s text)
          s)
        (list (list 0 200 200 200 30 30 40 0))))

(defun editor-form (lines &key (cols 40) (crow 0) (ccol 0))
  (pine.ui.wire:node->wire
   (pine.ui.build:column
    (pine.ui.build:cells (mapcar (lambda (l) (bench-row cols l)) lines)
                          :as 'pine.ui.node:buffer-view :crow crow :ccol ccol)
    (pine.ui.build:cells (list (bench-row cols "")) :as 'pine.ui.node:echo-view))))

(test a-label-crosses-with-its-content-and-style
  (let ((wire (pine.ui.wire:node->wire
               (pine.ui.build:label "hi" :class "a b" :face :keyword
                                         :pad-x 2 :min-w 9 :expand 3))))
    (is (eq :label (first wire)))
    (is (string= "hi" (getf (second wire) :content)))
    (is (string= "a b" (getf (second wire) :class)))
    (is (eq :keyword (getf (second wire) :face)))
    (is (= 2 (getf (second wire) :pad-x)))
    (is (= 9 (getf (second wire) :min-w)))
    (is (= 3 (getf (second wire) :expand)))
    (is (crosses-unchanged-p (pine.ui.wire:wire->node wire)))))

(test default-props-are-left-off-the-wire
  (let ((props (second (pine.ui.wire:node->wire (pine.ui.build:label "x")))))
    (is (null (getf props :pad-x)))
    (is (null (getf props :expand)))
    (is (null (getf props :radius)))))

(test every-node-kind-crosses-unchanged
  (dolist (node (list (pine.ui.build:label "text")
                      (pine.ui.build:rule :char #\= :vertical t)
                      (pine.ui.build:gap :expand 2)
                      (pine.ui.build:slider :value 40 :min 0 :max 80)
                      (pine.ui.build:ring :value 3 :thickness 2 :diameter 30
                                          (pine.ui.build:label "in"))
                      (pine.ui.build:calendar :year 2026 :month 7 :day 26)
                      (pine.ui.build:image "/tmp/none.png")
                      (pine.ui.build:cells (list (cons "row" nil)) :crow 1 :ccol 2)
                      (pine.ui.build:centerbox :orient :h
                                               :start (pine.ui.build:label "s")
                                               :center (pine.ui.build:label "c")
                                               :end (pine.ui.build:label "e"))
                      (pine.ui.build:choice :selected t (pine.ui.build:label "c"))
                      (pine.ui.build:box :width 12 :align :right
                                           (pine.ui.build:label "b"))
                      (pine.ui.build:scroll :height 4 (pine.ui.build:label "v"))
                      (pine.ui.build:center (pine.ui.build:label "m"))
                      (pine.ui.build:column :spacing 2 :align :stretch
                                            (pine.ui.build:label "a")
                                            (pine.ui.build:label "b"))
                      (pine.ui.build:row :spacing 1 (pine.ui.build:label "a"))))
    (is-true (crosses-unchanged-p node)
             "~a does not survive the wire" (type-of node))))

(test a-list-crosses-as-itself-and-comes-back-as-the-column-it-rendered
  "A list's rows are built by a closure and a closure does not cross, so what
goes out is the rows it built. It says so with its own tag rather than
borrowing a column's, and what comes back is those rows."
  (let ((wire (pine.ui.wire:node->wire
               (pine.ui.build:rows '("a" "b")
                                   (lambda (item i)
                                     (declare (ignore i))
                                     (pine.ui.build:label item))))))
    (is (eq :list (first wire)))
    (is (= 2 (length (cddr wire))))
    (let ((back (pine.ui.wire:wire->node wire)))
      (is (typep back 'pine.ui.node:vstack))
      (is (= 2 (length (pine.ui.layout:nodes-of back)))))))

(test a-handler-crosses-as-an-id-and-comes-back-as-a-function
  (let* ((ids nil)
         (wire (pine.ui.wire:node->wire
                (pine.ui.build:slider :on-change (lambda (v) v))
                :on-action (lambda (cb) (declare (ignore cb)) (push 7 ids) 7))))
    (is (= 7 (getf (second wire) :action)))
    (let ((node (pine.ui.wire:wire->node
                 wire :on-action (lambda (id) (lambda (&rest args)
                                                (list* id args))))))
      (is (equal '(7 42) (funcall (pine.ui.node:on-change node) 42))))))

(test an-arranged-rect-crosses-with-the-tree
  (let ((tree (pine.ui.build:column :align :stretch (pine.ui.build:label "a"))))
    (is-false (pine.ui.wire:arranged-p tree))
    (pine.ui.layout:measure tree 10 3)
    (pine.ui.layout:arrange tree 0 0 10 3)
    (let ((back (pine.ui.wire:wire->node (pine.ui.wire:node->wire tree))))
      (is-true (pine.ui.wire:arranged-p back))
      (is (equal (list (pine.ui.node:start-line tree) (pine.ui.node:start-col tree)
                       (pine.ui.node:end-line tree) (pine.ui.node:end-col tree))
                 (list (pine.ui.node:start-line back) (pine.ui.node:start-col back)
                       (pine.ui.node:end-line back) (pine.ui.node:end-col back)))))))

(test wire-views-finds-the-window-forms-in-tree-order
  (let ((form (editor-form '("a" "b"))))
    (is (= 2 (length (pine.ui.wire:wire-views form))))
    (is (every (lambda (w) (eq :view (first w)))
               (pine.ui.wire:wire-views form)))))

(test a-patch-applied-equals-the-tree-the-daemon-built
  (let* ((old (editor-form '("alpha" "beta" "gamma")))
         (new (editor-form '("alpha" "BETA!" "gamma")))
         (patch (pine.ui.wire:rows-patch old new)))
    (is-true patch)
    (is (equal new (pine.ui.wire:apply-rows-patch old patch)))))

(test a-patch-carries-only-the-lines-that-moved
  (let* ((old (editor-form '("a" "b" "c" "d" "e")))
         (new (editor-form '("a" "b" "CHANGED" "d" "e")))
         (lines (fourth (first (pine.ui.wire:rows-patch old new)))))
    (is (= 1 (length lines)))
    (is (= 2 (car (first lines))))))

(test the-cursor-moving-is-patchable-without-any-line
  (let* ((old (editor-form '("a" "b") :crow 0 :ccol 0))
         (new (editor-form '("a" "b") :crow 1 :ccol 3))
         (patch (pine.ui.wire:rows-patch old new)))
    (is-true patch)
    (is (null (fourth (first patch))))
    (is (equal new (pine.ui.wire:apply-rows-patch old patch)))))

(test scrolling-every-line-is-still-exact-when-patched
  (let* ((old (editor-form '("1" "2" "3" "4")))
         (new (editor-form '("5" "6" "7" "8")))
         (patch (pine.ui.wire:rows-patch old new)))
    (is (= 4 (length (fourth (first patch)))))
    (is (equal new (pine.ui.wire:apply-rows-patch old patch)))))

(test a-changed-tree-refuses-to-patch
  (let ((old (editor-form '("a" "b")))
        (new (pine.ui.wire:node->wire
              (pine.ui.build:column
               (pine.ui.build:cells (list (bench-row 40 "a")) :as 'pine.ui.node:buffer-view)
               (pine.ui.build:cells (list (bench-row 40 "b")) :as 'pine.ui.node:buffer-view)
               (pine.ui.build:cells (list (bench-row 40 "")) :as 'pine.ui.node:echo-view)))))
    (is (null (pine.ui.wire:rows-patch old new)))))

(test a-changed-line-count-refuses-to-patch
  (is (null (pine.ui.wire:rows-patch (editor-form '("a" "b"))
                                     (editor-form '("a" "b" "c"))))))

(test no-previous-form-refuses-to-patch
  (is (null (pine.ui.wire:rows-patch nil (editor-form '("a"))))))

(test scroll-to-selection-keeps-the-selection-in-the-window
  (is (= 0 (pine.ui.wire:scroll-to-selection 0 0 5)))
  (is (= 0 (pine.ui.wire:scroll-to-selection 4 0 5)))
  (is (= 1 (pine.ui.wire:scroll-to-selection 5 0 5)))
  (is (= 3 (pine.ui.wire:scroll-to-selection 3 6 5)))
  (is (= 6 (pine.ui.wire:scroll-to-selection -1 6 5))))
