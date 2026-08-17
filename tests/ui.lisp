(in-package :pine/test)

(def-suite* :pine/ui :in :pine)

(defclass ticker (surface:role) ()
  (:documentation "A role written outside the substrate, to prove nothing in it
knows the roles by name."))

(defmethod surface:anchor ((r ticker) width height)
  (d:map :edges '(:bottom :right) :wide width :tall height :keeps 0
         :margin '(4 4 4 4)))

(defmethod surface:asks ((r ticker)) nil)

(defun drawn (tree cols &optional (lines 4))
  (layout:with-pass
    (face:with-faces
      (layout:dress tree)
      (let ((m (grid:make-grid cols lines)))
        (layout:measure tree m cols lines)
        (layout:arrange tree m 0 0 cols lines)
        (layout:paint tree m)
        (values (mapcar (lambda (r) (string-right-trim " " (car r))) (grid:rows m))
                tree m)))))

(test a-tree-measures-arranges-and-paints
  (with-tree
    (is (equal '("hello" "there" "" "")
               (drawn (build:column :align :stretch
                                    (build:label "hello") (build:label "there"))
                      20)))
    (is (search "ab cd" (first (drawn (build:row (build:label "ab")
                                                 (build:label "cd"))
                                      20))))
    (is (equal '("ab" "----------" "" "")
               (drawn (build:column :align :stretch (build:label "ab")
                                    (build:rule :glyph #\-))
                      10)))))

(test the-more-particular-rule-wins
  (with-tree
    (pine/ui/sheet:attach (tree:root))
    (pine/ui/sheet:put (list (list ".a" (list :color "#ff0000" :min-width "20"))
                             (list ".a.b" (list :color "#00ff00"))))
    (let ((general (style:resolve '(("a"))))
          (both (style:resolve '(("a" "b")))))
      (is (equal '(255 0 0) (d:at general :fg))
          "and a rule says a colour the way everything else does")
      (is (equal '(0 255 0) (d:at both :fg))
          "whatever order the rules were written in")
      (is (eql 20 (d:at both :min-w))
          "and what the winning rule does not say still comes through"))
    (is (member :shadow (style:properties)))))

(test a-rule-and-a-face-say-a-colour-the-same-way
  "A painter takes a colour from a style and a colour from a face and paints with
both, so they have to be the same three numbers."
  (with-tree
    (pine/ui/sheet:attach (tree:root))
    (pine/ui/sheet:put (list (list ".x" (list :background-color
                                             (face:color :accent)))))
    (is (equal (face:rgb (face:color :accent))
               (subseq (d:at (style:resolve '(("x"))) :bg) 0 3)))
    (is (eql 1.0 (fourth (d:at (style:resolve '(("x"))) :bg)))
        "and how much of it shows is still a fraction")))

(test nothing-is-written-into-the-tree-during-a-pass
  (with-tree
    (let ((tree (build:column :pad 2 (build:label "ab"))))
      (drawn tree 20)
      (is (eql 2 (widget:pad tree)) "what a config authored is what it still says")
      (is (null (widget:font (first (widget:parts tree))))
          "and the style did not leak into it"))))

(test padding-grows-the-border-box
  (with-tree
    (let ((tree (build:column :pad 2 (build:label "ab"))))
      (layout:with-pass
        (let ((m (grid:make-grid 40 40)))
          (multiple-value-bind (cw ch) (layout:measure tree m 40 40)
            (is (equal '(6 5) (list cw ch)))))))))

(test a-control-takes-the-place-it-edits
  (with-tree
    (let ((volume (tree:ensure nil "dev" "audio" "volume")))
      (setf (node:contents volume) 40)
      (let ((s (build:slider volume :low 0 :high 100)))
        (is (= 40 (widget:value s)))
        (funcall (widget:changed s) 75)
        (is (= 75 (node:contents volume)))))))

(test a-click-lands-on-what-was-drawn-there
  (with-tree
    (let* ((fired nil)
           (tree (build:column :align :stretch
                               (build:label "plain")
                               (build:button :on-click (lambda ()
                                                         (setf fired :yes))
                                             (build:label "go")))))
      (multiple-value-bind (rows arranged) (drawn tree 10 2)
        (declare (ignore rows))
        (is (null (pine/ui/hit:at arranged 0 1)) "a label answers no hit")
        (let ((hit (pine/ui/hit:at arranged 1 1)))
          (is (typep hit 'widget:action))
          (funcall (pine/ui/hit:clicked hit 1))
          (is (eq :yes fired)))))))

(test a-surface-carries-its-role-and-follows-what-it-read
  (with-tree
    (let ((where (tree:ensure nil "probe")))
      (setf (node:contents where) "one")
      (let ((s (surface:builds "ticker"
                               (lambda () (build:label (node:contents where)))
                               :as 'ticker :shown :default)))
        (is (typep (surface:role s) 'ticker))
        (is (surface:shown s) "a role that is not asked for is up already")
        (let ((placed (surface:anchor (surface:role s) 100 20)))
          (is (equal '(:bottom :right) (d:at placed :edges)))
          (is (equal '(4 4 4 4) (d:at placed :margin))))
        (is (equal "one" (widget:content (node:contents s))))
        (setf (node:contents where) "two")
        (is (equal "two" (widget:content (node:contents s)))
            "it follows what it read, with nothing subscribing")
        (is (eq s (surface:named "ticker")))
        (setf (node:contents (tree:at nil "surface" "ticker" "shown")) nil)
        (is (null (surface:shown s)))))))

(test nothing-in-the-source-names-that-role
  (let ((named (loop :for f :in (directory
                                 (merge-pathnames
                                  "src/**/*.lisp"
                                  (asdf:system-source-directory :pine)))
                     :unless (char= #\. (char (file-namestring f) 0))
                       :when (search "ticker" (uiop:read-file-string f))
                         :collect (file-namestring f))))
    (is (null named) "~{~%  ~a names it~}" named)))

(test a-key-is-one-object-for-one-chord
  (is (key:key= (key:parse "C-x") (key:make-key "x" :ctrl t)))
  (is (equal "C-x C-s" (key:text (key:chord "C-x C-s"))))
  (is (key:selfp (key:parse "a")))
  (is (not (key:selfp (key:parse "C-a"))))
  (is (key:key= (key:parse "space") (key:parse "SPC"))
      "one name for a key however it was spelled"))

(test a-widget-crosses-the-wire-and-comes-back
  (let* ((tree (build:column :class "bar" (build:label "hi")))
         (form (pine/ui/wire:to-wire tree))
         (back (pine/ui/wire:from-wire form)))
    (is (typep back 'widget:column))
    (is (equal "bar" (widget:css-class back)))
    (is (equal "hi" (widget:content (first (widget:parts back)))))))
