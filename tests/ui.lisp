(in-package :pine/test)

(def-suite* :pine/ui :in :pine)

(defclass ticker (ui:role) ()
  (:documentation "A role written outside the substrate, to prove nothing in it
knows the roles by name."))

(defmethod ui:anchor ((r ticker) width height)
  (ui:placing :edges '(:bottom :right) :wide width :tall height
              :margin '(4 4 4 4)))

(defmethod ui:shows ((r ticker)) :always)

(defun drawn (tree cols &optional (lines 4))
  (ui:with-pass
    (ui:with-faces
      (ui:dress tree)
      (let ((m (ui:make-grid cols lines)))
        (ui:measure tree m cols lines)
        (ui:arrange tree m 0 0 cols lines)
        (ui:paint tree m)
        (values (mapcar (lambda (r) (string-right-trim " " (car r))) (ui:by-row m))
                tree m)))))

(test a-tree-measures-arranges-and-paints
  (with-tree
    (is (equal '("hello" "there" "" "")
               (drawn (ui:column :align :stretch
                                    (ui:label "hello") (ui:label "there"))
                      20)))
    (is (search "ab cd" (first (drawn (ui:row (ui:label "ab")
                                                 (ui:label "cd"))
                                      20))))
    (is (equal '("ab" "----------" "" "")
               (drawn (ui:column :align :stretch (ui:label "ab")
                                    (ui:rule :glyph #\-))
                      10)))))

(test the-more-particular-rule-wins
  (with-tree
    (tree:built)
    (pine/ui:put-rules (list (list ".a" (list :color "#ff0000" :min-width "20"))
                             (list ".a.b" (list :color "#00ff00"))))
    (let ((general (ui:resolve '(("a"))))
          (both (ui:resolve '(("a" "b")))))
      (is (equal '(255 0 0) (d:lookup general :fg))
          "and a rule says a colour the way everything else does")
      (is (equal '(0 255 0) (d:lookup both :fg))
          "whatever order the rules were written in")
      (is (eql 20 (d:lookup both :min-w))
          "and what the winning rule does not say still comes through"))
    (is (member :shadow (ui:properties)))))

(test a-rule-and-a-face-say-a-colour-the-same-way
  "A painter takes a colour from a style and a colour from a face and paints with
both, so they have to be the same three numbers."
  (with-tree
    (tree:built)
    (pine/ui:put-rules (list (list ".x" (list :background-color
                                             (ui:color :accent)))))
    (is (equal (ui:unhex (ui:color :accent))
               (subseq (d:lookup (ui:resolve '(("x"))) :bg) 0 3)))
    (is (eql 1.0 (fourth (d:lookup (ui:resolve '(("x"))) :bg)))
        "and how much of it shows is still a fraction")))

(test nothing-is-written-into-the-tree-during-a-pass
  (with-tree
    (let ((tree (ui:column :pad 2 (ui:label "ab"))))
      (drawn tree 20)
      (is (eql 2 (ui:pad tree)) "what a config authored is what it still says")
      (is (null (ui:font (first (ui:parts tree))))
          "and the style did not leak into it"))))

(test padding-grows-the-border-box
  (with-tree
    (let ((tree (ui:column :pad 2 (ui:label "ab"))))
      (ui:with-pass
        (let ((m (ui:make-grid 40 40)))
          (multiple-value-bind (cw ch) (ui:measure tree m 40 40)
            (is (equal '(6 5) (list cw ch)))))))))

(test a-control-takes-the-place-it-edits
  (with-tree
    (let ((volume (tree:ensure "/dev/audio" "volume")))
      (setf (node:contents volume) 40)
      (let ((s (ui:slider volume :low 0 :high 100)))
        (is (= 40 (ui:value s)))
        (funcall (ui:changed s) 75)
        (is (= 75 (node:contents volume)))))))

(test a-click-lands-on-what-was-drawn-there
  (with-tree
    (let* ((fired nil)
           (tree (ui:column :align :stretch
                               (ui:label "plain")
                               (ui:button :on-click (lambda ()
                                                         (setf fired :yes))
                                             (ui:label "go")))))
      (multiple-value-bind (rows arranged) (drawn tree 10 2)
        (declare (ignore rows))
        (is (null (pine/ui:under arranged 0 1)) "a label answers no hit")
        (let ((hit (pine/ui:under arranged 1 1)))
          (is (typep hit 'ui:action))
          (funcall (pine/ui:clicked hit 1))
          (is (eq :yes fired)))))))

(test a-surface-carries-its-role-and-follows-what-it-read
  (with-tree
    (let ((where (tree:ensure "/probe")))
      (setf (node:contents where) "one")
      (let ((s (ui:make-surface "ticker"
                               (lambda () (ui:label (node:contents where)))
                               :as 'ticker :starts :as-the-role-says)))
        (is (typep (ui:role s) 'ticker))
        (is (ui:shown s) "a role that shows :always is up already")
        (let ((placed (ui:anchor (ui:role s) 100 20)))
          (is (equal '(:bottom :right) (ui:edges-of placed)))
          (is (equal '(4 4 4 4) (ui:margin-of placed))))
        (is (equal "one" (ui:content (node:contents s))))
        (setf (node:contents where) "two")
        (is (equal "two" (ui:content (node:contents s)))
            "it follows what it read, with nothing subscribing")
        (is (eq s (ui:named "ticker")))
        (setf (node:contents (tree:at "/surface/ticker" "shown")) nil)
        (is (null (ui:shown s)))))))

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
  (is (ui:key= (ui:parse "C-x") (ui:make-key "x" :ctrl t)))
  (is (equal "C-x C-s" (ui:spelled (ui:chord "C-x C-s"))))
  (is (ui:selfp (ui:parse "a")))
  (is (not (ui:selfp (ui:parse "C-a"))))
  (is (ui:key= (ui:parse "space") (ui:parse "SPC"))
      "one name for a key however it was spelled"))

(test a-widget-crosses-the-wire-and-comes-back
  (let* ((tree (ui:column :class "bar" (ui:label "hi")))
         (form (pine/ui:to-wire tree))
         (back (pine/ui:from-wire form)))
    (is (typep back 'ui:column))
    (is (equal "bar" (ui:css-class back)))
    (is (equal "hi" (ui:content (first (ui:parts back)))))))

(test rows-over-a-pattern-are-what-it-matches
  "ROWS said it was a column over a pattern whose matches are the rows, and it
listed one node's children. The matcher was written and nothing called it."
  (with-tree
    (pine::write "/dev/audio/volume" 40)
    (pine::write "/dev/screen/volume" 70)
    (pine::write "/dev/audio/muted" nil)
    (let ((found (mapcar #'pine/fs/path:whole
                         (mapcar (lambda (n) (pine/fs/path:path
                                              (node:full-name n)))
                                 (path:matching (path:path "/dev/*/volume"))))))
      (is (equal '("/dev/audio/volume" "/dev/screen/volume") (sort found #'string<))
          "one name each, and not what is beside them"))
    (is (path:patternp (path:path "/dev/*/volume")))
    (is (not (path:patternp (path:path "/dev/audio/volume"))))))

(test the-part-on-top-is-the-part-a-click-lands-on
  "A stack paints its parts in the order they were written and the last is on top.
Answering with the first that covered a place gave the click to what was behind."
  (let* ((under (ui:button :class "under" :click "under" (ui:label "xxxx")))
         (over (ui:button :class "over" :click "over" (ui:label "yyyy")))
         (both (ui:stack under over))
         (g (ui:make-grid 8 2)))
    (ui:with-pass
      (ui:dress both)
      (ui:measure both g 8 2)
      (ui:arrange both g 0 0 8 2))
    (is (eq over (ui:under both 0 1)) "the one drawn last takes it")))

(defun %ids-in (form)
  "Every id a wire form carries, in the order they appear."
  (let (out)
    (labels ((walk (it)
               (when (consp it)
                 (destructuring-bind (tag props &rest parts) it
                   (declare (ignore tag))
                   (loop :for (k v) :on props :by #'cddr
                         :when (and (member k '(:click :changed)) (stringp v))
                           :do (push v out))
                   (mapc #'walk parts)))))
      (walk form))
    (nreverse out)))

(test a-widget-crosses-as-what-it-stands-for
  "A click that crossed during a repaint has to mean the widget it was on.
Numbered by counting the walk, every id after a row that went was the id of a
different widget -- so a listing that lost a row ran the wrong row's action for
every row below it."
  (with-tree
    (setf (node:contents (tree:ensure "/probe/rows/beta")) 2)
    (setf (node:contents (tree:ensure "/probe/rows/gamma")) 3)
    (let ((s (ui:make-surface
              "probe-ids"
              (lambda ()
                (ui:rows (path:path "/probe/rows")
                         (lambda () (ui:button :click (ui:here)
                                               (ui:label "x"))))))))
      (is (equal '("/probe/rows/beta/click" "/probe/rows/gamma/click")
                 (%ids-in (node:contents (tree:at "/surface/probe-ids/wire"))))
          "an id says what its row is for, not where it fell in the walk")
      (tree:erase "/probe/rows/beta")
      (is (equal '("/probe/rows/gamma/click")
                 (%ids-in (node:contents (tree:at "/surface/probe-ids/wire"))))
          "and the row that stayed keeps the id it had when the one above went")
      (pine/ui::forget-surface (node:name s)))))

(test a-widget-that-stands-for-nothing-crosses-as-where-it-sits
  "A bar's mute button stands for nothing of its own, so for it the shape is the
identity: the second thing in the first row."
  (with-tree
    (let ((s (ui:make-surface
              "probe-shape"
              (lambda ()
                (ui:column (ui:row (ui:label "x")
                                   (ui:button :click "mute" (ui:label "m"))))))))
      (is (equal '("@0.1/click")
                 (%ids-in (node:contents (tree:at "/surface/probe-shape/wire"))))
          "where it sits, root first")
      (pine/ui::forget-surface (node:name s)))))
