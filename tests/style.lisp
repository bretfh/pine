(in-package :pine.test)
(named-readtables:in-readtable pine.data:syntax)

(def-suite* :pine.style :in :pine)

;;;; INSTALL-RULES merges into one stylesheet that outlives a test, so every
;;;; selector here is unique to the test that installs it.

(defun style-for (classes &key hover)
  (pine.ui.style:resolve (list (pine.ui.cells:class-names classes)) :hover hover))

(defun style-under (ancestor-classes classes)
  (pine.ui.style:resolve (list (pine.ui.cells:class-names ancestor-classes)
                               (pine.ui.cells:class-names classes))))

(test lengths-parse-from-numbers-lists-and-css-strings
  (is (equal '(4) (pine.ui.style::parse-lengths 4)))
  (is (equal '(4 8) (pine.ui.style::parse-lengths '(4 8))))
  (is (equal '(10 12) (pine.ui.style::parse-lengths "10px 12px")))
  (is (equal '(10) (pine.ui.style::parse-lengths "10px 2rem 50%")))
  (is (null (pine.ui.style::parse-lengths nil))))

(test the-box-shorthand-expands-to-four-sides
  (is (equal '(1 1 1 1) (pine.ui.style::parse-box4 "1px")))
  (is (equal '(1 2 1 2) (pine.ui.style::parse-box4 "1px 2px")))
  (is (equal '(1 2 3 2) (pine.ui.style::parse-box4 "1px 2px 3px")))
  (is (equal '(1 2 3 4) (pine.ui.style::parse-box4 "1px 2px 3px 4px")))
  (is (null (pine.ui.style::parse-box4 nil))))

(test padding-averages-asymmetric-sides-onto-two-axes
  (multiple-value-bind (x y) (pine.ui.style::box-xy "6px")
    (is (equal '(6 6) (list x y))))
  (multiple-value-bind (x y) (pine.ui.style::box-xy "4px 10px")
    (is (equal '(10 4) (list x y))))
  (multiple-value-bind (x y) (pine.ui.style::box-xy "2px 10px 6px 20px")
    (is (equal '(15 4) (list x y)))))

(test colors-parse-from-hex-and-the-rgb-functions
  (multiple-value-bind (r g b a) (pine.ui.style::parse-color "#ff8000")
    (is (= 1.0 r))
    (is (< 0.5 g 0.51))
    (is (= 0.0 b))
    (is (= 1.0 a)))
  (multiple-value-bind (r g b a) (pine.ui.style::parse-color "rgba(255, 0, 0, 0.5)")
    (is (equal '(1.0 0.0 0.0 0.5) (list r g b a))))
  (is (null (pine.ui.style::parse-color "transparent")))
  (is (null (pine.ui.style::parse-color "none")))
  (is (null (pine.ui.style::parse-color nil))))

(test a-full-round-radius-is-a-keyword-not-a-length
  (is (= 0 (pine.ui.style::parse-radius nil)))
  (is (= 8 (pine.ui.style::parse-radius "8px")))
  (is (= 8 (pine.ui.style::parse-radius 8)))
  (is (eq :round (pine.ui.style::parse-radius "100%"))))

(test opacity-parses-from-a-string-or-a-number-and-clamps
  (is (= 0.42 (pine.ui.style::parse-opacity "0.42")))
  (is (= 0.42 (pine.ui.style::parse-opacity 0.42)))
  (is (= 1.0 (pine.ui.style::parse-opacity 5)))
  (is (= 0.0 (pine.ui.style::parse-opacity -1)))
  (is (null (pine.ui.style::parse-opacity "wat"))))

(test an-inset-shadow-and-a-drop-shadow-are-told-apart
  (is (equal '(1.0 0.0 0.0 3) (pine.ui.style::parse-inset "inset 3px 0 0 0 #ff0000")))
  (is (null (pine.ui.style::parse-inset "0 0 5px 0 #000000")))
  (is (null (pine.ui.style::parse-shadow "inset 3px 0 0 0 #ff0000")))
  (destructuring-bind (ox oy blur color) (pine.ui.style::parse-shadow "1px 2px 5px 0 #000000")
    (is (equal '(1 2 5) (list ox oy blur)))
    (is (equal '(0.0 0.0 0.0 1.0) color))))

(test a-gradient-yields-its-two-stops
  (let ((stops (pine.ui.style::parse-gradient "linear-gradient(135deg, #ff0000, #0000ff)")))
    (is (= 2 (length stops)))
    (is (equal '(1.0 0.0 0.0) (first stops)))
    (is (equal '(0.0 0.0 1.0) (second stops))))
  (is (null (pine.ui.style::parse-gradient "none"))))

(test a-keyword-selector-names-one-class
  (pine.ui.rules:install-rules (list (list :st-kw {:opacity 0.25})))
  (is (= 0.25 (pine.ui.style:st-opacity (style-for :st-kw)))))

(test lisp-values-carry-through-to-px-properties
  (pine.ui.rules:install-rules (list (list :st-num {:opacity 0.42 :min-width 28 :border-radius 6 :padding 4})))
  (let ((st (style-for :st-num)))
    (is (= 0.42 (pine.ui.style:st-opacity st)))
    (is (= 28 (pine.ui.style:st-min-w st)))
    (is (= 6 (pine.ui.style:st-radius st)))
    (is (= 4 (pine.ui.style:st-pad-x st)))))

(test css-strings-carry-through-too
  (pine.ui.rules:install-rules (list (list ".st-str" {:opacity "0.5" :min-height "12px"})))
  (let ((st (style-for :st-str)))
    (is (= 0.5 (pine.ui.style:st-opacity st)))
    (is (= 12 (pine.ui.style:st-min-h st)))))

(test a-compound-selector-needs-every-class
  (pine.ui.rules:install-rules (list (list '(:st-a :st-b) {:min-width 40})))
  (is (= 40 (pine.ui.style:st-min-w (style-for '(:st-a :st-b)))))
  (is (null (pine.ui.style:st-min-w (style-for :st-a)))))

(test a-descendant-selector-needs-the-ancestor
  (pine.ui.rules:install-rules (list (list ".st-outer .st-inner" {:min-width 33})))
  (is (= 33 (pine.ui.style:st-min-w (style-under :st-outer :st-inner))))
  (is (null (pine.ui.style:st-min-w (style-for :st-inner)))))

(test a-hover-pseudo-matches-only-while-hovered
  (pine.ui.rules:install-rules (list (list ".st-hov:hover" {:min-width 21})))
  (is (= 21 (pine.ui.style:st-min-w (style-for :st-hov :hover t))))
  (is (null (pine.ui.style:st-min-w (style-for :st-hov)))))

(test the-later-rule-wins-the-cascade
  (pine.ui.rules:install-rules (list (list ".st-cascade" {:min-width 10})))
  (pine.ui.rules:install-rules (list (list ".st-cascade" {:min-width 90})))
  (is (= 90 (pine.ui.style:st-min-w (style-for :st-cascade)))))

(test a-user-rule-outranks-a-built-in-one
  (pine.ui.rules:install-rules (list (list ".cand" {:color "#010203"})))
  (is (equal '(1/255 2/255 3/255)
             (mapcar #'rationalize (pine.ui.style:st-fg (style-for :cand))))))

(test a-bare-element-selector-never-matches-a-layout-node
  (is (null (pine.ui.style:st-bg (style-for :button)))))

(test margin-merges-the-shorthand-with-the-per-side-overrides
  (pine.ui.rules:install-rules (list (list ".st-margin" {:margin "4px" :margin-left 9})))
  (is (equal '(4 4 4 9) (pine.ui.style:st-margin (style-for :st-margin)))))

(test a-zero-margin-resolves-to-nothing-at-all
  (pine.ui.rules:install-rules (list (list ".st-nomargin" {:margin "0"})))
  (is (null (pine.ui.style:st-margin (style-for :st-nomargin)))))

(test a-border-needs-solid-to-take-its-width
  (pine.ui.rules:install-rules
   (list (list ".st-bord" {:border-style "solid" :border-width "2px"
                           :border-color "#00ff00"})
         (list ".st-noborder" {:border-width "2px" :border-color "#00ff00"})))
  (let ((st (style-for :st-bord)))
    (is (= 2 (pine.ui.style:st-border-w st)))
    (is (equal '(0.0 1.0 0.0) (pine.ui.style:st-border-color st))))
  (is (= 0 (pine.ui.style:st-border-w (style-for :st-noborder)))))

(test installing-a-selector-again-replaces-it
  (pine.ui.rules:install-rules (list (list ".st-once" {:min-width 5})))
  (let ((before (length (pine.ui.rules:user-rules))))
    (pine.ui.rules:install-rules (list (list ".st-once" {:min-width 6})))
    (is (= before (length (pine.ui.rules:user-rules))))
    (is (= 6 (pine.ui.style:st-min-w (style-for :st-once))))))

(test selector-strings-canonicalize-from-symbols-and-lists
  (is (string= ".foo" (pine.ui.rules:selector-string :foo)))
  (is (string= ".foo.bar" (pine.ui.rules:selector-string '(:foo :bar))))
  (is (string= ".a > .b" (pine.ui.rules:selector-string ".a > .b"))))
