(in-package :pine.test)

(def-suite* :pine.completion :in :pine)

(defun completed (input table)
  (mapcar #'pine.editor.completion:candidate-string
          (pine.editor.completion:complete input table)))

(test empty-input-matches-everything
  (is (equal '("aa" "bb" "cc") (completed "" '("aa" "bb" "cc")))))

(test a-component-matches-anywhere-in-the-candidate
  (is (equal '("forward-word") (completed "ward" '("forward-word" "kill-line")))))

(test components-match-in-any-order
  (is (equal '("forward-word")
             (completed "word forward" '("forward-word" "backward-line")))))

(test every-component-must-match
  (is (null (completed "forward zzz" '("forward-word")))))

(test matching-is-case-insensitive
  (is (equal '("Forward-Word") (completed "forward" '("Forward-Word")))))

(test a-tighter-match-ranks-first
  (is (equal '("word-x" "a-word") (completed "word" '("a-word" "word-x")))))

(test a-shorter-candidate-breaks-a-score-tie
  (is (equal '("ab" "abcd") (completed "ab" '("abcd" "ab")))))

(test ranking-is-lexicographic-when-score-and-length-tie
  (is (equal '("aab" "aac") (completed "aa" '("aac" "aab")))))

(test a-match-records-the-spans-it-hit
  (let ((cand (first (pine.editor.completion:complete "ward" '("forward-word")))))
    (is (equal '((3 . 7)) (pine.editor.completion::candidate-spans cand)))))

(test spans-do-not-leak-between-queries
  (let ((table (list (pine.editor.completion:candidate "forward-word"))))
    (pine.editor.completion:complete "ward" table)
    (is (null (pine.editor.completion::candidate-spans (first table)))
        "the table's own candidate must not carry a previous query's spans")))

(test a-function-table-is-queried-with-the-input
  (let ((asked nil))
    (is (equal '("in:xy")
               (completed "xy" (lambda (input)
                                 (setf asked input)
                                 (list (format nil "in:~a" input))))))
    (is (string= "xy" asked))))

(test a-vector-table-is-a-table-too
  (is (equal '("bb") (completed "bb" (vector "aa" "bb")))))

(test a-candidate-carries-its-metadata-through
  (let* ((table (list (pine.editor.completion:candidate
                       "open" :annotation "command" :category :command
                       :source :probe :value :the-value)))
         (hit (first (pine.editor.completion:complete "op" table))))
    (is (string= "open" (pine.editor.completion:candidate-string hit)))
    (is (string= "command" (pine.editor.completion:candidate-annotation hit)))
    (is (eq :command (pine.editor.completion:candidate-category hit)))
    (is (eq :probe (pine.editor.completion:candidate-source hit)))
    (is (eq :the-value (pine.editor.completion:candidate-value hit)))))

(test a-bare-string-is-upgraded-and-values-itself
  (let ((cand (pine.editor.completion:to-candidate "plain")))
    (is (string= "plain" (pine.editor.completion:candidate-string cand)))
    (is (string= "plain" (pine.editor.completion:candidate-value cand)))))

(test a-source-is-a-named-table
  (pine.editor.completion:register-source :probe-source '("one" "two"))
  (is (equal '("one" "two")
             (completed "" (pine.editor.completion:source-table :probe-source))))
  (is (null (pine.editor.completion:source-table :probe-absent))))

(test actions-are-looked-up-by-category
  (pine.editor.completion:register-actions :probe-cat '(("run" . identity)))
  (let ((cand (pine.editor.completion:candidate "x" :category :probe-cat)))
    (is (equal '(("run" . identity)) (pine.editor.completion:candidate-actions cand))))
  (is (null (pine.editor.completion:candidate-actions
             (pine.editor.completion:candidate "x")))))

(test the-popup-renders-a-row-per-candidate-with-its-annotation
  (let ((rows (mapcar (lambda (r) (string-right-trim " " (car r)))
                      (pine.ui.cells:render
                       (pine.editor.completion:completion-popup
                        (list (pine.editor.completion:candidate "one" :annotation "cmd")
                              (pine.editor.completion:candidate "two")))
                       20))))
    (is (= 2 (length rows)))
    (is (search "one" (first rows)))
    (is (search "cmd" (first rows)))
    (is (search "two" (second rows)))))

(test an-empty-popup-says-so
  (let ((rows (pine.ui.cells:render (pine.editor.completion:completion-popup nil) 20)))
    (is (search "no matches" (car (first rows))))))
