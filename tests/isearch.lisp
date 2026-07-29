(in-package :pine.test)

(def-suite* :pine.isearch :in :pine)

(defun lines-of (&rest strings)
  (fset:convert 'fset:seq strings))

(defun found (lines pattern from-line from-col direction)
  (multiple-value-bind (l c)
      (pine.editor.isearch::%isearch-find lines pattern from-line from-col direction)
    (and l (list l c))))

(test an-all-lowercase-pattern-folds-case
  (is-true (pine.editor.isearch::%isearch-fold-p "abc"))
  (is-false (pine.editor.isearch::%isearch-fold-p "aBc")))

(test a-lowercase-pattern-matches-either-case
  (let ((lines (lines-of "Alpha" "beta")))
    (is (equal '(0 0) (found lines "alpha" 0 0 :forward)))
    (is (null (found lines "BETA" 0 0 :forward))
        "a capital in the pattern makes it exact, so this must miss")))

(test a-pattern-with-a-capital-is-exact
  (let ((lines (lines-of "Alpha" "alpha")))
    (is (equal '(0 0) (found lines "Alpha" 0 0 :forward)))
    (is (null (found lines "ALPHA" 0 0 :forward)))))

(test a-forward-search-finds-the-first-match-at-or-after-the-origin
  (let ((lines (lines-of "xx ab" "ab yy" "ab")))
    (is (equal '(0 3) (found lines "ab" 0 0 :forward)))
    (is (equal '(1 0) (found lines "ab" 0 4 :forward)))
    (is (equal '(2 0) (found lines "ab" 2 0 :forward)))
    (is (null (found lines "ab" 2 1 :forward)))))

(test a-backward-search-finds-the-last-match-at-or-before-the-origin
  (let ((lines (lines-of "ab cd ab" "zz")))
    (is (equal '(0 6) (found lines "ab" 0 8 :backward)))
    (is (equal '(0 0) (found lines "ab" 0 3 :backward)))
    (is (equal '(0 6) (found lines "ab" 1 0 :backward)))))

(test a-pattern-never-spans-a-newline
  (is (null (found (lines-of "ab" "cd") "abcd" 0 0 :forward))))

(test an-empty-pattern-finds-nothing
  (is (null (found (lines-of "abc") "" 0 0 :forward))))

(test typing-extends-the-search-and-moves-point
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "isearch-probe")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (pine.ns:write (pine.buf:at buf :text) (format nil "alpha~%beta~%gamma"))
      (sleep 0.1)
      (pine.text.buffer:put-point buf 0 0)
      (sleep 0.05)
      (pine.editor.isearch:isearch-start :forward)
      (type-text "bet")
      (multiple-value-bind (l c) (pine.editor.ask:ask buf :point)
        (is (= 1 l))
        (is (= 3 c) "point lands after a forward match"))
      (is (search "I-search" (pine.editor.echo:current-message)))
      (press "Return")
      (is (null pine.editor.isearch:*isearch*)))))

(test backspace-shrinks-the-search-back-to-the-origin
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "isearch-shrink")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (pine.ns:write (pine.buf:at buf :text) (format nil "alpha~%beta"))
      (sleep 0.1)
      (pine.text.buffer:put-point buf 0 0)
      (sleep 0.05)
      (pine.editor.isearch:isearch-start :forward)
      (type-text "bet")
      (press* "BackSpace" "BackSpace" "BackSpace")
      (multiple-value-bind (l c) (pine.editor.ask:ask buf :point)
        (is (equal '(0 0) (list l c))))
      (press "Return"))))

(test c-g-aborts-back-to-where-the-search-began
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "isearch-abort")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (pine.ns:write (pine.buf:at buf :text) (format nil "alpha~%beta"))
      (sleep 0.1)
      (pine.text.buffer:put-point buf 0 2)
      (sleep 0.05)
      (pine.editor.isearch:isearch-start :forward)
      (type-text "bet")
      (press "C-g")
      (multiple-value-bind (l c) (pine.editor.ask:ask buf :point)
        (is (equal '(0 2) (list l c))))
      (is (null pine.editor.isearch:*isearch*))
      (is (string= "quit" (pine.editor.echo:current-message))))))

(test a-failing-search-says-so-and-leaves-point-alone
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "isearch-fail")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (pine.ns:write (pine.buf:at buf :text) "alpha")
      (sleep 0.1)
      (pine.text.buffer:put-point buf 0 0)
      (sleep 0.05)
      (pine.editor.isearch:isearch-start :forward)
      (type-text "zzz")
      (is (search "Failing" (pine.editor.echo:current-message)))
      (press "C-g"))))

(test an-accepted-search-is-remembered-for-the-next-one
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "isearch-last")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (pine.ns:write (pine.buf:at buf :text) (format nil "alpha~%beta"))
      (sleep 0.1)
      (pine.text.buffer:put-point buf 0 0)
      (sleep 0.05)
      (pine.editor.isearch:isearch-start :forward)
      (type-text "bet")
      (press "Return")
      (is (string= "bet" pine.editor.isearch:*isearch-last*)))))

(test any-other-key-accepts-the-search-and-then-does-its-own-job
  (with-fixture substrate ()
    (let ((buf (pine.editor.frame::make-buffer "isearch-fallthrough")))
      (pine.editor.frame::set-buffer-mode buf :text)
      (setf (pine.editor.frame::current-buffer *client*) buf)
      (pine.ns:write (pine.buf:at buf :text) "alpha beta")
      (sleep 0.1)
      (pine.text.buffer:put-point buf 0 0)
      (sleep 0.05)
      (pine.editor.isearch:isearch-start :forward)
      (type-text "beta")
      (press "C-a")
      (is (null pine.editor.isearch:*isearch*))
      (multiple-value-bind (l c) (pine.editor.ask:ask buf :point)
        (is (equal '(0 0) (list l c)) "C-a ran after the search accepted")))))
