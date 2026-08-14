(in-package :pine.test)

(def-suite* :pine.isearch :in :pine)

(defmacro with-lines ((&rest lines) &body body)
  `(unwind-protect
        (progn
          (pine:start)
          (setf (pine.fs.node:contents (pine.edit.buffer:current))
                (format nil "~{~a~^~%~}" (list ,@lines)))
          (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 0)
          ,@body)
     (pine:stop)))

(defun chord (c) (pine.edit.key:dispatch nil (pine.edit.key:parse-key c)))

(defun typing (text)
  (loop :for ch :across text
        :do (pine.edit.key:dispatch nil (pine.edit.key:make-key (string ch)))))

(test every-key-narrows-the-search-as-it-is-typed
  (with-lines ("alpha" "beta" "gamma beta")
    (chord "C-s")
    (is-true (pine.edit.isearch:searching) "C-s takes the keyboard")
    (typing "be")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current)))
        "it landed while you were still typing, not on RET")
    (is (search "I-search: be" (pine.edit.isearch:said)))))

(test c-s-again-advances-and-wraps-once
  (with-lines ("alpha" "beta" "gamma beta")
    (chord "C-s")
    (typing "beta")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current))))
    (chord "C-s")
    (is (equal '(2 6) (pine.edit.buffer:point (pine.edit.buffer:current))))
    (chord "C-s")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current)))
        "past the last match it comes round again")
    (is-true (pine.edit.isearch:wrapped (pine.edit.isearch:searching))
             "and says so")))

(test c-r-turns-the-search-round
  (with-lines ("beta" "alpha" "beta")
    (pine.edit.buffer:goto! (pine.edit.buffer:current) 2 0)
    (chord "C-s")
    (typing "beta")
    (chord "C-r")
    (is (equal '(0 0) (pine.edit.buffer:point (pine.edit.buffer:current)))
        "C-r mid-search searches back from here")))

(test backspace-widens-it-again
  (with-lines ("alpha" "beta")
    (chord "C-s")
    (typing "bet")
    (chord "DEL")
    (is (search "I-search: be" (pine.edit.isearch:said)))
    (is-true (pine.edit.isearch:searching) "and it is still searching")))

(test c-g-goes-back-to-where-it-started
  (with-lines ("alpha" "beta")
    (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 2)
    (chord "C-s")
    (typing "beta")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current))))
    (chord "C-g")
    (is (null (pine.edit.isearch:searching)))
    (is (equal '(0 2) (pine.edit.buffer:point (pine.edit.buffer:current))))))

(test a-key-that-is-not-the-search-ends-it-and-then-does-its-job
  (with-lines ("alpha" "beta")
    (chord "C-s")
    (typing "beta")
    (chord "C-a")
    (is (null (pine.edit.isearch:searching)) "the search ended")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current)))
        "and C-a did what C-a does")))

(test case-folds-until-you-type-a-capital
  (with-lines ("alpha" "Beta" "beta")
    (chord "C-s")
    (typing "bet")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current)))
        "lowercase matches either")
    (chord "C-g")
    (chord "C-s")
    (typing "Bet")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current)))
        "a capital asks for exactly that")))

(test an-empty-search-brings-the-last-one-back
  (with-lines ("alpha" "beta")
    (chord "C-s")
    (typing "beta")
    (chord "RET")
    (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 0)
    (chord "C-s")
    (chord "C-s")
    (is (equal '(1 0) (pine.edit.buffer:point (pine.edit.buffer:current))))))

(test the-match-is-marked-while-you-look-at-it
  (with-lines ("alpha" "beta")
    (chord "C-s")
    (typing "bet")
    (is (equal '((:face :match))
               (pine.edit.buffer:properties-at (pine.edit.buffer:current) 1 1)))
    (chord "RET")
    (is (null (pine.edit.buffer:properties-at (pine.edit.buffer:current) 1 1))
        "and the mark goes when the search does")))
