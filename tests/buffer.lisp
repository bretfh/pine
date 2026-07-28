(in-package :pine.test)

(def-suite* :pine.buffer :in :pine)

;;;; The reference model is a list of strings: the obvious implementation of
;;;; the same operations the fset buffer state implements. One script drives
;;;; both, so a divergence names the fset code.

(defun model-string (model)
  (format nil "~{~a~^~%~}" model))

(defun model-insert-string (model l c str)
  (let ((line (nth l model)))
    (append (subseq model 0 l)
            (list (concatenate 'string (subseq line 0 c) str (subseq line c)))
            (subseq model (1+ l)))))

(defun model-insert-newline (model l c)
  (let ((line (nth l model)))
    (append (subseq model 0 l)
            (list (subseq line 0 c) (subseq line c))
            (subseq model (1+ l)))))

(defun model-delete-char (model l c)
  (let ((line (nth l model)))
    (cond ((< c (length line))
           (append (subseq model 0 l)
                   (list (concatenate 'string (subseq line 0 c) (subseq line (1+ c))))
                   (subseq model (1+ l))))
          ((< (1+ l) (length model))
           (append (subseq model 0 l)
                   (list (concatenate 'string line (nth (1+ l) model)))
                   (subseq model (+ 2 l))))
          (t model))))

(defun model-delete-region (model sl sc el ec)
  (let ((first (nth sl model)) (last (nth el model)))
    (append (subseq model 0 sl)
            (list (concatenate 'string
                               (subseq first 0 (min sc (length first)))
                               (subseq last (min ec (length last)))))
            (subseq model (1+ el)))))

(defun apply-edit (state model op a b)
  "Apply the edit OP names to STATE and MODEL alike, placed by A and B.
Returns (values state model)."
  (let* ((nlines (length model))
         (l (mod a nlines))
         (line (nth l model))
         (c (mod b (1+ (length line)))))
    (ecase (mod op 5)
      (0 (values (pine.text.buffer:insert-string state l c "xy")
                 (model-insert-string model l c "xy")))
      (1 (values (pine.text.buffer:insert-char state l c #\z)
                 (model-insert-string model l c "z")))
      (2 (values (pine.text.buffer:insert-newline state l c)
                 (model-insert-newline model l c)))
      (3 (values (pine.text.buffer:delete-char state l c)
                 (model-delete-char model l c)))
      (4 (let* ((el (min (1- nlines) (+ l (mod b 3))))
                (eline (nth el model))
                (ec (if (= el l)
                        (min (1+ c) (length eline))
                        (mod b (1+ (length eline)))))
                (sc (if (= el l) (min c ec) c)))
           (values (pine.text.buffer:delete-region state l sc el ec)
                   (model-delete-region model l sc el ec)))))))

(test edits-match-the-reference-model
  (for-all ((script (gen-list :length (gen-integer :min 60 :max 240)
                              :elements (gen-integer :min 0 :max 255))))
    (let ((state (pine.text.buffer:load-content (format nil "alpha~%beta~%gamma")))
          (model (list "alpha" "beta" "gamma")))
      (loop :for (op a b) :on script :by #'cdddr
            :while b
            :do (multiple-value-setq (state model) (apply-edit state model op a b))
                (is (string= (model-string model)
                             (pine.text.buffer:state->string state)))))))

(test load-content-round-trips
  (dolist (text (list "" "one" (format nil "a~%b") (format nil "a~%b~%")
                      (format nil "~%~%") (format nil "trail sp ~%  lead")))
    (is (string= text (pine.text.buffer:state->string
                       (pine.text.buffer:load-content text))))))

(test load-content-round-trips-any-text
  (for-all ((text (gen-string :length (gen-integer :min 0 :max 60)
                              :elements (gen-one-element #\a #\b #\Space #\Newline))))
    (is (string= text (pine.text.buffer:state->string
                       (pine.text.buffer:load-content text))))))

(test split-lines-counts-the-newlines
  (for-all ((text (gen-string :length (gen-integer :min 0 :max 40)
                              :elements (gen-one-element #\x #\Newline))))
    (is (= (1+ (count #\Newline text))
           (length (pine.text.buffer:split-lines text))))))

(test multiline-insert-splits-and-leaves-point-after-the-text
  (let* ((state (pine.text.buffer:move-mark
                 (pine.text.buffer:load-content "ab") :point 0 1))
         (new (pine.text.buffer:insert-string state 0 1 (format nil "x~%y~%z"))))
    (is (string= (format nil "ax~%y~%zb") (pine.text.buffer:state->string new)))
    (let ((snap (pine.text.buffer:state->snapshot new)))
      (is (= 2 (pine.text.buffer:point-line snap)))
      (is (= 1 (pine.text.buffer:point-col snap))))))

(test word-motion-lands-exactly
  (let* ((state (pine.text.buffer:move-mark
                 (pine.text.buffer:load-content
                  (format nil "foo bar-baz  qux~%next line"))
                 :point 0 0))
         (snap (pine.text.buffer:state->snapshot state)))
    (loop :for (n want-line want-col) :in '((1 0 3) (2 0 7) (3 0 11) (4 0 16))
          :do (multiple-value-bind (l c)
                  (pine.text.buffer:point-after-move snap :word n)
                (is (equal (list want-line want-col) (list l c))
                    "word forward ~d" n)))
    (let ((end (pine.text.buffer:state->snapshot
                (pine.text.buffer:move-mark state :point 1 9))))
      (multiple-value-bind (l c) (pine.text.buffer:point-after-move end :word -2)
        (is (equal '(1 0) (list l c)))))))

(test char-motion-is-clamped-to-the-buffer
  (let ((snap (pine.text.buffer:state->snapshot
               (pine.text.buffer:move-mark
                (pine.text.buffer:load-content (format nil "ab~%cd")) :point 0 0))))
    (multiple-value-bind (l c) (pine.text.buffer:point-after-move snap :char -5)
      (is (equal '(0 0) (list l c))))
    (multiple-value-bind (l c) (pine.text.buffer:point-after-move snap :char 99)
      (is (equal '(1 2) (list l c))))
    (multiple-value-bind (l c) (pine.text.buffer:point-after-move snap :char 3)
      (is (equal '(1 0) (list l c))))))

(test line-motion-keeps-the-column-where-it-fits
  (let ((snap (pine.text.buffer:state->snapshot
               (pine.text.buffer:move-mark
                (pine.text.buffer:load-content (format nil "abcdef~%xy~%abcdef"))
                :point 0 5))))
    (multiple-value-bind (l c) (pine.text.buffer:point-after-move snap :line 1)
      (is (equal '(1 2) (list l c))))
    (multiple-value-bind (l c) (pine.text.buffer:point-after-move snap :line 9)
      (is (equal '(2 5) (list l c))))))

(test region-bounds-normalize-and-region-string-reads-them
  (let* ((state (pine.text.buffer:load-content (format nil "alpha~%beta~%gamma")))
         ;; the mark is one place, set in one write
         (marked (pine.text.buffer:set-meta state :mark (fset:seq 2 3)))
         (pointed (pine.text.buffer:move-mark marked :point 0 1)))
    (multiple-value-bind (sl sc el ec) (pine.text.buffer:region-bounds pointed)
      (is (equal '(0 1 2 3) (list sl sc el ec)))
      (is (string= (format nil "lpha~%beta~%gam")
                   (pine.text.buffer:region-string pointed sl sc el ec))))
    (is (null (pine.text.buffer:region-bounds state)))))

(test indent-width-counts-leading-blanks
  (is (= 0 (pine.text.buffer:line-indent-width "abc")))
  (is (= 3 (pine.text.buffer:line-indent-width "   abc")))
  (is (= 3 (pine.text.buffer:line-indent-width "   "))))

(test previous-line-indent-skips-blank-lines
  (let ((state (pine.text.buffer:load-content (format nil "  two~%~%     five~%~%x"))))
    (is (= 2 (pine.text.buffer:previous-line-indent state 1)))
    (is (= 2 (pine.text.buffer:previous-line-indent state 2)))
    (is (= 5 (pine.text.buffer:previous-line-indent state 4)))
    (is (= 0 (pine.text.buffer:previous-line-indent state 0)))))

(test reindent-line-moves-point-with-its-character
  (let ((state (pine.text.buffer:load-content (format nil "(foo~%  bar)"))))
    (multiple-value-bind (new col) (pine.text.buffer:reindent-line state 1 2 5 4)
      (is (= 7 col))
      (is (string= (format nil "(foo~%     bar)")
                   (pine.text.buffer:state->string new))))
    (multiple-value-bind (new col) (pine.text.buffer:reindent-line state 1 2 5 1)
      (declare (ignore new))
      (is (= 5 col)))))

(test buffer-local-reads-through-state-and-snapshot
  (let ((state (pine.text.buffer:set-meta
                (pine.text.buffer:load-content "x") :mode :lisp-mode)))
    (is (eq :lisp-mode (pine.text.buffer:buffer-local state :mode)))
    (is (eq :lisp-mode (pine.text.buffer:buffer-local
                        (pine.text.buffer:state->snapshot state) :mode)))
    (is (eq :fallback (pine.text.buffer:buffer-local state :absent :fallback)))))

(test an-edit-bumps-the-tick-and-a-motion-does-not
  (let* ((state (pine.text.buffer:load-content "abc"))
         (tick (pine.text.buffer:tick state)))
    (is (= (1+ tick) (pine.text.buffer:tick
                      (pine.text.buffer:insert-char state 0 0 #\x))))
    (is (= (1+ tick) (pine.text.buffer:tick
                      (pine.text.buffer:delete-char state 0 0))))
    (is (= tick (pine.text.buffer:tick
                 (pine.text.buffer:move-mark state :point 0 1))))))
