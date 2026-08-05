(in-package :pine.test)

(def-suite* :pine.index :in :pine)

;;;; The byte index is what lets tree-sitter read a buffer that has no flat
;;;; string behind it, so every question it answers is checked against the
;;;; string it is replacing: build the text, count the bytes, compare.

(defun lines-of (&rest strings)
  (fset:convert 'fset:seq strings))

(defun text-of (lines)
  (let ((out (make-string-output-stream)) (first t))
    (fset:do-seq (line lines)
      (unless first (write-char #\Newline out))
      (setf first nil)
      (write-string line out))
    (get-output-stream-string out)))

(defun naive-line-start (lines line)
  "LINE's byte offset, counted the slow way from the text itself."
  (let ((bytes 0))
    (dotimes (i (min line (fset:size lines)) bytes)
      (incf bytes (1+ (length (sb-ext:string-to-octets (fset:@ lines i)
                                                       :external-format :utf-8)))))))

(defun naive-total (lines)
  (length (sb-ext:string-to-octets (text-of lines) :external-format :utf-8)))

(defun index-agrees-p (index lines)
  "Whether INDEX answers what the text says, for every line and the total."
  (and (= (pine.ts.index:index-total index) (naive-total lines))
       (loop :for l :below (fset:size lines)
             :always (= (pine.ts.index:line-start index l)
                        (naive-line-start lines l)))))

(test a-fresh-index-agrees-with-the-text
  (let ((lines (lines-of "(defun f (x)" "  (+ x 1))" "" "(f 2)")))
    (is-true (index-agrees-p (pine.ts.index:build-index lines) lines))))

(test an-index-counts-bytes-not-characters
  (let* ((lines (lines-of (coerce (list (code-char 26085) (code-char 26412)) 'string)
                          (coerce (list (code-char 955)) 'string)
                          "ascii"))
         (index (pine.ts.index:build-index lines)))
    (is (= 7 (pine.ts.index:line-start index 1))
        "two three-byte characters and a newline should put the second line at byte 7")
    (is-true (index-agrees-p index lines))))

(test an-empty-buffer-has-one-empty-line
  (let* ((lines (lines-of ""))
         (index (pine.ts.index:build-index lines)))
    (is (= 0 (pine.ts.index:index-total index)))
    (is (= 0 (pine.ts.index:line-start index 0)))))

(test byte-line-inverts-line-start
  (let* ((lines (lines-of "alpha" "beta" "" "gamma delta" "e"))
         (index (pine.ts.index:build-index lines)))
    (loop :for l :below (fset:size lines)
          :do (multiple-value-bind (line offset)
                  (pine.ts.index:byte-line index (pine.ts.index:line-start index l))
                (is (= l line) "byte-line put line ~d at ~d" l line)
                (is (= 0 offset))))))

(test byte-line-finds-the-offset-inside-a-line
  (let* ((lines (lines-of "alpha" "beta"))
         (index (pine.ts.index:build-index lines)))
    (multiple-value-bind (line offset) (pine.ts.index:byte-line index 8)
      (is (= 1 line) "byte 8 is on the second line")
      (is (= 2 offset) "byte 8 is two bytes into it"))))

;;;; Edits: the index has to track the seq without rebuilding, so each of these
;;;; applies the same change to both and asks whether they still agree.

(defun apply-replace (lines index line text)
  (let* ((old (fset:@ lines line))
         (new-lines (fset:with lines line text))
         (delta (- (length (sb-ext:string-to-octets text :external-format :utf-8))
                   (length (sb-ext:string-to-octets old :external-format :utf-8)))))
    (values new-lines (pine.ts.index:index-edit index new-lines line delta 0))))

(defun apply-insert-line (lines index line text)
  (let ((new-lines (fset:insert lines line text)))
    (values new-lines
            (pine.ts.index:index-edit
             index new-lines line
             (1+ (length (sb-ext:string-to-octets text :external-format :utf-8))) 1))))

(defun apply-delete-line (lines index line)
  (let* ((old (fset:@ lines line))
         (new-lines (fset:less lines line)))
    (values new-lines
            (pine.ts.index:index-edit
             index new-lines line
             (- (1+ (length (sb-ext:string-to-octets old :external-format :utf-8)))) -1))))

(test an-edit-on-one-line-keeps-the-index-honest
  (let* ((lines (lines-of "alpha" "beta" "gamma"))
         (index (pine.ts.index:build-index lines)))
    (multiple-value-bind (l2 i2) (apply-replace lines index 1 "beta-and-more")
      (is-true (index-agrees-p i2 l2)))))

(test inserting-and-deleting-lines-keeps-the-index-honest
  (let* ((lines (lines-of "alpha" "beta" "gamma"))
         (index (pine.ts.index:build-index lines)))
    (multiple-value-bind (l2 i2) (apply-insert-line lines index 1 "inserted")
      (is-true (index-agrees-p i2 l2))
      (multiple-value-bind (l3 i3) (apply-delete-line l2 i2 0)
        (is-true (index-agrees-p i3 l3))))))

(defun apply-split (lines index line col)
  "Split LINE at COL, the way typing a newline does."
  (let* ((text (fset:@ lines line))
         (head (subseq text 0 col))
         (tail (subseq text col))
         (new-lines (fset:insert (fset:with lines line head) (1+ line) tail)))
    (values new-lines (pine.ts.index:index-edit index new-lines line 1 1))))

(defun apply-join (lines index line)
  "Join LINE with the one after it, the way backspace at column 0 does."
  (let* ((joined (concatenate 'string (fset:@ lines line) (fset:@ lines (1+ line))))
         (new-lines (fset:less (fset:with lines line joined) (1+ line))))
    (values new-lines (pine.ts.index:index-edit index new-lines line -1 -1))))

(test splitting-a-line-keeps-the-index-honest
  "A newline puts the start of the new line in the middle of the old one, which
is not a place any whole-line shift can name."
  (let* ((lines (lines-of "(defun f ()" "(bar))"))
         (index (pine.ts.index:build-index lines)))
    (multiple-value-bind (l2 i2) (apply-split lines index 0 11)
      (is (equal (list "(defun f ()" "" "(bar))") (fset:convert 'list l2)))
      (is (= 12 (pine.ts.index:line-start i2 1))
          "the line after the split starts at byte 12, not ~d"
          (pine.ts.index:line-start i2 1))
      (is-true (index-agrees-p i2 l2)))))

(test splitting-mid-line-keeps-the-index-honest
  (let* ((lines (lines-of "alphabeta" "gamma"))
         (index (pine.ts.index:build-index lines)))
    (multiple-value-bind (l2 i2) (apply-split lines index 0 5)
      (is-true (index-agrees-p i2 l2)))))

(test joining-two-lines-keeps-the-index-honest
  (let* ((lines (lines-of "alpha" "beta" "gamma"))
         (index (pine.ts.index:build-index lines)))
    (multiple-value-bind (l2 i2) (apply-join lines index 0)
      (is (equal (list "alphabeta" "gamma") (fset:convert 'list l2)))
      (is-true (index-agrees-p i2 l2)))))

(test a-run-of-splits-and-joins-keeps-the-index-honest
  (let* ((lines (lines-of "one two three" "four five" "six"))
         (index (pine.ts.index:build-index lines)))
    (dotimes (i 20)
      (multiple-value-bind (l2 i2) (apply-split lines index 0 4)
        (setf lines l2 index i2))
      (is-true (index-agrees-p index lines) "after split ~d" i)
      (multiple-value-bind (l2 i2) (apply-join lines index 0)
        (setf lines l2 index i2))
      (is-true (index-agrees-p index lines) "after join ~d" i))))

(test a-run-of-edits-past-the-compaction-point-still-agrees
  "More edits than the pending list carries, so the base gets rebuilt underneath
and the answers must not change."
  (let* ((lines (lines-of "alpha" "beta" "gamma" "delta" "epsilon"))
         (index (pine.ts.index:build-index lines)))
    (dotimes (i 100)
      (multiple-value-bind (l2 i2)
          (apply-replace lines index (mod i (fset:size lines))
                         (format nil "line-~d-~a" i (make-string (mod i 7)
                                                                 :initial-element #\x)))
        (setf lines l2 index i2)))
    (is-true (index-agrees-p index lines))))

;;;; Reading the source through the index. Each of these is a question the walks
;;;; used to ask a flat string, so each is checked against that string.

(defun naive-line-col (text byte)
  "The (values line char-col) of a byte offset, counted from the text."
  (let ((line 0) (col 0) (bytes 0))
    (loop :for ch :across text
          :while (< bytes byte)
          :do (incf bytes (length (sb-ext:string-to-octets (string ch)
                                                           :external-format :utf-8)))
              (if (char= ch #\Newline)
                  (setf line (1+ line) col 0)
                  (incf col)))
    (values line col)))

(test source-line-col-agrees-with-the-text
  (let* ((lines (lines-of "alpha" "beta" "gamma delta" "e"))
         (index (pine.ts.index:build-index lines))
         (text (text-of lines)))
    (loop :for byte :below (naive-total lines)
          :do (multiple-value-bind (want-line want-col) (naive-line-col text byte)
                (multiple-value-bind (line col) (pine.ts.index:source-line-col index byte)
                  (is (and (= want-line line) (= want-col col))
                      "byte ~d: wanted ~d,~d got ~d,~d" byte want-line want-col line col))))))

(test source-line-col-agrees-outside-ascii
  (let* ((lines (lines-of (coerce (list (code-char 26085) #\a (code-char 955)) 'string)
                          "plain"))
         (index (pine.ts.index:build-index lines))
         (text (text-of lines)))
    (loop :for byte :below (naive-total lines)
          :do (multiple-value-bind (want-line want-col) (naive-line-col text byte)
                (multiple-value-bind (line col) (pine.ts.index:source-line-col index byte)
                  (is (and (= want-line line) (= want-col col))
                      "byte ~d: wanted ~d,~d got ~d,~d" byte want-line want-col line col))))))

(test source-byte-inverts-source-line-col
  (let* ((lines (lines-of "alpha" (coerce (list (code-char 26085) #\x) 'string) "z"))
         (index (pine.ts.index:build-index lines)))
    (loop :for line :below (fset:size lines)
          :do (loop :for col :to (length (fset:@ lines line))
                    :do (multiple-value-bind (l c)
                            (pine.ts.index:source-line-col
                             index (pine.ts.index:source-byte index line col))
                          (is (and (= line l) (= col c))
                              "line ~d col ~d round-tripped to ~d,~d" line col l c))))))

(test source-substring-agrees-with-subseq
  (let* ((lines (lines-of "(defun f (x)" "  (+ x 1))" "" "(f 2)"))
         (index (pine.ts.index:build-index lines))
         (text (text-of lines))
         (total (naive-total lines)))
    (loop :for start :below total :by 3
          :do (loop :for end :from start :to total :by 5
                    :do (let ((want (subseq text
                                            (nth-value 0 (%char-of-byte text start))
                                            (nth-value 0 (%char-of-byte text end))))
                              (got (pine.ts.index:source-substring index start end)))
                          (is (string= want got)
                              "bytes ~d..~d: wanted ~s got ~s" start end want got))))))

(defun %char-of-byte (text byte)
  "The character index BYTE bytes into TEXT."
  (let ((bytes 0) (i 0))
    (loop :for ch :across text
          :while (< bytes byte)
          :do (incf bytes (length (sb-ext:string-to-octets (string ch)
                                                           :external-format :utf-8)))
              (incf i))
    (values i)))

(test source-char-at-agrees-with-char
  (let* ((lines (lines-of "ab" "cd"))
         (index (pine.ts.index:build-index lines))
         (text (text-of lines)))
    (loop :for byte :below (naive-total lines)
          :do (is (eql (char text (%char-of-byte text byte))
                       (pine.ts.index:source-char-at index byte))
                  "byte ~d disagrees" byte))
    (is (null (pine.ts.index:source-char-at index (naive-total lines)))
        "past the end should be nil")))

;;;; Reading the parser from the seq. These go through libtree-sitter, so they
;;;; are what says the callback's ABI is right: a wrong struct layout or a wrong
;;;; byte offset produces a tree that is plausible and wrong.

(defun ts-available-p ()
  (let ((rt (pine.core.server:ts-runtime pine.core.server:*server*)))
    (and rt (pine.ts.runtime:make-parse-state rt :commonlisp))))

(defun root-span (ps)
  (let ((root (pine.ts.runtime:ts-tree-root-node (pine.ts.runtime:ps-tree ps))))
    (list (pine.ts.runtime:ts-node-start-byte root)
          (pine.ts.runtime:ts-node-end-byte root)
          (pine.ts.runtime:ts-node-named-count root))))

(test parsing-from-the-lines-agrees-with-parsing-the-string
  "The same source through both paths has to produce the same tree."
  (with-fixture substrate ()
    (let ((from-string (ts-available-p))
          (from-lines (ts-available-p))
          (text (format nil "(defun f (x)~%  \"doc\"~%  (+ x 1))~%~%(f 2)")))
      (is (not (null from-string)) "the commonlisp grammar is unavailable")
      (when (and from-string from-lines)
        (pine.ts.runtime:parse-text! from-string text)
        (pine.ts.runtime:parse-lines! from-lines
                                      (fset:convert 'fset:seq
                                                    (pine.text:split-lines text)))
        (is (equal (root-span from-string) (root-span from-lines))
            "the tree read from the seq differs from the tree read from the string")
        (pine.ts.runtime:free-parse-state from-string)
        (pine.ts.runtime:free-parse-state from-lines)))))

(test parsing-from-the-lines-handles-text-outside-ascii
  (with-fixture substrate ()
    (let ((from-string (ts-available-p))
          (from-lines (ts-available-p))
          (text (format nil "(defun f ()~%  \"~a\")~%(f)"
                        (coerce (list (code-char 26085) (code-char 26412)) 'string))))
      (when (and from-string from-lines)
        (pine.ts.runtime:parse-text! from-string text)
        (pine.ts.runtime:parse-lines! from-lines
                                      (fset:convert 'fset:seq
                                                    (pine.text:split-lines text)))
        (is (equal (root-span from-string) (root-span from-lines))
            "multi-byte characters put the two paths out of step")
        (pine.ts.runtime:free-parse-state from-string)
        (pine.ts.runtime:free-parse-state from-lines)))))

(test an-edited-reparse-from-the-lines-matches-a-fresh-parse
  "An incremental parse over the seq must land where a fresh parse of the same
lines lands, which is what makes the carried byte index trustworthy."
  (with-fixture substrate ()
    (let ((incremental (ts-available-p))
          (fresh (ts-available-p))
          (lines (fset:convert 'fset:seq (list "(defun f (x)" "  (+ x 1))" "(f 2)"))))
      (when (and incremental fresh)
        (pine.ts.runtime:parse-lines! incremental lines)
        (let* ((edited (fset:with lines 1 "  (+ x 100))"))
               (delta (- (length "  (+ x 100))") (length "  (+ x 1))"))))
          (pine.ts.runtime:parse-lines! incremental edited :edit (list 1 1 1 delta))
          (pine.ts.runtime:parse-lines! fresh edited)
          (is (equal (root-span fresh) (root-span incremental))
              "the incremental parse disagrees with a fresh one"))
        ;; a line appearing and a line going away are the cases the whole-line
        ;; edit span has to get right, since both move every row after them
        (let ((grown (fset:insert (fset:with lines 1 "  (+ x 100))") 1 "  ;; note")))
          (pine.ts.runtime:parse-lines! incremental grown :edit (list 1 1 2 10))
          (pine.ts.runtime:parse-lines! fresh grown)
          (is (equal (root-span fresh) (root-span incremental))
              "an inserted line put the incremental parse out of step"))
        (pine.ts.runtime:free-parse-state incremental)
        (pine.ts.runtime:free-parse-state fresh)))))

(test random-edits-keep-the-index-honest
  (let ((*num-trials* 20))
    (for-all ((seed (gen-integer :min 0 :max 100000)))
      (let* ((lines (lines-of "alpha" "beta" "gamma" "delta"))
             (index (pine.ts.index:build-index lines))
             (state seed))
        (flet ((next (n) (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
                 (mod state n)))
          (dotimes (i 30)
            (let ((line (next (fset:size lines))))
              (multiple-value-bind (l2 i2)
                  (case (next 3)
                    (0 (apply-replace lines index line (format nil "r~d" (next 1000))))
                    (1 (apply-insert-line lines index line (format nil "i~d" (next 1000))))
                    (t (if (> (fset:size lines) 1)
                           (apply-delete-line lines index line)
                           (apply-replace lines index line "x"))))
                (setf lines l2 index i2)))))
        (is-true (index-agrees-p index lines)
                 "seed ~d produced an index that disagrees with its text" seed)))))
