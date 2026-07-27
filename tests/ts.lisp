(in-package :pine.test)

(def-suite* :pine.ts :in :pine)

;;;; The grammar is a dependency, not a maybe: the manifest ships
;;;; tree-sitter-commonlisp. A run without it is a broken environment, so
;;;; GRAMMAR-LOADS asserts it and the rest of the file uses it unguarded.

(defvar *runtime* nil)

(defun runtime ()
  (or *runtime* (setf *runtime* (pine.ts.runtime:make-ts-runtime))))

(defmacro with-parse-state ((var language) &body body)
  `(let ((,var (pine.ts.runtime:make-parse-state (runtime) ,language)))
     (unwind-protect (progn ,@body)
       (when ,var (pine.ts.runtime:free-parse-state ,var)))))

(defun sorted-highlights (highlights)
  (sort (copy-list highlights)
        (lambda (a b)
          (or (< (first a) (first b))
              (and (= (first a) (first b))
                   (or (< (second a) (second b))
                       (and (= (second a) (second b))
                            (string< (princ-to-string a) (princ-to-string b)))))))))

(defun full-highlights (text)
  (with-parse-state (ps :commonlisp)
    (pine.ts.runtime:parse-text! ps text)
    (sorted-highlights (pine.ts.highlight:parse-highlights ps))))

(defun generated-source (nforms)
  (with-output-to-string (out)
    (dotimes (i nforms)
      (format out "(defun fn~d (a b)~%  \"doc ~d\"~%  (let ((x (+ a ~d)))~%    (* x b)))~%~%"
              i i i))))

(defun mutate (text op pos)
  "Apply the edit OP names at POS in TEXT and return the new text."
  (let* ((len (length text))
         (i (mod pos (max 1 len))))
    (case (mod op 5)
      (0 (concatenate 'string (subseq text 0 i) "q" (subseq text i)))
      (1 (if (and (plusp len) (char/= (char text i) #\Newline))
             (concatenate 'string (subseq text 0 i) (subseq text (min len (1+ i))))
             text))
      (2 (if (and (plusp len) (char/= (char text i) #\Newline))
             (concatenate 'string (subseq text 0 i) "w" (subseq text (min len (1+ i))))
             text))
      (3 (concatenate 'string (subseq text 0 i) (string #\Newline) (subseq text i)))
      (4 (let ((nl (position #\Newline text :start i)))
           (if nl
               (concatenate 'string (subseq text 0 nl) (subseq text (1+ nl)))
               text))))))

(test grammar-loads
  (with-parse-state (ps :commonlisp)
    (is (not (null ps))
        "the commonlisp grammar must load; manifest.scm ships it")))

(test a-form-head-and-its-operands-take-their-faces
  (let ((hl (full-highlights "(defvar *x* 1)")))
    (is (member '(0 1 7 :keyword) hl :test #'equal))
    (is (member '(0 8 11 :variable) hl :test #'equal))
    (is (member '(0 12 13 :number) hl :test #'equal))))

(test strings-comments-and-characters-are-distinguished
  (let ((hl (full-highlights (format nil ";; note~%\"text\"~%#\\a"))))
    (is (member '(0 0 999 :comment) hl :test #'equal)
        "a comment runs to the end of its line, so the span is open-ended")
    (is (member '(1 0 6 :string) hl :test #'equal))
    (is (member '(2 0 3 :character) hl :test #'equal))))

(test delimiters-cycle-by-depth
  (let ((hl (full-highlights "(a (b (c)))")))
    (is (member '(0 0 1 :delimiter.0) hl :test #'equal))
    (is (member '(0 3 4 :delimiter.1) hl :test #'equal))
    (is (member '(0 6 7 :delimiter.2) hl :test #'equal))))

(test a-defun-header-names-the-function-and-its-parameters
  (let ((hl (full-highlights "(defun f (x) x)")))
    (is (member '(0 7 8 :function-name) hl :test #'equal))
    (is (member '(0 10 11 :variable-param) hl :test #'equal))))

;;;; Each trial of the two properties below reparses and re-walks a whole file
;;;; per edit, so the trial count is set to what the cost affords rather than
;;;; left at FOR-ALL's default hundred.

(test incremental-highlighting-equals-the-full-walk-on-generated-source
  (let ((*num-trials* 5))
    (for-all ((script (gen-list :length (gen-integer :min 40 :max 120)
                                :elements (gen-integer :min 0 :max 4095))))
    (with-parse-state (ps :commonlisp)
      (let ((text (generated-source 8)))
        (pine.ts.runtime:parse-text! ps text)
        (pine.ts.highlight:parse-highlights ps)
        (loop :for (op pos) :on script :by #'cddr
              :while pos
              :do (setf text (mutate text op pos))
                  (pine.ts.runtime:parse-text! ps text)
                    (is (equal (full-highlights text)
                               (sorted-highlights
                                (pine.ts.highlight:parse-highlights ps))))))))))

(test incremental-highlighting-equals-the-full-walk-on-a-real-file
  (let ((path (merge-pathnames "../src/text/buffer.lisp"
                               #.(or *compile-file-truename* *load-truename*)))
        (*num-trials* 2))
    (is (not (null (probe-file path)))
        "src/text/buffer.lisp must exist; a moved file fails here")
    (let ((source (uiop:read-file-string path)))
      (for-all ((script (gen-list :length (gen-integer :min 8 :max 16)
                                  :elements (gen-integer :min 0 :max 65535))))
        (with-parse-state (ps :commonlisp)
          (let ((text source))
            (pine.ts.runtime:parse-text! ps text)
            (pine.ts.highlight:parse-highlights ps)
            (loop :for (op pos) :on script :by #'cddr
                  :while pos
                  :do (setf text (mutate text op pos))
                      (pine.ts.runtime:parse-text! ps text)
                      (is (equal (full-highlights text)
                                 (sorted-highlights
                                  (pine.ts.highlight:parse-highlights ps)))))))))))

(test a-viewport-walk-agrees-with-the-full-walk-inside-the-viewport
  "The window a buffer paints is highlighted by walking only its lines. That is
only sound if the tuples for those lines are the ones the whole-file walk would
emit, which is what the descent from the root buys."
  (let ((*num-trials* 20))
    (for-all ((forms (gen-integer :min 2 :max 14))
              (from (gen-integer :min 0 :max 60))
              (span (gen-integer :min 0 :max 12)))
      (with-parse-state (ps :commonlisp)
        (let ((text (generated-source forms))
              (to (+ from span)))
          (pine.ts.runtime:parse-text! ps text)
          (flet ((inside (highlights)
                   (sorted-highlights
                    (remove-if-not (lambda (tuple) (<= from (first tuple) to))
                                   highlights))))
            (is (equal (inside (pine.ts.highlight:parse-highlights ps))
                       (inside (pine.ts.highlight:parse-highlights
                                ps :from-line from :to-line to)))
                "viewport ~d..~d over ~d forms" from to forms)))))))

(test a-windowed-walk-stays-correct-across-edits-in-the-window
  "Repeated edits reuse the window's cached tuples and re-walk only the forms the
edit touched. Every call must still agree with a walk of the whole file."
  (let ((*num-trials* 10))
    (for-all ((script (gen-list :length (gen-integer :min 12 :max 40)
                                :elements (gen-integer :min 0 :max 4095)))
              (from (gen-integer :min 0 :max 40)))
      (with-parse-state (ps :commonlisp)
        (let ((text (generated-source 10))
              (to (+ from 20)))
          (pine.ts.runtime:parse-text! ps text)
          (pine.ts.highlight:parse-highlights ps :from-line from :to-line to)
          (flet ((inside (highlights)
                   (sorted-highlights
                    (remove-if-not (lambda (tuple) (<= from (first tuple) to))
                                   highlights))))
            (loop :for (op pos) :on script :by #'cddr
                  :while pos
                  :do (setf text (mutate text op pos))
                      (pine.ts.runtime:parse-text! ps text)
                      (is (equal (inside (full-highlights text))
                                 (inside (pine.ts.highlight:parse-highlights
                                          ps :from-line from :to-line to)))
                          "viewport ~d..~d" from to))))))))

(test a-viewport-walk-costs-less-than-the-whole-file
  "The point of the window: a screenful out of a large file allocates a small
fraction of what the full walk does."
  (with-parse-state (ps :commonlisp)
    (let ((text (generated-source 200)))
      (pine.ts.runtime:parse-text! ps text)
      (let* ((before (sb-ext:get-bytes-consed))
             (full (progn (pine.ts.highlight:parse-highlights ps)
                          (- (sb-ext:get-bytes-consed) before)))
             (mark (sb-ext:get-bytes-consed))
             (windowed (progn (pine.ts.highlight:parse-highlights
                               ps :from-line 100 :to-line 130)
                              (- (sb-ext:get-bytes-consed) mark))))
        (is (< (* 4 windowed) full)
            "a 30-line window allocated ~:d bytes against the full walk's ~:d"
            windowed full)))))

(test indent-columns-follow-the-tree
  (with-parse-state (ps :commonlisp)
    (pine.ts.runtime:parse-text!
     ps (format nil "(defun f (x)~%(let ((a 1)~%(b 2))~%(+ a~%b)))~%(foo bar~%baz)"))
    (is (equal '(0 2 6 2 3 0 5)
               (loop :for i :from 0 :to 6
                     :collect (pine.ts.highlight:parse-indent ps i))))))

(test a-multiline-string-interior-is-left-alone
  (with-parse-state (ps :commonlisp)
    (pine.ts.runtime:parse-text!
     ps (concatenate 'string "(defvar *x*" (string #\Newline)
                     (string #\") "a" (string #\Newline) "b"
                     (string #\") (string #\Newline) "1)"))
    (is (equal '(0 2 nil 2)
               (loop :for i :from 0 :to 3
                     :collect (pine.ts.highlight:parse-indent ps i))))))

(test a-user-def-macro-indents-its-body
  (with-parse-state (ps :commonlisp)
    (pine.ts.runtime:parse-text! ps (format nil "(defcommand foo ()~%(bar))"))
    (is (equal '(0 2)
               (list (pine.ts.highlight:parse-indent ps 0)
                     (pine.ts.highlight:parse-indent ps 1))))))

(test body-forms-are-recognized-by-family
  (is-true (pine.ts.highlight:body-form-p :commonlisp "defun"))
  (is-true (pine.ts.highlight:body-form-p :commonlisp "with-open-file"))
  (is-true (pine.ts.highlight:body-form-p :commonlisp "do-symbols"))
  (is-true (pine.ts.highlight:body-form-p :commonlisp "let"))
  (is-true (pine.ts.highlight:body-form-p :commonlisp "loop"))
  (is-false (pine.ts.highlight:body-form-p :commonlisp "format"))
  (is-false (pine.ts.highlight:body-form-p :commonlisp nil)))

(test head-kinds-name-the-role-of-a-form
  (is (eq :binder-nested (pine.ts.highlight:cl-head-kind "let"))
      "a binder is classified as one before it is classified as a special form")
  (is (eq :binder-flat-all (pine.ts.highlight:cl-head-kind "multiple-value-bind")))
  (is (eq :binder-flat-first (pine.ts.highlight:cl-head-kind "dolist")))
  (is (eq :special (pine.ts.highlight:cl-head-kind "if")))
  (is (eq :builtin (pine.ts.highlight:cl-head-kind "car")))
  (is (eq :def-var (pine.ts.highlight:cl-head-kind "defparameter")))
  (is (eq :def-class (pine.ts.highlight:cl-head-kind "defclass")))
  (is (eq :def-package (pine.ts.highlight:cl-head-kind "defpackage")))
  (is (eq :call (pine.ts.highlight:cl-head-kind "frobnicate"))))

(test structural-motion-crosses-forms
  (let ((text (format nil "(a b)~%(c d)")))
    (multiple-value-bind (l c)
        (pine.ts.runtime:forward-sexp-pos (runtime) :commonlisp text 0 0)
      (is (equal '(0 5) (list l c)) "over the whole first form"))
    (multiple-value-bind (l c)
        (pine.ts.runtime:backward-sexp-pos (runtime) :commonlisp text 1 4)
      (is (equal '(1 3) (list l c)) "back to the start of the atom before point"))))

(test defun-bounds-span-the-enclosing-top-level-form
  (let ((text (format nil "(a b)~%(defun f (x)~%  x)~%(c)")))
    (multiple-value-bind (sl sc el ec)
        (pine.ts.runtime:defun-bounds-pos (runtime) :commonlisp text 2 2)
      (is (equal '(1 0 2 4) (list sl sc el ec))))))

(test the-line-index-converts-both-ways-through-utf8
  (let* ((text (format nil "ab~c~%cd" (code-char #x00E9)))
         (index (pine.ts.runtime:build-line-index text)))
    (is (= 2 (length index)))
    (is (= 5 (car (aref index 1))) "e-acute is two bytes")
    (is (= 4 (cdr (aref index 1))) "but one character")
    (multiple-value-bind (line col)
        (pine.ts.runtime:byte-to-line-col 5 index text)
      (is (equal '(1 0) (list line col))))
    (is (= 5 (pine.ts.runtime:pos-to-byte text 1 0 index)))))

(test byte-length-counts-utf8-octets
  (is (= 1 (pine.ts.runtime:char-byte-length #\a)))
  (is (= 2 (pine.ts.runtime:char-byte-length (code-char #x00E9))))
  (is (= 3 (pine.ts.runtime:char-byte-length (code-char #x4F60))))
  (is (= 4 (pine.ts.runtime:byte-length (format nil "a~c" (code-char #x4F60))))))

(test freeing-a-parse-state-twice-is-safe
  (let ((ps (pine.ts.runtime:make-parse-state (runtime) :commonlisp)))
    (is (not (null ps)))
    (finishes (pine.ts.runtime:free-parse-state ps))
    (finishes (pine.ts.runtime:free-parse-state ps))))

;;;; The band. Past +WHOLE-FILE-LINES+ only the lines around the window are
;;;; given to tree-sitter, so a scroll has to move the band: the line seq is
;;;; unchanged and EQ across a scroll, and a parse keyed on that alone leaves
;;;; the old band's tree in place while the window asks about lines it does not
;;;; cover.

(defun banded-lines (n)
  (fset:convert 'fset:seq
                (loop :repeat (ceiling n 4)
                      :append (list "(defun f (x y)" "  (let ((z (+ x y)))"
                                    "    (* z 1)))" ""))))

(test a-scroll-moves-the-band-and-highlights-the-lines-it-lands-on
  (let* ((rt (pine.ts.runtime:make-ts-runtime))
         (lines (banded-lines 40000))
         (n (fset:size lines))
         (ps (progn (pine.ts.runtime:ensure-ts rt)
                    (pine.ts.runtime:make-parse-state rt :commonlisp))))
    (when ps
      (unwind-protect
           (let ((top (cons 0 43))
                 (end (cons (- n 44) (1- n))))
             (pine.ts.runtime:parse-lines! ps lines :viewport top)
             (let ((band-at-top (pine.ts.runtime:ps-band ps))
                   (hl-top (pine.ts.highlight:parse-highlights
                            ps :from-line (car top) :to-line (cdr top))))
               (is (not (null band-at-top))
                   "a buffer this size should be banded, not parsed whole")
               (is (plusp (length hl-top)) "the top window should be highlighted")
               ;; the seq is EQ across a scroll; only the viewport moved
               (pine.ts.runtime:parse-lines! ps lines :viewport end)
               (let ((band-at-end (pine.ts.runtime:ps-band ps))
                     (hl-end (pine.ts.highlight:parse-highlights
                              ps :from-line (car end) :to-line (cdr end))))
                 (is (not (equal band-at-top band-at-end))
                     "the band must follow the window, was ~s still ~s"
                     band-at-top band-at-end)
                 (is (plusp (length hl-end))
                     "the lines scrolled to must be highlighted, got none")
                 (is (every (lambda (tuple) (<= (car end) (first tuple) (cdr end)))
                            hl-end)
                     "every tuple should name a line the window shows"))))
        (pine.ts.runtime:free-parse-state ps)))))

(test a-repeated-viewport-does-no-work
  (let* ((rt (pine.ts.runtime:make-ts-runtime))
         (lines (banded-lines 40000))
         (ps (progn (pine.ts.runtime:ensure-ts rt)
                    (pine.ts.runtime:make-parse-state rt :commonlisp))))
    (when ps
      (unwind-protect
           (let ((vp (cons 0 43)))
             (pine.ts.runtime:parse-lines! ps lines :viewport vp)
             (let ((tree (pine.ts.runtime:ps-tree ps)))
               (pine.ts.runtime:parse-lines! ps lines :viewport vp)
               (is (eq tree (pine.ts.runtime:ps-tree ps))
                   "the same lines at the same viewport should keep the tree")))
        (pine.ts.runtime:free-parse-state ps)))))
