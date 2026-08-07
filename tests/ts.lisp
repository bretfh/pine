(in-package :pine.test)

(def-suite* :pine.ts :in :pine)

;; a macro whose name gives nothing away, so only the image can say what it is
(defmacro with-probe (thing &body body) `(progn ,thing ,@body))

;;;; The grammar is a dependency, not a maybe: the manifest ships
;;;; tree-sitter-commonlisp. A run without it is a broken environment, so
;;;; GRAMMAR-LOADS asserts it and the rest of the file uses it unguarded.

(defvar *runtime* nil)

(defun runtime ()
  (or *runtime* (setf *runtime* (pine.ts.runtime:make-ts-runtime))))

(defun pine.syntax-declared ()
  (pine.ts.syntax:declare-all))

(defun cl-syntax ()
  "The compiled Common Lisp rules, declared the way a running pine declares
them."
  (pine.ts.syntax:declare-all)
  (pine.ts.syntax:for :commonlisp))

(defmacro with-parse-state ((var language) &body body)
  ;; a language is a declaration, so something has to have declared it
  `(let ((,var (progn
                 (pine.ts.syntax:declare-all)
                 (multiple-value-bind (lib fn) (pine.ts.syntax:grammar-of ,language)
                   (pine.ts.runtime:make-parse-state
                    (runtime) ,language lib fn
                    :syntax (pine.ts.syntax:for ,language))))))
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
  (let ((path (merge-pathnames "../src/text.lisp"
                               #.(or *compile-file-truename* *load-truename*)))
        (*num-trials* 2))
    (is (not (null (probe-file path)))
        "src/text.lisp must exist; a moved file fails here")
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

(test a-forms-head-is-its-head-and-not-its-header
  "The grammar gives a defun a header node of its own, and the head sits inside
it. Taking the first named node answered the whole header text, which only ever
worked because the body test was a string prefix: anything starting with def
indents its body whatever the rest of the text says."
  (with-parse-state (ps :commonlisp)
    (pine.ts.runtime:parse-text! ps (format nil "(defun f (x)~%1)"))
    (let* ((src (pine.ts.runtime:ps-byte-index ps))
           ;; the node the indenter actually asks about: the form enclosing the
           ;; body line
           (form (pine.ts.highlight::%enclosing-form
                  (pine.ts.runtime:ts-node-named-descendant-for-byte-range
                   (pine.ts.runtime:ts-tree-root-node (pine.ts.runtime:ps-tree ps))
                   13 13)
                  13 src))
           (name (and form (pine.ts.highlight::%form-head-name form src))))
      (is (string= "defun" name)
          "the head came back as ~s, not the symbol that heads the form" name))))

(test indent-width-comes-from-the-mode
  "A body indents by what the mode says, not by a two written into the indenter."
  (with-parse-state (ps :commonlisp)
    (pine.ts.runtime:parse-text! ps (format nil "(defun f (x)~%1)"))
    (is (= 2 (pine.ts.highlight:parse-indent ps 1)))
    (is (= 4 (pine.ts.highlight:parse-indent ps 1 :width 4))
        "a mode indenting by four still got two")))

(test a-set-is-painted-like-any-other-bracket
  "#{...} parses and used to fall through to the default walk, so it got no
delimiter face and no rainbow depth at all."
  (let ((runs (full-highlights "(f #{:a :b})")))
    (is (member '(0 3 5 :delimiter.1) runs :test #'equal)
        "the opening #{ was not painted as a two-character delimiter: ~s" runs)
    (is (member '(0 5 7 :constant) runs :test #'equal)
        "an element of the set was not painted")))

(test a-macro-indents-by-where-its-own-body-begins
  "The payoff of asking the image: WITH-PROBE distinguishes one argument before
its &BODY, so the first argument aligns and the body indents. Nothing anywhere
was written down about it, and its name says nothing either."
  (pine.ns:with-space ()
    (let ((rule (pine.ts.highlight:head-rule (cl-syntax) "with-probe"
                                             (find-package :pine.test))))
      (is-true rule "the image said nothing about a macro it holds")
      (when rule
        (is (eql 1 (fset:lookup rule :indent))
            "&body sits after one argument, so one argument is distinguished")))
    (with-parse-state (ps :commonlisp)
      (pine.ts.runtime:parse-text!
       ps (format nil "(with-probe a~%b~%c)"))
      (setf (pine.ts.runtime:ps-package ps) (find-package :pine.test))
      ;; a aligns under the first argument; b and c are body
      (is (= 2 (pine.ts.highlight:parse-indent ps 1))
          "the body did not indent, got ~s" (pine.ts.highlight:parse-indent ps 1))
      (is (= 2 (pine.ts.highlight:parse-indent ps 2))))))

(defun %error-nodes (root)
  (let ((n 0))
    (labels ((walk (nd)
               (when (string= "ERROR" (pine.ts.runtime:ts-node-type nd)) (incf n))
               (dotimes (i (pine.ts.runtime:ts-node-named-count nd))
                 (walk (pine.ts.runtime:ts-node-named-nth nd i)))))
      (walk root))
    n))

(defparameter +unparsed+
  '("backlight.lisp" "echo.lisp")
  "Files the grammar still cannot read, and why. Both fail identically under
:commonlisp, so both are the Common Lisp grammar's and not pine's reader.

backlight.lisp uses LOOP's hash-table extension, :being :the :hash-keys :of,
which the grammar does not cover.

echo.lisp holds the string \"~/\". Every string is parsed as a format control,
and ~/ begins the call-a-function directive, which wants a closing slash. It is
a legal string that is not a legal format directive, and nothing in the text
says which of the two it is.")

(test pine-source-parses-under-the-grammar-its-readtable-names
  "The claim this whole subsystem is for: a file written in pine's own reader is
not a broken Common Lisp file, it is a different language, and pine knows which
one because the text says so."
  (pine.ns:with-space ()
    (pine.syntax-declared)
    (let ((rt (pine.ts.runtime:make-ts-runtime))
          (bad nil))
      (pine.ts.runtime:ensure-ts rt)
      (dolist (f (directory (merge-pathnames "src/**/*.lisp"
                                             (asdf:system-source-directory :pine))))
        (unless (member (file-namestring f) +unparsed+ :test #'string=)
          (let* ((text (pine.buf:read-file f))
                 (lang (if (pine.text:readtable-in text) :pine :commonlisp)))
            (multiple-value-bind (lib fn) (pine.ts.syntax:grammar-of lang)
              (let ((ps (pine.ts.runtime:make-parse-state rt lang lib fn)))
                (when ps
                  (unwind-protect
                       (progn
                         (pine.ts.runtime:parse-text! ps text)
                         (let ((n (%error-nodes
                                   (pine.ts.runtime:ts-tree-root-node
                                    (pine.ts.runtime:ps-tree ps)))))
                           (when (plusp n)
                             (push (list (file-namestring f) lang n) bad))))
                    (pine.ts.runtime:free-parse-state ps))))))))
      (is (null bad)
          "~d file~:p did not parse:~{~%  ~{~a under ~a: ~a ERROR nodes~}~}"
          (length bad) bad))))

(test a-pine-file-is-parsed-by-pines-own-grammar
  "The readtable chooses the grammar. Nothing about the file name says which
reader a lisp file wants, and the mode covers a family rather than one reader."
  (pine.ns:with-space ()
    (pine.syntax-declared)
    (pine.ns:up :mode)
    (pine.ns:up :buf)
    (pine.ns:write (pine.buf:at "g" :mode) :lisp)
    (pine.ns:write (pine.buf:at "g" :readtable) 'pine.path:syntax)
    (is (eq :pine (pine.buf::%grammar-of "g"))
        "a buffer declaring pine's reader did not get pine's grammar")
    (pine.ns:write (pine.buf:at "g" :readtable) nil)
    (is (eq :commonlisp (pine.buf::%grammar-of "g"))
        "a buffer declaring no reader did not fall back to its mode")))

(test where-a-file-is-can-say-what-it-is
  "A config is written in pine's reader and its name says nothing, so the mode
claims it by path."
  (pine.ns:with-space ()
    (pine.ns:up :mode)
    (is (eq :pine (pine.mode:for-file "/home/someone/.config/pine/init.lisp")))
    (is (eq :lisp (pine.mode:for-file "/home/someone/src/other/init.lisp"))
        "a lisp file outside a pine directory is ordinary lisp")))

(test body-forms-are-recognized-by-family
 (pine.ns:with-space ()
  (is-true (pine.ts.highlight:body-form-p (cl-syntax) "defun"))
  (is-true (pine.ts.highlight:body-form-p (cl-syntax) "with-open-file"))
  (is-true (pine.ts.highlight:body-form-p (cl-syntax) "do-symbols"))
  (is-true (pine.ts.highlight:body-form-p (cl-syntax) "let"))
  (is-true (pine.ts.highlight:body-form-p (cl-syntax) "loop"))
  (is-false (pine.ts.highlight:body-form-p (cl-syntax) "format"))
  (is-false (pine.ts.highlight:body-form-p (cl-syntax) nil))))

(defun head-face (name)
  (let ((rule (pine.ts.highlight:head-rule (cl-syntax) name)))
    (and rule (fset:lookup rule :face))))

(defun head-shape (name)
  "What a head puts at each position, as (index . role) pairs in order."
  (let ((rule (pine.ts.highlight:head-rule (cl-syntax) name)))
    (and rule (let ((s (fset:lookup rule :shape)) (out nil))
                (when (fset:map? s)
                  (fset:do-map (i role s) (push (cons i role) out)))
                (sort out #'< :key #'car)))))

(test what-a-head-does-is-written-or-worked-out
  "A head with a shape is written down, because only the shape of the language
says it. What a symbol IS -- macro, special operator, builtin -- is not written
anywhere: the image is asked."
  (pine.ns:with-space ()
    (is (equal '((1 . :bindings)) (head-shape "let")))
    (is (equal '((1 . :vars)) (head-shape "multiple-value-bind")))
    (is (equal '((1 . :var)) (head-shape "dolist")))
    (is (equal '((1 . :name) (2 . :types) (3 . :slots)) (head-shape "defclass")))
    ;; asked of the image, written nowhere
    (is (eq :keyword (head-face "if")) "a special operator is not a call")
    (is (eq :builtin (head-face "car")) "a CL function is a builtin")
    (is (eq :keyword (head-face "when")) "a CL macro is not a call")
    (is (null (head-face "frobnicate")) "an unknown symbol is an ordinary call")))

(test a-config-can-say-what-its-own-macro-does
  "The whole point: someone writes a macro and says how it is walked, with a
write, without touching pine."
  (pine.ns:with-space ()
    (pine.ts.syntax:declare-all)
    (pine.ns:write (pine.path:parse "/syntax/commonlisp/head/with-frobnitz")
                   (fset:map (:face :keyword) (:shape (fset:map (1 :bindings)))
                             (:rest :body)))
    (is (eq :keyword (head-face "with-frobnitz")))
    (is (equal '((1 . :bindings)) (head-shape "with-frobnitz")))
    (is-true (pine.ts.highlight:body-form-p (cl-syntax) "with-frobnitz")
             "a form a config says takes a body did not indent as one")))

(test the-image-answers-for-a-macro-nobody-wrote-down
  "sb-introspect says where &body sits in a macro's lambda list, which is
exactly the number of arguments to align before indenting. A guess from the
name cannot know that; the running image does."
  (pine.ns:with-space ()
    (let ((rule (pine.ts.highlight:head-rule (cl-syntax) "with-open-stream")))
      (is-true rule "the image said nothing about a macro it holds")
      (when rule
        (is (eq :keyword (fset:lookup rule :face)))))
    ;; defcmd is pine's own, defined in this image with &body
    (let ((rule (pine.ts.highlight:head-rule (cl-syntax) "defcmd")))
      (is-true rule "the image said nothing about pine's own macro")
      (when rule
        (is (eq :body (fset:lookup rule :rest))
            "a macro with &body did not take a body")))))

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
                    (pine.ts.syntax:declare-all)
                    (multiple-value-bind (lib fn)
                        (pine.ts.syntax:grammar-of :commonlisp)
                      (pine.ts.runtime:make-parse-state
                       rt :commonlisp lib fn
                       :syntax (pine.ts.syntax:for :commonlisp))))))
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
                    (pine.ts.syntax:declare-all)
                    (multiple-value-bind (lib fn)
                        (pine.ts.syntax:grammar-of :commonlisp)
                      (pine.ts.runtime:make-parse-state
                       rt :commonlisp lib fn
                       :syntax (pine.ts.syntax:for :commonlisp))))))
    (when ps
      (unwind-protect
           (let ((vp (cons 0 43)))
             (pine.ts.runtime:parse-lines! ps lines :viewport vp)
             (let ((tree (pine.ts.runtime:ps-tree ps)))
               (pine.ts.runtime:parse-lines! ps lines :viewport vp)
               (is (eq tree (pine.ts.runtime:ps-tree ps))
                   "the same lines at the same viewport should keep the tree")))
        (pine.ts.runtime:free-parse-state ps)))))

(test an-edit-to-a-line-forgets-the-text-that-line-used-to-have
  "The index memoises the last line asked about. An edit carried as a pending
shift changes that line's text in place, and a walk restricted to it -- which is
exactly what the incremental window does -- would otherwise count columns
against the text it used to have. The last span on the line collapses to zero
width and is never emitted."
  (let* ((rt (pine.ts.runtime:make-ts-runtime))
         (before (fset:seq "(g 1)z(defun f (x)  (+ x 1))" "(f 2)"))
         (after (fset:seq "(g 1)z(defun f (x)  (+ x 1))" "z(f 2)"))
         (viewport '(0 . 200))
         (ps (progn (pine.ts.runtime:ensure-ts rt)
                    (pine.ts.runtime:make-parse-state rt :commonlisp)))
         (fresh-ps (pine.ts.runtime:make-parse-state rt :commonlisp)))
    (when (and ps fresh-ps)
      (unwind-protect
           (progn
             (pine.ts.runtime:parse-lines! ps before :viewport viewport)
             (pine.ts.highlight:parse-highlights ps :from-line 0 :to-line 200)
             ;; one character at the head of the second line
             (pine.ts.runtime:parse-lines! ps after :edit (list 1 1 1 1)
                                                    :viewport viewport)
             (pine.ts.runtime:parse-lines! fresh-ps after :viewport viewport)
             (let ((incremental (pine.ts.highlight:parse-highlights
                                 ps :from-line 0 :to-line 200))
                   (whole (pine.ts.highlight:parse-highlights
                           fresh-ps :from-line 0 :to-line 200)))
               (is (null (set-difference whole incremental :test #'equal))
                   "the incremental walk lost ~s"
                   (set-difference whole incremental :test #'equal))
               (is (null (set-difference incremental whole :test #'equal))
                   "the incremental walk invented ~s"
                   (set-difference incremental whole :test #'equal))))
        (pine.ts.runtime:free-parse-state ps)
        (pine.ts.runtime:free-parse-state fresh-ps)))))
