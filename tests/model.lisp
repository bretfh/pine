(in-package :pine.test)

(def-suite :pine.model :in :pine)
(in-suite :pine.model)

;;;; Buffer edits against a list-of-strings reference model.

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
  (let* ((first (nth sl model)) (last (nth el model)))
    (append (subseq model 0 sl)
            (list (concatenate 'string
                               (subseq first 0 (min sc (length first)))
                               (subseq last (min ec (length last)))))
            (subseq model (1+ el)))))

(test buffer-model
  "2000 random edits: buffer state->string agrees with the reference model."
  (setf *seed* 42)
  (let ((state (pine.buffer:load-content (format nil "alpha~%beta~%gamma")))
        (model (list "alpha" "beta" "gamma"))
        (divergence nil))
    (dotimes (i 2000)
      (let* ((nlines (length model))
             (l (rnd nlines))
             (line (nth l model))
             (c (rnd (1+ (length line))))
             (op (rnd 5)))
        (case op
          (0 (setf state (pine.buffer:insert-string state l c "xy")
                   model (model-insert-string model l c "xy")))
          (1 (setf state (pine.buffer:insert-char state l c #\z)
                   model (model-insert-string model l c "z")))
          (2 (setf state (pine.buffer:insert-newline state l c)
                   model (model-insert-newline model l c)))
          (3 (setf state (pine.buffer:delete-char state l c)
                   model (model-delete-char model l c)))
          (4 (let* ((el (min (1- nlines) (+ l (rnd 3))))
                    (eline (nth el model))
                    (ec (if (= el l) (min (1+ c) (length eline)) (rnd (1+ (length eline)))))
                    (sc (if (= el l) (min c ec) c)))
               (setf state (pine.buffer:delete-region state l sc el ec)
                     model (model-delete-region model l sc el ec)))))
        (unless divergence
          (let ((got (pine.buffer:state->string state))
                (want (model-string model)))
            (unless (string= got want)
              (setf divergence (list i op l c got want)))))))
    (is (null divergence) "diverged at ~s" divergence)))

(test load-content-round-trips
  (dolist (text (list "" "one" (format nil "a~%b") (format nil "a~%b~%")
                      (format nil "~%~%") (format nil "trail sp ~%  lead")))
    (is (string= text (pine.buffer:state->string (pine.buffer:load-content text))))))

(test multiline-insert-splits
  "A multi-line insert splits on its newlines; point lands after the text."
  (let* ((state (pine.buffer:move-mark (pine.buffer:load-content "ab") :point 0 1))
         (new (pine.buffer:insert-string state 0 1 (format nil "x~%y~%z"))))
    (is (string= (format nil "ax~%y~%zb") (pine.buffer:state->string new)))
    (let ((snap (pine.buffer:state->snapshot new)))
      (is (= 2 (pine.buffer:point-line snap)))
      (is (= 1 (pine.buffer:point-col snap))))))

(test word-motion
  "Word motion lands exactly; hyphen splits words."
  (let* ((st (pine.buffer:move-mark
              (pine.buffer:load-content (format nil "foo bar-baz  qux~%next line"))
              :point 0 0))
         (snap (pine.buffer:state->snapshot st)))
    (loop for (n wl wc) in '((1 0 3) (2 0 7) (3 0 11) (4 0 16))
          do (multiple-value-bind (l c) (pine.buffer:point-after-move snap :word n)
               (is (and (= l wl) (= c wc))
                   "word fwd ~d: got (~d ~d) want (~d ~d)" n l c wl wc)))
    (let ((end (pine.buffer:state->snapshot (pine.buffer:move-mark st :point 1 9))))
      (multiple-value-bind (l c) (pine.buffer:point-after-move end :word -2)
        (is (and (= l 1) (= c 0)) "word back -2: got (~d ~d)" l c)))))

;;;; VT golden outputs.

(defparameter *vt-script*
  (format nil "plain text line~%~c[31mred~c[0m normal ~c[1;4;32mbold-ul-green~c[0m~%~c]0;my-title~c~c[2J~c[Htop-left after clear~%~c[31;44mcolored~c[m done"
          #\escape #\escape #\escape #\escape
          #\escape #\bel
          #\escape #\escape
          #\escape #\escape))

(defun feed-lines (term n)
  (dotimes (i n)
    (pine.vt:term-process-output term (format nil "line-~d~%" i))))

(defun vt-visible (term)
  (string-trim '(#\newline)
               (with-output-to-string (o)
                 (with-input-from-string (i (pine.vt:term-dump-to-string term))
                   (loop for line = (read-line i nil) while line
                         do (write-line (string-right-trim " " line) o))))))

(test vt-sgr-osc-clear
  (let ((term (pine.vt:make-term :width 40 :height 6)))
    (pine.vt:term-process-output term *vt-script*)
    (is (string= "my-title" (pine.vt:term-title term)))
    (is (string= (format nil "top-left after clear~%colored done") (vt-visible term)))
    (let* ((row (pine.vt:term-grid-row term 1))
           (face (pine.vt:cell-face (aref row 0))))
      (is (and face (eql 1 (pine.vt:face-fg face)) (eql 4 (pine.vt:face-bg face)))))))

(test vt-scrollback
  "Scroll pushes exact rows to scrollback; the visible window is correct."
  (let ((term (pine.vt:make-term :width 20 :height 4 :max-scrollback 8)))
    (feed-lines term 10)
    (is (string= (format nil "line-7~%line-8~%line-9") (vt-visible term)))
    (let ((n (pine.vt::term-scrollback-size term)))
      (is (= 7 n))
      (dotimes (i (min n 7))
        (let* ((head (pine.vt::term-scrollback-head term))
               (row (aref (pine.vt::term-scrollback term)
                          (mod (+ head i) (length (pine.vt::term-scrollback term)))))
               (text (string-trim " " (map 'string #'pine.vt:cell-char row))))
          (is (string= (format nil "line-~d" i) text)))))))

(test vt-chunked-osc
  (let ((term (pine.vt:make-term :width 20 :height 4)))
    (pine.vt:term-process-output term (format nil "~c]0;half" #\escape))
    (pine.vt:term-process-output term (format nil "-and-half~c" #\bel))
    (is (string= "half-and-half" (pine.vt:term-title term)))))

(test vt-render-line
  "render-line reports face runs at the right columns."
  (let ((term (pine.vt:make-term :width 20 :height 2)))
    (pine.vt:term-process-output term (format nil "ab~c[7mcd~c[0mef" #\escape #\escape))
    (multiple-value-bind (chars changes) (pine.vt:term-render-line term 0)
      (is (string= "abcdef" (subseq chars 0 6)))
      (is (= 4 (length changes)))
      (is (equal 2 (first (second changes))))
      (is (equal '(:inverse t) (second (second changes)))))))

;;;; Incremental highlights == full walk.

(defun random-lisp-text (nforms)
  (with-output-to-string (o)
    (dotimes (i nforms)
      (format o "(defun fn~d (a b)~%  \"doc ~d\"~%  (let ((x (+ a ~d)))~%    (* x b)))~%~%"
              i i i))))

(defun mutate-line (text)
  (let* ((len (length text))
         (pos (rnd (max 1 len))))
    (case (rnd 5)
      (0 (concatenate 'string (subseq text 0 pos) "q" (subseq text pos)))
      (1 (if (and (plusp len) (char/= (char text pos) #\newline))
             (concatenate 'string (subseq text 0 pos) (subseq text (min len (1+ pos))))
             text))
      (2 (if (and (plusp len) (char/= (char text pos) #\newline))
             (concatenate 'string (subseq text 0 pos) "w" (subseq text (min len (1+ pos))))
             text))
      (3 (concatenate 'string (subseq text 0 pos) (string #\newline)
                      (subseq text pos)))
      (4 (let ((nl (position #\newline text :start pos)))
           (if nl
               (concatenate 'string (subseq text 0 nl) (subseq text (1+ nl)))
               text))))))

(defun sort-highlights (hl)
  (sort (copy-list hl)
        (lambda (a b)
          (or (< (first a) (first b))
              (and (= (first a) (first b))
                   (or (< (second a) (second b))
                       (and (= (second a) (second b))
                            (string< (princ-to-string a) (princ-to-string b)))))))))

(defun highlights-equivalent-p (rt text edits)
  "Apply EDITS random mutations; incremental highlights must equal the full
walk after every one. Returns nil, or the divergence description."
  (let ((inc (pine.ts:make-parse-state rt :commonlisp)))
    (unless inc (return-from highlights-equivalent-p :no-grammar))
    (pine.ts:parse-full! inc text)
    (pine.ts:parse-highlights inc text)
    (unwind-protect
         (dotimes (i edits nil)
           (setf text (mutate-line text))
           (pine.ts:reparse! inc text)
           (let* ((got (sort-highlights (pine.ts:parse-highlights inc text)))
                  (fresh (pine.ts:make-parse-state rt :commonlisp)))
             (pine.ts:parse-full! fresh text)
             (let ((want (sort-highlights (pine.ts:parse-highlights fresh text))))
               (pine.ts:free-parse-state fresh)
               (unless (equal got want)
                 (return (list i
                               (set-difference got want :test #'equal)
                               (set-difference want got :test #'equal)))))))
      (pine.ts:free-parse-state inc))))

(test highlights-incremental
  "Incremental highlighting equals the full walk over random edits, on
generated source and on a real file."
  (setf *seed* 42)
  (let ((rt (pine.ts:make-ts-runtime)))
    (let ((d (highlights-equivalent-p rt (random-lisp-text 12) 200)))
      (case d
        (:no-grammar (skip "no commonlisp grammar"))
        (t (is (null d) "generated-source divergence: ~s" d))))
    (let ((path (probe-file (merge-pathnames "../src/buffer/buffer.lisp"
                                             #.(or *compile-file-truename*
                                                   *load-truename*)))))
      (when path
        (let ((real (with-open-file (s path)
                      (let ((str (make-string (file-length s))))
                        (subseq str 0 (read-sequence str s))))))
          (let ((d (highlights-equivalent-p rt real 150)))
            (unless (eq d :no-grammar)
              (is (null d) "real-file divergence: ~s" d))))))))

;;;; Tree-driven indentation.

(test indent-columns
  (let* ((rt (pine.ts:make-ts-runtime))
         (ps (pine.ts:make-parse-state rt :commonlisp)))
    (if (null ps)
        (skip "no commonlisp grammar")
        (unwind-protect
             (progn
               (pine.ts:parse-full!
                ps (format nil "(defun f (x)~%(let ((a 1)~%(b 2))~%(+ a~%b)))~%(foo bar~%baz)"))
               (is (equal '(0 2 6 2 3 0 5)
                          (loop for i from 0 to 6 collect (pine.ts:parse-indent ps i)))))
          (pine.ts:free-parse-state ps)))))

(test indent-string-interior
  (let* ((rt (pine.ts:make-ts-runtime))
         (ps (pine.ts:make-parse-state rt :commonlisp)))
    (if (null ps)
        (skip "no commonlisp grammar")
        (unwind-protect
             (progn
               (pine.ts:parse-full!
                ps (concatenate 'string "(defvar *x*" (string #\newline)
                                (string #\") "a" (string #\newline) "b"
                                (string #\") (string #\newline) "1)"))
               (is (equal '(0 2 nil 2)
                          (loop for i from 0 to 3 collect (pine.ts:parse-indent ps i)))))
          (pine.ts:free-parse-state ps)))))

(test indent-user-def-macro
  (let* ((rt (pine.ts:make-ts-runtime))
         (ps (pine.ts:make-parse-state rt :commonlisp)))
    (if (null ps)
        (skip "no commonlisp grammar")
        (unwind-protect
             (progn
               (pine.ts:parse-full! ps (format nil "(defcommand foo ()~%(bar))"))
               (is (equal '(0 2)
                          (list (pine.ts:parse-indent ps 0) (pine.ts:parse-indent ps 1)))))
          (pine.ts:free-parse-state ps)))))

(test reindent-line
  (let ((st (pine.buffer:load-content (format nil "(foo~%  bar)"))))
    (multiple-value-bind (new col) (pine.buffer:reindent-line st 1 2 5 4)
      (is (= 7 col))
      (is (string= (format nil "(foo~%     bar)") (pine.buffer:state->string new))))))

;;;; Refs: one accessor, read and setf; reactive views track their reads.

(test refs-setf
  (is (eql 41 (setf (pine.ref:ref :probe-ref) 41)))
  (is (eql 41 (pine.ref:ref :probe-ref)))
  (is (eq :dflt (pine.ref:ref :probe-missing :dflt))))

(test refs-reactive-view
  "A view re-renders when a setf'd ref it read changes; an equal write is
deduped and renders nothing."
  (let ((hits 0) (view nil))
    (setf (pine.ref:ref :probe-view) 1)
    (setf view (pine.ref:make-view
                (lambda () (pine.ref:ref :probe-view))
                (lambda () (incf hits) (pine.ref:render-view view))))
    (is (eql 1 (pine.ref:render-view view)))
    (setf (pine.ref:ref :probe-view) 2)
    (is (= 1 hits))
    (setf (pine.ref:ref :probe-view) 2)
    (is (= 1 hits))
    (pine.ref:dispose-view view)))

(test style-opacity
  "An :opacity rule resolves to a float the pixel painter reads."
  (pine.buffer:install-rules '((".op-probe" (:opacity "0.42"))))
  (is (= 0.42 (pine.style:st-opacity (pine.style:resolve '(("op-probe")))))))

(test style-lisp-values
  "Rules take lisp values: numbers for opacity and px props; keyword
selectors name one class."
  (pine.buffer:install-rules
   '((:num-probe (:opacity 0.42 :min-width 28 :border-radius 6 :padding 4))))
  (let ((st (pine.style:resolve '(("num-probe")))))
    (is (= 0.42 (pine.style:st-opacity st)))
    (is (= 28 (pine.style:st-min-w st)))
    (is (= 6 (pine.style:st-radius st)))
    (is (= 4 (pine.style:st-pad-x st)))))

(test class-names
  "Node classes normalize from keywords, lists, and strings alike."
  (is (equal '("a") (pine.layout:class-names :a)))
  (is (equal '("nm-row" "sel") (pine.layout:class-names '(:nm-row :sel))))
  (is (equal '("nm-row" "sel") (pine.layout:class-names "nm-row sel")))
  (is (null (pine.layout:class-names nil))))
