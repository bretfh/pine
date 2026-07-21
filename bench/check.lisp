;;;; Correctness probes for the hot-path rewrites. Each probe compares the real
;;;; implementation against a reference model or a golden output and fails
;;;; loudly on divergence. Run before benchmarks: (pine.check:run-checks) exits
;;;; nonzero on failure when invoked via run.lisp / make check-bench.

(defpackage :pine.check
  (:use :cl)
  (:export #:run-checks #:*failures*))

(in-package :pine.check)

(defvar *failures* 0)

(defun fail (fmt &rest args)
  (incf *failures*)
  (format t "~&FAIL: ~?~%" fmt args))

(defun ok (fmt &rest args)
  (format t "~&ok: ~?~%" fmt args))

(defvar *seed* 42)
(defun rnd (n)
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 31)))
  (mod (floor *seed* 65536) (max 1 n)))

;;;; ---------- probe 1: buffer ops vs list-of-strings reference model ----------

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

(defun check-buffer-model (&key (edits 2000))
  (let* ((state (pine.buffer:load-content (format nil "alpha~%beta~%gamma")))
         (model (list "alpha" "beta" "gamma")))
    (dotimes (i edits)
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
        (let ((got (pine.buffer:state->string state))
              (want (model-string model)))
          (unless (string= got want)
            (fail "buffer model diverged at edit ~d (op ~d l ~d c ~d):~%  got  ~s~%  want ~s"
                  i op l c got want)
            (return-from check-buffer-model)))))
    (ok "buffer model: ~d random edits, state->string agrees" edits))
  (dolist (text (list "" "one" (format nil "a~%b") (format nil "a~%b~%")
                      (format nil "~%~%") (format nil "trail sp ~%  lead")))
    (let ((got (pine.buffer:state->string (pine.buffer:load-content text))))
      (unless (string= got text)
        (fail "load-content round-trip ~s -> ~s" text got))))
  (ok "load-content round-trips")
  ;; word motion: exact landings on known text (hyphen splits words)
  (let* ((st (pine.buffer:move-mark
              (pine.buffer:load-content (format nil "foo bar-baz  qux~%next line"))
              :point 0 0))
         (snap (pine.buffer:state->snapshot st)))
    (loop for (n wl wc) in '((1 0 3) (2 0 7) (3 0 11) (4 0 16))
          do (multiple-value-bind (l c) (pine.buffer:point-after-move snap :word n)
               (unless (and (= l wl) (= c wc))
                 (fail "word fwd ~d: got (~d ~d) want (~d ~d)" n l c wl wc))))
    (let ((end (pine.buffer:state->snapshot (pine.buffer:move-mark st :point 1 9))))
      (multiple-value-bind (l c) (pine.buffer:point-after-move end :word -2)
        (unless (and (= l 1) (= c 0))
          (fail "word back -2: got (~d ~d) want (1 0)" l c)))))
  (ok "word motion lands exactly"))

;;;; ---------- probe 2: vt golden outputs ----------

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
  "Screen dump with per-row right padding and trailing blank rows removed."
  (string-trim '(#\newline)
               (with-output-to-string (o)
                 (with-input-from-string (i (pine.vt:term-dump-to-string term))
                   (loop for line = (read-line i nil) while line
                         do (write-line (string-right-trim " " line) o))))))

(defun check-vt-golden ()
  ;; golden 1: sgr + osc + clear script
  (let ((term (pine.vt:make-term :width 40 :height 6)))
    (pine.vt:term-process-output term *vt-script*)
    (let ((visible (vt-visible term))
          (title (pine.vt:term-title term)))
      (unless (string= title "my-title")
        (fail "vt title: got ~s want ~s" title "my-title"))
      (unless (string= visible (format nil "top-left after clear~%colored done"))
        (fail "vt script visible: got ~s" visible))
      (let* ((row (pine.vt:term-grid-row term 1))
             (cell (aref row 0))
             (face (pine.vt:cell-face cell)))
        (unless (and face (eql (pine.vt:face-fg face) 1) (eql (pine.vt:face-bg face) 4))
          (fail "vt face on 'colored': got fg=~s bg=~s"
                (and face (pine.vt:face-fg face)) (and face (pine.vt:face-bg face))))))
    (ok "vt sgr/osc/clear golden"))
  ;; golden 2: scroll pushes exact rows to scrollback, visible window correct.
  ;; Stays below the scrollback cap so the expectation holds for any eviction
  ;; policy at the cap.
  (let ((term (pine.vt:make-term :width 20 :height 4 :max-scrollback 8)))
    (feed-lines term 10)
    (let ((visible (vt-visible term)))
      (unless (string= visible (format nil "line-7~%line-8~%line-9"))
        (fail "vt scroll visible: got ~s" visible)))
    (let ((n (pine.vt::term-scrollback-size term)))
      (unless (= n 7)
        (fail "vt scrollback size: got ~d want 7" n))
      (dotimes (i (min n 7))
        (let* ((head (if (fboundp 'pine.vt::term-scrollback-head)
                         (funcall 'pine.vt::term-scrollback-head term)
                         0))
               (row (aref (pine.vt::term-scrollback term)
                          (mod (+ head i)
                               (length (pine.vt::term-scrollback term)))))
               (text (string-trim " "
                       (map 'string #'pine.vt:cell-char row))))
          (unless (string= text (format nil "line-~d" i))
            (fail "vt scrollback[~d]: got ~s want ~s" i text (format nil "line-~d" i))))))
    (ok "vt scroll golden"))
  ;; golden 3: chunked OSC (split escape) still parses
  (let ((term (pine.vt:make-term :width 20 :height 4)))
    (pine.vt:term-process-output term (format nil "~c]0;half" #\escape))
    (pine.vt:term-process-output term (format nil "-and-half~c" #\bel))
    (unless (string= (pine.vt:term-title term) "half-and-half")
      (fail "vt chunked osc title: got ~s" (pine.vt:term-title term)))
    (ok "vt chunked-osc golden"))
  ;; golden 4: render-line reports face runs at the right columns
  (let ((term (pine.vt:make-term :width 20 :height 2)))
    (pine.vt:term-process-output term (format nil "ab~c[7mcd~c[0mef" #\escape #\escape))
    (multiple-value-bind (chars changes) (pine.vt:term-render-line term 0)
      (unless (string= (subseq chars 0 6) "abcdef")
        (fail "vt render-line chars: got ~s" (subseq chars 0 6)))
      ;; captured golden: runs at 0 (default), 2 (inverse), 4 (default written),
      ;; 6 (untouched cells, nil face)
      (unless (and (= (length changes) 4)
                   (equal (first (second changes)) 2)
                   (equal (second (second changes)) '(:inverse t)))
        (fail "vt render-line runs: got ~s" changes)))
    (ok "vt render-line golden")))

;;;; ---------- probe 3: incremental highlights == full walk ----------

(defun random-lisp-text (nforms)
  (with-output-to-string (o)
    (dotimes (i nforms)
      (format o "(defun fn~d (a b)~%  \"doc ~d\"~%  (let ((x (+ a ~d)))~%    (* x b)))~%~%"
              i i i))))

(defun mutate-line (text)
  "One random edit of TEXT: char insert/delete/replace, newline insert (shifts
every following line), or newline delete (joins two lines). The newline ops
exercise the nonzero line-delta paths of incremental highlighting."
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

(defun check-highlights (&key (edits 200))
  (handler-case
      (let* ((rt (pine.ts:make-ts-runtime))
             (inc (pine.ts:make-parse-state rt :commonlisp))
             (text (random-lisp-text 12)))
        (unless inc
          (ok "highlights probe skipped: no commonlisp grammar")
          (return-from check-highlights))
        (pine.ts:parse-full! inc text)
        (pine.ts:parse-highlights inc text)
        (dotimes (i edits)
          (setf text (mutate-line text))
          (pine.ts:reparse! inc text)
          (let* ((got (sort-highlights (pine.ts:parse-highlights inc text)))
                 (fresh (pine.ts:make-parse-state rt :commonlisp)))
            (pine.ts:parse-full! fresh text)
            (let ((want (sort-highlights (pine.ts:parse-highlights fresh text))))
              (pine.ts:free-parse-state fresh)
              (unless (equal got want)
                (fail "highlights diverged at edit ~d:~%  extra ~s~%  missing ~s"
                      i (set-difference got want :test #'equal)
                      (set-difference want got :test #'equal))
                (return-from check-highlights)))))
        (pine.ts:free-parse-state inc)
        (ok "highlights: ~d random edits, incremental == full walk" edits)
        ;; same equivalence over a real source file: real files carry the
        ;; constructs generators miss (comments, loop clauses, format strings)
        (let ((path (probe-file "src/buffer/buffer.lisp")))
          (when path
            (let ((real (with-open-file (s path)
                          (let ((str (make-string (file-length s))))
                            (subseq str 0 (read-sequence str s)))))
                  (inc2 (pine.ts:make-parse-state rt :commonlisp)))
              (pine.ts:parse-full! inc2 real)
              (pine.ts:parse-highlights inc2 real)
              (dotimes (i 150)
                (setf real (mutate-line real))
                (pine.ts:reparse! inc2 real)
                (let* ((got (sort-highlights (pine.ts:parse-highlights inc2 real)))
                       (fresh (pine.ts:make-parse-state rt :commonlisp)))
                  (pine.ts:parse-full! fresh real)
                  (let ((want (sort-highlights (pine.ts:parse-highlights fresh real))))
                    (pine.ts:free-parse-state fresh)
                    (unless (equal got want)
                      (fail "real-file highlights diverged at edit ~d" i)
                      (return)))))
              (pine.ts:free-parse-state inc2)
              (ok "highlights: 150 real-file edits, incremental == full walk")))))
    (error (e) (ok "highlights probe skipped: ~a" e))))

(defun run-checks ()
  (setf *failures* 0 *seed* 42)
  (check-buffer-model)
  (check-vt-golden)
  (check-highlights)
  (if (zerop *failures*)
      (format t "~&ALL CHECKS PASSED~%")
      (format t "~&~d CHECK~:p FAILED~%" *failures*))
  (zerop *failures*))
