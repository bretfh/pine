(in-package :pine.editor)

;;;; The completion mechanism, UI-agnostic. Three pieces, mirroring the real
;;;; primitive the editor packages are built on, not the packages themselves:
;;;;
;;;;   candidate   a completion value plus its metadata (display, annotation,
;;;;               preview, action, category); the matched spans are filled in.
;;;;   table       a source of candidates: a list, or a function of the current
;;;;               input returning (values candidates metadata). This is the
;;;;               extension seam -- any source is a table.
;;;;   style       how input is matched against candidates: (input candidates
;;;;               matchers separator) -> the matching candidates with spans and
;;;;               a score. ORDERLESS is the default policy; the set of styles is
;;;;               a variable users reorder or extend.
;;;;
;;;; COMPLETE ties them: query the table, run the styles, rank. The live UI and
;;;; the readers consume COMPLETE's output; they are policy on top of this API.

(defstruct (candidate (:constructor %make-candidate) (:copier nil))
  (string "" :type string)   ; the completion text
  (annotation nil)           ; right-column metadata string, or nil
  (preview nil)              ; thunk run transiently when selection lands here
  (action nil)               ; thunk run on accept, or nil
  (category nil)             ; keyword: :command :buffer :file ...
  (source nil)               ; owning source name, for narrowing
  (spans nil)                ; list of (start . end) matched char ranges
  (score 0))                 ; lower is a tighter match

(defun candidate (string &key annotation preview action category source)
  "Construct a candidate. Bare strings passed to the engine are upgraded via
TO-CANDIDATE; use this when a table wants to carry metadata."
  (%make-candidate :string string :annotation annotation :preview preview
                   :action action :category category :source source))

(defun to-candidate (x)
  (etypecase x
    (candidate x)
    (string (%make-candidate :string x))))

;;;; Tables

(defun table-query (table input)
  "Return (values candidate-list metadata) from TABLE for INPUT. A list or vector
is a static table; a function is a dynamic one and may return a metadata plist as
its second value. Candidates are freshly consed per query so match spans do not
leak between inputs."
  (flet ((fresh (c) (let ((cc (to-candidate c)))
                      ;; copy so per-query spans/score never mutate a shared object
                      (%make-candidate :string (candidate-string cc)
                                       :annotation (candidate-annotation cc)
                                       :preview (candidate-preview cc)
                                       :action (candidate-action cc)
                                       :category (candidate-category cc)
                                       :source (candidate-source cc)))))
    (etypecase table
      (function (multiple-value-bind (cands meta) (funcall table input)
                  (values (mapcar #'fresh cands) meta)))
      (list   (values (mapcar #'fresh table) nil))
      (vector (values (map 'list #'fresh table) nil)))))

;;;; Component matchers (how one input token matches one candidate)

(defun literal-matcher (token string)
  "Case-insensitive substring. Returns (start . end) of the first hit, or nil."
  (when (plusp (length token))
    (let ((pos (search token string :test #'char-equal)))
      (when pos (cons pos (+ pos (length token)))))))

(defun prefix-matcher (token string)
  (when (and (<= (length token) (length string))
             (string-equal token string :end2 (length token)))
    (cons 0 (length token))))

;;;; The orderless style: split input on the separator, every component must
;;;; match somewhere, in any order.

(defparameter +separator+ " ")

(defun %split (input separator)
  (remove "" (uiop:split-string input :separator separator) :test #'string=))

(defun orderless-match (input cand matchers separator)
  "Return (values matched-p spans score). Each component is matched by the first
matcher that hits; the candidate matches iff every component hits. Empty input
matches everything (no spans)."
  (let ((s (candidate-string cand)) (spans nil) (score 0))
    (dolist (token (%split input separator) (values t (nreverse spans) score))
      (let ((hit (loop for m in matchers thereis (funcall m token s))))
        (if hit
            (progn (push hit spans) (incf score (car hit)))
            (return (values nil nil 0)))))))

(defun orderless-style (input candidates matchers separator)
  (let (out)
    (dolist (c candidates)
      (multiple-value-bind (ok spans score) (orderless-match input c matchers separator)
        (when ok
          (setf (candidate-spans c) spans (candidate-score c) score)
          (push c out))))
    (nreverse out)))

(defun rank-candidates (candidates)
  "Tighter (lower score) first, then shorter, then lexicographic -- a stable
order the UI can page through."
  (stable-sort (copy-list candidates)
               (lambda (a b)
                 (let ((sa (candidate-score a)) (sb (candidate-score b)))
                   (cond ((/= sa sb) (< sa sb))
                         ((/= (length (candidate-string a)) (length (candidate-string b)))
                          (< (length (candidate-string a)) (length (candidate-string b))))
                         (t (string-lessp (candidate-string a) (candidate-string b))))))))

;;;; The engine entry. STYLES is a list of style functions tried in order (the
;;;; first that yields matches wins, as in the real completion-styles list);
;;;; MATCHERS and SEPARATOR configure the orderless components. Callers pass the
;;;; values that live in the pine.var configuration.

(defun complete (input table &key (styles (list #'orderless-style))
                                  (matchers (list #'literal-matcher))
                                  (separator +separator+))
  (multiple-value-bind (cands meta) (table-query table input)
    (declare (ignore meta))
    (loop for style in styles
          for matched = (funcall style input cands matchers separator)
          when matched return (rank-candidates matched)
          finally (return nil))))
