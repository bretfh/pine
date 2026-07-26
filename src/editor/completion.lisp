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
  (value nil)                ; the object acted on; defaults to the string
  (spans nil)                ; list of (start . end) matched char ranges
  (score 0))                 ; lower is a tighter match

(defun candidate (string &key annotation preview action category source
                              (value string))
  "Construct a candidate. Bare strings passed to the engine are upgraded via
TO-CANDIDATE; use this when a table wants to carry metadata."
  (%make-candidate :string string :annotation annotation :preview preview
                   :action action :category category :source source
                   :value value))

(defun to-candidate (x)
  (etypecase x
    (candidate x)
    (string (%make-candidate :string x :value x))))

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
                                       :source (candidate-source cc)
                                       :value (candidate-value cc)))))
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

;;;; Sources and actions. A source is a named table (any table the engine
;;;; accepts); actions are per-category alists of (label . function-of-value),
;;;; run on the selected candidate's value. One registry pair for every
;;;; completion surface -- minibuffer popup and desktop widget alike.

(defvar *sources* (make-hash-table :test 'eq))
(defun register-source (name table) (setf (gethash name *sources*) table))
(defun source-table (name) (gethash name *sources*))

(defvar *actions* (make-hash-table :test 'eq))
(defun register-actions (category alist) (setf (gethash category *actions*) alist))
(defun candidate-actions (cand)
  (and cand (gethash (candidate-category cand) *actions*)))

;;;; The candidate list as a node tree. Two builders over the one candidate
;;;; struct: COMPLETION-POPUP renders to cells (blitted into the chrome above
;;;; the echo row), COMPLETION-WIDGET is the desktop's pixel rendering of the
;;;; same data. Selection is flagged at render (pine.ui.node:render :selection),
;;;; styled by the .cand-row.sel rules.

(defun completion-popup (visible)
  "The VISIBLE candidates as selectable rows: string left, annotation right."
  (apply #'pine.ui.node:column :align :stretch
         (if visible
             (mapcar (lambda (cand)
                       (pine.ui.node:choice
                        :class "cand-row" :prefix-selected "" :prefix-unselected ""
                        (pine.ui.node:row :spacing 1
                          (pine.ui.node:label (candidate-string cand) :class "cand")
                          (pine.ui.node:gap)
                          (let ((a (candidate-annotation cand)))
                            (when a (pine.ui.node:label a :class "cand-annot"))))))
                     visible)
             (list (pine.ui.node:label "(no matches)" :class "cand")))))

(defun %category-glyph (category)
  (case category
    (:command #x0F0493) (:buffer #x0F0219) (:file #x0F0210)
    (:window #x0F02D1) (:system #x0F0709) (t #x0F0766)))

(defun completion-widget (candidates &key (query "") (selected 0) (title ""))
  "The candidates as a desktop widget tree (the launcher look): a query header,
one row per candidate with a category glyph + annotation, and the selected
candidate's actions in the footer."
  (let* ((sel (and candidates
                   (nth (max 0 (min selected (1- (length candidates)))) candidates)))
         (acts (candidate-actions sel)))
    (pine.ui.node:column :class "netmenu" :align :stretch
      (pine.ui.node:row :class "nm-card nm-head" :align :center :spacing 10
        (pine.ui.node:icon #x0F0349 :class "nm-head-ico")
        (pine.ui.node:label query :class "nm-title")
        (pine.ui.node:label "█" :class "nm-title")
        (pine.ui.node:label title :class "nm-subhead"))
      (apply #'pine.ui.node:column :class "nm-card nm-list-card" :align :stretch :spacing 3
        (loop for c in candidates for i from 0 collect
          (pine.ui.node:row :class (if (= i selected) "nm-row sel" "nm-row")
                           :align :center :spacing 12
            (pine.ui.node:icon (%category-glyph (candidate-category c)) :class "nm-sig")
            (pine.ui.node:label (candidate-string c) :expand 1
                               :class (if (= i selected) "nm-name active" "nm-name"))
            (pine.ui.node:label (or (candidate-annotation c) "") :class "nm-lock"))))
      (apply #'pine.ui.node:row :class "nm-actions" :align :center :spacing 16
        (pine.ui.node:label "RET run" :class "nm-name active")
        (append
         (loop for (label . nil) in acts collect
           (pine.ui.node:label label :class "nm-subhead"))
         (list (pine.ui.node:gap :expand 1)
               (pine.ui.node:label "actions" :class "nm-sig hi")))))))
