(defpackage #:pine.path
  (:use #:cl)
  (:shadow #:parse)
  (:export #:path #:pathp #:segments #:segment-count #:root #:rootp
           #:parent #:leaf #:child #:under #:prefixp #:subpath
           #:text #:parse #:name #:spliced
           #:patternp #:binders #:match #:literal-at #:key #:keys
           #:syntax #:data))

(in-package #:pine.path)

;;;; A path is a place in the namespace: a vector of segments, immutable,
;;;; self evaluating, printable and readable back. A literal segment is a
;;;; string; anything that names more than one place, or binds, is a SEG.
;;;;
;;;; The reader owns everything between the leading / and the next terminator,
;;;; so the syntax inside a path is its own and never meets the syntax around
;;;; it: {a b} is a constraint here and a map literal everywhere else.

(defstruct (seg (:constructor %seg (kind value &optional constraint))
                (:copier nil))
  (kind :any :type keyword)   ; :literal :any :deep :one-of :bind :bind-rest
  value                       ; string, set of names, or a binder symbol
  constraint                  ; nil, or a map of child name to pattern
  (tests nil))                ; constraint key -> compiled test, filled on use

(defstruct (path (:constructor %make-path (segments))
                 (:predicate pathp)
                 (:copier nil))
  (segments #() :type simple-vector)
  ;; the tree's key for each segment, computed once: a path is immutable and
  ;; usually a constant, so a lookup never pays to make one
  (cached-keys nil))

;;;; Dumped through their constructors rather than slot by slot: a compiled
;;;; test is built on use and belongs to the image that built it, so a segment
;;;; loaded from a fasl starts without one.

(defmethod make-load-form ((s seg) &optional environment)
  (declare (ignore environment))
  `(%seg ,(seg-kind s) ',(seg-value s) ',(seg-constraint s)))

(defmethod make-load-form ((p path) &optional environment)
  (declare (ignore environment))
  `(%make-path ',(path-segments p)))

(defvar *root* (%make-path #()))
(defun root () "The path naming the whole namespace." *root*)

(defun segment-count (p) (length (path-segments p)))
(defun rootp (p) (zerop (segment-count p)))
(defun segments (p) (coerce (path-segments p) 'list))

;;;; Building

(defun name (x)
  "X as one segment string. An integer is written in decimal, so an
interpolated index and a written one name the same place."
  (etypecase x
    (string x)
    (symbol (if (keywordp x) (string-downcase (symbol-name x)) (string x)))
    (integer (format nil "~d" x))
    (character (string x))))

(defun key (segment)
  "The name a segment is stored under: a keyword where the name survives the
trip back, and the string itself where it would not."
  (let ((upper (string-upcase segment)))
    (if (string= segment (string-downcase upper))
        (intern upper :keyword)
        segment)))

(defun keys (p)
  "P's segments as the names they are stored under."
  (or (path-cached-keys p)
      (setf (path-cached-keys p)
            (map 'list (lambda (s) (if (stringp s) (key s) s))
                 (path-segments p)))))

(defun spliced (x)
  "X as a list of segments: a path's own, a string split on /, or a list."
  (etypecase x
    (path (coerce (path-segments x) 'list))
    (string (remove "" (uiop:split-string x :separator "/") :test #'string=))
    (list (mapcar #'name x))))

(defun path (&rest pieces)
  "A path from PIECES. A path or a list splices its segments; anything else is
one segment."
  (%make-path
   (coerce (loop :for piece :in pieces
                 :append (cond ((pathp piece) (spliced piece))
                               ((seg-p piece) (list piece))
                               ((null piece) nil)
                               ((listp piece) (spliced piece))
                               (t (list (name piece)))))
           'simple-vector)))

(defun parse (string)
  "The path STRING names."
  (%make-path (coerce (spliced string) 'simple-vector)))

(defun %seg-text (s)
  (with-output-to-string (out)
    (ecase (seg-kind s)
      (:literal (write-string (seg-value s) out))
      (:any (write-char #\* out))
      (:deep (write-string "**" out))
      (:one-of (write-string "#{" out)
               (let ((first t))
                 (fset:do-set (m (seg-value s))
                   (if first (setf first nil) (write-char #\Space out))
                   (write-string m out)))
               (write-char #\} out))
      ((:bind :bind-rest) (format out "~(~a~)" (symbol-name (seg-value s)))))
    (when (seg-constraint s)
      (write-string (pine.data:serialize (seg-constraint s)) out))))

(defun text (p)
  "P as the string it reads from."
  (if (rootp p)
      "/"
      (with-output-to-string (out)
        (loop :for s :across (path-segments p)
              :do (write-char #\/ out)
                  (write-string (if (stringp s) s (%seg-text s)) out)))))

(defmethod print-object ((p path) stream)
  (write-string (text p) stream))

(defmethod fset:compare ((a path) (b path))
  (let ((x (path-segments a)) (y (path-segments b)))
    (loop :for i :from 0 :below (min (length x) (length y))
          :for xi = (aref x i)
          :for yi = (aref y i)
          :do (unless (and (stringp xi) (stringp yi))
                (return-from fset:compare (if (equalp xi yi) :equal :unequal)))
              (cond ((string< xi yi) (return-from fset:compare :less))
                    ((string> xi yi) (return-from fset:compare :greater))))
    (cond ((< (length x) (length y)) :less)
          ((> (length x) (length y)) :greater)
          (t :equal))))

(defun parent (p)
  "The path P sits in, or the root."
  (if (rootp p)
      (root)
      (%make-path (subseq (path-segments p) 0 (1- (segment-count p))))))

(defun leaf (p)
  "P's last segment, as a string."
  (unless (rootp p)
    (let ((s (aref (path-segments p) (1- (segment-count p)))))
      (if (stringp s) s (%seg-text s)))))

(defun child (p &rest pieces)
  "P extended by PIECES."
  (apply #'path p pieces))

(defun subpath (p start &optional end)
  (%make-path (subseq (path-segments p) start end)))

(defun prefixp (prefix p)
  "True when P is PREFIX or sits under it."
  (let ((a (path-segments prefix)) (b (path-segments p)))
    (and (<= (length a) (length b))
         (every #'equal a (subseq b 0 (length a))))))

(defun under (prefix p)
  "P's segments below PREFIX, as a list, or NIL when P is not under it."
  (when (prefixp prefix p)
    (coerce (subseq (path-segments p) (segment-count prefix)) 'list)))

(defun patternp (p)
  "True when P names more than one place, or binds."
  (some (lambda (s) (not (stringp s))) (path-segments p)))

;;;; Matching. A constraint's value is a pattern in its own right: a literal is
;;;; equality, a set is membership, ?name binds, and a list is a Lisp form with
;;;; % bound to the value, so nothing about a constraint is capped.

(defun %variable (sym)
  "The variable a ?name or ?@name binder names."
  (let ((text (symbol-name sym)))
    (when (and (plusp (length text)) (char= #\? (char text 0)))
      (setf text (subseq text 1)))
    (when (and (plusp (length text)) (char= #\@ (char text 0)))
      (setf text (subseq text 1)))
    (intern text (symbol-package sym))))

(defun %binderp (x)
  (and (symbolp x) x
       (plusp (length (symbol-name x)))
       (char= #\? (char (symbol-name x) 0))))

(defun %constraint-binders (s)
  (let ((acc nil))
    (when (seg-constraint s)
      (fset:do-map (key pattern (seg-constraint s))
        (declare (ignore key))
        (when (%binderp pattern) (push (%variable pattern) acc))))
    (nreverse acc)))

(defun literal-at (p n)
  "P's Nth segment when it is a literal that nothing before it can shift, so a
walk can try that child without being told the child exists. NIL otherwise."
  (let ((segments (path-segments p)))
    (when (< n (length segments))
      (loop :for i :from 0 :below n
            :for s = (aref segments i)
            :when (and (seg-p s) (member (seg-kind s) '(:deep :bind-rest)))
              :do (return-from literal-at nil))
      (let ((s (aref segments n)))
        (when (stringp s) s)))))

(defun binders (p)
  "The variables P's binders name, in the order they appear. A fan-out write
and a provider clause both bind these around the body they run."
  (loop :for s :across (path-segments p)
        :when (seg-p s)
          :append (append (when (member (seg-kind s) '(:bind :bind-rest))
                            (list (%variable (seg-value s))))
                          (%constraint-binders s))))

(defun %substitute-% (form)
  "FORM with every symbol named % replaced by this package's, so a constraint
written in any package compiles against one variable."
  (cond ((and (symbolp form) form (string= (symbol-name form) "%")) '%)
        ((consp form) (cons (%substitute-% (car form)) (%substitute-% (cdr form))))
        (t form)))

(defun %test (s key form)
  "The compiled test for S's constraint on KEY, compiled once."
  (or (cdr (assoc key (seg-tests s) :test #'equal))
      (let ((fn (coerce `(lambda (%) (declare (ignorable %)) ,(%substitute-% form))
                        'function)))
        (push (cons key fn) (seg-tests s))
        fn)))

(defun %constraint-ok (s prefix value bindings)
  "Test S's constraint against the value at each named child of PREFIX.
Answers (values ok bindings)."
  (let ((constraint (seg-constraint s))
        (acc bindings)
        (ok t))
    (when (null constraint)
      (return-from %constraint-ok (values t bindings)))
    (fset:do-map (key pattern constraint)
      (when ok
        (let ((v (funcall value (child prefix (name key)))))
          (cond ((%binderp pattern)
                 (setf acc (fset:with acc (%variable pattern) v)))
                ((fset:set? pattern)
                 (unless (fset:contains? pattern v) (setf ok nil)))
                ((consp pattern)
                 (unless (funcall (%test s key pattern) v) (setf ok nil)))
                (t (unless (fset:equal? pattern v) (setf ok nil)))))))
    (values ok acc)))

(defun %segment-ok (s segment)
  "Whether the segment pattern S admits the literal SEGMENT."
  (ecase (seg-kind s)
    (:literal (string= (seg-value s) segment))
    (:one-of (fset:contains? (seg-value s) segment))
    ((:any :bind :deep :bind-rest) t)))

(defun match (pattern subject &key (value (constantly nil)))
  "Match the literal path SUBJECT against PATTERN.

Answers (values t bindings), or NIL. BINDINGS maps each binder's variable to
the segment it took, and a ?@rest binder to the list it took. VALUE answers the
namespace value at a path, for a constraint to test."
  (let ((ps (path-segments pattern))
        (ss (path-segments subject)))
    (labels
        ((walk (pn sn bindings)
           (if (= pn (length ps))
               (and (= sn (length ss)) (list bindings))
               (let ((p (aref ps pn)))
                 (cond
                   ((stringp p)
                    (and (< sn (length ss))
                         (equal p (aref ss sn))
                         (walk (1+ pn) (1+ sn) bindings)))
                   ;; ** and ?@rest take any number of segments, including none
                   ((member (seg-kind p) '(:deep :bind-rest))
                    (loop :for take :from (- (length ss) sn) :downto 0
                          :for acc = (if (eq (seg-kind p) :bind-rest)
                                         (fset:with bindings
                                                    (%variable (seg-value p))
                                                    (coerce (subseq ss sn (+ sn take))
                                                            'list))
                                         bindings)
                          :for found = (walk (1+ pn) (+ sn take) acc)
                          :when found :return found))
                   (t
                    (and (< sn (length ss))
                         (%segment-ok p (aref ss sn))
                         (multiple-value-bind (ok acc)
                             (%constraint-ok p (subpath subject 0 (1+ sn))
                                             value bindings)
                           (and ok
                                (walk (1+ pn) (1+ sn)
                                      (if (eq (seg-kind p) :bind)
                                          (fset:with acc
                                                     (%variable (seg-value p))
                                                     (aref ss sn))
                                          acc)))))))))))
      (let ((found (walk 0 0 (fset:empty-map))))
        (when found (values t (first found)))))))

;;;; The reader

(defparameter +terminators+
  '(#\Space #\Tab #\Newline #\Return #\Page
    #\( #\) #\" #\; #\' #\` #\, #\] #\})
  "Characters that end a path literal. { does not: it opens a constraint.")

(defun %read-token (stream)
  "Characters up to the next /, {, or terminator."
  (with-output-to-string (out)
    (loop :for ch = (peek-char nil stream nil nil t)
          :until (or (null ch) (char= ch #\/) (char= ch #\{)
                     (member ch +terminators+))
          :do (write-char (read-char stream) out))))

(defun %read-constraint (stream)
  "A {k v} constraint, read as data: its values are patterns, not forms to run."
  (when (eql #\{ (peek-char nil stream nil nil t))
    (let ((*readtable* (named-readtables:find-readtable 'pine.data:data)))
      (read stream t nil t))))

(defun %read-alternation (stream)
  "#{a b} inside a path: one of these segment names, unevaluated."
  (read-char stream)                    ; #
  (read-char stream)                    ; {
  (let ((names (with-output-to-string (out)
                 (loop :for ch = (read-char stream t nil t)
                       :until (char= ch #\})
                       :do (write-char ch out)))))
    (fset:convert 'fset:set
                  (remove "" (uiop:split-string
                              names :separator '(#\Space #\Tab #\Newline #\,))
                          :test #'string=))))

(defun %read-interpolation (stream)
  "${form} or $@{form}. Answers (values form splicep)."
  (read-char stream)                    ; $
  (let ((splice (eql #\@ (peek-char nil stream nil nil t))))
    (when splice (read-char stream))
    (unless (eql #\{ (peek-char nil stream nil nil t))
      (error "A ~a in a path must be followed by {form}." (if splice "$@" "$")))
    (read-char stream)                  ; {
    (let ((forms (read-delimited-list #\} stream t)))
      (unless (= 1 (length forms))
        (error "~a{...} takes one form, not ~d." (if splice "$@" "$")
               (length forms)))
      (values (first forms) splice))))

(defun %token-segment (token constraint)
  "The segment TOKEN names, with CONSTRAINT attached."
  (cond
    ((string= token "*") (%seg :any nil constraint))
    ((string= token "**") (%seg :deep nil constraint))
    ((and (> (length token) 2) (string= "?@" (subseq token 0 2)))
     (%seg :bind-rest (intern (string-upcase token) *package*) constraint))
    ((and (> (length token) 1) (char= #\? (char token 0)))
     (%seg :bind (intern (string-upcase token) *package*) constraint))
    (constraint (%seg :literal token constraint))
    (t token)))

(defun read-path (stream char)
  "/a/b -- a constant path. With an interpolation, the form that builds one."
  (declare (ignore char))
  (let ((parts nil) (interpolated nil))
    (loop
      (case (peek-char nil stream nil nil t)
        ((nil))
        (#\$ (multiple-value-bind (form splice) (%read-interpolation stream)
               (push (if splice `(spliced ,form) `(name ,form)) parts)
               (setf interpolated t)))
        (#\# (let* ((names (%read-alternation stream))
                    (constraint (%read-constraint stream)))
               (push (%seg :one-of names constraint) parts)))
        (t (let ((token (%read-token stream)))
             (when (plusp (length token))
               (push (%token-segment token (%read-constraint stream)) parts)))))
      (if (eql #\/ (peek-char nil stream nil nil t))
          (read-char stream)
          (return)))
    (setf parts (nreverse parts))
    (if interpolated
        `(path ,@(mapcar (lambda (p) (if (or (stringp p) (seg-p p)) `',p p)) parts))
        (%make-path (coerce parts 'simple-vector)))))

(named-readtables:defreadtable syntax
  (:merge pine.data:syntax)
  (:macro-char #\/ #'read-path))

;;;; The serialization readtable with paths in it: a stored value may hold one,
;;;; and it has to read back as a path rather than as whatever /a/b would mean
;;;; to the standard reader.

(named-readtables:defreadtable data
  (:merge pine.data:data)
  (:macro-char #\/ #'read-path))
