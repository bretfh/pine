(defpackage #:pine.path
  (:use #:cl)
  (:shadow #:parse)
  (:export #:path #:pathp #:segments #:segment-count #:root #:rootp
           #:parent #:leaf #:child #:under #:prefixp #:subpath
           #:text #:parse #:name #:spliced
           #:patternp #:binders #:match #:named-at #:expansions #:literal-tail
           #:key #:keys #:any
           #:seg #:seg-value #:seg-constraint #:many-seg #:binding-seg
           #:literal-seg #:any-seg #:deep-seg #:one-of-seg #:bind-seg
           #:bind-rest-seg
           #:segment-text #:segment-admits #:segment-took
           #:*here* #:here #:up #:transactionp
           #:syntax #:data))

(in-package #:pine.path)

(defclass seg ()
  ((value      :initarg :value      :accessor seg-value      :initform nil)
   (constraint :initarg :constraint :accessor seg-constraint :initform nil)
   (binder     :initarg :binder     :accessor seg-binder     :initform nil)
   (pieces     :initarg :pieces     :accessor seg-pieces     :initform nil)
   (tests      :initarg :tests      :accessor seg-tests      :initform nil))
  (:documentation "A segment that names more than one place, or binds."))

(defclass many-seg (seg) ()
  (:documentation "A segment that takes any number of segments, including none."))

(defclass binding-seg (seg) ()
  (:documentation "A segment whose value is the variable it binds."))

(defclass literal-seg (seg) ())
(defclass any-seg (seg) ())
(defclass deep-seg (many-seg) ())
(defclass one-of-seg (seg) ())
(defclass bind-seg (binding-seg) ())
(defclass bind-rest-seg (binding-seg many-seg) ())

(defstruct (path (:constructor %make-path (segments))
                 (:predicate pathp)
                 (:copier nil))
  (segments #() :type simple-vector)
  (cached-keys nil))

(defparameter +seg-classes+
  '((:literal . literal-seg) (:any . any-seg) (:deep . deep-seg)
    (:one-of . one-of-seg) (:bind . bind-seg) (:bind-rest . bind-rest-seg))
  "The class each segment the reader can write makes.")

(defparameter +terminators+
  '(#\Space #\Tab #\Newline #\Return #\Page
    #\( #\) #\" #\; #\' #\` #\, #\])
  "Characters that end a path literal.")

(defvar *root* (%make-path #()))

(defvar *here* nil
  "The path a relative path is read against: the one a provider clause is
answering for, or the row a ROWS body is building.")

(defun seg-p (x) (typep x 'seg))

(defun %seg (kind value &optional constraint)
  (make-instance (or (cdr (assoc kind +seg-classes+))
                     (error "no segment kind ~s" kind))
                 :value value :constraint constraint))

(defmethod make-load-form ((s seg) &optional environment)
  (declare (ignore environment))
  `(%loaded-seg (make-instance ',(class-name (class-of s))
                               :value ',(seg-value s)
                               :constraint ',(seg-constraint s))
                ',(seg-binder s) ',(seg-pieces s)))

(defun %loaded-seg (s binder pieces)
  "S with what the constructor does not take."
  (setf (seg-binder s) binder
        (seg-pieces s) pieces)
  s)

(defmethod make-load-form ((p path) &optional environment)
  (declare (ignore environment))
  `(%make-path ',(path-segments p)))

(defun root () "The path naming the whole namespace." *root*)

(defun segment-count (p) (length (path-segments p)))
(defun rootp (p) (zerop (segment-count p)))
(defun segments (p) (coerce (path-segments p) 'list))

(defgeneric name (x)
  (:documentation "X as one segment string."))

(defmethod name ((x string)) x)
(defmethod name ((x symbol))
  (if (keywordp x) (string-downcase (symbol-name x)) (string x)))
(defmethod name ((x integer)) (format nil "~d" x))
(defmethod name ((x character)) (string x))
(defmethod name ((x pathname)) (namestring x))

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

(defgeneric spliced (x)
  (:documentation "X as a list of segments, for a $@ splice: a path's own, a
string cut at every slash, or a list of anything NAME takes."))

(defmethod spliced ((x path)) (coerce (path-segments x) 'list))
(defmethod spliced ((x string))
  (pine.data:split x :on #\/ :empties nil))
(defmethod spliced ((x pathname)) (spliced (namestring x)))
(defmethod spliced ((x list)) (mapcar #'name x))
(defmethod spliced ((x null)) nil)

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
  "The path STRING names. A segment that is exactly * is a wildcard and one
that is exactly ** is a deep one; nothing else in the text is a pattern."
  (%make-path
   (coerce (mapcar (lambda (segment)
                     (cond ((string= segment "*") (%seg :any nil))
                           ((string= segment "**") (%seg :deep nil))
                           (t segment)))
                   (spliced string))
           'simple-vector)))

(defun any ()
  "A wildcard segment, for building a pattern where the shape is computed."
  (%seg :any nil))

(defgeneric segment-text (s out)
  (:documentation "Write S to OUT, as it reads."))

(defmethod segment-text ((s literal-seg) out)
  (write-string (seg-value s) out))

(defmethod segment-text ((s any-seg) out)
  (declare (ignore s))
  (write-char #\* out))

(defmethod segment-text ((s deep-seg) out)
  (declare (ignore s))
  (write-string "**" out))

(defmethod segment-text ((s one-of-seg) out)
  (write-string "#{" out)
  (let ((first t))
    (fset:do-set (m (seg-value s))
      (if first (setf first nil) (write-char #\Space out))
      (write-string m out)))
  (write-char #\} out))

(defmethod segment-text ((s binding-seg) out)
  (format out "~(~a~)" (symbol-name (seg-value s))))

(defun %seg-text (s)
  (with-output-to-string (out)
    (segment-text s out)
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

(defun %same-segment-p (a b)
  "Whether two segments name the same thing."
  (if (and (stringp a) (stringp b))
      (string= a b)
      (and (seg-p a) (seg-p b)
           (eq (class-of a) (class-of b))
           (fset:equal? (seg-value a) (seg-value b))
           (fset:equal? (seg-constraint a) (seg-constraint b)))))

(defmethod fset:compare ((a path) (b path))
  (let ((x (path-segments a)) (y (path-segments b)))
    (loop :for i :from 0 :below (min (length x) (length y))
          :for xi = (aref x i)
          :for yi = (aref y i)
          :do (unless (and (stringp xi) (stringp yi))
                (return-from fset:compare
                  (if (%same-segment-p xi yi) :equal :unequal)))
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

(defun transactionp (map)
  "Whether MAP is a transaction rather than a directory: its keys are paths."
  (and (fset:map? map)
       (let ((yes nil))
         (fset:do-map (key value map)
           (declare (ignore value))
           (when (pathp key) (setf yes t)))
         yes)))

(defun here ()
  "The path a relative path is relative to."
  *here*)

(defun up (&optional (levels 1))
  "LEVELS above the path a relative path is relative to."
  (let ((p (or *here* (root))))
    (dotimes (i levels p) (setf p (parent p)))))

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

(defun named-at (p n)
  "The names P's Nth segment can be, when nothing before it can shift them."
  (let ((segments (path-segments p)))
    (when (< n (length segments))
      (loop :for i :from 0 :below n
            :for s = (aref segments i)
            :when (typep s 'many-seg)
              :do (return-from named-at nil))
      (let ((s (aref segments n)))
        (cond ((stringp s) (list s))
              ((typep s 'one-of-seg) (fset:convert 'list (seg-value s))))))))

(defun expansions (p)
  "Every concrete path P names, when it names concrete ones: literal segments
and {a,b} groups. NIL when a segment has to be looked up."
  (let ((acc (list nil)))
    (loop :for s :across (path-segments p)
          :do (cond
                ((stringp s) (setf acc (mapcar (lambda (names) (cons s names)) acc)))
                ((and (typep s 'one-of-seg) (null (seg-constraint s)))
                 (setf acc (loop :for names :in acc
                                 :append (loop :for name :in (fset:convert
                                                              'list (seg-value s))
                                               :collect (cons name names)))))
                (t (return-from expansions nil))))
    (mapcar (lambda (names)
              (%make-path (coerce (reverse names) 'simple-vector)))
            acc)))

(defun literal-tail (p)
  "(values head tail): the run of literal segments at the end of P as names,
and the pattern before them. NIL tail when P does not end in one."
  (let* ((segments (path-segments p))
         (n (length segments))
         (start n))
    (loop :while (and (plusp start) (stringp (aref segments (1- start))))
          :do (decf start))
    (if (or (zerop start) (= start n))
        (values p nil)
        (values (%make-path (subseq segments 0 start))
                (coerce (subseq segments start) 'list)))))

(defun binders (p)
  "The variables P's binders name, in the order they appear."
  (loop :for s :across (path-segments p)
        :when (seg-p s)
          :append (append (when (typep s 'binding-seg)
                            (list (%variable (seg-value s))))
                          (when (seg-binder s) (list (seg-binder s)))
                          (%constraint-binders s))))

(defun %substitute-% (form)
  "FORM with every symbol named % replaced by this package's."
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
        (let ((v (if (eq key :.)
                     (funcall value prefix)
                     (funcall value (child prefix (name key))))))
          (cond ((%binderp pattern)
                 (setf acc (fset:with acc (%variable pattern) v)))
                ((fset:set? pattern)
                 (unless (fset:contains? pattern v) (setf ok nil)))
                ((consp pattern)
                 (unless (funcall (%test s key pattern) v) (setf ok nil)))
                (t (unless (fset:equal? pattern v) (setf ok nil)))))))
    (values ok acc)))

(defgeneric segment-admits (s segment)
  (:documentation "Whether the segment pattern S admits SEGMENT.")
  (:method ((s seg) segment) (declare (ignore segment)) t))

(defmethod segment-admits ((s literal-seg) segment)
  (string= (seg-value s) segment))

(defmethod segment-admits ((s one-of-seg) segment)
  (fset:contains? (seg-value s) segment))

(defgeneric segment-took (s bindings taken)
  (:documentation "BINDINGS with what S bound of the TAKEN it matched.")
  (:method ((s seg) bindings taken)
    (if (seg-binder s)
        (fset:with bindings (seg-binder s) (fset:lookup (seg-pieces s) taken))
        bindings)))

(defmethod segment-took ((s binding-seg) bindings taken)
  (fset:with (call-next-method) (%variable (seg-value s)) taken))

(defmethod segment-took ((s deep-seg) bindings taken)
  (declare (ignore taken))
  bindings)

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
                   ((typep p 'many-seg)
                    (loop :for take :from (- (length ss) sn) :downto 0
                          :for acc = (segment-took p bindings
                                                   (coerce (subseq ss sn (+ sn take))
                                                           'list))
                          :for found = (walk (1+ pn) (+ sn take) acc)
                          :when found :return found))
                   (t
                    (and (< sn (length ss))
                         (segment-admits p (aref ss sn))
                         (multiple-value-bind (ok acc)
                             (%constraint-ok p (subpath subject 0 (1+ sn))
                                             value bindings)
                           (and ok
                                (walk (1+ pn) (1+ sn)
                                      (segment-took p acc (aref ss sn))))))))))))
      (let ((found (walk 0 0 (fset:empty-map))))
        (when found (values t (first found)))))))

(defun %read-token (stream)
  "Characters up to the next /, [, or terminator. A {a,b} group or a {1..9}
range is part of the token, and its comma and closing brace are its own."
  (with-output-to-string (out)
    (let ((depth 0))
      (loop :for ch = (peek-char nil stream nil nil t)
            :until (or (null ch)
                       (and (zerop depth)
                            (or (char= ch #\/) (char= ch #\[)
                                (char= ch #\})
                                (member ch +terminators+))))
            :do (case ch (#\{ (incf depth)) (#\} (decf depth)))
                (write-char (read-char stream) out)))))

(defun %read-constraint (stream)
  "A [pred] filter. PRED is a name (that child is non-nil), NAME = PATTERN, or
= PATTERN for the segment's own value. Several may follow one another."
  (let ((acc nil))
    (loop :while (eql #\[ (peek-char nil stream nil nil t))
          :do (read-char stream)
              (let ((body (with-output-to-string (out)
                            (loop :for ch = (read-char stream t nil t)
                                  :until (char= ch #\])
                                  :do (write-char ch out)))))
                (push (%constraint-of body) acc)))
    (when acc
      (let ((out (fset:empty-map)))
        (dolist (m (nreverse acc) out)
          (fset:do-map (k v m) (setf out (fset:with out k v))))))))

(defun %constraint-of (body)
  "The map a [pred] body names."
  (let* ((text (string-trim " " body))
         (eq-at (search "=" text)))
    (if (null eq-at)
        (fset:map ((key text) (quote (not (null %)))))
        (let ((lhs (string-trim " " (subseq text 0 eq-at)))
              (rhs (string-trim " " (subseq text (1+ eq-at)))))
          (let ((value (let ((*readtable* (named-readtables:find-readtable
                                           (quote pine.data:data))))
                         (read-from-string rhs))))
            (if (string= lhs "")
                (fset:map (:. value))
                (fset:map ((key lhs) value))))))))

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

(defun %range (text)
  "The names {a..b} covers, or NIL when TEXT is not a range."
  (let ((dots (search ".." text)))
    (when dots
      (let ((from (parse-integer (subseq text 0 dots) :junk-allowed t))
            (to (parse-integer (subseq text (+ dots 2)) :junk-allowed t)))
        (when (and from to (<= from to))
          (loop :for i :from from :to to :collect (princ-to-string i)))))))

(defun %group (token)
  "The names TOKEN spells when it holds a {a,b} group or a {1..9} range, or
NIL when it holds neither."
  (let ((open (position #\{ token)))
    (when open
      (let ((close (position #\} token :start open)))
        (when close
          (let* ((before (subseq token 0 open))
                 (after (subseq token (1+ close)))
                 (inside (subseq token (1+ open) close))
                 (names (or (%range inside)
                            (remove "" (uiop:split-string
                                        inside :separator (list #\, #\Space))
                                    :test #'string=))))
            (loop :for name :in names
                  :append (let ((whole (concatenate 'string before name after)))
                            (or (%group whole) (list whole))))))))))

(defun %binder-group (token)
  "The names ?n{a,b} spells inside a segment, as (name . piece), and the
variable each piece binds."
  (let ((mark (position #\? token)))
    (when mark
      (let ((open (position #\{ token :start mark)))
        (when open
          (let ((close (position #\} token :start open)))
            (when close
              (let* ((before (subseq token 0 mark))
                     (after  (subseq token (1+ close)))
                     (name   (subseq token (1+ mark) open))
                     (inside (subseq token (1+ open) close))
                     (pieces (or (%range inside)
                                 (remove "" (uiop:split-string
                                             inside :separator (list #\, #\Space))
                                         :test #'string=))))
                (when (and (plusp (length name)) pieces)
                  (values (loop :for piece :in pieces
                                :collect (cons (concatenate 'string
                                                            before piece after)
                                               piece))
                          (intern (string-upcase name) *package*)))))))))))

(defun %token-segment (token constraint)
  "The segment TOKEN names, with CONSTRAINT attached."
  (multiple-value-bind (pairs variable) (%binder-group token)
    (when pairs
      (let ((s (%seg :one-of (fset:convert 'fset:set (mapcar #'car pairs))
                     constraint)))
        (setf (seg-binder s) variable
              (seg-pieces s) (fset:convert 'fset:map pairs))
        (return-from %token-segment s))))
  (let ((group (%group token)))
    (cond
      (group (%seg :one-of (fset:convert 'fset:set group) constraint))
      ((string= token "*") (%seg :any nil constraint))
      ((string= token "**") (%seg :deep nil constraint))
      ((and (> (length token) 2) (string= "?@" (subseq token 0 2)))
       (%seg :bind-rest (intern (string-upcase token) *package*) constraint))
      ((and (> (length token) 1) (char= #\? (char token 0)))
       (%seg :bind (intern (string-upcase token) *package*) constraint))
      (constraint (%seg :literal token constraint))
      (t token))))

(defun read-unquote (stream char)
  "${form} where a value is wanted: {/audio/sink ${(leaf /.)}}."
  (unread-char char stream)
  (multiple-value-bind (form splice) (%read-interpolation stream)
    (if splice `(spliced ,form) form)))

(defun %relative-base (token)
  "The form a leading run of dots names: /. is the path being answered for and
/.. the one above it."
  (when (and (plusp (length token))
             (every (lambda (ch) (char= ch #\.)) token))
    (if (= 1 (length token)) `(here) `(up ,(1- (length token))))))

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
    (let ((base (and (stringp (first parts)) (%relative-base (first parts)))))
      (when base
        (setf parts (cons base (rest parts))
              interpolated t)))
    (if interpolated
        `(path ,@(mapcar (lambda (p) (if (or (stringp p) (seg-p p)) `',p p)) parts))
        (%make-path (coerce parts 'simple-vector)))))

(named-readtables:defreadtable syntax
  (:merge pine.data:syntax)
  (:macro-char #\/ #'read-path)
  (:macro-char #\$ #'read-unquote))

(named-readtables:defreadtable data
  (:merge pine.data:data)
  (:macro-char #\/ #'read-path))
