(defpackage #:pine.ns
  (:use #:cl)
  (:shadow #:read #:write #:space)
  (:local-nicknames (#:p #:pine.path))
  (:export #:space #:fresh #:*space* #:with-space
           #:read #:write #:watch #:preview #:toggle
           #:provider #:here
           #:kind #:setting #:*after-commit*
           #:refused #:no-verb #:cycle #:at #:why))

(in-package #:pine.ns)
(named-readtables:in-readtable pine.path:syntax)

;;;; The namespace is one nested persistent map, and a path is a lookup into
;;;; it. A write produces a new root that shares everything it did not touch,
;;;; so the question the engine asks all day -- did this value change -- is a
;;;; pointer compare.
;;;;
;;;; A directory and a map are the same thing seen from two sides: every
;;;; non-map at a path is a leaf, and a map is the children under it.

(defstruct (space (:constructor fresh ()) (:copier nil))
  (root (fset:empty-map))
  (mounts nil)                        ; (path . backing), newest first
  (reactions (fset:empty-map))        ; path -> reaction
  (watches nil)                       ; (name pattern function)
  (settings (fset:empty-map))         ; path -> the write options that outlive it
  (lock (bordeaux-threads:make-recursive-lock "pine-ns")))

(defvar *space* (fresh)
  "The namespace this image serves.")

(defmacro with-space ((&optional (space '(fresh))) &body body)
  "Run BODY against SPACE, so a test or a tool can hold one of its own."
  `(let ((*space* ,space)) ,@body))

(defstruct (reaction (:copier nil))
  path                                ; where the value lands
  thunk                               ; how it is computed
  (deps (fset:empty-set)))            ; the paths it read last time

(define-condition refused (error)
  ((at :initarg :at :reader at)
   (why :initarg :why :reader why))
  (:report (lambda (c stream)
             (format stream "Refused to write ~a: ~a. Pass :force t to mean it."
                     (at c) (why c)))))

(define-condition no-verb (error)
  ((at :initarg :at :reader at)
   (why :initarg :why :reader why))
  (:report (lambda (c stream)
             (format stream "~a does not take the verb ~s." (at c) (why c)))))

(define-condition cycle (error)
  ((at :initarg :at :reader at))
  (:report (lambda (c stream)
             (format stream "These paths keep recomputing each other: ~{~a~^ ~}."
                     (at c)))))

;;;; The tree. A path's segments are strings, and the name each is stored under
;;;; is computed once per path. A value goes in already keyed that way, so a
;;;; read hands back the object that is there: an unchanged subtree comes back
;;;; the same object, which is what a tree diff rests on.

(defun %inward (value)
  "VALUE with its keys as the names the tree stores, and its nothings dropped,
since nil is nothing here as it is in Lisp. Answers VALUE itself when nothing
had to move, so what is stored stays shared with what was written."
  (if (fset:map? value)
      (let ((out value))
        (fset:do-map (k v value)
          (let ((key (if (stringp k) (p:key k) k))
                (inner (%inward v)))
            (cond ((null inner) (setf out (fset:less out k)))
                  ((and (eql key k) (eq inner v)))
                  (t (setf out (fset:with (fset:less out k) key inner))))))
        out)
      value))

(defun %get (root keys)
  (if (null keys)
      root
      (let ((node (and (fset:map? root) (fset:lookup root (first keys)))))
        (and node (%get node (rest keys))))))

(defun %put (root keys value)
  (cond ((null keys) value)
        ((null (rest keys))
         (let ((base (if (fset:map? root) root (fset:empty-map))))
           (if (null value)
               (fset:less base (first keys))
               (fset:with base (first keys) value))))
        (t (let* ((base (if (fset:map? root) root (fset:empty-map)))
                  (node (fset:lookup base (first keys))))
             (fset:with base (first keys)
                        (%put (if (fset:map? node) node (fset:empty-map))
                              (rest keys) value))))))

;;;; Providers. A subtree is backed by something that knows one system; nothing
;;;; above it learns there is a subprocess, a socket, a poll or a file behind
;;;; it. Mounting is a write like any other.

(defstruct (clause (:constructor %clause (pattern handlers)) (:copier nil))
  pattern
  handlers)                           ; (lambda (binders...) -> a handler map)

(defstruct (backing (:constructor %backing (clauses options)) (:copier nil))
  clauses
  options)

(defvar *here* nil "The path a provider clause is answering for.")

(defun here ()
  "The path the clause running now was asked about."
  *here*)

(defmacro provider (&rest forms)
  "A provider: an optional options map, then (PATTERN HANDLERS) clauses.

A clause's pattern binds its ?name segments around HANDLERS, so a handler names
the segment it matched. HERE answers the path being served."
  (let ((options (when (and forms (consp (first forms))
                            (eq 'fset:map (first (first forms))))
                   (pop forms))))
    `(%backing
      (list ,@(mapcar (lambda (clause)
                        (destructuring-bind (pattern handlers) clause
                          `(%clause ,pattern
                                    (lambda ,(p:binders pattern)
                                      (declare (ignorable ,@(p:binders pattern)))
                                      ,handlers))))
                      forms))
      ,(or options '(fset:empty-map)))))

(defun %backings (space path)
  "Every provider whose mount point is at or above PATH, newest first."
  (loop :for (at . backing) :in (space-mounts space)
        :when (p:prefixp at path)
          :collect backing))

(defun %handlers (backing path)
  "The handler map BACKING answers for PATH, or NIL."
  (loop :for clause :in (backing-clauses backing)
        :do (multiple-value-bind (ok bindings)
                (p:match (clause-pattern clause) path :value #'%read-one)
              (when ok
                (let ((*here* path))
                  (return (apply (clause-handlers clause)
                                 (mapcar (lambda (v) (fset:lookup bindings v))
                                         (p:binders (clause-pattern clause))))))))))

(defun %served-read (space path)
  "Answer (values value t) when a mounted provider reads PATH."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :for fn = (and handlers (fset:lookup handlers :read))
        :when fn :do (return (values (funcall fn) t))
        :finally (return (values nil nil))))

(defun %servedp (space path)
  "True when some mounted provider has a clause for PATH."
  (loop :for backing :in (%backings space path)
        :thereis (and (%handlers backing path) t)))

(defun %served-write (space path value)
  "Give VALUE to the provider serving PATH. True when one took it."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :when handlers
          :do (let ((verbs (fset:lookup handlers :verbs))
                    (write (fset:lookup handlers :write)))
                (cond ((and (%verbp value) verbs
                            (fset:lookup verbs (%verb-name value)))
                       (apply (fset:lookup verbs (%verb-name value))
                              (%verb-args value))
                       (return t))
                      ((%verbp value)
                       (error 'no-verb :at path :why (%verb-name value)))
                      (write (funcall write value) (return t))
                      (t (error 'refused :at path
                                :why "no provider writes it"))))
        :finally (return nil)))

(defun %served-children (space path)
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :for fn = (and handlers (fset:lookup handlers :ls))
        :when fn
          :do (return (mapcar (lambda (n) (p:child path n)) (funcall fn)))
        :finally (return nil)))

;;;; Reading. Inside a write body or a handler a read is recorded, and that
;;;; recording is the whole of the reactive system.

(defvar *reads* nil "Where the paths read while computing a value collect.")

(defun %read-one (path &optional default)
  (let ((space *space*))
    (when *reads* (push path (cdr *reads*)))
    (multiple-value-bind (value served) (%served-read space path)
      (if served
          (if (null value) default value)
          (let ((value (%get (space-root space) (p:keys path))))
            (if (null value) default value))))))

(defun %children (space path)
  "PATH's children: the tree's, or the provider's."
  (let ((node (%get (space-root space) (p:keys path))))
    (or (when (fset:map? node)
          (let ((acc nil))
            (fset:do-map (k v node)
              (declare (ignore v))
              (push (p:child path (p:name k)) acc))
            (nreverse acc)))
        (%served-children space path))))

(defun %literal-prefix (pattern)
  "The longest run of literal segments at the front of PATTERN."
  (apply #'p:path (loop :for s :in (p:segments pattern)
                        :while (stringp s)
                        :collect s)))

(defun %exists (space path)
  "True when something is there: a value, a provider that answers, or children.
A pattern only ever matches what exists, so a walk asks this before it counts
a candidate."
  (or (p:rootp path)
      (not (null (%get (space-root space) (p:keys path))))
      (nth-value 1 (%served-read space path))
      (not (null (%children space path)))))

(defun %candidates (space pattern prefix)
  "Where a walk of PATTERN can go from PREFIX: the children that exist, plus
the literal segment the pattern asks for next, which needs nothing enumerated."
  (let* ((children (%children space prefix))
         (wanted (p:literal-at pattern (p:segment-count prefix)))
         (extra (and wanted (p:child prefix wanted))))
    (if (and extra (notany (lambda (c) (fset:equal? c extra)) children))
        (cons extra children)
        children)))

(defun %matches (pattern)
  "Every path that exists now and matches PATTERN."
  (let ((space *space*)
        (found nil))
    (labels ((walk (prefix depth)
               (when (< depth 64)
                 (when (and (p:match pattern prefix :value #'%read-one)
                            (%exists space prefix))
                   (push prefix found))
                 (dolist (child (%candidates space pattern prefix))
                   (walk child (1+ depth))))))
      (walk (%literal-prefix pattern) 0))
    (nreverse found)))

(defun read (path &optional default)
  "The value at PATH, or a map from path to value when PATH is a pattern."
  (if (p:patternp path)
      (let ((out (fset:empty-map)))
        (dolist (found (%matches path) out)
          (setf out (fset:with out found (%read-one found)))))
      (%read-one path default)))

;;;; Writing. A value is state and de-duplicates; a verb is an event, handed to
;;;; whoever serves the path. A map is a transaction: one new root, so nothing
;;;; observes the system half changed.

(defun %verbp (value)
  (and (fset:seq? value)
       (plusp (fset:size value))
       (keywordp (fset:lookup value 0))))

(defun %verb-name (value) (fset:lookup value 0))
(defun %verb-args (value) (fset:convert 'list (fset:subseq value 1)))

(defun %merge (a b)
  (let ((out (if (fset:map? a) a (fset:empty-map))))
    (fset:do-map (k v b) (setf out (fset:with out k v)))
    out))

(defun %apply-verb (path current value)
  "What a built-in verb makes of what is already there."
  (let ((verb (%verb-name value))
        (args (%verb-args value)))
    (case verb
      (:set (first args))
      (:toggle (not current))
      (:conj (fset:with (if (fset:set? current) current (fset:empty-set))
                        (first args)))
      (:disj (fset:less (if (fset:set? current) current (fset:empty-set))
                        (first args)))
      (:merge (%merge current (first args)))
      (t (error 'no-verb :at path :why verb)))))

(defvar *preview* nil "When bound, writes collect here instead of being made.")
(defvar *propagating* nil)

(defvar *after-commit* nil
  "Called with the list of (path old new) each commit moved. The store hangs
here, because whether a value outlives the daemon is a property of the path and
not something anyone should have to remember to ask for.")

(defun kind (path)
  "Where the value at PATH comes from.

:LIVE   a provider reads it; the world it came from is the storage
:DERIVED an expression computed it from other paths, so it is computed again
:HELD   someone wrote it, and nothing else determines it"
  (let ((space *space*))
    (cond ((%servedp space path) :live)
          ((let ((r (fset:lookup (space-reactions space) path)))
             (and r (not (fset:empty? (reaction-deps r)))))
           :derived)
          (t :held))))

(defun setting (path key &optional default)
  "The write option KEY that PATH was given, or DEFAULT."
  (let ((options (fset:lookup (space-settings *space*) path)))
    (if (and options (fset:domain-contains? options key))
        (fset:lookup options key)
        default)))

(defun %remember (path options)
  "Keep the write options that outlive the write itself."
  (let ((space *space*))
    (loop :for (key value) :on options :by #'cddr
          :when (member key '(:keep :max))
            :do (setf (space-settings space)
                      (fset:with (space-settings space) path
                                 (fset:with (or (fset:lookup (space-settings space)
                                                             path)
                                                (fset:empty-map))
                                            key value))))))

(defun %ring-push (current value limit)
  (let ((seq (fset:with-first (if (fset:seq? current) current (fset:empty-seq))
                              value)))
    (if (> (fset:size seq) limit) (fset:subseq seq 0 limit) seq)))

(defun %whole-tree-p (path)
  (let ((text (p:text path)))
    (or (string= text "/**") (uiop:string-prefix-p "/**/" text))))

(defun %commit (space changes)
  "Put CHANGES, a list of (path . value), into one new root. Answers the list
of (path old new) that moved."
  (bordeaux-threads:with-recursive-lock-held ((space-lock space))
    (let ((root (space-root space))
          (moved nil))
      (dolist (change changes)
        (destructuring-bind (path . value) change
          (let ((old (%get root (p:keys path))))
            (unless (fset:equal? old value)
              (setf root (%put root (p:keys path) value))
              (push (list path old value) moved)))))
      (setf (space-root space) root)
      (setf moved (nreverse moved))
      (when (and moved *after-commit*) (funcall *after-commit* moved))
      moved)))

(defun %evaluate (thunk)
  "Run THUNK, answering (values value paths-it-read)."
  (let ((*reads* (cons :reads nil)))
    (let ((value (funcall thunk)))
      (values value (fset:convert 'fset:set (cdr *reads*))))))

(defun %write-one (path value &rest options &key when max force keep)
  "Put VALUE at PATH. Answers the (path old new) it moved, as a list."
  (declare (ignore keep))
  (let* ((space *space*)
         (current (%read-one path)))
    (when options (%remember path options))
    (when (and (not force) (%whole-tree-p path))
      (error 'refused :at path :why "** at the root would take the whole tree"))
    (when (and when (not (fset:equal? current when)))
      (return-from %write-one nil))
    (if (%served-write space path value)
        (list (list path current value))
        (let ((stored (cond (max (%ring-push (%get (space-root space)
                                                   (p:keys path))
                                             value max))
                            ((%verbp value)
                             (%inward (%apply-verb path current value)))
                            (t (%inward value)))))
          (if *preview*
              (progn (push (cons path stored) (cdr *preview*))
                     (list (list path current stored)))
              (%commit space (list (cons path stored))))))))

;;;; Propagation: everything that read a path that moved is computed again, and
;;;; every watch over it runs, until nothing more moves.

(defun %dependents (space moved)
  (let ((acc nil)
        (paths (mapcar #'first moved)))
    (fset:do-map (path reaction (space-reactions space))
      (declare (ignore path))
      (when (some (lambda (m) (fset:contains? (reaction-deps reaction) m)) paths)
        (push reaction acc)))
    acc))

(defun %recompute (reaction)
  (multiple-value-bind (value deps) (%evaluate (reaction-thunk reaction))
    (setf (reaction-deps reaction) deps)
    (%write-one (reaction-path reaction) value)))

(defun %watching-p (pattern path)
  "A watch fires for its own path, for anything its pattern matches, and for
anything under it: watching a directory watches the subtree."
  (or (p:match pattern path :value #'%read-one)
      (and (not (p:patternp pattern)) (p:prefixp pattern path))))

(defun %run-watches (space moved)
  (let ((out nil))
    (dolist (change moved out)
      (destructuring-bind (path old new) change
        (declare (ignore old))
        (dolist (watch (space-watches space))
          (destructuring-bind (name pattern fn) watch
            (declare (ignore name))
            (when (%watching-p pattern path)
              (let ((answer (funcall fn new)))
                (when (fset:map? answer)
                  (setf out (append out (%apply answer))))))))))))

(defun %apply (map)
  "Write every entry of MAP, without propagating: the caller owns that."
  (let ((moved nil))
    (fset:do-map (path value map)
      (setf moved (append moved (%write-one path value))))
    moved))

(defun %propagate (space moved)
  (when (and moved (not *preview*) (not *propagating*))
    (let ((*propagating* t)
          (seen (fset:empty-set))
          (wave moved)
          (passes 0))
      (loop :while wave
            :do (when (> (incf passes) 100)
                  (error 'cycle :at (mapcar #'first wave)))
                (let ((next nil))
                  (dolist (reaction (%dependents space wave))
                    (let ((at (reaction-path reaction)))
                      (when (fset:contains? seen at)
                        (error 'cycle :at (list at)))
                      (setf seen (fset:with seen at))
                      (setf next (append next (%recompute reaction)))))
                  (setf next (append next (%run-watches space wave)))
                  (setf wave next))))))

(defun %changes (moved)
  (let ((out (fset:empty-map)))
    (dolist (change moved out)
      (destructuring-bind (path old new) change
        (setf out (fset:with out path (fset:seq old new)))))))

(defun %transact (map)
  "Apply a map of path to value as one change."
  (let ((moved (%apply map)))
    (if *propagating*
        (%changes moved)
        (progn (%propagate *space* moved) (%changes moved)))))

(defun %write-pattern (pattern maker &rest options)
  "Write every path PATTERN matches, with its binders bound around the value."
  (let ((moved nil)
        (variables (p:binders pattern)))
    (when (and (%whole-tree-p pattern) (not (getf options :force)))
      (error 'refused :at pattern
                      :why "** at the root would take the whole tree"))
    (dolist (found (%matches pattern))
      (multiple-value-bind (ok bindings) (p:match pattern found :value #'%read-one)
        (when ok
          (let ((value (apply maker (mapcar (lambda (v) (fset:lookup bindings v))
                                            variables))))
            (setf moved (append moved
                                (apply #'%write-one found value options)))))))
    (%propagate *space* moved)
    (%changes moved)))

(defun %mount (path backing)
  "Bind BACKING under PATH. Writing one over another stacks it: a read falls
through to the one underneath for what the top does not answer, so a machine
overrides a single leaf of a shipped provider without forking it."
  (let ((space *space*))
    (push (cons path backing) (space-mounts space))
    (fset:empty-map)))

(defun %unmount (path)
  (let ((space *space*))
    (setf (space-mounts space)
          (remove-if (lambda (m) (fset:equal? (car m) path))
                     (space-mounts space)))))

(defun %write-reactive (path thunk &rest options)
  "Put THUNK's value at PATH and remember what it read, so it is computed again
whenever one of those paths moves. A provider written here mounts instead."
  (let ((space *space*))
    (multiple-value-bind (value deps) (%evaluate thunk)
      (when (and (null value) (%backings space path))
        (%unmount path))
      (if (backing-p value)
          (%mount path value)
          ;; the reaction is registered before the value lands, so whatever
          ;; watches the commit already knows this path is computed rather than
          ;; held, and does not store it
          (progn
            (setf (space-reactions space)
                  (if (fset:empty? deps)
                      (fset:less (space-reactions space) path)
                      (fset:with (space-reactions space) path
                                 (make-reaction :path path :thunk thunk
                                                :deps deps))))
            (let ((moved (apply #'%write-one path value options)))
              (%propagate space moved)
              (%changes moved)))))))

(defmacro write (place &optional (value nil value-p) &rest options)
  "Put VALUE at the path PLACE, or apply PLACE as a transaction when it is a
map of path to value.

VALUE is an expression, and it is evaluated again whenever a path it read
changes. A pattern writes every path it matches, with its binders bound."
  (if (not value-p)
      `(%transact ,place)
      (let ((literal (and (p:pathp place) place)))
        (cond
          ((and literal (p:patternp literal))
           `(%write-pattern ,place
                            (lambda ,(p:binders literal)
                              (declare (ignorable ,@(p:binders literal)))
                              ,value)
                            ,@options))
          (t `(%write-reactive ,place (lambda () ,value) ,@options))))))

(defmacro toggle (path)
  "Write PATH the opposite of what it holds."
  `(write ,path [:toggle]))

(defun watch (path fn &key as)
  "Call FN with the new value whenever a path at or under PATH changes, and
apply the map it answers. AS names the watch, so registering it again replaces
it and re-loading a config is safe. A NIL function removes it."
  (let* ((space *space*)
         (name (or as (gensym "WATCH"))))
    (setf (space-watches space)
          (remove name (space-watches space) :key #'first :test #'equal))
    (when fn
      (setf (space-watches space)
            (append (space-watches space) (list (list name path fn)))))
    name))

(defmacro preview (&body body)
  "The changes BODY would make, without making them."
  `(let ((*preview* (cons :preview nil)))
     ,@body
     (let ((out (fset:empty-map)))
       (dolist (change (reverse (cdr *preview*)) out)
         (setf out (fset:with out (car change) (cdr change)))))))
