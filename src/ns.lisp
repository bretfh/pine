(defpackage #:pine.ns
  (:use #:cl)
  (:shadow #:read #:write #:space)
  (:local-nicknames (#:p #:pine.path))
  (:export #:space #:fresh #:*space* #:with-space
           #:read #:write #:put #:watch #:preview #:toggle #:held
           #:provider #:here #:options #:doc #:diff #:mounts #:unmount
           #:stir #:matches #:verbs
           #:server #:name #:serves #:after #:register #:unregister #:servers
           #:kept #:root
           #:raise #:lower #:raise-all #:lower-all
           #:roots #:root-at #:root-ago #:at-root #:as-of #:revert #:*roots-kept*
           #:kind #:setting #:watched #:on-commit #:on-interval #:again
           #:refused #:no-verb #:cycle #:at #:why))

(in-package #:pine.ns)
(named-readtables:in-readtable pine.path:syntax)

;;;; The whole namespace is one value in an atomic reference. A read is a slot
;;;; read of an immutable tree; a write is a compare-and-swap over a pure
;;;; function of the old space to the new one. Nothing inside a swap may do IO:
;;;; a provider's write, the store's write-through and every watch run outside.

(defstruct (space (:constructor %space) (:copier copy-space))
  (root (fset:empty-map))
  (mounts nil)                        ; (path . backing), newest first
  (reactions (fset:empty-map))        ; path -> reaction
  (watches nil)                       ; (name pattern function)
  (settings (fset:empty-map))         ; path -> the write options that outlive it
  (roots nil)                         ; the superseded roots, newest first
  (root-times nil)                    ; when each of them was superseded
  (on-commit (fset:empty-map))        ; name -> told what each commit moved
  (on-interval nil)                   ; told (path seconds) when :every is asked
  (kept (fset:empty-map))             ; server name -> what that server holds
  (moved nil))                        ; what the swap that made this space did

(defstruct (reaction (:copier nil))
  path                                ; where the value lands
  thunk                               ; how it is computed
  (deps (fset:empty-set)))            ; the paths it read last time

(defstruct (clause (:constructor %clause (pattern handlers)) (:copier nil))
  pattern
  handlers)                           ; (lambda (binders...) -> a handler map)

(defstruct (backing (:constructor %backing (clauses options)) (:copier nil))
  clauses
  options)

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

(defun fresh ()
  "A namespace of its own."
  (sento.atomic:make-atomic-reference :value (%space)))

(defparameter *roots-kept* 200
  "How many superseded roots a space keeps.")

(defparameter +settles+ 100
  "How many passes one write's wave may take before it is called a cycle.")

(defparameter +built-in-verbs+ '(:set :toggle :conj :disj :merge)
  "The verbs the tree itself knows, which a provider need not declare.")

(defvar *space* (fresh)
  "The namespace this image serves. One per image, and every thread sees the
same one.")

(defvar *servers*
  (sento.atomic:make-atomic-reference :value (fset:empty-seq))
  "Every server this image knows how to raise, in the order they were declared.
Image-wide: what pine can serve is what its code says.")

(defvar *reads* nil "Where the paths read while computing a value collect.")

(defvar *options* nil
  "The options the write being served asked for, bound around a provider's
handler. HERE says which path; this says what the write wanted of it.")

(defvar *preview* nil "When bound, writes collect here instead of being made.")

(defvar *propagating* nil)

(defvar *cascade* nil
  "Where a write made while a cascade is running on this thread collects, so it
joins that wave instead of being lost.")

(defun current ()
  "The namespace as it stands. One slot read."
  (sento.atomic:atomic-get *space*))

(defun %keeping (old next)
  "NEXT with OLD's root remembered, when the root moved, and with when it was
superseded: /was/-1h asks about an hour ago, and a count of writes is no clock."
  (unless (eq (space-root old) (space-root next))
    (let ((kept (cons (space-root old) (space-roots old)))
          (times (cons (get-universal-time) (space-root-times old))))
      (when (> (length kept) *roots-kept*)
        (setf kept (subseq kept 0 *roots-kept*)
              times (subseq times 0 *roots-kept*)))
      (setf (space-roots next) kept
            (space-root-times next) times)))
  next)

(defun %swap (fn)
  "Replace the space with FN's answer, retrying until it lands. FN is pure."
  (sento.atomic:atomic-swap *space* (lambda (old) (%keeping old (funcall fn old)))))

(defun roots ()
  "The superseded roots, newest first."
  (space-roots (current)))

(defun root-at (n)
  "The root as of N supersessions ago, or NIL."
  (nth n (space-roots (current))))

(defun root-ago (seconds)
  "How many supersessions back the newest root at least SECONDS old is, or the
oldest still kept when nothing is that old."
  (let ((cutoff (- (get-universal-time) seconds))
        (times (space-root-times (current))))
    (or (position-if (lambda (at) (<= at cutoff)) times)
        (max 0 (1- (length times))))))

(defmacro with-space ((&optional (space '(fresh))) &body body)
  "Run BODY against SPACE, so a test or a tool can hold one of its own. Sets
the variable rather than binding it: the work pine does happens on other
threads, which a binding would not reach."
  (let ((previous (gensym "PREVIOUS")))
    `(let ((,previous *space*))
       (setf *space* ,space)
       (unwind-protect (progn ,@body)
         (setf *space* ,previous)))))

(defun %inward (value)
  "VALUE with its keys as the names the tree stores and its nothings dropped.
Answers VALUE itself when nothing had to move, so it stays shared."
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

(defun %directory-p (map)
  "Whether MAP names places under a path rather than being a transaction."
  (not (p:transactionp map)))

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

(defun here ()
  "The path the clause running now was asked about."
  (p:here))

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

(defun %mounted-at (path)
  "True when a provider is mounted at PATH itself."
  (loop :for mount :in (space-mounts (current))
        :thereis (fset:equal? (car mount) path)))

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
                (let ((p:*here* path))
                  (return (apply (clause-handlers clause)
                                 (mapcar (lambda (v) (fset:lookup bindings v))
                                         (p:binders (clause-pattern clause))))))))))

;;;; A clause says either what a path is, or how it is written and read.
;;;;
;;;;   :read   the provider is the value; the world behind it is the storage
;;;;   :write  the provider takes the write, as an effect on that world
;;;;   :out    the tree holds the value, and this is how it reads
;;;;   :in     the tree takes the write, and this is what lands

(defun %served-read (space path)
  "Answer (values value t) when a mounted provider answers a read of PATH."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :when handlers
          :do (let ((read (fset:lookup handlers :read))
                    (out (fset:lookup handlers :out)))
                (cond (read (return (values (funcall read) t)))
                      (out (let ((held (%get (space-root space) (p:keys path))))
                             (unless (null held)
                               (return (values (funcall out held) t)))))))
        :finally (return (values nil nil))))

(defun held (path)
  "The value in the tree at PATH, with no provider between it and the caller:
what a clause's :IN put there, rather than what its :OUT makes of it."
  (%get (space-root (current)) (p:keys path)))

(defun %servedp (space path)
  "True when a mounted provider reads PATH."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :thereis (and handlers (fset:lookup handlers :read) t)))

(defun %served-write (space path value)
  "Give VALUE to the provider serving PATH.

Answers :DONE when a provider took it, or (values :STORE V) when a clause says
what a written value becomes and V is to land in the tree. A clause declaring
only :VERBS lets an ordinary value fall through."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :when handlers
          :do (let ((verbs (fset:lookup handlers :verbs))
                    (write (fset:lookup handlers :write))
                    (in (fset:lookup handlers :in)))
                (cond ((and (%verbp value) verbs
                            (fset:lookup verbs (%verb-name value)))
                       (apply (fset:lookup verbs (%verb-name value))
                              (%verb-args value))
                       (return :done))
                      ;; a clause that takes any verb is given the whole of it,
                      ;; which is what a path standing for another one does
                      ((and (%verbp value) verbs (fset:lookup verbs t))
                       (funcall (fset:lookup verbs t) value)
                       (return :done))
                      ;; the tree answers its own verbs, so a clause that
                      ;; says only what a path is for does not have to list
                      ;; them again
                      ((and (%verbp value)
                            (not (member (%verb-name value) +built-in-verbs+)))
                       (error 'no-verb :at path :why (%verb-name value)))
                      ((%verbp value) (return nil))
                      ((or in (fset:lookup handlers :at))
                       (let ((where (fset:lookup handlers :at)))
                         (return (values :store
                                         (if in (funcall in value) value)
                                         (when where (funcall where))))))
                      (write (funcall write value) (return :done))
                      ((fset:lookup handlers :read)
                       ;; :FORCE is what the refusal tells the caller to pass,
                       ;; so it means what it says: the value lands in the tree,
                       ;; over the provider, and reading answers it until the
                       ;; provider is asked again
                       (if (getf *options* :force)
                           (return nil)
                           (error 'refused :at path
                                  :why "no provider writes it")))))
        :finally (return nil)))

(defun verbs (path)
  "The verbs PATH takes, as a set of names, with T when it takes any. The
tree's own are in it whether a provider mentioned them or not."
  (let ((acc (fset:convert 'fset:set +built-in-verbs+)))
    (dolist (backing (%backings (current) path) acc)
      (let* ((handlers (%handlers backing path))
             (declared (and handlers (fset:lookup handlers :verbs))))
        (when declared
          (fset:do-map (name fn declared)
            (declare (ignore fn))
            (setf acc (fset:with acc name))))))))

(defun mounts ()
  "Where a provider is bound, newest first."
  (mapcar #'car (space-mounts (current))))

(defclass server ()
  ((name :initarg :name :reader name
         :documentation "What it is called, as a keyword.")
   (serves :initarg :serves :initform nil :reader serves
           :documentation "The paths it puts at the tree, so LOWER can take
them off and so a reader can find out what a subtree belongs to.")
   (after :initarg :after :initform nil :reader after
          :documentation "The servers that must be up before this one, by name.
What says nothing is raised in the order it was declared in."))
  (:documentation "Something that serves a subtree: a subclass, a RAISE method
and a REGISTER."))

(defun register (server)
  "Say that SERVER is one of the things pine serves, and answer it. Registering
a name again replaces it, so loading a file twice is safe."
  (sento.atomic:atomic-swap
   *servers*
   (lambda (all)
     (let ((at (position (name server) (fset:convert 'list all) :key #'name)))
       (if at (fset:with all at server) (fset:with-last all server)))))
  server)

(defun unregister (name)
  "Take the server called NAME out of the list. It is not lowered: what it put
at the tree is the space's, and this only says pine no longer knows how to
raise it."
  (sento.atomic:atomic-swap
   *servers*
   (lambda (all)
     (fset:convert 'fset:seq
                   (remove name (fset:convert 'list all) :key #'name))))
  name)

(defun %in-order (all)
  "ALL, each after everything it says it is after."
  (let ((out nil) (doing (fset:empty-set)))
    (labels ((visit (s)
               (unless (member s out)
                 (when (fset:contains? doing (name s))
                   (error 'cycle :at (list (name s))))
                 (setf doing (fset:with doing (name s)))
                 (dolist (need (after s))
                   (let ((it (find need all :key #'name)))
                     (when it (visit it))))
                 (setf doing (fset:less doing (name s)))
                 (push s out))))
      (mapc #'visit all))
    (nreverse out)))

(defun servers (&optional name)
  "Every server, in the order they must be raised, or the one called NAME."
  (let ((all (%in-order (fset:convert 'list (sento.atomic:atomic-get *servers*)))))
    (if name (find name all :key #'name) all)))

(defun root ()
  "The tree as it stands, as one object. A write supersedes rather than
mutates, so EQ on it answers whether anything anywhere moved."
  (space-root (current)))

(defun kept (&optional name)
  "What the servers this space raised are holding: the whole map, or NAME's.
A supervisor's table, a connection, a subscription -- what cannot be a value."
  (let ((all (space-kept (current))))
    (if name (fset:lookup all name) all)))

(defun (setf kept) (value name)
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-kept next) (if (null value)
                                         (fset:less (space-kept old) name)
                                         (fset:with (space-kept old) name value))
                   (space-moved next) nil)
             next)))
  value)

(defgeneric raise (server &key &allow-other-keys)
  (:documentation "Put SERVER's paths at the tree.

Answers whatever the caller may have to hold on to, and NIL where there is
nothing. A server takes the keywords it needs and ignores the rest, so one call
raises all of them.")
  (:method ((name symbol) &rest args &key &allow-other-keys)
    "Raise the server called NAME, and keep what it answered."
    (let ((it (servers name)))
      (unless it (error 'refused :at name :why "there is no server by that name"))
      (setf (kept name) (apply #'raise it args)))))

(defgeneric lower (server)
  (:documentation "Take SERVER off the tree.")
  (:method ((s server))
    (dolist (path (serves s)) (%write-one path nil))
    nil)
  (:method ((name symbol))
    (let ((it (servers name)))
      (when it (lower it)))))

(defun raise-all (&rest args &key &allow-other-keys)
  "Raise every server, in order, and answer a map of name to what each said."
  (dolist (s (servers) (kept))
    (apply #'raise (name s) args)))

(defun lower-all ()
  "Take every server off, in the reverse of the order they went up."
  (dolist (s (reverse (servers))) (lower s))
  nil)

(defun doc (path)
  "What PATH is, and what it takes: the :DOC of the clause serving it, or of
the provider holding that clause. A :DOC that is a handler is called."
  (flet ((said (it) (if (functionp it) (funcall it) it)))
    (let ((backings (%backings (current) path)))
      (or (loop :for backing :in backings
                :for handlers = (%handlers backing path)
                :thereis (and handlers (said (fset:lookup handlers :doc))))
          (loop :for backing :in backings
                :thereis (said (fset:lookup (backing-options backing) :doc)))))))

(defun %served-watch (space path listening)
  "Tell the provider serving PATH that someone started or stopped listening."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :for fn = (and handlers (fset:lookup handlers :watch))
        :when fn
          :do (return (funcall fn listening))))

(defun %served-children (space path)
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :for fn = (and handlers (fset:lookup handlers :ls))
        :when fn
          :do (return (mapcar (lambda (n) (p:child path n)) (funcall fn)))
        :finally (return nil)))

;;;; Reading. Inside a write body or a handler a read is recorded, and that
;;;; recording is the whole of the reactive system.
;;;;
;;;; A provider may declare {:scope /}, and then a leaf with no value under one
;;;; of its names reads the same leaf there: buffer-local is where the value is.

(defun %ringp (path)
  (and (setting path :max) t))

(defun %index (text)
  (multiple-value-bind (n used) (parse-integer text :junk-allowed t)
    (and n (= used (length text)) n)))

(defun %scoped (space path)
  "Where PATH falls back to, or NIL. A provider mounted at /buf declaring
{:scope /} makes /buf/foo.py/tab-width fall back to /tab-width."
  (loop :for (at . backing) :in (space-mounts space)
        :for scope = (fset:lookup (backing-options backing) :scope)
        :when (and scope
                   (p:prefixp at path)
                   (= (p:segment-count path) (+ 2 (p:segment-count at))))
          :do (return (p:child scope (p:leaf path)))))

(defun %fallback (space path default)
  "What PATH reads when nothing is there. Only a leaf falls back: a directory
at the scope's root is that directory, not this path's value."
  (let ((up (%scoped space path)))
    (if (null up)
        default
        (let ((value (%get (space-root space) (p:keys up))))
          (when *reads* (push up (cdr *reads*)))
          (if (or (null value) (fset:map? value)) default value)))))

(defun %read-one (path &optional default)
  (let ((space (current)))
    (when *reads* (push path (cdr *reads*)))
    (multiple-value-bind (value served) (%served-read space path)
      (cond
        (served (if (null value) default value))
        ;; a ring reads as its newest entry, and each entry has a place
        ((%ringp path)
         (let ((seq (%get (space-root space) (p:keys path))))
           (if (fset:seq? seq) (fset:lookup seq 0) default)))
        ((and (not (p:rootp path)) (%ringp (p:parent path)) (%index (p:leaf path)))
         (let ((seq (%get (space-root space) (p:keys (p:parent path)))))
           (if (fset:seq? seq)
               (or (fset:lookup seq (%index (p:leaf path))) default)
               default)))
        (t (let ((value (%get (space-root space) (p:keys path))))
             (if (null value) (%fallback space path default) value)))))))

(defun %children (space path)
  "PATH's children: the tree's, or the provider's."
  (let ((node (%get (space-root space) (p:keys path))))
    (when (and (%ringp path) (fset:seq? node))
      (return-from %children
        (loop :for i :from 0 :below (fset:size node)
              :collect (p:child path i))))
    (or (when (and (fset:map? node) (%directory-p node))
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
  "True when something is there: a value, a provider that answers, or children."
  (or (p:rootp path)
      (not (null (%get (space-root space) (p:keys path))))
      (and (not (p:rootp path))
           (%ringp (p:parent path))
           (let ((n (%index (p:leaf path)))
                 (seq (%get (space-root space) (p:keys (p:parent path)))))
             (and n (fset:seq? seq) (< n (fset:size seq)))))
      (nth-value 1 (%served-read space path))
      (not (null (%children space path)))))

(defun %candidates (space pattern prefix)
  "Where a walk of PATTERN can go from PREFIX: the children that exist, plus
the literal segment the pattern asks for next, which needs nothing enumerated."
  (let* ((children (%children space prefix))
         (wanted (p:named-at pattern (p:segment-count prefix)))
         (extra (remove-if (lambda (c) (some (lambda (k) (fset:equal? c k)) children))
                           (mapcar (lambda (name) (p:child prefix name)) wanted))))
    (append extra children)))

(defun %matches (pattern)
  "Every path that exists now and matches PATTERN."
  (let ((space (current))
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

(defun at-root (root path)
  "What PATH held in ROOT."
  (%get root (p:keys path)))

(defun diff (was now)
  "Every leaf under WAS and NOW that does not hold the same value, as a map
from the path under NOW to [what WAS had, what NOW has]."
  (let ((space (current))
        (out (fset:empty-map)))
    (labels ((names (root)
               (mapcar #'p:leaf (%children space root)))
             (walk (a b depth)
               (when (< depth 64)
                 (let ((children (remove-duplicates (append (names a) (names b))
                                                    :test #'equal)))
                   (if children
                       (dolist (name children)
                         (walk (p:child a name) (p:child b name) (1+ depth)))
                       (let ((before (%read-one a))
                             (after (%read-one b)))
                         (unless (fset:equal? before after)
                           (setf out (fset:with out b
                                                (fset:seq before after))))))))))
      (walk was now 0))
    out))

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
  "A with B's keys in it, all the way down: a map that says one key leaves the
rest of the directory alone, and a key written as nothing goes away."
  (let ((out (if (fset:map? a) a (fset:empty-map))))
    (fset:do-map (k v b)
      (let* ((key (if (stringp k) (p:key k) k))
             (was (fset:lookup out key)))
        (setf out (cond ((null v) (fset:less out key))
                        ((and (fset:map? was) (fset:map? v))
                         (fset:with out key (%merge was v)))
                        (t (fset:with out key (%inward v)))))))
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

(defun options ()
  "What the write being served asked for: (:keep t :max 60 ...), or NIL."
  *options*)

(defun on-interval ()
  "What this space tells an :EVERY to, or NIL."
  (space-on-interval (current)))

(defun (setf on-interval) (fn)
  "Call FN with (path seconds) whenever a write asks to be computed again on an
interval. The tree owns no clock: whoever has a scheduler hangs it here."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-on-interval next) fn
                   (space-moved next) nil)
             next)))
  fn)

(defun on-commit (&optional name)
  "What this space tells each commit to: the whole map, or NAME's."
  (let ((all (space-on-commit (current))))
    (if name (fset:lookup all name) all)))

(defun (setf on-commit) (fn name)
  "Call FN with the list of (path old new) each commit moves, under NAME.
Named the way a watch is, so registering again replaces and more than one thing
can listen."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-on-commit next)
                   (if fn
                       (fset:with (space-on-commit old) name fn)
                       (fset:less (space-on-commit old) name))
                   (space-moved next) nil)
             next)))
  fn)

(defun kind (path)
  "Where the value at PATH comes from.

:LIVE   a provider reads it; the world it came from is the storage
:DERIVED an expression computed it from other paths, so it is computed again
:HELD   someone wrote it, and nothing else determines it"
  (let ((space (current)))
    (cond ((%servedp space path) :live)
          ((let ((r (fset:lookup (space-reactions space) path)))
             (and r (not (fset:empty? (reaction-deps r)))))
           :derived)
          (t :held))))

(defun %setting-in (space path key &optional default)
  "The write option KEY that PATH carries in SPACE."
  (let ((options (fset:lookup (space-settings space) path)))
    (if (and options (fset:domain-contains? options key))
        (fset:lookup options key)
        default)))

(defun setting (path key &optional default)
  "The write option KEY that PATH was given, or DEFAULT."
  (%setting-in (current) path key default))

(defun %settings-with (settings path key value)
  "SETTINGS with PATH's option KEY set. Pure."
  (fset:with settings path
             (fset:with (or (fset:lookup settings path) (fset:empty-map))
                        key value)))

(defun %remembering (space path options)
  "SPACE with the write options from OPTIONS that outlive the write. Pure."
  (let ((settings (space-settings space)))
    (loop :for (key value) :on options :by #'cddr
          :when (member key '(:keep :max :every :on))
            :do (setf settings (%settings-with settings path key value)))
    (if (eq settings (space-settings space))
        space
        (let ((next (copy-space space)))
          (setf (space-settings next) settings)
          next))))

(defun (setf setting) (value path key)
  "Say that PATH carries the write option KEY, without writing the path."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-settings next)
                   (%settings-with (space-settings old) path key value)
                   (space-moved next) nil)
             next)))
  value)

(defun %ring-push (current value limit)
  (let ((seq (fset:with-first (if (fset:seq? current) current (fset:empty-seq))
                              value)))
    (if (> (fset:size seq) limit) (fset:subseq seq 0 limit) seq)))

(defun %whole-tree-p (path)
  (let ((text (p:text path)))
    (or (string= text "/**") (uiop:string-prefix-p "/**/" text))))

(defun %stored (path was value max)
  "What lands at PATH, given WAS is what is there. Pure."
  (cond (max (%ring-push was value max))
        ((%verbp value) (%inward (%apply-verb path was value)))
        ;; a map merges into a directory; a transaction replaces
        ((and (fset:map? value) (fset:map? was)
              (%directory-p value) (%directory-p was))
         (%merge was value))
        (t (%inward value))))

(defun %committing (old path value options)
  "OLD with VALUE at PATH. Pure, so a swap may run it again."
  (destructuring-bind (&key when max &allow-other-keys) options
    (let* ((space (if options (%remembering old path options) old))
           (root (space-root space))
           (was (%get root (p:keys path)))
           ;; a ring is a property of the path, so a later write needs no :max
           (bound (or max (%setting-in space path :max))))
      (if (and when (not (fset:equal? was when)))
          (let ((next (copy-space space)))
            (setf (space-moved next) nil)
            next)
          (let ((stored (%stored path was value bound))
                (next (copy-space space)))
            (cond ((fset:equal? was stored)
                   (setf (space-moved next) nil))
                  (t (setf (space-root next) (%put root (p:keys path) stored)
                           (space-moved next) (list (list path was stored)))))
            next)))))

(defun %guarded (root guard)
  "Whether everything GUARD says a path still holds is what ROOT holds."
  (let ((ok t))
    (when guard
      (fset:do-map (path value guard)
        (unless (fset:equal? value (%get root (p:keys path)))
          (setf ok nil))))
    ok))

(defun %commit (changes &optional guard)
  "Put CHANGES, a list of (path . value), in as one step: one new root, so
nothing ever observes the system half changed. Answers what moved.

GUARD is a map of path to the value it must still hold; nothing lands unless
every one of them does, and the test is inside the swap."
  (let ((new (%swap (lambda (old)
                      (let ((root (space-root old))
                            (moved nil))
                        (when (%guarded root guard)
                          (dolist (change changes)
                            (destructuring-bind (path . value) change
                              (let* ((was (%get root (p:keys path)))
                                     (stored (%stored path was value
                                                      (%setting-in old path :max))))
                                (unless (fset:equal? was stored)
                                  (setf root (%put root (p:keys path) stored))
                                  (push (list path was stored) moved))))))
                        (let ((next (copy-space old)))
                          (setf (space-root next) root
                                (space-moved next) (nreverse moved))
                          next))))))
    (%told (space-moved new))))

(defun %told (moved)
  "Tell every commit listener what moved, outside the swap."
  (when moved
    (fset:do-map (name fn (space-on-commit (current)))
      (declare (ignore name))
      (funcall fn moved)))
  moved)

(defun %evaluate (thunk)
  "Run THUNK, answering (values value paths-it-read)."
  (let ((*reads* (cons :reads nil)))
    (let ((value (funcall thunk)))
      (values value (fset:convert 'fset:set (cdr *reads*))))))

(defun %emptying-a-ring-p (space path value)
  "Whether writing VALUE at PATH would empty a kept ring that had something in
it."
  (and (null value)
       (%setting-in space path :keep)
       (let ((was (%get (space-root space) (p:keys path))))
         (and (fset:seq? was) (not (fset:empty? was))))))

(defun %write-one (path value &rest options
                   &key when max force keep &allow-other-keys)
  "Put VALUE at PATH. Answers the (path old new) it moved, as a list."
  (declare (ignore when keep))
  (when (and (not force) (%whole-tree-p path))
    (error 'refused :at path :why "** at the root would take the whole tree"))
  (let ((space (current)))
    (when (and (not force) (%emptying-a-ring-p space path value))
      (error 'refused :at path :why "it would empty a ring the file keeps"))
    ;; a provider's IO cannot run inside a swap that may be retried
    (multiple-value-bind (served normalized where)
        (let ((*options* options)) (%served-write space path value))
      (case served
        (:done (return-from %write-one
                 (list (list path (%read-one path) value))))
        (:store (setf value normalized)
                (when where (setf path where)))))
    (when *preview*
      (let* ((was (%get (space-root space) (p:keys path)))
             (stored (%stored path was value
                              (or max (%setting-in space path :max)))))
        (push (list path was stored) (cdr *preview*))
        (return-from %write-one (list (list path was stored)))))
    (%told (space-moved (%swap (lambda (old)
                                 (%committing old path value options)))))))

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
    ;; what it read may have changed with the value, so it is put back whole
    (let* ((path (reaction-path reaction))
           (also (setting path :on))
           (interval (setting path :every)))
      (%remember-reaction path (reaction-thunk reaction)
                          (%also-on deps also)
                          (or interval also))
      (%write-one path value))))

(defun %watching-p (pattern path)
  "A watch fires for its own path, for anything its pattern matches, and for
anything under it: watching a directory watches the subtree."
  (or (p:match pattern path :value #'%read-one)
      (and (not (p:patternp pattern)) (p:prefixp pattern path))))

(defun watched (path)
  "True when some watch would hear about a change at PATH."
  (loop :for watch :in (space-watches (current))
        :thereis (%watching-p (second watch) path)))

(defun %run-watches (space moved)
  (let ((out nil))
    (dolist (change moved out)
      (destructuring-bind (path old new) change
        (declare (ignore old))
        (dolist (watch (space-watches space))
          (destructuring-bind (name pattern fn) watch
            (declare (ignore name))
            (when (%watching-p pattern path)
              ;; HERE answers which path moved, as it does inside a provider
              (let ((answer (let ((p:*here* path)) (funcall fn new))))
                (when (fset:map? answer)
                  (setf out (append out (%apply answer))))))))))))

(defun %ordered (map)
  "MAP's entries, deepest path first and the later name of two at the same
depth before the earlier. A value needs no order; two verbs in one map do:

  {/buf/${buf}/text [:newline] /buf/${buf} [:indent-line]}

splits the line before it indents it."
  (let ((acc nil))
    (fset:do-map (path value map) (push (cons path value) acc))
    (sort acc (lambda (a b)
                (let ((da (p:segment-count (car a)))
                      (db (p:segment-count (car b))))
                  (if (= da db)
                      (string> (p:text (car a)) (p:text (car b)))
                      (> da db)))))))

(defun %apply (map &optional guard)
  "Write every entry of MAP as one change, without propagating.

A path a provider serves is given to the provider; everything else lands in one
swap. GUARD says what has to still be true for any of it to land."
  (let ((space (current))
        (moved nil)
        (staged nil))
    (loop :for (path . value) :in (%ordered map)
          :do (multiple-value-bind (served normalized where)
                  (unless *preview* (%served-write space path value))
                (case served
                  (:done (setf moved
                               (append moved
                                       (list (list path (%read-one path) value)))))
                  (:store (push (cons (or where path) normalized) staged))
                  (t (push (cons path value) staged)))))
    (setf staged (nreverse staged))
    (append moved
            (if *preview*
                (loop :for (path . value) :in staged
                      :append (%write-one path value))
                (%commit staged guard)))))

(defun %expand (moved)
  "MOVED, with what landed under each map in it: (write /buf/notes {:mode
:lisp}) and (write /buf/notes/mode :lisp) are the same change."
  (let ((out nil))
    (labels ((walk (change)
               (destructuring-bind (path old new) change
                 (push change out)
                 (when (and (fset:map? new) (%directory-p new))
                   (fset:do-map (key value new)
                     (let ((was (and (fset:map? old) (fset:lookup old key))))
                       (unless (fset:equal? was value)
                         (walk (list (p:child path (p:name key)) was value)))))))))
      (mapc #'walk moved))
    (nreverse out)))

(defun %propagate (moved)
  "Compute again everything that read a path that moved, then run the watches
over it, until nothing more moves.

Runs on the caller's thread, after the swap and never inside one, because both
a reaction and a watch may write. A write made while this runs joins the wave
rather than starting a cascade of its own. *PROPAGATING* is per thread: two
threads each carrying their own change is the normal case.

A cycle is a wave that does not settle, and +SETTLES+ is where waiting stops."
  (cond ((or (null moved) *preview*) nil)
        (*propagating*
         (when *cascade*
           (setf (cdr *cascade*) (append (cdr *cascade*) moved))))
        (t
         (let* ((*propagating* t)
                (*cascade* (cons :cascade nil))
                (wave (%expand moved))
                (passes 0))
           (loop :while wave
                 :do (when (> (incf passes) +settles+)
                       (error 'cycle :at (mapcar #'first wave)))
                     (let ((next nil)
                           (space (current)))
                       (dolist (reaction (%dependents space wave))
                         (setf next (append next (%recompute reaction))))
                       (setf next (append next (%run-watches space wave)))
                       (setf next (append next (cdr *cascade*)))
                       (setf (cdr *cascade*) nil)
                       (setf wave (%expand next))))))))

(defun %stirred (space at)
  "What to announce when the world under AT has moved: AT, and every path under
it that something read."
  (let ((paths (list at)))
    (fset:do-map (path reaction (space-reactions space))
      (declare (ignore path))
      (fset:do-set (dep (reaction-deps reaction))
        (when (and (p:prefixp at dep)
                   (notany (lambda (p) (fset:equal? p dep)) paths))
          (push dep paths))))
    paths))

(defun stir (at)
  "Say that what a provider under AT reads has moved: everything that read a
path under AT is computed again, as though the value had been written. What a
provider's :ON does when the world behind it speaks."
  (let* ((space (current))
         (moved (mapcar (lambda (path) (list path nil (%read-one path)))
                        (%stirred space at))))
    (%propagate moved)
    moved))

(defun %changes (moved)
  (let ((out (fset:empty-map)))
    (dolist (change moved out)
      (destructuring-bind (path old new) change
        (setf out (fset:with out path (fset:seq old new)))))))

(defun %transact (map &optional guard)
  "Apply a map of path to value as one change."
  (let ((moved (%apply map guard)))
    (if *propagating*
        (%changes moved)
        (progn (%propagate moved) (%changes moved)))))

(defun matches (pattern)
  "Every path PATTERN names now: what a write to it would touch. A run of
literal segments at the end is where the write lands rather than something it
has to find, so (write /win/*/weight 1) reaches a window with no weight yet."
  (or (p:expansions pattern)
      (multiple-value-bind (head tail) (p:literal-tail pattern)
        (if tail
            (mapcar (lambda (found) (apply #'p:path found tail)) (matches head))
            (%matches pattern)))))

(defun %write-pattern (pattern maker &rest options)
  "Write every path PATTERN matches, with its binders bound around the value."
  (let ((moved nil)
        (variables (p:binders pattern)))
    (when (and (%whole-tree-p pattern) (not (getf options :force)))
      (error 'refused :at pattern
                      :why "** at the root would take the whole tree"))
    (dolist (found (matches pattern))
      (multiple-value-bind (ok bindings) (p:match pattern found :value #'%read-one)
        (when ok
          (let ((value (apply maker (mapcar (lambda (v) (fset:lookup bindings v))
                                            variables))))
            (setf moved (append moved
                                (apply #'%write-one found value options)))))))
    (%propagate moved)
    (%changes moved)))

(defun %told-by (path)
  "The name of the watch that carries a provider's :ON to PATH."
  (list :on (p:text path)))

(defun %mount (path backing)
  "Bind BACKING under PATH. Writing one over another stacks it: a read falls
through to the one underneath for what the top does not answer. A backing with
an :ON is told by the world instead of polled."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-mounts next) (cons (cons path backing)
                                             (space-mounts old))
                   (space-moved next) nil)
             next)))
  (let ((on (fset:lookup (backing-options backing) :on)))
    (when on
      (watch on (lambda (value) (declare (ignore value)) (stir path) nil)
             :as (%told-by path))))
  (let ((up (fset:lookup (backing-options backing) :mounted)))
    (when up (funcall up path)))
  (fset:empty-map))

(defun unmount (path)
  "Take the provider mounted at PATH off, and clear what it stood over."
  (%unmount path)
  (put path nil (list :force t))
  nil)

(defun %unmount (path)
  (watch path nil :as (%told-by path))
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-mounts next)
                   (remove-if (lambda (m) (fset:equal? (car m) path))
                              (space-mounts old))
                   (space-moved next) nil)
             next))))

(defun %remember-reaction (path thunk deps &optional triggered)
  "Note how PATH is computed, or that it no longer is. TRIGGERED keeps the
expression even when it read nothing: :EVERY and :ON are not value reads."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-reactions next)
                   (if (and (fset:empty? deps) (not triggered))
                       (fset:less (space-reactions old) path)
                       (fset:with (space-reactions old) path
                                  (make-reaction :path path :thunk thunk
                                                 :deps deps)))
                   (space-moved next) nil)
             next))))

(defun %also-on (deps also)
  "DEPS with the paths ALSO names in it. :ON is how a write says it depends on
something it did not read: a subscription's output, a poll's tick."
  (let ((out deps))
    (dolist (path (cond ((null also) nil)
                        ((p:pathp also) (list also))
                        ((fset:set? also) (fset:convert 'list also))
                        ((fset:seq? also) (fset:convert 'list also))
                        ((listp also) also)
                        (t (list also)))
                  out)
      (when (p:pathp path) (setf out (fset:with out path))))))

(defun %write-reactive (path thunk &rest options)
  "Put THUNK's value at PATH and remember what it read, so it is computed again
whenever one of those paths moves. A provider written here mounts instead."
  (multiple-value-bind (value deps) (%evaluate thunk)
    (setf deps (%also-on deps (getf options :on)))
    ;; writing nothing where a provider is mounted takes it off, but only where
    ;; the provider is what answers a read there
    (when (and (null value) (%mounted-at path) (%servedp (current) path))
      (%unmount path))
    (if (backing-p value)
        (%mount path value)
        ;; the reaction is registered before the value lands, so what watches
        ;; the commit knows this path is computed rather than held
        (progn
          (%remember-reaction path thunk deps
                              (or (getf options :every) (getf options :on)))
          (let ((moved (apply #'%write-one path value options))
                (seconds (getf options :every))
                (told (on-interval)))
            (when (and seconds told)
              (funcall told path seconds))
            (%propagate moved)
            (%changes moved))))))

(defun again (path)
  "Compute PATH's expression again, as though something it read had moved. A
path nobody wrote as an expression has nothing to compute."
  (let ((reaction (fset:lookup (space-reactions (current)) path)))
    (when reaction
      (let ((moved (%recompute reaction)))
        (%propagate moved)
        (%changes moved)))))

(defun put (path value &optional options)
  "Put VALUE at PATH with OPTIONS, all three already in hand. WRITE is a macro
over an expression; this is for a value that arrived as data."
  (let ((moved (apply #'%write-one path value options)))
    (%propagate moved)
    (%changes moved)))

(defmacro write (place &optional (value nil value-p) &rest options)
  "Put VALUE at the path PLACE, or apply PLACE as a transaction when it is a
map of path to value.

VALUE is an expression, evaluated again whenever a path it read changes. A
pattern writes every path it matches, with its binders bound. A transaction
takes :WHEN a map of path to the value it must still hold."
  (cond
    ((not value-p) `(%transact ,place))
    ((eq value :when) `(%transact ,place ,(first options)))
    (t
      (let ((literal (and (p:pathp place) place)))
        (cond
          ((and literal (p:patternp literal))
           `(%write-pattern ,place
                            (lambda ,(p:binders literal)
                              (declare (ignorable ,@(p:binders literal)))
                              ,value)
                            ,@options))
          (t `(%write-reactive ,place (lambda () ,value) ,@options)))))))

(defmacro toggle (path)
  "Write PATH the opposite of what it holds."
  `(write ,path [:toggle]))

(defun watch (path fn &key as)
  "Call FN with the new value whenever a path at or under PATH changes, and
apply the map it answers. AS names the watch, so registering it again replaces
it and re-loading a config is safe. A NIL function removes it."
  (let ((name (or as (gensym "WATCH"))))
    (%swap (lambda (old)
             (let ((next (copy-space old))
                   (rest (remove name (space-watches old)
                                 :key #'first :test #'equal)))
               (setf (space-watches next)
                     (if fn (append rest (list (list name path fn))) rest)
                     (space-moved next) nil)
               next)))
    (%served-watch (current) path (and (watched path) t))
    name))

(defmacro preview (&body body)
  "The changes BODY would make, without making them: the map WRITE would have
answered, path to [old new]."
  `(let ((*preview* (cons :preview nil)))
     ,@body
     (%changes (reverse (cdr *preview*)))))

(defun %ago (text)
  "The seconds back a relative time names -- -30s -5m -1h -2d -- or NIL."
  (when (and (> (length text) 1) (char= #\- (char text 0)))
    (let* ((body (subseq text 1))
           (unit (char body (1- (length body))))
           (n (ignore-errors (parse-integer body :end (1- (length body))))))
      (when n
        (case unit
          (#\s n)
          (#\m (* 60 n))
          (#\h (* 3600 n))
          (#\d (* 86400 n)))))))

(defun as-of (text)
  "Which root TEXT names: a number is how many supersessions back, and -1h is
the newest root that old."
  (let ((ago (%ago text)))
    (if ago
        (root-ago ago)
        (or (parse-integer text :junk-allowed t) 0))))

(defun revert (n)
  "Put the root as of N back, which is one write of the whole tree."
  (let ((root (root-at n)))
    (when root
      (write / (fset:seq :set root) :force t)
      n)))

(defun history-provider ()
  (provider
   (/history
    (fset:map
     (:read (lambda ()
              (fset:convert 'fset:seq
                            (loop :for i :from 0
                                  :for root :in (roots)
                                  :collect (fset:map (:n i) (:root root))))))
     (:verbs (fset:map (:revert (lambda (n) (revert n)))))
     (:doc "the roots, newest first")))))

(defun was-provider ()
  (provider
   (/was/?n/?@rest
    (fset:map
     (:read (lambda ()
              (let ((root (root-at (as-of n))))
                (when root (at-root root (apply #'p:path rest))))))
     (:ls (lambda ()
            (let* ((root (root-at (as-of n)))
                   (value (and root (at-root root (apply #'p:path rest)))))
              (when (fset:map? value)
                (loop :for key :in (fset:convert 'list (fset:domain value))
                      :collect (p:name key))))))
     (:doc "what a path held as of a root")))
   (/was (fset:map (:doc "the tree as of a root: /was/${n}/any/path")))))

(defclass roots-server (server) ()
  (:default-initargs :name :roots :serves (list /history /was))
  (:documentation "The superseded roots, at /history and /was. They are the
space's and not a file's, so a pine with no store has its history."))

(defmethod raise ((s roots-server) &key &allow-other-keys)
  (declare (ignore s))
  (write /history (history-provider))
  (write /was (was-provider))
  nil)

(register (make-instance 'roots-server))
