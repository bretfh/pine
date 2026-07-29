(defpackage #:pine.ns
  (:use #:cl)
  (:shadow #:read #:write #:space)
  (:local-nicknames (#:p #:pine.path))
  (:export #:space #:fresh #:*space* #:with-space
           #:read #:write #:watch #:preview #:toggle #:held
           #:provider #:here #:doc #:diff #:mounts #:stir
           #:kind #:setting #:watched #:on-commit
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

;;;; The whole namespace is one value, and it lives in an atomic reference.
;;;;
;;;; Reading is a slot read: no message, no wait, safe from inside an actor's
;;;; receive, because what comes back is an immutable fset structure -- a whole
;;;; consistent tree, never a torn one. Writing is a compare-and-swap over a
;;;; pure function of the old space to the new one, so the read, the verb and
;;;; the write are one step rather than three that can be interleaved.
;;;;
;;;; The function handed to a swap may be run again against a space another
;;;; thread committed in the meantime. Nothing inside one may do IO: a
;;;; provider's write, the store's write-through and every watch run outside.

(defstruct (space (:constructor %space) (:copier copy-space))
  (root (fset:empty-map))
  (mounts nil)                        ; (path . backing), newest first
  (reactions (fset:empty-map))        ; path -> reaction
  (watches nil)                       ; (name pattern function)
  (settings (fset:empty-map))         ; path -> the write options that outlive it
  (on-commit nil)                     ; told what each commit moved
  (moved nil))                        ; what the swap that made this space did

(defun fresh ()
  "A namespace of its own."
  (sento.atomic:make-atomic-reference :value (%space)))

(defvar *space* (fresh)
  "The namespace this image serves. One per image, and every thread sees the
same one: a buffer's actor, an evaluation's own thread and a provider's poll
are all looking at the tree the daemon is serving.")

(defun current ()
  "The namespace as it stands. One slot read."
  (sento.atomic:atomic-get *space*))

(defun %swap (fn)
  "Replace the space with FN's answer, retrying until it lands. FN is pure."
  (sento.atomic:atomic-swap *space* fn))

(defmacro with-space ((&optional (space '(fresh))) &body body)
  "Run BODY against SPACE, so a test or a tool can hold one of its own.

Sets the variable rather than binding it, because a dynamic binding reaches
only this thread and the work pine does happens on others."
  (let ((previous (gensym "PREVIOUS")))
    `(let ((,previous *space*))
       (setf *space* ,space)
       (unwind-protect (progn ,@body)
         (setf *space* ,previous)))))

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
                (let ((*here* path))
                  (return (apply (clause-handlers clause)
                                 (mapcar (lambda (v) (fset:lookup bindings v))
                                         (p:binders (clause-pattern clause))))))))))

;;;; A clause says either what a path is, or how it is written and read.
;;;;
;;;;   :read   the provider is the value; the world behind it is the storage
;;;;   :write  the provider takes the write, as an effect on that world
;;;;   :out    the tree holds the value, and this is how it reads
;;;;   :in     the tree takes the write, and this is what lands
;;;;
;;;; The second pair is a representation and not a place: a chord written to
;;;; /key normalizes on the way in, a buffer's text is written as a string and
;;;; held as its lines, and both are still values in the tree, so both are
;;;; still undone, watched and stored like anything else.

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
  "True when a mounted provider reads PATH: the world behind it is the storage.

A clause that only says what a path does -- its verbs, or how a written value
is normalized -- leaves the value in the tree, so the path is still held."
  (loop :for backing :in (%backings space path)
        :for handlers = (%handlers backing path)
        :thereis (and handlers (fset:lookup handlers :read) t)))

(defun %served-write (space path value)
  "Give VALUE to the provider serving PATH.

Answers :DONE when a provider took it, or (values :STORE V) when a clause says
what a written value becomes and V is to land in the tree.

A clause that declares only :VERBS says what the path does, not what it holds,
so an ordinary value falls through. That is how a path can take a verb and
still be a place: /buf/NAME/point is [line col] and also takes [:move :word 1]."
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
                      ((%verbp value)
                       (error 'no-verb :at path :why (%verb-name value)))
                      ((or in (fset:lookup handlers :at))
                       (let ((where (fset:lookup handlers :at)))
                         (return (values :store
                                         (if in (funcall in value) value)
                                         (when where (funcall where))))))
                      (write (funcall write value) (return :done))
                      ((fset:lookup handlers :read)
                       (error 'refused :at path
                              :why "no provider writes it"))))
        :finally (return nil)))

(defun mounts ()
  "Where a provider is bound, newest first. What the tree is made of, as
opposed to what someone put in it."
  (mapcar #'car (space-mounts (current))))

(defun doc (path)
  "What PATH is, and what it takes: the :DOC of the clause serving it, or of
the provider holding that clause.

Documentation is a handler like any other, so it sits beside the code that
answers and there is no second place for it to be wrong in."
  (let ((backings (%backings (current) path)))
    (or (loop :for backing :in backings
              :for handlers = (%handlers backing path)
              :thereis (and handlers (fset:lookup handlers :doc)))
        (loop :for backing :in backings
              :thereis (fset:lookup (backing-options backing) :doc)))))

(defun %served-watch (space path listening)
  "Tell the provider serving PATH that someone started or stopped listening.

A value pine reads on demand needs nothing: whoever asks, asks. One the world
pushes -- a subprocess's output -- has to be started, and stopped when the last
listener goes, so the clause that answers it is told."
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

(defvar *reads* nil "Where the paths read while computing a value collect.")

(defun %ringp (path)
  (and (setting path :max) t))

(defun %index (text)
  (multiple-value-bind (n used) (parse-integer text :junk-allowed t)
    (and n (= used (length text)) n)))

;;;; Scope. A provider may say that the subtree under each name it holds falls
;;;; back to somewhere else: a leaf with no value here reads the same leaf
;;;; there. Buffer-local is not a mechanism, it is where the value is.

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
         (wanted (p:literal-at pattern (p:segment-count prefix)))
         (extra (and wanted (p:child prefix wanted))))
    (if (and extra (notany (lambda (c) (fset:equal? c extra)) children))
        (cons extra children)
        children)))

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

(defun diff (was now)
  "Every leaf under WAS and NOW that does not hold the same value, as a map
from the path under NOW to [what WAS had, what NOW has].

Both sides are ordinary paths, so this compares a subtree with its own past --
(diff /was/${n} /) is everything that moved since -- or with another host's,
with nothing here knowing which it was given."
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

(defvar *cascade* nil
  "Where a write made while a cascade is running on this thread collects, so it
joins that wave instead of being lost.")

(defun on-commit ()
  "What this space tells each commit to, or NIL."
  (space-on-commit (current)))

(defun (setf on-commit) (fn)
  "Call FN with the list of (path old new) each commit moves. The store hangs
here, because whether a value outlives the daemon is a property of the path and
not something anyone should have to remember to ask for.

It belongs to the space rather than the image: a pine is a space, and an image
can hold more than one."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-on-commit next) fn
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
  "The write option KEY that PATH carries in SPACE. Inside a swap the space is
the one being swapped, not whichever one CURRENT answers a moment later."
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
          :when (member key '(:keep :max))
            :do (setf settings (%settings-with settings path key value)))
    (if (eq settings (space-settings space))
        space
        (let ((next (copy-space space)))
          (setf (space-settings next) settings)
          next))))

(defun (setf setting) (value path key)
  "Say that PATH carries the write option KEY, without writing the path. The
store uses it to give a restored ring back its bound."
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
        (t (%inward value))))

(defun %committing (old path value options)
  "OLD with VALUE at PATH. Pure, so a swap may run it again: nothing here does
IO, and everything it decides -- the guard, the verb, the ring -- is decided
against the space it is handed rather than one read a moment ago."
  (destructuring-bind (&key when max &allow-other-keys) options
    (let* ((space (if options (%remembering old path options) old))
           (root (space-root space))
           (was (%get root (p:keys path)))
           ;; a ring is a property of the path, not of the write that made it
           ;; one, so every later write pushes without saying :max again
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

(defun %commit (changes)
  "Put CHANGES, a list of (path . value), in as one step: one new root, so
nothing ever observes the system half changed. Answers what moved.

Pure, so a swap may run it again: a verb, a ring and a de-duplication are all
decided against the space the write lands on."
  (let ((new (%swap (lambda (old)
                      (let ((root (space-root old))
                            (moved nil))
                        (dolist (change changes)
                          (destructuring-bind (path . value) change
                            (let* ((was (%get root (p:keys path)))
                                   (stored (%stored path was value
                                                    (%setting-in old path :max))))
                              (unless (fset:equal? was stored)
                                (setf root (%put root (p:keys path) stored))
                                (push (list path was stored) moved)))))
                        (let ((next (copy-space old)))
                          (setf (space-root next) root
                                (space-moved next) (nreverse moved))
                          next))))))
    (%told (space-moved new))))

(defun %told (moved)
  "Tell whoever is keeping the file what moved, outside the swap: a disk write
has no business inside a compare-and-swap."
  (let ((fn (space-on-commit (current))))
    (when (and moved fn) (funcall fn moved)))
  moved)

(defun %evaluate (thunk)
  "Run THUNK, answering (values value paths-it-read)."
  (let ((*reads* (cons :reads nil)))
    (let ((value (funcall thunk)))
      (values value (fset:convert 'fset:set (cdr *reads*))))))

(defun %write-one (path value &rest options &key when max force keep)
  "Put VALUE at PATH. Answers the (path old new) it moved, as a list."
  (declare (ignore when keep))
  (when (and (not force) (%whole-tree-p path))
    (error 'refused :at path :why "** at the root would take the whole tree"))
  (let ((space (current)))
    ;; a provider decides what a write to its subtree means, and its IO cannot
    ;; run inside a swap that may be retried
    (multiple-value-bind (served normalized where)
        (%served-write space path value)
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
    ;; what it read may have changed with the value, so the reaction is put
    ;; back rather than edited where it sits
    (%remember-reaction (reaction-path reaction) (reaction-thunk reaction) deps)
    (%write-one (reaction-path reaction) value)))

(defun %watching-p (pattern path)
  "A watch fires for its own path, for anything its pattern matches, and for
anything under it: watching a directory watches the subtree."
  (or (p:match pattern path :value #'%read-one)
      (and (not (p:patternp pattern)) (p:prefixp pattern path))))

(defun watched (path)
  "True when some watch would hear about a change at PATH: whether anyone is
listening, which is the difference between a fault someone can attend to and
one nobody will ever look at."
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
              ;; a watch over a pattern hears about several paths, so HERE
              ;; answers which one moved, as it does inside a provider
              (let ((answer (let ((*here* path)) (funcall fn new))))
                (when (fset:map? answer)
                  (setf out (append out (%apply answer))))))))))))

(defun %ordered (map)
  "MAP's entries, deepest path first and the later name of two at the same
depth before the earlier.

A map has no order and a value does not need one: every path in a transaction
gets its value and nothing observes the half of it. A verb is an event, and two
events in one map do need one, so pine says what it is. Deepest first is what
makes the doc's own handlers mean what they read as:

  {/buf/${buf}/text [:newline] /buf/${buf} [:indent-line]}
  {/buf/${buf}/text [:insert \")\"] /buf/${buf}/point [:move :char -1]}

the line splits before it is indented, and the paren is typed before point
steps back over it."
  (let ((acc nil))
    (fset:do-map (path value map) (push (cons path value) acc))
    (sort acc (lambda (a b)
                (let ((da (p:segment-count (car a)))
                      (db (p:segment-count (car b))))
                  (if (= da db)
                      (string> (p:text (car a)) (p:text (car b)))
                      (> da db)))))))

(defun %apply (map)
  "Write every entry of MAP as one change, without propagating: the caller owns
that.

A path a provider serves is given to the provider, because that is an effect on
the world and not a place in the tree; everything else lands in one swap, which
is what makes a map a transaction."
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
                (%commit staged)))))

(defun %expand (moved)
  "MOVED, with what landed under each map in it.

Writing a directory is writing every leaf in it, so a watch or an expression
over one leaf hears about it whether it was written on its own or as part of
the map its parent took: (write /buf/notes {:mode :lisp}) and
(write /buf/notes/mode :lisp) are the same change."
  (let ((out nil))
    (labels ((walk (change)
               (destructuring-bind (path old new) change
                 (push change out)
                 (when (fset:map? new)
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
a reaction and a watch may write. A write made while this is running joins the
wave rather than starting a cascade of its own, so a watch that writes -- a
mode installing a view, a parser landing its answer -- carries on to whatever
was watching what it wrote.

*PROPAGATING* is per thread on purpose: it says this thread is already carrying
a change through the graph, and two threads each carrying their own is the
normal case rather than a collision."
  (cond ((or (null moved) *preview*) nil)
        (*propagating*
         (when *cascade*
           (setf (cdr *cascade*) (append (cdr *cascade*) moved))))
        (t
         (let* ((*propagating* t)
                (*cascade* (cons :cascade nil))
                (seen (fset:empty-set))
                (wave (%expand moved))
                (passes 0))
           (loop :while wave
                 :do (when (> (incf passes) 100)
                       (error 'cycle :at (mapcar #'first wave)))
                     (let ((next nil)
                           (space (current)))
                       (dolist (reaction (%dependents space wave))
                         (let ((at (reaction-path reaction)))
                           (when (fset:contains? seen at)
                             (error 'cycle :at (list at)))
                           (setf seen (fset:with seen at))
                           (setf next (append next (%recompute reaction)))))
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
  "Say that what a provider under AT reads has moved.

A live path holds no value, so nothing swapped and nothing here would otherwise
notice. This is what a provider's :ON does when the world behind it speaks:
everything that read a path under AT is computed again, and every watch over one
runs, exactly as though the value had been written."
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

(defun %transact (map)
  "Apply a map of path to value as one change."
  (let ((moved (%apply map)))
    (if *propagating*
        (%changes moved)
        (progn (%propagate moved) (%changes moved)))))

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
    (%propagate moved)
    (%changes moved)))

(defun %told-by (path)
  "The name of the watch that carries a provider's :ON to PATH."
  (list :on (p:text path)))

(defun %mount (path backing)
  "Bind BACKING under PATH. Writing one over another stacks it: a read falls
through to the one underneath for what the top does not answer, so a machine
overrides a single leaf of a shipped provider without forking it.

A backing with an :ON is told by the world instead of polled: the path it names
is watched, and anything that moves there stirs everything read under here."
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
  (fset:empty-map))

(defun %unmount (path)
  (watch path nil :as (%told-by path))
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-mounts next)
                   (remove-if (lambda (m) (fset:equal? (car m) path))
                              (space-mounts old))
                   (space-moved next) nil)
             next))))

(defun %remember-reaction (path thunk deps)
  "Note how PATH is computed, or that it no longer is."
  (%swap (lambda (old)
           (let ((next (copy-space old)))
             (setf (space-reactions next)
                   (if (fset:empty? deps)
                       (fset:less (space-reactions old) path)
                       (fset:with (space-reactions old) path
                                  (make-reaction :path path :thunk thunk
                                                 :deps deps)))
                   (space-moved next) nil)
             next))))

(defun %write-reactive (path thunk &rest options)
  "Put THUNK's value at PATH and remember what it read, so it is computed again
whenever one of those paths moves. A provider written here mounts instead."
  (multiple-value-bind (value deps) (%evaluate thunk)
    ;; writing nothing where a provider is mounted takes it off -- where it is
    ;; mounted, not anywhere under it: clearing one leaf of a served subtree is
    ;; an ordinary write
    (when (and (null value) (%mounted-at path))
      (%unmount path))
    (if (backing-p value)
        (%mount path value)
        ;; the reaction is registered before the value lands, so whatever
        ;; watches the commit already knows this path is computed rather than
        ;; held, and does not store it
        (progn
          (%remember-reaction path thunk deps)
          (let ((moved (apply #'%write-one path value options)))
            (%propagate moved)
            (%changes moved))))))

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
  (let ((name (or as (gensym "WATCH"))))
    (%swap (lambda (old)
             (let ((next (copy-space old))
                   (rest (remove name (space-watches old)
                                 :key #'first :test #'equal)))
               (setf (space-watches next)
                     (if fn (append rest (list (list name path fn))) rest)
                     (space-moved next) nil)
               next)))
    ;; some paths have to be made to speak, and stop speaking when the last
    ;; listener goes
    (%served-watch (current) path (and (watched path) t))
    name))

(defmacro preview (&body body)
  "The changes BODY would make, without making them: the map WRITE would have
answered, path to [old new]."
  `(let ((*preview* (cons :preview nil)))
     ,@body
     (%changes (reverse (cdr *preview*)))))
