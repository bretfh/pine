(defpackage #:pine/data
  (:use #:cl)
  (:shadow #:map #:set #:remove #:subseq #:rest #:sort #:append)
  (:export
   #:map #:seq #:set #:mapp #:seqp
   #:setp #:collectionp #:lookup #:with #:without
   #:size #:keys #:vals #:pairs #:do-each
   #:do-pairs #:do-map #:as #:merged #:contains
   #:rest #:append #:subseq #:remove #:no-map
   #:no-seq #:no-set #:capped #:swap #:cas
   #:emptied #:table #:all #:keep! #:drop!
   #:claim #:clear!))
(in-package #:pine/data)

(defvar +no-map+ (fset:empty-map))
(defvar +no-seq+ (fset:empty-seq))
(defvar +no-set+ (fset:empty-set))

(defun no-map () +no-map+)
(defun no-seq () +no-seq+)
(defun no-set () +no-set+)

(defun map (&rest pairs)
  (loop :with out := +no-map+
        :for (key value) :on pairs :by #'cddr
        :do (setf out (fset:with out key value))
        :finally (return out)))

(defun seq (&rest values) (fset:convert 'fset:seq values))

(defun set (&rest values) (fset:convert 'fset:set values))

(defun mapp (x) (fset:map? x))
(defun seqp (x) (fset:seq? x))
(defun setp (x) (fset:set? x))
(defun collectionp (x) (or (mapp x) (seqp x) (setp x)))

(defgeneric lookup (collection key &optional default)
  (:documentation "What COLLECTION holds at KEY, or DEFAULT where it holds nothing.

Not AT: a node is at a path and a value is looked up in a collection, and reading
(d:at (d:all *commands*) name) beside (tree:at nil \"wm\") meant knowing which was
which before you could read either.")
  (:method ((c fset:map) key &optional default)
    (multiple-value-bind (value foundp) (fset:lookup c key)
      (if foundp value default)))
  (:method ((c fset:seq) key &optional default)
    (if (and (integerp key) (>= key 0) (< key (fset:size c)))
        (fset:@ c key)
        default))
  (:method ((c fset:set) key &optional default)
    (if (fset:contains? c key) key default))
  (:method ((c null) key &optional default)
    (declare (ignore key))
    default)
  (:method ((c hash-table) key &optional default)
    (gethash key c default))
  (:method ((c cons) key &optional default)
    (if (integerp key) (or (nth key c) default) (getf c key default))))

(defgeneric with (collection key &optional value)
  (:method ((c fset:map) key &optional value) (fset:with c key value))
  (:method ((c fset:seq) key &optional value)
    (if (integerp key) (fset:with c key value) (fset:with-last c key)))
  (:method ((c fset:set) key &optional value)
    (declare (ignore value))
    (fset:with c key))
  (:method ((c null) key &optional value)
    (if value (fset:with +no-map+ key value) (fset:with-last +no-seq+ key))))



(defgeneric without (collection key)
  (:method ((c fset:map) key) (fset:less c key))
  (:method ((c fset:seq) key) (fset:less c key))
  (:method ((c fset:set) key) (fset:less c key))
  (:method ((c null) key) (declare (ignore key)) nil))

(defgeneric size (collection)
  (:method ((c fset:collection)) (fset:size c))
  (:method ((c null)) 0)
  (:method ((c sequence)) (length c))
  (:method ((c hash-table)) (hash-table-count c)))


(defun emptyp (collection) (zerop (size collection)))

(defgeneric contains (collection value)
  (:method ((c fset:set) value) (fset:contains? c value))
  (:method ((c fset:seq) value) (and (fset:position value c) t))
  (:method ((c fset:map) value) (nth-value 1 (fset:lookup c value)))
  (:method ((c null) value) (declare (ignore value)) nil))

(defgeneric keys (collection)
  (:method ((c fset:map)) (fset:convert 'list (fset:domain c)))
  (:method ((c fset:set)) (fset:convert 'list c))
  (:method ((c fset:seq)) (loop :for i :below (fset:size c) :collect i))
  (:method ((c null)) nil))

(defgeneric vals (collection)
  (:method ((c fset:map)) (fset:convert 'list (fset:range c)))
  (:method ((c fset:seq)) (fset:convert 'list c))
  (:method ((c fset:set)) (fset:convert 'list c))
  (:method ((c null)) nil))

(defun pairs (collection)
  (loop :for key :in (keys collection) :collect (cons key (lookup collection key))))

(defmacro do-map ((key value collection &optional result) &body body)
  (let ((c (gensym)) (k (gensym)) (v (gensym)))
    `(let ((,c ,collection))
       (when (mapp ,c)
         (fset:do-map (,k ,v ,c)
           (let ((,key ,k) (,value ,v))
             (declare (ignorable ,key ,value))
             ,@body)))
       ,result)))

(defmacro do-pairs ((key value collection &optional result) &body body)
  (let ((c (gensym)) (k (gensym)) (v (gensym)) (i (gensym)))
    `(let ((,c ,collection))
       (cond ((mapp ,c) (fset:do-map (,k ,v ,c)
                          (let ((,key ,k) (,value ,v))
                            (declare (ignorable ,key ,value))
                            ,@body))
                        ,result)
             ((seqp ,c) (let ((,i -1))
                          (fset:do-seq (,v ,c)
                            (incf ,i)
                            (let ((,key ,i) (,value ,v))
                              (declare (ignorable ,key ,value))
                              ,@body))
                          ,result))
             ((setp ,c) (fset:do-set (,v ,c)
                          (let ((,key ,v) (,value ,v))
                            (declare (ignorable ,key ,value))
                            ,@body))
                        ,result)
             (t ,result)))))

(defmacro do-each ((value collection &optional result) &body body)
  "Every value in a collection: a map's values, a seq's elements, a set's members,
a list's. KEYS and VALS answer lists, so a walk over one has to be a walk and not
a shape this quietly steps over.

VALUE is bound by a LET of its own, so a declaration at the head of the body is
about what the walk binds and not about a variable of the same name further out."
  (let ((c (gensym)) (k (gensym)) (v (gensym)))
    (flet ((each () `(let ((,value ,v))
                       (declare (ignorable ,value))
                       ,@body)))
      `(let ((,c ,collection))
         (cond ((mapp ,c) (fset:do-map (,k ,v ,c) (declare (ignorable ,k)) ,(each)))
               ((seqp ,c) (fset:do-seq (,v ,c) ,(each)))
               ((setp ,c) (fset:do-set (,v ,c) ,(each)))
               ((listp ,c) (dolist (,v ,c) ,(each)))
               (t (error "~s is not something to walk." ,c)))
         ,result))))

(defgeneric as (kind collection)
  (:method ((kind (eql :list)) collection)
    (if (collectionp collection) (fset:convert 'list collection) collection))
  (:method ((kind (eql :seq)) collection) (fset:convert 'fset:seq collection))
  (:method ((kind (eql :set)) collection) (fset:convert 'fset:set collection))
  (:method ((kind (eql :map)) collection) (fset:convert 'fset:map collection))
  (:method ((kind (eql :vector)) collection) (fset:convert 'vector collection)))

(defun same (a b) (fset:equal? a b))

(defun merged (&rest collections)
  (reduce (lambda (a b) (if (and (mapp a) (mapp b)) (fset:map-union a b) (or b a)))
          collections :initial-value +no-map+))

(defun rest (c) (fset:subseq c 1))
(defun append (a b) (fset:concat a b))
(defun subseq (c from &optional to) (fset:subseq c from (or to (size c))))
(defun sort (c predicate &key key) (fset:sort c predicate :key key))
(defun remove (item c) (fset:remove item c))

(defun capped (list value n)
  "LIST with VALUE in front of it, no longer than N: the newest N of something
there is no point keeping all of. Takes what it is given first, so it is what
SWAP! is handed rather than something wrapped in a lambda."
  (let ((next (cons value list)))
    (if (> (length next) n) (cl:subseq next 0 n) next)))

(defmacro swap (place function &rest arguments &environment env)
  "Replace what PLACE holds with FUNCTION of it, and answer that.

A place, not a cell: a slot, a global, anywhere a value is kept. FUNCTION runs
again if another thread got there first, so it must be pure. Every value in pine
is immutable, so this is the whole of how one is replaced, and there is no box
to hold it in.

PLACE's subforms are evaluated once, left to right, and so are FUNCTION and
ARGUMENTS: a retry runs the function again and nothing else. Two things follow
from the compare being EQ on the place itself:

A global is compared in whatever dynamic binding is in force here, not the
global one, so a variable this replaces must be one nothing rebinds.

A number is compared by identity, which holds for a fixnum and stops holding
above MOST-POSITIVE-FIXNUM. A counter this replaces must be one that cannot
reach it."
  (multiple-value-bind (temps values old new cas-form read-form)
      (sb-ext:get-cas-expansion place env)
    (let ((fn (gensym "FN"))
          (args (loop :repeat (length arguments) :collect (gensym "ARG"))))
      `(let* (,@(mapcar #'list temps values)
              (,fn ,function)
              ,@(mapcar #'list args arguments))
         (loop :for ,old := ,read-form
               :for ,new := (funcall ,fn ,old ,@args)
               :until (eq ,old ,cas-form)
               :finally (return ,new))))))

(defmacro emptied (place &environment env)
  "Take what PLACE holds, leaving nothing there, and answer what was there.

The other half of SWAP. A queue two threads share is pushed with one and emptied
with the other, and neither of them holds a lock: what the emptier gets is exactly
what was there when it looked, and anything handed over after that is still there
for the next look.

PLACE's subforms are evaluated once, and the same two rules about EQ hold."
  (multiple-value-bind (temps values old new cas-form read-form)
      (sb-ext:get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps values)
            (,new nil))
       (loop :for ,old := ,read-form
             :until (eq ,old ,cas-form)
             :finally (return ,old)))))

(defmacro cas (place old new &environment env)
  "Put NEW in PLACE if OLD is still what it holds. Answers whether it was.

PLACE's subforms, OLD and NEW are each evaluated once, left to right."
  (multiple-value-bind (temps values old-var new-var cas-form read-form)
      (sb-ext:get-cas-expansion place env)
    (declare (ignore read-form))
    `(let* (,@(mapcar #'list temps values)
            (,old-var ,old)
            (,new-var ,new))
       (eq ,old-var ,cas-form))))

(defstruct (table (:constructor %table) (:copier nil))
  "A name-to-thing registry nothing has to lock to touch. A thing in its own
right, unlike a slot: it is passed around and kept, so it is a struct and not a
place somebody else owns."
  (of +no-map+))

(defun table () (%table))

(defun all (table) (table-of table))

(defun keep! (table key value)
  (swap (table-of table) (lambda (m) (fset:with m key value)))
  value)

(defun drop! (table key)
  (swap (table-of table) (lambda (m) (fset:less m key)))
  nil)

(defun claim (table key value)
  "Put VALUE at KEY unless something is there already, and answer whatever is
there afterwards, so the loser of a race gets the winner's object."
  (fset:lookup (swap (table-of table)
                     (lambda (m)
                       (if (fset:lookup m key) m (fset:with m key value))))
               key))

(defun clear! (table)
  (swap (table-of table) (constantly +no-map+))
  table)

(pine/word:lends "map" "seq" "set" "lookup" "with" "without" "keys" "vals"
                "pairs" "size" "swap" "cas" "capped" "emptied")
