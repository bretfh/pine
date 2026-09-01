(defpackage #:pine/data
  (:use #:cl)
  (:shadow #:map #:set #:remove #:subseq #:rest #:append)
  (:export
   #:map #:seq #:set #:mapp #:seqp
   #:setp #:collectionp #:lookup #:with #:without
   #:size #:keys #:vals #:pairs #:do-each
   #:do-pairs #:do-map #:as #:merged #:contains
   #:rest #:append #:subseq #:remove #:no-map
   #:no-seq #:no-set #:capped #:swap #:cas
   #:emptied #:table #:all #:keep! #:drop!
   #:claim #:clear! #:update! #:same #:emptyp))
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
  (:documentation "What COLLECTION holds at KEY, or DEFAULT where it holds nothing,
and whether anything was there.

Two values, because a collection may hold NIL and holding it is not the same as
holding nothing. Whoever only wants the value reads the first and never knows.

Not AT: a node is at a path and a value is looked up in a collection, and reading
(d:at (d:all *commands*) name) beside (tree:at \"/wm\") meant knowing which was
which before you could read either.")
  (:method ((c fset:map) key &optional default)
    (multiple-value-bind (value foundp) (fset:lookup c key)
      (if foundp (values value t) (values default nil))))
  (:method ((c fset:seq) key &optional default)
    (if (and (integerp key) (>= key 0) (< key (fset:size c)))
        (values (fset:@ c key) t)
        (values default nil)))
  (:method ((c fset:set) key &optional default)
    (if (fset:contains? c key) (values key t) (values default nil)))
  (:method ((c null) key &optional default)
    (declare (ignore key))
    (values default nil))
  (:method ((c hash-table) key &optional default)
    (multiple-value-bind (value foundp) (gethash key c default)
      (values value foundp)))
  (:method ((c cons) key &optional default)
    (if (integerp key)
        (let ((tail (and (>= key 0) (nthcdr key c))))
          (if tail (values (car tail) t) (values default nil)))
        (let ((tail (loop :for rest :on c :by #'cddr
                          :when (eql (car rest) key) :do (return rest))))
          (if tail (values (second tail) t) (values default nil))))))

(defgeneric with (collection key &optional value)
  (:documentation "COLLECTION with VALUE at KEY, or with KEY in it where that is
what the kind means. What comes back is the same kind that went in.

The building half of this vocabulary is the fset kinds and nothing, because
building one a piece at a time and sharing what did not move is what they are for.
A list says so rather than being copied behind your back.")
  (:method ((c fset:map) key &optional value) (fset:with c key value))
  (:method ((c fset:seq) key &optional (value nil valuep))
    "Given a value it is put at that index; given only a thing, that thing goes on
the end. Asked of what was handed over and not of what the thing looks like, the
way the NULL method asks: told to decide by type, (with (seq) 5) put NIL at index
five and padded the four before it, so a seq of numbers was one WITH could not
build and INCLUDE on one quietly wrecked it."
    (if valuep (fset:with c key value) (fset:with-last c key)))
  (:method ((c fset:set) key &optional value)
    (declare (ignore value))
    (fset:with c key))
  (:method ((c null) key &optional (value nil valuep))
    "Nothing is the empty one of whichever kind is being built: given a value it is
a map, and given only a key it is a seq. Asked of what was handed over, not of
whether the value happens to be NIL."
    (if valuep (fset:with +no-map+ key value) (fset:with-last +no-seq+ key)))
  (:method ((c cons) key &optional value)
    (declare (ignore key value))
    (error "~s is a list; WITH builds the kinds that share what they keep." c)))

(defgeneric without (collection key)
  (:method ((c fset:map) key) (fset:less c key))
  (:method ((c fset:seq) key) (fset:less c key))
  (:method ((c fset:set) key) (fset:less c key))
  (:method ((c null) key) (declare (ignore key)) nil)
  (:method ((c cons) key)
    (declare (ignore key))
    (error "~s is a list; WITHOUT builds the kinds that share what they keep." c)))

(defgeneric size (collection)
  (:method ((c fset:collection)) (fset:size c))
  (:method ((c null)) 0)
  (:method ((c sequence)) (length c))
  (:method ((c hash-table)) (hash-table-count c)))


(defun emptyp (collection) (zerop (size collection)))

(defgeneric contains (collection value)
  (:documentation "Whether VALUE is one of the things COLLECTION holds.

What a map holds is its values, the way a seq holds its elements. Whether a map has
a key is LOOKUP's second answer, which is a different question and is asked with a
different word.")
  (:method ((c fset:set) value) (fset:contains? c value))
  (:method ((c fset:seq) value) (and (fset:position value c) t))
  (:method ((c fset:map) value)
    (block found
      (fset:do-map (k v c) (declare (ignore k))
        (when (fset:equal? v value) (return-from found t)))))
  (:method ((c null) value) (declare (ignore value)) nil)
  (:method ((c cons) value) (and (cl:member value c :test #'fset:equal?) t))
  (:method ((c hash-table) value)
    (block found
      (maphash (lambda (k v) (declare (ignore k))
                 (when (fset:equal? v value) (return-from found t)))
               c))))

(defgeneric keys (collection)
  (:method ((c fset:map)) (fset:convert 'list (fset:domain c)))
  (:method ((c fset:set)) (fset:convert 'list c))
  (:method ((c fset:seq)) (loop :for i :below (fset:size c) :collect i))
  (:method ((c null)) nil)
  (:method ((c cons)) (loop :for i :below (length c) :collect i))
  (:method ((c hash-table))
    (loop :for k :being :the :hash-keys :of c :collect k)))

(defgeneric vals (collection)
  (:method ((c fset:map)) (fset:convert 'list (fset:range c)))
  (:method ((c fset:seq)) (fset:convert 'list c))
  (:method ((c fset:set)) (fset:convert 'list c))
  (:method ((c null)) nil)
  (:method ((c cons)) (copy-list c))
  (:method ((c hash-table))
    (loop :for v :being :the :hash-values :of c :collect v)))

(defun pairs (collection)
  (loop :for key :in (keys collection) :collect (cons key (lookup collection key))))

(defmacro do-map ((key value collection &optional result) &body body)
  "Every pair in a map. Nothing is the empty map and walks none of them; anything
that is not a map at all says so, the way the other two walks here do."
  (let ((c (gensym)) (k (gensym)) (v (gensym)))
    `(let ((,c ,collection))
       (cond ((mapp ,c)
              (fset:do-map (,k ,v ,c)
                (let ((,key ,k) (,value ,v))
                  (declare (ignorable ,key ,value))
                  ,@body)))
             ((null ,c))
             (t (error "~s is not a map to walk." ,c)))
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
             ((null ,c) ,result)
             (t (error "~s is not something to walk in pairs." ,c))))))

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
               ((hash-table-p ,c)
                (maphash (lambda (,k ,v) (declare (ignorable ,k)) ,(each)) ,c))
               (t (error "~s is not something to walk." ,c)))
         ,result))))

(defgeneric as (kind collection)
  (:method ((kind (eql :list)) collection)
    (if (collectionp collection) (fset:convert 'list collection) collection))
  (:method ((kind (eql :seq)) collection) (fset:convert 'fset:seq collection))
  (:method ((kind (eql :set)) collection) (fset:convert 'fset:set collection))
  (:method ((kind (eql :map)) collection) (fset:convert 'fset:map collection))
  (:method ((kind (eql :vector)) collection) (fset:convert 'vector collection)))

(defun same (a b)
  "Whether A and B are the same value. Two maps holding the same things are the
same map: EQUAL asks whether they are the same object, which for anything built
here is a question about the last edit rather than about the value."
  (fset:equal? a b))

(defun merged (&rest collections)
  "Every map laid over the ones before it, the later winning where both say.

Maps, and nothing standing for the empty one. Two seqs have no one answer here,
and quietly keeping the second is worse than saying there is none."
  (reduce (lambda (a b)
            (cond ((null b) a)
                  ((and (mapp a) (mapp b)) (fset:map-union a b))
                  (t (error "~s and ~s are not both maps." a b))))
          collections :initial-value +no-map+))

(defun rest (c) (fset:subseq c 1))
(defun append (a b) (fset:concat a b))
(defun subseq (c from &optional to) (fset:subseq c from (or to (size c))))
(defun remove (item c) (fset:remove item c))

(defun capped (list value n)
  "LIST with VALUE in front of it, no longer than N: the newest N of something
there is no point keeping all of. Takes what it is given first, so it is what
SWAP is handed rather than something wrapped in a lambda.

Walks as far as the cap and no further: what is past it is dropped rather than
counted, so a ring that is already full costs its length and not twice it."
  (let ((next (cons value list)))
    (if (nthcdr n next) (cl:subseq next 0 n) next)))

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

(defun update! (table key function &rest arguments)
  "Replace what TABLE holds at KEY with FUNCTION of it, and answer that.

One act. A LOOKUP and a KEEP! with a gap between them are two, and whoever writes
in the gap is lost; FUNCTION runs again if somebody got there first, so it must be
pure the way SWAP's is."
  (fset:lookup (swap (table-of table)
                     (lambda (m)
                       (fset:with m key
                                  (apply function (fset:lookup m key) arguments))))
               key))

(defun claim (table key value)
  "Put VALUE at KEY unless something is there already, and answer whatever is
there afterwards, so the loser of a race gets the winner's object.

Whether something is there is asked of the map and not of what it holds: a key
somebody claimed with NIL is claimed, and the next to ask must not take it."
  (fset:lookup (swap (table-of table)
                     (lambda (m)
                       (if (nth-value 1 (fset:lookup m key))
                           m
                           (fset:with m key value))))
               key))

(defun clear! (table)
  (swap (table-of table) (constantly +no-map+))
  table)

