(defpackage #:pine/fs/node
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:commit #:pine/fs/commit))
  (:export
   #:node #:derived #:live #:nodep #:name
   #:over #:describes #:savedp #:livep #:announces
   #:refreshes #:contents #:nodes #:resolve #:stir
   #:moved #:verb #:full-name #:make #:derive
   #:place #:attach #:detach #:child #:memo
   #:slots #:make-child #:erase-child #:reads #:writes
   #:depend #:undepend #:reading))
(in-package #:pine/fs/node)

(defvar *reading* nil)
(defparameter +unread+ '#:unread)

(defclass node ()
  ((name      :initarg :name      :reader name)
   (over      :initarg :over      :accessor over      :initform nil)
   (describes :initarg :describes :accessor describes :initform nil)
   (beneath   :initform (d:no-seq) :reader beneath)
   (memo      :initform (d:table)  :reader memo)
   (readers   :initform (d:no-set) :reader readers)
   (named     :initform nil        :accessor named))
  (:documentation "A name, what it sits under, and what is under it. On its own it
is a branch: it holds nothing, and what is under it is whatever was attached.

SAVEDP says what it holds outlives the image; LIVEP says the world behind it
answers rather than a value kept here, which is what stops a snapshot walking into
it and what makes a watcher ask rather than be told. Neither is a slot: they are
questions a class answers by being that class.

What a node can be instead of a branch is the classes below, and they are classes
rather than flags because each answers CONTENTS its own way: a VALUE holds what
was written, a DERIVED works it out and remembers, and a LIVE one asks whatever
stands behind it."))

(defgeneric savedp (node)
  (:documentation "Whether what this holds outlives the image.")
  (:method ((n node)) nil))

(defgeneric livep (node)
  (:documentation "Whether the world behind this answers, rather than a value kept
here. A snapshot has no business walking into one, and a watcher on one has to ask
rather than be told.")
  (:method ((n node)) nil))

(defgeneric announces (node)
  (:documentation "The lines whose output says the world behind this moved.")
  (:method ((n node)) nil))

(defgeneric refreshes (node)
  (:documentation "How often to ask again, where nothing announces itself.")
  (:method ((n node)) nil))

(defclass value (node)
  ((held :initarg :held :accessor held :initform nil))
  (:documentation "Holds one. Nothing works it out and nothing outside answers for
it: it is written, and what it holds stands until it is written again.

It holds it here, in the object that is the place. It used to live in a map keyed
by path string, which meant the tree was kept twice and a write had to say where it
landed in words."))

(defclass derived (node)
  ((reads  :initarg :reads   :accessor reads  :initform nil)
   (writes :initarg :writes  :accessor writes :initform nil)
   (cached :initform +unread+ :accessor cached)
   (saw    :initform nil      :accessor saw))
  (:documentation "Works its value out, and remembers it until something it read
moves. What READS reads is recorded while it runs, so a write invalidates exactly
what depended on it and nothing subscribes to anything.

WRITES because working a value out and being able to write it are two questions:
/dev/audio/volume is worked out from what wpctl says and writing it sets the
volume. Without one, writing replaces what it works out with what you wrote."))

(defclass live (node) ()
  (:documentation "The world behind this answers, rather than a value kept here.

Nothing is remembered, because the world moves without anybody writing it; that is
what DERIVED, which does remember, is the other half of. A job, a mounted
directory, a compositor and a region of a document are all this: each answers
CONTENTS from whatever stands behind it, and saying so is the whole of what they
have in common.

PLACE below is the one that answers through closures rather than through a method
of its own."))

(defclass place (live)
  ((reads     :initarg :reads     :accessor reads     :initform nil)
   (writes    :initarg :writes    :accessor writes    :initform nil)
   (names     :initarg :names     :reader  names-of   :initform nil)
   (each      :initarg :each      :reader  each-of    :initform nil)
   (listing   :initarg :nodes     :reader  nodes-of   :initform nil)
   (announces :initarg :announces :reader  announces  :initform nil)
   (refreshes :initarg :refreshes :reader  refreshes  :initform nil))
  (:documentation "Somewhere the world answers, through the closures it was given.

READS answers what it holds and WRITES says what writing it means. NAMES says what
is under it and EACH makes one, and what EACH made is kept, so the same name is
the same node every time; with NODES the children are nodes something else already
keeps.

A device is one of these and each of its readings is a closure pair, which is why
a device is a table of rows rather than a class per reading."))

(defclass slot (node)
  ((object-of :initarg :object :reader object-of)
   (slot-of   :initarg :slot   :reader slot-of)
   (into      :initarg :into   :reader into-of :initform nil))
  (:documentation "One slot of a lisp object, standing in the tree.

A value like any other, held somewhere else: the object owns it and this is where
it is read and written from. Not LIVE -- nothing outside answers for it, and
writing it here says so, so a watcher is told rather than having to ask."))

(defmethod savedp ((n value)) t)
(defmethod savedp ((n slot)) t)
(defmethod livep ((n live)) t)

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (write-string (full-name n) stream)))

(defun nodep (x) (typep x 'node))

(defun make (name &rest initargs &key (class 'value) &allow-other-keys)
  "A node holding one value, kept where it will be found again. That it is kept is
the class saying so, not a flag anybody has to remember to pass."
  (apply #'make-instance class :name name
         (alexandria:remove-from-plist initargs :class)))

(defun derive (name reads &rest initargs)
  "A node that works its value out and remembers it until something it read moves."
  (apply #'make-instance 'derived :name name :reads reads initargs))

(defun place (name &rest initargs)
  "Somewhere the world answers: how to read it, how to write it, and what is
under it. Nothing is remembered, because the world moves without anybody writing
it; that is what DERIVE, which does remember, is the other half of."
  (apply #'make-instance 'place :name (princ-to-string name) initargs))

(defun %named (n)
  (let ((names (loop :for at := n :then (over at)
                     :while at
                     :when (name at) :collect (name at))))
    (if names (format nil "/~{~a~^/~}" (reverse names)) "/")))

(defun full-name (n)
  "The path this node is at."
  (or (named n) (setf (named n) (%named n))))

(defun %renamed (n)
  (setf (named n) nil)
  (d:do-each (each (beneath n)) (%renamed each))
  (dolist (each (d:vals (d:all (memo n)))) (%renamed each))
  n)

(defun root (n)
  (loop :for at := n :then (over at)
        :while (over at)
        :finally (return at)))

(defun children (n) (d:vals (d:all (memo n))))

(defun child (n name builder)
  "The child N keeps under NAME, made once. Two threads asking at once both answer
the one that landed, which is what lets anything reading it be worked out again."
  (let ((name (princ-to-string name)))
    (or (d:lookup (d:all (memo n)) name)
        (d:claim (memo n) name (funcall builder)))))

(defun %kid (n name)
  (child n name
         (lambda ()
           (let ((it (funcall (each-of n) name)))
             (when it (setf (over it) n))
             it))))

(defun %listed (n) (mapcar #'princ-to-string (funcall (names-of n))))

(defgeneric nodes (node)
  (:documentation "What is under NODE, in order.

The one place a class says what it contains. RESOLVE and LEAFP are written on this,
so a class that answers here answers everywhere.")
  (:method ((n node)) (d:as :list (beneath n)))
  (:method ((n place))
    (cond ((nodes-of n) (funcall (nodes-of n)))
          ((names-of n)
           (remove nil (mapcar (lambda (each) (%kid n each)) (%listed n))))
          (t (call-next-method)))))

(defgeneric resolve (node name)
  (:documentation "What NODE has under NAME.

Derived from NODES where the children are attached, so the two cannot disagree.
Where they are worked out, EACH answers what is there and nothing where it
answers nothing: NAMES says what to list, which is not always the same question.
Every shell line is a place whether or not one has been run, and /sh lists the
ones that have.")
  (:method ((n node) name)
    (find (princ-to-string name) (d:as :list (beneath n))
          :key #'name :test #'equal))
  (:method ((n place) name)
    (let ((name (princ-to-string name)))
      (cond ((nodes-of n)
             (find name (funcall (nodes-of n)) :key #'name :test #'equal))
            ((each-of n) (%kid n name))
            (t (call-next-method))))))

(defun leafp (n) (null (nodes n)))

(defgeneric attach (node into)
  (:documentation "Put NODE under INTO, replacing whatever stood at its name.")
  (:method ((n node) (into node))
    (setf (over n) into)
    (%renamed n)
    (d:swap (slot-value into 'beneath)
             (lambda (all)
               (d:with (d:as :seq (cl:remove (name n) (d:as :list all)
                                             :key #'name :test #'equal))
                       n)))
    n))

(defgeneric detach (node name)
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (d:swap (slot-value n 'beneath) (lambda (all) (d:remove gone all)))
        (setf (over gone) nil)
        (%renamed gone))
      gone)))

(defgeneric make-child (node name)
  (:documentation "A fresh child of NODE named NAME, made in whatever stands behind
it: a plain node keeps it here, a mounted directory makes a file on the disk. A name
that ends in / asks for a branch.")
  (:method ((n node) name)
    (attach (make (string-right-trim "/" (princ-to-string name))) n)))

(defgeneric erase-child (node name)
  (:documentation "Take NAME out of NODE, and out of whatever stands behind it.

Saying the path went is COMMIT:FORGET's, not this one's: a store keeping a copy
of the tree hears an erasure the same way it hears a write, and this layer names
nothing that is listening.")
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (commit:forget (full-name gone)))
      (d:drop! (memo n) (princ-to-string name))
      (detach n name))))

(defun reading (n)
  (when *reading* (pushnew n (cdr *reading*)))
  n)

(defgeneric depend (node on)
  (:documentation "Say N read ON, so moving ON works N out again.")
  (:method ((n node) (on node))
    (d:swap (slot-value on 'readers) (lambda (all) (d:with all n)))
    n))

(defgeneric undepend (node on)
  (:documentation "Say N is no longer reading ON.")
  (:method ((n node) (on node))
    (d:swap (slot-value on 'readers) (lambda (all) (d:without all n)))
    n))

(defgeneric stir (node)
  (:documentation "Say NODE moved. Whatever read it is worked out again.")
  (:method ((n node))
    (d:do-each (each (readers n))
      (stir each))
    n)
  (:method :before ((n derived))
    "What it worked out is no longer what it would work out."
    (setf (cached n) +unread+)))

(defun %work-out (n)
  "Work N out, and record what it read while doing it. What it read is recorded
whether or not it finished: a node that threw has still read what it read, and one
that keeps nothing is one nothing can ever stir again."
  (let ((reading (cons :reading nil)))
    (unwind-protect
         (let* ((*reading* reading)
                (v (funcall (reads n))))
           (setf (cached n) v)
           v)
      (setf (saw n) (cdr reading))
      (dolist (on (cdr reading))
        (unless (eq on n) (depend n on))))))

(defun stalep (n) (eq (cached n) +unread+))

(defgeneric verb (node name arguments)
  (:documentation "What writing (:toggle) and its like means.")
  (:method ((n node) name arguments)
    (let ((had (contents n)))
      (setf (contents n)
            (case name
              (:set    (cl:first arguments))
              (:toggle (not had))
              (:conj   (d:with (or had (d:no-set)) (cl:first arguments)))
              (:disj   (d:without (or had (d:no-set)) (cl:first arguments)))
              (:merge  (d:merged (or had (d:no-map)) (cl:first arguments)))
              (t       (cl:first arguments)))))))

(defun %verbp (v)
  (and (d:seqp v) (plusp (d:size v)) (keywordp (d:lookup v 0))))

(defgeneric contents (node)
  (:documentation "What NODE holds. One method per kind, because there is one
answer per kind: a branch holds nothing, a value holds what was written, a derived
works it out and remembers, and a live one asks the world.")
  (:method ((n node)) nil)
  (:method ((n value)) (held n))
  (:method ((n derived))
    (let ((v (cached n)))
      (if (eq v +unread+) (%work-out n) v)))
  (:method ((n place))
    (cond ((reads n) (funcall (reads n)))
          ((names-of n) (%listed n))
          ((nodes-of n) (mapcar #'name (funcall (nodes-of n))))
          (t nil))))

(defun moved (n)
  "Say N moved: what read it is worked out again, and whoever is waiting for a
batch of writes to land is told. One word, because they are one event."
  (stir n)
  (commit:announce n)
  n)

(defgeneric (setf contents) (value node)
  (:documentation "Write NODE. A class says only what writing means; that it moved
is not its to remember, because a class written outside pine would have to
remember it too.")
  (:method (v (n node))
    (declare (ignore v))
    (error "~a holds nothing that can be written." (full-name n)))
  (:method (v (n value))
    (setf (held n) v))
  (:method (v (n derived))
    (if (writes n)
        (funcall (writes n) v)
        (progn (setf (reads n) (constantly v))
               (setf (cached n) +unread+)))
    v)
  (:method (v (n place))
    (unless (writes n)
      (error "~a holds nothing that can be written." (full-name n)))
    (funcall (writes n) v)
    v))

(defmethod contents ((n slot))
  (slot-value (object-of n) (slot-of n)))

(defmethod (setf contents) (v (n slot))
  "Writing it writes the object's slot, and says the object moved -- unless the
object is what this hangs under, which has already been said."
  (setf (slot-value (object-of n) (slot-of n)) v)
  (let ((o (object-of n)))
    (when (and (nodep o) (not (eq o (into-of n)))) (stir o)))
  v)

(defmethod contents :around ((n node))
  (reading n)
  (call-next-method))

(defmethod (setf contents) :around (value (n node))
  (if (%verbp value)
      (verb n (d:lookup value 0) (d:as :list (d:rest value)))
      (call-next-method)))

(defmethod (setf contents) :after (value (n node))
  (declare (ignore value))
  (moved n))

(defun slots (object into &rest pairs)
  "One node per slot of OBJECT, under INTO."
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot
                                        :name (string-downcase (string name))
                                        :object object :slot slot :into into)
                         into)))

(pine/word:lends "node" "contents" "nodes" "resolve" "stir" "name" "over"
                "full-name" "attach" "detach" "child" "derive" "describes"
                "nodep" "savedp" "livep" "announces" "refreshes" "slots" "verb"
                "place")
