(defpackage #:pine/fs/node
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:commit #:pine/fs/commit))
  (:export #:node #:nodep
           #:name #:over #:describes #:savedp #:livep #:storedp
           #:announces #:refreshes
           #:contents #:nodes #:resolve #:stir #:moved #:verb
           #:full-name #:leafp #:stalep #:root
           #:make #:derive #:place #:attach #:detach #:child #:children #:memo
           #:slots #:make-child #:erase-child
           #:reads #:writes #:names-of #:each-of #:nodes-of #:saw
           #:readers #:depend #:undepend #:reading #:*reading*))
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
   (named     :initform nil        :accessor named)
   (reads     :initarg :reads   :accessor reads    :initform nil)
   (writes    :initarg :writes  :accessor writes   :initform nil)
   (names     :initarg :names   :reader  names-of  :initform nil)
   (each      :initarg :each    :reader  each-of   :initform nil)
   (listing   :initarg :nodes   :reader  nodes-of  :initform nil)
   (announces :initarg :announces :reader announces :initform nil)
   (refreshes :initarg :refreshes :reader refreshes :initform nil)
   (savedp    :initarg :savedp  :reader  savedp    :initform nil)
   (cachedp   :initarg :cachedp :reader  cachedp   :initform nil)
   (livep     :initarg :livep   :reader  livep     :initform nil)
   (cached    :initform +unread+ :accessor cached)
   (saw       :initform nil      :accessor saw))
  (:documentation "A name, what it sits under, and what is under it.

One class, four closures and three flags, because that is all a node ever was.

READS answers what it holds and WRITES says what writing it means; with neither,
the value is kept in the commit store and SAVEDP says so. NAMES says what is
under it and EACH makes one, and what EACH made is kept, so the same name is the
same node every time; with NODES the children are nodes something else already
keeps. With none of the three it is a leaf.

Three flags, because there are three questions and no two of them answer each
other. SAVEDP: does what this holds outlive the image. CACHEDP: may the answer be
remembered until something says it moved. LIVEP: does the world behind this
answer, rather than a value kept here -- which is what says a snapshot has no
business walking into it and what says a watcher has to ask rather than be
told."))

(defun storedp (n)
  "Whether this node's value is kept in the commit store, which is what a write
of several at once can go through."
  (and (savedp n) (null (reads n)) (null (writes n))))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (write-string (full-name n) stream)))

(defun nodep (x) (typep x 'node))

(defun make (name &rest initargs &key (class 'node) (savedp t) &allow-other-keys)
  "A node holding one value, kept where it will be found again."
  (apply #'make-instance class :name name :savedp savedp
         (alexandria:remove-from-plist initargs :class :savedp)))

(defun derive (name reads &rest initargs)
  "A node that works its value out and remembers it until something it read moves."
  (apply #'make-instance 'node :name name :reads reads :cachedp t initargs))

(defun place (name &rest initargs)
  "Somewhere the world answers: how to read it, how to write it, and what is
under it. Nothing is remembered, because the world moves without anybody writing
it; that is what DERIVE, which does remember, is the other half of."
  (apply #'make-instance 'node :name (princ-to-string name)
         (append initargs (list :livep t))))

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
    (or (d:at (d:all (memo n)) name)
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
  (:method ((n node))
    (cond ((nodes-of n) (funcall (nodes-of n)))
          ((names-of n)
           (remove nil (mapcar (lambda (each) (%kid n each)) (%listed n))))
          (t (d:as :list (beneath n))))))

(defgeneric resolve (node name)
  (:documentation "What NODE has under NAME.

Derived from NODES where the children are attached, so the two cannot disagree.
Where they are worked out, EACH answers what is there and nothing where it
answers nothing: NAMES says what to list, which is not always the same question.
Every shell line is a place whether or not one has been run, and /sh lists the
ones that have.")
  (:method ((n node) name)
    (let ((name (princ-to-string name)))
      (cond ((nodes-of n)
             (find name (funcall (nodes-of n)) :key #'name :test #'equal))
            ((each-of n) (%kid n name))
            (t (find name (d:as :list (beneath n)) :key #'name :test #'equal))))))

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
    (when (cachedp n) (setf (cached n) +unread+))
    (d:do-each (each (readers n))
      (stir each))
    n))

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
  (and (d:seqp v) (plusp (d:size v)) (keywordp (d:at v 0))))

(defgeneric contents (node)
  (:documentation "What NODE holds.")
  (:method ((n node))
    (cond ((cachedp n)
           (let ((v (cached n)))
             (if (eq v +unread+) (%work-out n) v)))
          ((reads n) (funcall (reads n)))
          ((names-of n) (%listed n))
          ((nodes-of n) (mapcar #'name (funcall (nodes-of n))))
          ((savedp n) (commit:at (full-name n)))
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
  (:method (value (n node))
    (cond ((writes n) (funcall (writes n) value))
          ((cachedp n)
           (setf (reads n) (constantly value))
           (setf (cached n) +unread+))
          ((savedp n) (commit:change (d:map (full-name n) value)))
          (t (error "~a holds nothing that can be written." (full-name n))))
    value))

(defmethod contents :around ((n node))
  (reading n)
  (call-next-method))

(defmethod (setf contents) :around (value (n node))
  (if (%verbp value)
      (verb n (d:at value 0) (d:as :list (d:rest value)))
      (call-next-method)))

(defmethod (setf contents) :after (value (n node))
  (declare (ignore value))
  (moved n))

(defun slots (object into &rest pairs)
  "One node per slot of OBJECT, under INTO. A slot is a place like any other: how
to read it and how to write it are two closures over the object."
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (let ((slot slot))
                   (attach (make-instance
                            'node :name (string-downcase (string name))
                            :savedp t
                            :reads (lambda () (slot-value object slot))
                                  :writes (lambda (value)
                                            (setf (slot-value object slot) value)
                                            (when (and (nodep object)
                                                       (not (eq object into)))
                                              (stir object))))
                           into))))

(pine/word:lends "node" "contents" "nodes" "resolve" "stir" "name" "over"
                "full-name" "attach" "detach" "child" "derive" "describes"
                "nodep" "savedp" "livep" "announces" "refreshes" "slots" "verb")
