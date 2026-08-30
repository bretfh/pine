(defpackage #:pine/fs/node
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:commit #:pine/fs/commit))
  (:export
   #:node #:derived #:live #:value #:slot #:place
   #:contents #:nodes #:resolve #:holding #:verb
   #:make-child #:erase-child #:savedp #:livep
   #:announces #:refreshes #:moved)
  (:export
   #:nodep #:name #:parent #:describes #:full-name #:memo
   #:make #:derive #:answers #:lists #:child #:slots
   #:attach #:detach #:announced #:reads #:writes)
  (:export
   #:depend #:undepend #:reading #:version #:mark #:currentp #:stalep
   #:work-out #:freshp #:cached #:saw #:in-of
   #:*broke* #:*elsewhere* #:*waiting-on* #:*waited*)
  (:documentation "What is addressable, and what answering for one means.

Three lists, and which one a name is in is the whole of what somebody writing a
node class has to know.

The first is the protocol: what a class answers. Everything under src/ that is not
a node itself specialises only these, which is the evidence that the line is here
and not somewhere else.

The second is what pine does with a node and what makes one: called, never
specialised.

The third is the graph -- what read what, at which version, and what is worked out
again when something moves. It is in src/fs/graph.lisp, it is pine's own, and a
class that answers one of these is a class fighting the substrate."))
(in-package #:pine/fs/node)

(defvar *reading* nil)
(defvar *walking* nil
  "The walk going on, where one is. One number threaded through a walk, so a ring
is stepped once; the stamp it leaves is shared, so two threads walking at the same
time stop on each other's work rather than each doing all of it.")
(defvar *epoch* 0)
(defvar *writes* 0
  "How many times anything anywhere has been written.

Raised before a write does anything else, so it is never behind. Nothing decides
anything by it on its own: it is what says whether an answer found to stand a
moment ago is still worth trusting without looking again, and the looking again is
what is being saved.")
(defparameter +unread+ '#:unread)
(defvar *elsewhere* nil
  "How to work a node out in another image, given where and the form. Filled in by
whatever knows what an image is, because this layer loads before there is one --
the same reason *BROKE* is one.")

(defclass node ()
  ((name      :initarg :name      :reader name)
   (parent      :initarg :parent      :accessor parent      :initform nil)
   (describes :initarg :describes :accessor describes :initform nil)
   (beneath   :initform (d:no-seq) :reader beneath)
   (by-name   :initform (d:no-map) :reader by-name)
   (memo      :initform (d:table)  :reader memo)
   (readers   :initform (d:no-set) :reader readers)
   (saw       :initform nil        :accessor saw)
   (version   :initform 0          :accessor version)
   (stamp     :initform 0          :accessor stamp)
   (checked   :initform -1         :accessor checked)
   (named     :initform nil        :accessor named))
  (:documentation "A name, what it sits under, and what is under it. On its own it
is a branch: it holds nothing, and what is under it is whatever was attached.

READERS is who is worked out from this one and SAW is what this one was worked
out from -- the same edges, kept at both ends, because a write walks one way and
a node being taken off the tree has to let go the other.

BENEATH is what is under it in the order it was attached, and BY-NAME is the same
nodes under the names they answer to. Two, because the two questions are different
and only one of them is asked often: what is under this is asked when somebody
lists it, and which one is called that is asked for every piece of every name
anybody ever says. Kept only in order, the second is a walk of the whole list --
and a copy of it, because a seq has to be made a list to be walked -- for every
piece of every path.

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
   (in     :initarg :in      :reader  in-of   :initform nil)
   (cached  :initform +unread+ :accessor cached)
   (claim   :initform nil :accessor claim)
   (waiting :initform nil :accessor waiting))
  (:documentation "Works its value out, and remembers it until something it read
moves. What READS reads is recorded while it runs, so a write invalidates exactly
what depended on it and nothing subscribes to anything.

WRITES because working a value out and being able to write it are two questions:
/dev/audio/volume is worked out from what wpctl says and writing it sets the
volume. Without one, writing replaces what it works out with what you wrote.

IN is where the working-out happens, where it does not happen here. READS is then a
form rather than a closure, because a form is a value and a closure is not, and it
is evaluated in that image. What it read there is that image's business: a node
worked out somewhere else records no edges here, so it is stirred by being told and
not by a walk."))

(defclass live (node) ()
  (:documentation "The world behind this answers, rather than a value kept here.

Nothing is remembered, because the world moves without anybody writing it; that is
what DERIVED, which does remember, is the other half of. A job, a mounted
directory, a compositor and a region of a document are all this: each answers
CONTENTS from whatever stands behind it, and saying so is the whole of what they
have in common.

PLACE, in the file beside this one, is the one that answers through closures
rather than through a method of its own."))

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
  "A node that works its value out and remembers it until something it read moves.

:IN <image> works it out there instead of here, and READS is then a form. That is
the boundary: a READS is somebody else's code and it talks to the world, and one
that will not stop takes the image it runs in with it. Everything else here --
the claim, the deadline, the fault that comes home with its restarts -- protects
the other readers of a node. This is what protects the image."
  (apply #'make-instance 'derived :name name :reads reads initargs))

(defun %named (n)
  (let ((names (loop :for at := n :then (parent at)
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
  (loop :for at := n :then (parent at)
        :while (parent at)
        :finally (return at)))

(defun children (n) (d:vals (d:all (memo n))))

(defun child (n name builder)
  "The child N keeps under NAME, made once. Two threads asking at once both answer
the one that landed, which is what lets anything reading it be worked out again.

A builder that answers nothing leaves nothing behind: a name nobody has put
anything at is asked about once per ask, rather than filling the memo with an
entry that says so and that every walk of the children then has to step over."
  (let ((name (princ-to-string name)))
    (or (d:lookup (d:all (memo n)) name)
        (let ((made (funcall builder)))
          (when made (d:claim (memo n) name made))))))

(defgeneric nodes (node)
  (:documentation "What is under NODE, in order.

The one place a class says what it contains. RESOLVE and LEAFP are written on this,
so a class that answers here answers everywhere.")
  (:method ((n node)) (d:as :list (beneath n))))

(declaim (inline %said))
(defun %said (name)
  "NAME as the string it answers to, without copying one that already is.
PRINC-TO-STRING of a string is a fresh string, and this is on the path every
name-walk takes."
  (if (stringp name) name (princ-to-string name)))

(defgeneric resolve (node name)
  (:documentation "What NODE has under NAME.

Answered from BY-NAME, which holds the same nodes ATTACH put in BENEATH, so the
two cannot disagree. Where they are worked out, EACH answers what is there and
nothing where it answers nothing: NAMES says what to list, which is not always
the same question. Every shell line is a place whether or not one has been run,
and /sh lists the ones that have.")
  (:method ((n node) name)
    (d:lookup (by-name n) (%said name))))

(defun leafp (n) (null (nodes n)))

(defgeneric attach (node into)
  (:documentation "Put NODE under INTO, replacing whatever stood at its name.

Both go together: the order it was attached in, and the name it answers to. A
node put in one and not the other is one that lists and cannot be reached, or is
reached and never listed.")
  (:method ((n node) (into node))
    (setf (parent n) into)
    (%renamed n)
    (let ((said (%said (name n))))
      (d:swap (slot-value into 'beneath)
              (lambda (all)
                (d:with (d:as :seq (cl:remove said (d:as :list all)
                                              :key #'name :test #'equal))
                        n)))
      (d:swap (slot-value into 'by-name) (lambda (all) (d:with all said n))))
    n))

(defgeneric detach (node name)
  (:documentation "Take NAME off NODE, and stop it reading anything.

What it read has to be given up here. A node taken off the tree and left in the
reader sets of what it read is worked out for ever after, every time any of that
moves, and holds all of it for as long as the image runs. A surface that was
erased goes on being worked out from /dev/cpu.")
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (d:swap (slot-value n 'beneath) (lambda (all) (d:remove gone all)))
        (d:swap (slot-value n 'by-name)
                (lambda (all) (d:without all (%said (name gone)))))
        (dolist (on (saw gone)) (undepend gone on))
        (setf (saw gone) nil)
        (setf (parent gone) nil)
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
    "What was kept is let go after it is detached, not before. Dropped first, the
detach asks for it again, a worked-out child is built again to be taken off, and
what is left behind in the memo is that second one with nothing over it."
    (let ((gone (resolve n name)))
      (when gone (commit:forget (full-name gone)))
      (let ((it (detach n name)))
        (d:drop! (memo n) (princ-to-string name))
        (or it gone)))))

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
  (and (d:seqp v) (plusp (d:size v)) (keywordp (d:lookup v 0))
       (not (eq :quoted (d:lookup v 0)))))

(defun %quotedp (v)
  "Whether V is a seq somebody meant as a value rather than as something to do.
Without this a seq beginning with a keyword can only ever be an instruction, and
storing one is storing something else."
  (and (d:seqp v) (plusp (d:size v)) (eq :quoted (d:lookup v 0))))

(defgeneric contents (node)
  (:documentation "What NODE holds. One method per kind, because there is one
answer per kind: a branch holds nothing, a value holds what was written, a derived
works it out and remembers, and a live one asks the world.")
  (:method ((n node)) nil)
  (:method ((n value)) (held n)))

(defgeneric holding (node)
  (:documentation "Which kind of nothing this holds when it holds nothing: :BRANCH
where it is a name with things under it, :HELD where NIL is what it holds.

A live one is never asked what is under it. What a device has beneath it belongs to
the world, and walking it to label a read would be shelling out to answer a question
about the answer.")
  (:method ((n node)) (if (and (null (contents n)) (nodes n)) :branch :held))
  (:method ((n live)) :held))

(defgeneric (setf contents) (value node)
  (:documentation "Write NODE. A class says only what writing means; that it moved
is not its to remember, because a class written outside pine would have to
remember it too.")
  (:method (v (n node))
    (declare (ignore v))
    (error "~a holds nothing that can be written." (full-name n)))
  (:method (v (n value))
    (setf (held n) v)))

(defmethod contents ((n slot))
  (slot-value (object-of n) (slot-of n)))

(defmethod (setf contents) (v (n slot))
  "Writing it writes the object's slot, and says the object moved -- unless the
object is what this hangs under, which has already been said."
  (setf (slot-value (object-of n) (slot-of n)) v)
  (let ((o (object-of n)))
    (when (and (nodep o) (not (eq o (into-of n)))) (moved o)))
  v)

(defmethod (setf contents) :around (value (n node))
  (cond ((%verbp value)
         (verb n (d:lookup value 0) (d:as :list (d:rest value))))
        ((%quotedp value) (call-next-method (d:rest value) n))
        (t (call-next-method))))

(defun slots (object into &rest pairs)
  "One node per slot of OBJECT, under INTO."
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot
                                        :name (string-downcase (string name))
                                        :object object :slot slot :into into)
                         into)))

