(defpackage #:pine/fs
  (:use #:cl)
  (:shadow #:directory #:read #:write)
  (:local-nicknames (#:d #:pine/data))
  (:export
   #:mount #:value #:derived #:kind #:read #:write #:of #:key #:served
   #:contents #:holding #:verb #:savedp #:livep #:announces #:refreshes #:moved
   #:entries #:entry #:make-entry #:erase-entry #:works #:takes
   #:name #:parent #:owner #:describes #:full-name #:child
   #:detach #:announced #:reading #:depend #:undepend #:as-value #:let-go
   #:reads #:writes
   #:root #:make-root #:at #:erase #:walk #:paths #:split-name
   #:directory #:file #:truename-of #:node-for
   #:*owner* #:absent #:not-a-place #:*root*
   #:writing #:on-commit #:on-forget #:forget-listeners
   #:*broke* #:*elsewhere* #:*working* #:*awaiting* #:*waiting-on* #:*waited*)
  (:documentation "The tree: three kinds of thing stand at a name.

A MOUNT serves what is under it and holds nothing of its own. A VALUE holds one
thing that was written. A DERIVED works one out from what it read and keeps it
until something it read moves -- or, where the world behind it answers and nothing
here can see that move, asks every time.

MOUNT is also the verb: what puts a thing at a name. A kind of mount answers for
the names under it by methods on READ and WRITE, one per name; each such name is a
derived under every mount of that kind, live unless the kind's LIVEP says it is
worked out and kept. What a class answers besides is CONTENTS, (SETF CONTENTS),
HOLDING, VERB, SAVEDP, LIVEP, ANNOUNCES, REFRESHES and MOVED; a mount may answer
ENTRIES, ENTRY, MAKE-ENTRY and ERASE-ENTRY for what it lists from the world; a
derived may answer WORKS and TAKES. Everything else here is called and not
specialised."))
(in-package #:pine/fs)

(defvar *reading* nil)
(defvar *walking* nil)
(defvar *epoch* 0)
(defvar *writes* 0)
(defparameter +unread+ '#:unread)
(defvar *broke* nil)
(defvar *elsewhere* nil)
(defvar *working* nil)
(defvar *awaiting* nil)

(defstruct (under (:constructor %under (&optional order by-name)) (:copier nil))
  (order (d:no-seq))
  (by-name (d:no-map)))

(defclass standing ()
  ((name      :initarg :name      :reader name      :initform nil)
   (parent    :initarg :parent    :accessor parent    :initform nil)
   (owner     :initarg :owner     :accessor owner     :initform nil)
   (of        :initarg :of        :reader of          :initform nil)
   (describes :initarg :describes :accessor describes :initform nil)
   (readers   :initform (d:no-set) :reader readers)
   (saw       :initform nil        :accessor saw)
   (version   :initform 0          :accessor version)
   (stamp     :initform 0          :accessor stamp)
   (checked   :initform -1         :accessor checked)
   (named     :initform nil        :accessor named)))

(defgeneric savedp (x) (:method ((x standing)) nil))
(defvar *owner* nil
  "Whose what is mounted now is: the package of the system starting, while it does.")

(defgeneric livep (x &optional name)
  (:documentation "Whether the world behind X answers, so it is asked every time.
With NAME, whether that name under X is: a name answered by method is, unless the
kind says it is worked out and kept.")
  (:method ((x standing) &optional name) (declare (ignore name)) nil))
(defgeneric announces (x) (:method ((x standing)) nil))
(defgeneric refreshes (x) (:method ((x standing)) nil))
(defgeneric moved (x))

(defclass mount (standing)
  ((under     :initform (%under)  :reader under)
   (memo      :initform (d:no-map) :accessor memo)
   (names     :initarg :names     :reader names-of   :initform nil)
   (each      :initarg :each      :reader each-of    :initform nil)
   (entries   :initarg :entries   :reader entries-of :initform nil)
   (announces :initarg :announces :reader announces  :initform nil)
   (refreshes :initarg :refreshes :reader refreshes  :initform nil))
  (:documentation "Serves a subtree: what was mounted under it, or what it lists from
the world. NAMES says what is under it and EACH makes one, kept so the same name is
the same entry every time; ENTRIES answers them all, already made."))

(defclass value (standing)
  ((held :initarg :held :accessor held :initform nil))
  (:documentation "Holds what was written, until it is written again."))

(defclass derived (standing)
  ((reads     :initarg :reads     :accessor reads    :initform nil)
   (writes    :initarg :writes    :accessor writes   :initform nil)
   (key       :initarg :key       :reader  key       :initform nil)
   (in        :initarg :in        :reader  in-of     :initform nil)
   (waits     :initarg :waits     :accessor waits-of :initform nil)
   (live      :initarg :live      :reader  live-of   :initform nil)
   (announces :initarg :announces :reader  announces :initform nil)
   (refreshes :initarg :refreshes :reader  refreshes :initform nil)
   (cached    :initform +unread+ :accessor cached)
   (stood     :initform +unread+ :accessor stood)
   (claim     :initform nil :accessor claim)
   (claimed   :initform nil :accessor claimed)
   (waiting   :initform nil :accessor waiting)
   (scheduled :initform nil :accessor scheduled))
  (:documentation "Works its value out and remembers it until something it read
moves. KEY, with OF, says it is a name a mount answers for by method. LIVE says
the world behind it answers and nothing here sees that move, so it is asked every
time. IN is another image, and READS is then a form worked out there."))

(defmethod savedp ((x value)) t)

(defmethod livep ((n derived) &optional name)
  (declare (ignore name))
  (live-of n))

(defmethod livep ((d mount) &optional name)
  (if name
      t
      (and (or (names-of d) (each-of d) (entries-of d)) t)))

(defgeneric read (mount name)
  (:documentation "What MOUNT answers for NAME, a keyword. A method on a kind of
mount and a name is what stands at that name under every mount of that kind.")
  (:method ((d mount) name) (declare (ignore name)) nil))

(defgeneric write (mount name value)
  (:documentation "What writing NAME under MOUNT means.")
  (:method ((d mount) name value)
    (declare (ignore value))
    (error "~a answers ~(~a~), and takes no writing there." (full-name d) name)))

(defvar *served* (make-hash-table :test 'eq :synchronized t)
  "What each kind of mount answers by method, worked out once per class for as
long as the methods stay the same.")

(defun %methods ()
  (append (reverse (sb-mop:generic-function-methods #'read))
          (reverse (sb-mop:generic-function-methods #'write))))

(defun %count ()
  (+ (length (sb-mop:generic-function-methods #'read))
     (length (sb-mop:generic-function-methods #'write))))

(defun %served (d)
  "What D answers for by method, as (key . name) pairs in the order they were
said, worked out once per class and again only when a method is added. Asked on
every lookup of a name, so nothing here conses until it has to."
  (let* ((class (class-of d))
         (count (%count))
         (had (gethash class *served*)))
    (if (and had (eql (car had) count))
        (cdr had)
        (let ((keys (loop :for m :in (%methods)
                          :for (on key) := (sb-mop:method-specializers m)
                          :when (and (typep on 'class) (typep d on)
                                     (typep key 'sb-mop:eql-specializer))
                            :collect (sb-mop:eql-specializer-object key))))
          (let ((pairs (loop :for key :in (remove-duplicates keys :from-end t)
                             :collect (cons key (string-downcase (symbol-name key))))))
            (setf (gethash class *served*) (cons count pairs))
            pairs)))))

(defun served (d)
  "The names D answers for by a method on READ or WRITE, as keywords, in the order
they were said."
  (mapcar #'car (%served d)))

(defun %key-of (d name)
  (car (find name (%served d) :key #'cdr :test #'string=)))

(defun %doc (d key)
  (let ((m (find-if (lambda (m)
                      (destructuring-bind (on k &rest more) (sb-mop:method-specializers m)
                        (declare (ignore more))
                        (and (typep on 'class) (typep d on)
                             (typep k 'sb-mop:eql-specializer)
                             (eq key (sb-mop:eql-specializer-object k)))))
                    (%methods))))
    (and m (documentation m t))))

(defun kind (x)
  (typecase x (mount :mount) (value :value) (derived :derived)))

(defmethod initialize-instance :after ((x standing) &key)
  (let ((said (slot-value x 'name)))
    (unless (or (null said) (stringp said))
      (setf (slot-value x 'name) (princ-to-string said)))))

(defmethod print-object ((x standing) stream)
  (print-unreadable-object (x stream :type t)
    (write-string (full-name x) stream)))

(defun beneath (d) (under-order (under d)))
(defun by-name (d) (under-by-name (under d)))

(defun %named (x)
  (let ((names (loop :for at := x :then (parent at)
                     :while at
                     :when (name at) :collect (name at))))
    (if names (format nil "/~{~a~^/~}" (reverse names)) "/")))

(defun full-name (x)
  (or (named x) (setf (named x) (%named x))))

(defun %renamed (x)
  (setf (named x) nil)
  (when (typep x 'mount)
    (d:do-each (each (beneath x)) (%renamed each))
    (dolist (each (d:vals (memo x))) (%renamed each)))
  x)

(defmethod (setf parent) :after (value (x standing))
  (declare (ignore value))
  (%renamed x))

(declaim (inline %said))
(defun %said (name)
  (if (stringp name) name (princ-to-string name)))

(defun child (d name builder)
  "The entry D keeps under NAME, made once by BUILDER; nothing where it makes
nothing."
  (let ((name (%said name)))
    (or (d:lookup (memo d) name)
        (let ((made (funcall builder)))
          (when made
            (d:lookup (d:swap (slot-value d 'memo)
                              (lambda (m)
                                (if (nth-value 1 (d:lookup m name))
                                    m
                                    (d:with m name made))))
                      name))))))

(defun %kid (d name)
  (child d name
         (lambda ()
           (let ((it (funcall (each-of d) name)))
             (when it (setf (parent it) d))
             it))))

(defun %listed (d) (mapcar #'princ-to-string (funcall (names-of d))))

(defun %answered (d key)
  "The derived under D for a name it answers by method, made once."
  (let ((name (string-downcase (symbol-name key))))
    (child d name
           (lambda ()
             (make-instance 'derived :name name :key key :of d :parent d
                                     :live (livep d name)
                                     :describes (%doc d key))))))

(defun %answering (d)
  (loop :for (key . name) :in (%served d)
        :unless (d:lookup (by-name d) name)
          :collect (%answered d key)))

(defgeneric entries (x)
  (:documentation "What is under X, in order: what was mounted, then what it
answers for by method.")
  (:method ((x standing)) nil)
  (:method ((d mount))
    (cond ((entries-of d) (funcall (entries-of d)))
          ((names-of d) (remove nil (mapcar (lambda (each) (%kid d each)) (%listed d))))
          (t (append (d:as :list (beneath d)) (%answering d))))))

(defgeneric entry (x name)
  (:documentation "What X has under NAME, or nothing.")
  (:method ((x standing) name) (declare (ignore name)) nil)
  (:method ((d mount) name)
    (let ((name (%said name)))
      (cond ((entries-of d)
             (find name (funcall (entries-of d)) :key #'name :test #'equal))
            ((each-of d) (%kid d name))
            (t (or (d:lookup (by-name d) name)
                   (let ((key (%key-of d name)))
                     (and key (%answered d key)))))))))

(defgeneric depend (x on)
  (:method (x (on standing))
    (d:swap (slot-value on 'readers) (lambda (all) (d:with all x)))
    x))

(defgeneric undepend (x on)
  (:method (x (on standing))
    (d:swap (slot-value on 'readers) (lambda (all) (d:without all x)))
    x))

(defun %unlisted (d x)
  (d:swap (slot-value d 'under)
          (lambda (all)
            (%under (fset:remove x (under-order all))
                    (d:without (under-by-name all) (%said (name x))))))
  d)

(defun %plainp (x)
  "A bare mount, or a value with nothing in it: one made only to stand at a name."
  (or (eq (class-of x) (find-class 'mount))
      (and (eq (class-of x) (find-class 'value)) (null (held x)))))

(defgeneric mount (what where)
  (:documentation "Put WHAT at WHERE: under a mount by its own name, or at the path
WHERE spells, making the way there. It stands in place of whatever stood at the
name, and out of wherever it was; a bare mount or an empty value put where something
stands answers what stands. What is mounted while a system starts is the system's.
A function is what it makes, mounted now and again on every root made later."))

(defmethod mount ((x standing) (into mount))
  (let* ((said (%said (name x)))
         (had (d:lookup (by-name into) said))
         (was (parent x)))
    (when (or (each-of into) (entries-of into))
      (error "~a works out what is under it; ~a is not a place to mount at."
             (full-name into) said))
    (flet ((put ()
             (when had
               (detach into said)
               (when (eq had (d:lookup (memo into) said))
                 (d:swap (slot-value into 'memo) #'d:without said)))
             (when (and (typep was 'mount) (not (eq was into)))
               (%unlisted was x)
               (moved was))
             (when *owner* (setf (owner x) *owner*))
             (setf (parent x) into)
             (d:swap (slot-value into 'under)
                     (lambda (all)
                       (%under (d:with (d:as :seq (cl:remove said
                                                             (d:as :list (under-order all))
                                                             :key #'name :test #'equal))
                                       x)
                               (d:with (under-by-name all) said x))))
             (moved into)
             x))
      (cond ((eq had x) x)
            ((%plainp x)
             (let ((stands (or had (entry into said))))
               (cond (stands
                      (when (describes x) (setf (describes stands) (describes x)))
                      stands)
                     (t (put)))))
            (t (put))))))

(defgeneric detach (d name)
  (:documentation "Take NAME off D and stop it reading anything. What the memo keeps
stays, so a name taken off and put back answers the same entry; ERASE-ENTRY is the
one that means it has gone.")
  (:method ((d mount) name)
    (let ((gone (entry d name)))
      (when gone
        (%unlisted d gone)
        (dolist (on (saw gone)) (undepend gone on))
        (setf (saw gone) nil)
        (setf (parent gone) nil)
        (moved d))
      gone)))

(defgeneric make-entry (d name kind)
  (:documentation "A fresh entry under D called NAME, of KIND -- :MOUNT or :VALUE --
made in whatever stands behind D: a plain mount keeps it here, a mounted directory
makes a file on the disk.")
  (:method ((x standing) name kind)
    (declare (ignore kind))
    (error "~a holds a ~(~a~); nothing goes under it, so there is no ~a there."
           (full-name x) (kind x) name))
  (:method ((d mount) name kind)
    (when (or (each-of d) (entries-of d))
      (error "~a works out what is under it; ~a is not a place to make."
             (full-name d) name))
    (mount (make-instance (ecase kind (:mount 'mount) (:value 'value))
                          :name (%said name))
           d)))

(defgeneric erase-entry (d name)
  (:documentation "Take NAME out of D and out of whatever stands behind it.")
  (:method ((d mount) name)
    (let ((gone (entry d name)))
      (when gone (%went (full-name gone)))
      (let ((it (detach d name)))
        (d:swap (slot-value d 'memo) #'d:without (%said name))
        (or it gone)))))

(defgeneric let-go (x owner)
  (:documentation "Take back what OWNER put into X, where X holds what several
owners put there.")
  (:method ((x standing) owner) (declare (ignore owner)) nil))

(defgeneric verb (x name arguments)
  (:documentation "What writing (:toggle) and its like means.")
  (:method ((x standing) name arguments)
    (let ((had (contents x)))
      (setf (contents x)
            (case name
              (:set    (first arguments))
              (:toggle (not had))
              (:conj   (d:with (or had (d:no-set)) (first arguments)))
              (:disj   (d:without (or had (d:no-set)) (first arguments)))
              (:merge  (d:merged (or had (d:no-map)) (first arguments)))
              (t       (first arguments)))))))

(defun %verbp (v)
  (and (d:seqp v) (plusp (d:size v)) (keywordp (d:lookup v 0))
       (not (eq :quoted (d:lookup v 0)))))

(defun as-value (v)
  "V spelled so that writing it stores it: a seq beginning with a keyword is
otherwise an instruction to VERB."
  (if (%verbp v) (fset:concat (d:seq :quoted) v) v))

(defun %quotedp (v)
  (and (d:seqp v) (plusp (d:size v)) (eq :quoted (d:lookup v 0))))

(defgeneric works (n)
  (:documentation "What a derived works out to: what the mount it is under answers
for its name, or what a class answers, or the READS it was given.")
  (:method ((n derived))
    (cond ((key n) (read (of n) (key n)))
          ((reads n) (funcall (reads n))))))

(defgeneric takes (n value)
  (:documentation "What writing a derived means: what the mount it is under does
with its name, or what a class answers, or the WRITES it was given.")
  (:method ((n derived) value)
    (cond ((key n) (write (of n) (key n) value))
          ((writes n) (funcall (writes n) value))
          (t (error "~a is worked out, and takes no writing." (full-name n))))))

(defgeneric contents (x)
  (:documentation "What X holds: a mount the names under it, a value what was
written, a derived what it works out to.")
  (:method ((d mount))
    (if (names-of d) (%listed d) (mapcar #'name (entries d))))
  (:method ((x value)) (held x)))

(defgeneric holding (x)
  (:documentation "Which of :BRANCH, :HELD, :WORKING or :ABSENT stands here.")
  (:method ((d mount)) :branch)
  (:method ((x value)) :held))

(defgeneric (setf contents) (value x)
  (:method (v (d mount))
    (declare (ignore v))
    (error "~a is a mount; what is written is what is under it." (full-name d)))
  (:method (v (x value)) (setf (held x) v)))

(defmethod (setf contents) :around (v (x standing))
  (cond ((%verbp v) (verb x (d:lookup v 0) (d:as :list (fset:subseq v 1))))
        ((%quotedp v) (call-next-method (fset:subseq v 1) x))
        (t (call-next-method))))
