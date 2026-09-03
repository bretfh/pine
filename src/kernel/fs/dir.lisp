(defpackage #:pine/fs
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export
   #:dir #:value #:derived #:kind
   #:contents #:holding #:verb #:savedp #:livep #:announces #:refreshes #:moved
   #:entries #:entry #:make-entry #:erase-entry #:works #:takes
   #:name #:parent #:owner #:describes #:full-name #:child #:slots
   #:attach #:detach #:announced #:reading #:depend #:undepend #:as-value #:let-go
   #:reads #:writes
   #:root #:make-root #:at #:ensure #:leaf #:erase #:walk #:paths #:split-name
   #:builder #:built #:declared #:undeclared #:*owner* #:absent #:not-a-place #:*root*
   #:writing #:on-commit #:on-forget #:forget-listeners
   #:*broke* #:*elsewhere* #:*working* #:*awaiting* #:*waiting-on* #:*waited*)
  (:documentation "The tree: three kinds of thing stand at a name.

A DIR has entries and holds nothing. A VALUE holds one thing that was written. A
DERIVED works one out from what it read and keeps it until something it read moves
-- or, where the world behind it answers and nothing here can see that move, asks
every time.

What a class answers is CONTENTS, (SETF CONTENTS), HOLDING, VERB, SAVEDP, LIVEP,
ANNOUNCES, REFRESHES and MOVED; a dir answers ENTRIES, ENTRY, MAKE-ENTRY and
ERASE-ENTRY; a derived may answer WORKS and TAKES instead of being given READS and
WRITES. Everything else here is called and not specialised."))
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
  ((name      :initarg :name      :reader name)
   (parent    :initarg :parent    :accessor parent    :initform nil)
   (owner     :initarg :owner     :accessor owner     :initform nil)
   (describes :initarg :describes :accessor describes :initform nil)
   (readers   :initform (d:no-set) :reader readers)
   (saw       :initform nil        :accessor saw)
   (version   :initform 0          :accessor version)
   (stamp     :initform 0          :accessor stamp)
   (checked   :initform -1         :accessor checked)
   (named     :initform nil        :accessor named)))

(defgeneric savedp (x) (:method ((x standing)) nil))
(defvar *owner* nil
  "Whose what is put up now is: the package of the system starting, while it does.")

(defgeneric livep (x) (:method ((x standing)) nil))
(defgeneric announces (x) (:method ((x standing)) nil))
(defgeneric refreshes (x) (:method ((x standing)) nil))
(defgeneric moved (x))

(defclass dir (standing)
  ((under     :initform (%under)  :reader under)
   (memo      :initform (d:table) :reader memo)
   (names     :initarg :names     :reader names-of   :initform nil)
   (each      :initarg :each      :reader each-of    :initform nil)
   (entries   :initarg :entries   :reader entries-of :initform nil)
   (announces :initarg :announces :reader announces  :initform nil)
   (refreshes :initarg :refreshes :reader refreshes  :initform nil))
  (:documentation "Entries. Attached one at a time, or listed from the world: NAMES
says what is under it and EACH makes one, kept so the same name is the same entry
every time; ENTRIES answers them all, already made."))

(defclass value (standing)
  ((held :initarg :held :accessor held :initform nil))
  (:documentation "Holds what was written, until it is written again."))

(defclass slot (value)
  ((object-of :initarg :object :reader object-of)
   (slot-of   :initarg :slot   :reader slot-of)
   (into      :initarg :into   :reader into-of :initform nil))
  (:documentation "A value held in a lisp object's slot."))

(defclass derived (standing)
  ((reads     :initarg :reads     :accessor reads    :initform nil)
   (writes    :initarg :writes    :accessor writes   :initform nil)
   (in        :initarg :in        :reader  in-of     :initform nil)
   (waits     :initarg :waits     :accessor waits-of :initform nil)
   (live      :initarg :live      :reader  livep     :initform nil)
   (announces :initarg :announces :reader  announces :initform nil)
   (refreshes :initarg :refreshes :reader  refreshes :initform nil)
   (cached    :initform +unread+ :accessor cached)
   (stood     :initform +unread+ :accessor stood)
   (claim     :initform nil :accessor claim)
   (claimed   :initform nil :accessor claimed)
   (waiting   :initform nil :accessor waiting)
   (scheduled :initform nil :accessor scheduled))
  (:documentation "Works its value out and remembers it until something it read
moves. LIVE says the world behind it answers and nothing here sees that move, so it
is asked every time. IN is another image, and READS is then a form worked out
there."))

(defmethod savedp ((x value)) t)

(defmethod livep ((d dir))
  (and (or (names-of d) (each-of d) (entries-of d)) t))

(defun kind (x)
  (typecase x (dir :dir) (value :value) (derived :derived)))

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
  (when (typep x 'dir)
    (d:do-each (each (beneath x)) (%renamed each))
    (dolist (each (d:vals (d:all (memo x)))) (%renamed each)))
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
    (or (d:lookup (d:all (memo d)) name)
        (let ((made (funcall builder)))
          (when made (d:claim (memo d) name made))))))

(defun %kid (d name)
  (child d name
         (lambda ()
           (let ((it (funcall (each-of d) name)))
             (when it (setf (parent it) d))
             it))))

(defun %listed (d) (mapcar #'princ-to-string (funcall (names-of d))))

(defgeneric entries (x)
  (:documentation "What is under X, in order.")
  (:method ((x standing)) nil)
  (:method ((d dir))
    (cond ((entries-of d) (funcall (entries-of d)))
          ((names-of d) (remove nil (mapcar (lambda (each) (%kid d each)) (%listed d))))
          (t (d:as :list (beneath d))))))

(defgeneric entry (x name)
  (:documentation "What X has under NAME, or nothing.")
  (:method ((x standing) name) (declare (ignore name)) nil)
  (:method ((d dir) name)
    (let ((name (%said name)))
      (cond ((entries-of d)
             (find name (funcall (entries-of d)) :key #'name :test #'equal))
            ((each-of d) (%kid d name))
            (t (d:lookup (by-name d) name))))))

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

(defgeneric attach (x into)
  (:documentation "Put X under INTO, in place of whatever stood at its name, and out
of wherever it was.")
  (:method ((x standing) (into dir))
    (let* ((said (%said (name x)))
           (had (d:lookup (by-name into) said))
           (was (parent x)))
      (when (and had (not (eq had x)))
        (detach into said)
        (when (eq had (d:lookup (d:all (memo into)) said))
          (d:drop! (memo into) said)))
      (when (and (typep was 'dir) (not (eq was into)))
        (%unlisted was x)
        (moved was))
      (setf (parent x) into)
      (d:swap (slot-value into 'under)
              (lambda (all)
                (%under (d:with (d:as :seq (cl:remove said
                                                      (d:as :list (under-order all))
                                                      :key #'name :test #'equal))
                                x)
                        (d:with (under-by-name all) said x))))
      (moved into))
    x))

(defgeneric detach (d name)
  (:documentation "Take NAME off D and stop it reading anything. What the memo keeps
stays, so a name taken off and put back answers the same entry; ERASE-ENTRY is the
one that means it has gone.")
  (:method ((d dir) name)
    (let ((gone (entry d name)))
      (when gone
        (%unlisted d gone)
        (dolist (on (saw gone)) (undepend gone on))
        (setf (saw gone) nil)
        (setf (parent gone) nil)
        (moved d))
      gone)))

(defgeneric make-entry (d name kind)
  (:documentation "A fresh entry under D called NAME, of KIND -- :DIR or :VALUE --
made in whatever stands behind D: a plain dir keeps it here, a mounted directory
makes a file on the disk.")
  (:method ((x standing) name kind)
    (declare (ignore kind))
    (error "~a holds a ~(~a~); nothing goes under it, so there is no ~a there."
           (full-name x) (kind x) name))
  (:method ((d dir) name kind)
    (when (or (each-of d) (entries-of d))
      (error "~a works out what is under it; ~a is not a place to make."
             (full-name d) name))
    (attach (make-instance (ecase kind (:dir 'dir) (:value 'value))
                           :name (%said name))
            d)))

(defgeneric erase-entry (d name)
  (:documentation "Take NAME out of D and out of whatever stands behind it.")
  (:method ((d dir) name)
    (let ((gone (entry d name)))
      (when gone (%went (full-name gone)))
      (let ((it (detach d name)))
        (d:drop! (memo d) (%said name))
        (or it gone)))))

(defgeneric let-go (x owner)
  (:documentation "Take back what OWNER put into X, where X holds what several
owners put there.")
  (:method ((x standing) owner) (declare (ignore owner)) nil))

(defun slots (object into &rest pairs)
  "One value under INTO per slot of OBJECT named in PAIRS."
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot
                                        :name (string-downcase (string name))
                                        :object object :slot slot :into into)
                         into)))

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
  (:documentation "What a derived works out to. Answered by a class, or by the
READS it was given.")
  (:method ((n derived))
    (let ((f (reads n))) (and f (funcall f)))))

(defgeneric takes (n value)
  (:documentation "What writing a derived means. Answered by a class, or by the
WRITES it was given.")
  (:method ((n derived) value)
    (let ((f (writes n)))
      (unless f (error "~a is worked out, and takes no writing." (full-name n)))
      (funcall f value))))

(defgeneric contents (x)
  (:documentation "What X holds: a dir the names under it, a value what was written,
a derived what it works out to.")
  (:method ((d dir))
    (if (names-of d) (%listed d) (mapcar #'name (entries d))))
  (:method ((x value)) (held x))
  (:method ((x slot)) (slot-value (object-of x) (slot-of x))))

(defgeneric holding (x)
  (:documentation "Which of :BRANCH, :HELD, :WORKING or :ABSENT stands here.")
  (:method ((d dir)) :branch)
  (:method ((x value)) :held))

(defgeneric (setf contents) (value x)
  (:method (v (d dir))
    (declare (ignore v))
    (error "~a is a dir; what is written is what is under it." (full-name d)))
  (:method (v (x value)) (setf (held x) v))
  (:method (v (x slot))
    (setf (slot-value (object-of x) (slot-of x)) v)
    (let ((o (object-of x)))
      (when (and (kind o) (not (eq o (into-of x)))) (moved o)))
    v))

(defmethod (setf contents) :around (v (x standing))
  (cond ((%verbp v) (verb x (d:lookup v 0) (d:as :list (fset:subseq v 1))))
        ((%quotedp v) (call-next-method (fset:subseq v 1) x))
        (t (call-next-method))))
