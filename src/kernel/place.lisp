(defpackage #:pine/kernel/place
  (:use #:cl)
  (:shadow #:read)
  (:local-nicknames (#:d #:pine/data) (#:tell #:pine/kernel/tell))
  (:export
   #:place #:placep #:value #:derived #:world #:listing #:mount #:job
   #:name-of #:under #:full-name #:beneath #:kids #:resolve #:attach #:detach
   #:held #:read #:works #:puts #:asks #:names-of #:each-of #:reached #:runs
   #:took #:state #:holds #:written #:age #:stamp #:claim #:waiting #:saw #:readers
   #:depend #:undepend #:reading #:*reading* #:livep #:keptp #:describes
   #:worked #:worked-value #:worked-age #:workedp #:freshp #:+stale+
   #:stale #:moved #:make-place #:kinds))
(in-package #:pine/kernel/place)

(defvar *reading* nil
  "What the working-out running on this thread has read so far, or nothing where
none is running. Bound, never assigned: two threads working two places out read
their own inputs and neither can see the other's.")

(defvar *epoch* 0
  "Which marking walk is going on. One walk raises it once and stamps every place
it reaches, so a ring is walked once and two threads writing at the same time do
not walk each other's work again.")

(defvar +stale+ '#:stale
  "What a worked-out place holds before it has ever been worked out. Uninterned,
so nothing anybody writes can be mistaken for it.")

(defstruct (worked (:constructor worked (value age)) (:copier nil)
                   (:predicate workedp))
  "A worked-out value and the age it was worked out at, in one object.

The two are published together because they are checked together. A value put
down beside an age that has already moved is a stale value wearing a fresh mark,
and there is no way to write two slots at once -- so the age rides along inside
the value, and whoever reads it decides for themselves whether it still stands."
  value age)

(defclass place ()
  ((name      :initarg :name      :reader name-of)
   (under     :initarg :under     :accessor under     :initform nil)
   (describes :initarg :describes :reader describes   :initform nil)
   (beneath   :initform (d:no-map) :accessor beneath)
   (readers   :initform (d:no-set) :accessor readers)
   (saw       :initform nil        :accessor saw)
   (stamp     :initform 0          :accessor stamp))
  (:documentation "One thing in the namespace: a name, where it stands, what
stands under it, and who is worked out from it.

What it holds is its own class's business. This is the part every kind shares,
and it is why a document, a device, a running job and another machine all answer
the same calls."))

(defclass value (place)
  ((holds   :initarg :holds :accessor holds :initform nil)
   (written :initform nil   :accessor written))
  (:documentation "Holds what you write. The plain kind, and what a write makes
where nothing stood.

WRITTEN is how a branch is told from a place somebody wrote nothing into. Making
/a/b makes /a, and /a holds nothing -- but nobody put nothing there, and a read
of it should say so rather than answering the same as a place that was deliberately
emptied."))

(defclass derived (place)
  ((works   :initarg :works :accessor works)
   (puts    :initarg :puts  :accessor puts :initform nil)
   (holds   :initform +stale+ :accessor holds)
   (age     :initform 0       :accessor age)
   (claim   :initform nil     :accessor claim)
   (waiting :initform nil     :accessor waiting))
  (:documentation "Worked out from what it reads, and worked out again when any
of that moves.

What it read is recorded as it reads, so nothing declares its inputs. AGE moves
every time it is marked stale, and what it holds carries the age it was worked
out at, so a value worked out from inputs that have since moved is not mistaken
for one that still stands."))

(defclass world (place)
  ((asks :initarg :asks :accessor asks)
   (puts :initarg :puts :accessor puts :initform nil))
  (:documentation "A leaf the world answers. Asked every time and never
remembered, because the world moves without anybody writing it."))

(defclass listing (place)
  ((names-of :initarg :names :accessor names-of)
   (each-of  :initarg :each  :accessor each-of))
  (:documentation "A branch the world fills. NAMES says what is under it now and
EACH makes the one asked for, so what is under it is what is there rather than
what was attached."))

(defclass mount (place)
  ((reached :initarg :reached :accessor reached))
  (:documentation "Another namespace, grafted in. A directory on this machine and
another pine on another machine are the same thing here, which is why a read of
/host/laptop/dev/audio and a read of /dev/audio are the same act."))

(defclass job (place)
  ((runs  :initarg :runs  :accessor runs :initform nil)
   (took  :initarg :took  :accessor took :initform nil)
   (state :initform :stopped :accessor state))
  (:documentation "Something running. Written to, to be told something."))

(defun placep (x) (typep x 'place))

(defun kinds () '(:value :derived :world :listing :mount :job))

(defun make-place (kind name &rest initargs)
  "A place of KIND. One call and one keyword, because the kinds differ in what
they hold and in nothing else, and a slot left empty is not a kind."
  (apply #'make-instance
         (ecase kind
           (:value 'value) (:derived 'derived) (:world 'world)
           (:listing 'listing) (:mount 'mount) (:job 'job))
         :name (princ-to-string name) initargs))

(defun full-name (p)
  (let ((pieces (loop :for at := p :then (under at)
                      :while (and at (under at))
                      :collect (name-of at))))
    (if pieces (format nil "~{/~a~}" (nreverse pieces)) "/")))

(defmethod print-object ((p place) stream)
  (print-unreadable-object (p stream :type t)
    (write-string (full-name p) stream)))

(defgeneric livep (place)
  (:documentation "Whether it answers differently without anybody writing it.")
  (:method ((p place)) nil)
  (:method ((p world)) t)
  (:method ((p listing)) t)
  (:method ((p mount)) t))

(defgeneric keptp (place)
  (:documentation "Whether what it holds outlives the image. What is worked out
is worked out again, and what the world answers the world still answers, so
neither is written down.")
  (:method ((p place)) nil)
  (:method ((p value)) t))

(defun reading (p)
  "Say the working-out running on this thread read P."
  (when *reading* (pushnew p (cdr *reading*)))
  p)

(defun depend (reader on)
  (d:swap (slot-value on 'readers) (lambda (all) (d:with all reader)))
  reader)

(defun undepend (reader on)
  (d:swap (slot-value on 'readers) (lambda (all) (d:without all reader)))
  reader)

(defgeneric stale (place)
  (:documentation "Say what it holds is no longer what it would hold.

One fixnum, raised. What it holds carries the age it was worked out at, so
raising the age is the whole of marking it: the old value is still there and is
simply no longer anybody's answer.")
  (:method ((p place)) p)
  (:method ((p derived)) (d:swap (slot-value p 'age) #'1+) p))

(defun %seen (p epoch)
  "Whether this walk is the first to reach P. Raises the stamp and never lowers
it, so two walks at once cannot hand a ring back and forth between them."
  (loop :for had := (stamp p)
        :do (when (>= had epoch) (return nil))
            (when (d:cas (slot-value p 'stamp) had epoch) (return t))))

(defun %mark (p epoch)
  (when (%seen p epoch)
    (d:do-each (each (readers p))
      (stale each)
      (tell:moved each)
      (%mark each epoch))))

(defun moved (p)
  "Say P moved: mark everything worked out from it, and hand the news on.

Everything reached is news, not only what was written. A place worked out from
what moved has moved, whoever is watching it is owed that, and nobody should have
to know what it was worked out from in order to be told.

Held together, so a write that reaches forty places is one piece of news and not
forty. The marking is serial and cheap -- one fixnum for each place reached. The
working out is neither, and is not done here: it happens when somebody reads,
which is what lets it happen on whatever thread and however many at once."
  (tell:together
    (tell:moved p)
    (%mark p (d:swap *epoch* #'1+)))
  p)

(defgeneric kids (place)
  (:documentation "What stands under it, as places.")
  (:method ((p place)) (d:vals (beneath p)))
  (:method ((p listing))
    (remove nil (mapcar (lambda (each) (resolve p each)) (funcall (names-of p)))))
  (:method ((p mount)) (funcall (reached p) :kids)))

(defgeneric resolve (place said)
  (:documentation "The one place under PLACE called SAID, or nothing.")
  (:method ((p place) said) (d:lookup (beneath p) (princ-to-string said)))
  (:method ((p listing) said)
    (let ((said (princ-to-string said)))
      (or (d:lookup (beneath p) said)
          (when (member said (mapcar #'princ-to-string (funcall (names-of p)))
                        :test #'equal)
            (let ((made (funcall (each-of p) said)))
              (when made
                (setf (under made) p)
                (d:swap (slot-value p 'beneath)
                        (lambda (all) (if (nth-value 1 (d:lookup all said))
                                          all
                                          (d:with all said made))))
                (d:lookup (beneath p) said)))))))
  (:method ((p mount) said) (funcall (reached p) :resolve said)))

(defun attach (p under)
  "Put P under UNDER. Whatever stood at its name is taken off first, because a
name is where something stands and two things cannot stand in one place."
  (let ((had (d:lookup (beneath under) (name-of p))))
    (when (and had (not (eq had p))) (detach had)))
  (setf (under p) under)
  (d:swap (slot-value under 'beneath) (lambda (all) (d:with all (name-of p) p)))
  p)

(defun detach (p)
  "Take P off the tree, and stop it reading anything.

What it read has to be given up here. A place taken away and left in the reader
sets of what it read is worked out for ever after whenever any of that moves, and
holds all of it for as long as the image runs -- which is what an erased surface
did before this line existed."
  (let ((over (under p)))
    (dolist (on (saw p)) (undepend p on))
    (setf (saw p) nil)
    (d:do-each (each (readers p)) (stale each))
    (d:swap (slot-value p 'readers) (constantly (d:no-set)))
    (when over
      (d:swap (slot-value over 'beneath)
              (lambda (all) (d:without all (name-of p)))))
    (setf (under p) nil)
    p))

(defgeneric held (place)
  (:documentation "What it holds. One method per kind, because there is one
answer per kind and no place answers two.")
  (:method ((p value)) (reading p) (holds p))
  (:method ((p world)) (reading p) (funcall (asks p)))
  (:method ((p listing)) (reading p) (mapcar #'name-of (kids p)))
  (:method ((p mount)) (reading p) (funcall (reached p) :held))
  (:method ((p job)) (reading p) (state p)))

(defgeneric (setf held) (said place)
  (:documentation "Put SAID in PLACE, and say it moved. A kind that cannot be
written says so rather than quietly becoming something it was not.")
  (:method (said (p place))
    (declare (ignore said))
    (error "~a holds nothing that can be written." (full-name p)))
  (:method (said (p value))
    (setf (holds p) said (written p) t)
    (moved p)
    said)
  (:method (said (p world))
    (unless (puts p)
      (error "~a is what the world says, and takes no writing." (full-name p)))
    (funcall (puts p) said)
    (moved p)
    said)
  (:method (said (p mount)) (funcall (reached p) :write said))
  (:method (said (p job))
    (unless (took p) (error "~a takes no telling." (full-name p)))
    (funcall (took p) said)))

(defun read (p) (held p))
