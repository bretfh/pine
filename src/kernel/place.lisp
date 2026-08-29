(defpackage #:pine/kernel/place
  (:use #:cl)
  (:shadow #:read)
  (:local-nicknames (#:d #:pine/data) (#:tell #:pine/kernel/tell)
                    (#:bt #:bordeaux-threads))
  (:export
   #:place #:placep #:value #:derived #:world #:listing #:mount #:job
   #:name-of #:under #:full-name #:beneath #:kids #:resolve #:attach #:detach
   #:held #:read #:works #:puts #:asks #:names-of #:each-of #:reached #:runs
   #:took #:state #:holds #:written #:stamp #:claim #:waiting #:saw #:readers
   #:depend #:undepend #:read-at #:source #:mark #:*reading* #:livep #:keptp #:describes
   #:worked #:worked-value #:worked-from #:worked-at #:workedp #:+stale+ #:version
   #:dirty #:checked #:stale #:moved-on #:moving #:moved #:told-about #:make-place
   #:kinds #:*epoch* #:*writes* #:*settling* #:settled #:quietly))
(in-package #:pine/kernel/place)

(defvar *reading* nil
  "What the working-out running on this thread has read so far, or nothing where
none is running. Bound, never assigned: two threads working two places out read
their own inputs and neither can see the other's.")

(defvar *writes* 0
  "How many times anything anywhere has been written.

Raised before a write does anything else, so it is never behind. Nothing decides
anything by it on its own -- it is what says whether an answer worked out a moment
ago is still worth trusting without looking again, and looking again is what is
being saved.")

(defvar *epoch* 0
  "Which marking walk is going on. One walk raises it once and stamps every place
it reaches, so a ring is walked once and two threads writing at the same time do
not walk each other's work again.")

(defvar *settling* 0
  "How many workings-out have given up on being lucky.

While it is anything but nothing, a write waits. A working-out that keeps adding
up a moving state and getting nowhere asks for one moment of quiet and takes it,
and what it costs is that the writers pause for the length of one working-out.

Not a lock. A writer waits on a number and a reader raises it, so there is no pair
of them each holding what the other wants -- and a reader never waits for another
reader here, because what it asked for is that nothing is *written*, and another
reader is not writing."
  )

(defvar +stale+ '#:stale
  "What a worked-out place holds before it has ever been worked out. Uninterned,
so nothing anybody writes can be mistaken for it.")

(defstruct (worked (:constructor worked (value from at)) (:copier nil)
                   (:predicate workedp))
  "A worked-out value, what it was worked out from, and which version it is.

FROM is every place it read while it ran, each with the version that place was at
when it was read. That list is the whole of how anybody decides afterwards whether
this value still stands -- not a mark somebody else set, but what it actually
looked at, checked against what those places say now. A value is thrown away when
something it read has moved, and never when something it did not read has.

AT rides along for the same reason the value and the list do: one object in one
slot is read all at once. Kept in a second slot it could be read a moment late,
and then somebody holds a value labelled with a version it is not -- which looks
whole for ever after, because the label is what it would be checked against."
  value from at)

(defclass place ()
  ((name      :initarg :name      :reader name-of)
   (under     :initarg :under     :accessor under     :initform nil)
   (describes :initarg :describes :reader describes   :initform nil)
   (beneath   :initform (d:no-map) :accessor beneath)
   (readers   :initform (d:no-set) :accessor readers)
   (saw       :initform nil        :accessor saw)
   (version   :initform 0          :accessor version)
   (dirty     :initform nil        :accessor dirty)
   (checked   :initform -1         :accessor checked)
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
   (claim   :initform nil     :accessor claim)
   (waiting :initform nil     :accessor waiting))
  (:documentation "Worked out from what it reads, and worked out again when any
of that moves.

What it read is recorded as it reads, so nothing declares its inputs. What it
holds carries that list with it -- every place it looked at, and the version that
place was at when it looked -- so whether the value still stands is a question
about what it read and not about a mark somebody set on it."))

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

(defgeneric mark (place)
  (:documentation "Which version of itself it is holding right now.

For a place that is written, the version slot. For one that is worked out, the
version that came out of the slot holding the value -- because those two were put
there as one object and reading that object is how the pair is got whole.")
  (:method ((p place)) (version p))
  (:method ((p derived))
    (let ((h (holds p))) (if (workedp h) (worked-at h) -1))))

(defun read-at (p at value)
  "Say the working-out running on this thread read P at version AT."
  (when *reading* (push (cons p at) (cdr *reading*)))
  value)

(defun source (p get)
  "What a place that is not worked out holds, written down with the version it
belongs to.

The version is read on both sides of the value. A write moves the two of them one
after the other, so reading the value and then the version can hand back the old
value wearing the new version -- and then it looks whole for ever after, because
the version it is checked against is the one it was wrongly given. Read on both
sides, that pair cannot be built: either nothing moved, or this is asked again."
  (loop
    (let* ((before (version p))
           (value (funcall get))
           (after (version p)))
      (when (eql before after)
        (return (read-at p before value))))))

(defun depend (reader on)
  (d:swap (slot-value on 'readers) (lambda (all) (d:with all reader)))
  reader)

(defun undepend (reader on)
  (d:swap (slot-value on 'readers) (lambda (all) (d:without all reader)))
  reader)

(defun stale (p)
  "Say what it holds may no longer be what it would hold.

A mark, and only a mark. Whether it really moved is decided when somebody asks,
by looking at what it read -- this only saves them the looking in the usual case
where nothing did."
  (setf (dirty p) t)
  p)

(defun moved-on (p)
  "Say what it holds has become something else. One fixnum, raised.

This is what anybody who read it checks against. It is raised where the value
really changed and not merely where it might have, so a place worked out from
this one is left alone when this one works out to what it said before."
  (d:swap (slot-value p 'version) #'1+)
  p)

(defun settled ()
  "Wait for anybody who asked for a moment of quiet."
  (loop :while (plusp *settling*) :do (bt:thread-yield))
  t)

(defmacro quietly (&body body)
  "BODY with nothing being written while it runs.

The floor under being lucky. A working-out tries the fast way first and this is
what it falls back on, so it is rare -- but without it a place that reads sixty
others can be interrupted for ever by somebody writing in a loop, and never get an
answer at all."
  `(progn (d:swap *settling* #'1+)
          (unwind-protect (progn ,@body)
            (d:swap *settling* #'1-))))

(defun moving (p)
  "Say what it holds is about to become something else.

Raised before the value changes as well as after, and this is the one that is easy
to leave out. Raised only after, there is a moment when the new value is standing
under the old version -- and two readings taken either side of that moment write
down the same version for two different values, which is a pair nothing downstream
can ever tell apart. Raised before as well, a reading taken during a write is a
reading whose version has already moved by the time it is checked.

The count of writes goes up here too, and after the version, not before. Whoever
remembers that a place stood remembers it for as long as that count holds still,
so a version that moves while the count does not is a memory that is wrong and
stays wrong -- and that is what raising the count first would allow, in the moment
between the two."
  (settled)
  (d:swap (slot-value p 'version) #'1+)
  (d:swap *writes* #'1+)
  p)

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

(defun told-about (p)
  "Mark everything worked out from P, and hand the news on."
  (%mark p (d:swap *epoch* #'1+))
  p)

(defun moved (p)
  "Say P moved: mark everything worked out from it, and hand the news on.

Everything reached is news, not only what was written. A place worked out from
what moved has moved, whoever is watching it is owed that, and nobody should have
to know what it was worked out from in order to be told.

The version of what was written is raised first and the count of writes second,
and both before the walk. Those two are what correctness rests on and they are
immediate. The walk is a hint: it saves the looking, and while it is still going
nothing is wrong, because whether a place really moved is settled by what it read
and not by whether the walk has got there yet.

Held together, so a write that reaches forty places is one piece of news and not
forty.

The working out is not done here. It happens when somebody reads, which is what
lets it happen on whatever thread and however many at once."
  (settled)
  (moved-on p)
  (d:swap *writes* #'1+)
  (tell:together
    (tell:moved p)
    (told-about p))
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
  (:method ((p value)) (source p (lambda () (holds p))))
  (:method ((p world)) (source p (lambda () (funcall (asks p)))))
  (:method ((p listing)) (source p (lambda () (mapcar #'name-of (kids p)))))
  (:method ((p mount)) (source p (lambda () (funcall (reached p) :held))))
  (:method ((p job)) (source p (lambda () (state p)))))

(defgeneric (setf held) (said place)
  (:documentation "Put SAID in PLACE, and say it moved. A kind that cannot be
written says so rather than quietly becoming something it was not.")
  (:method (said (p place))
    (declare (ignore said))
    (error "~a holds nothing that can be written." (full-name p)))
  (:method (said (p value))
    (moving p)
    (setf (holds p) said (written p) t)
    (moved p)
    said)
  (:method (said (p world))
    (unless (puts p)
      (error "~a is what the world says, and takes no writing." (full-name p)))
    (moving p)
    (funcall (puts p) said)
    (moved p)
    said)
  (:method (said (p mount)) (funcall (reached p) :write said))
  (:method (said (p job))
    (unless (took p) (error "~a takes no telling." (full-name p)))
    (funcall (took p) said)))

(defun read (p) (held p))
