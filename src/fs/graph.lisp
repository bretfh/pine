(in-package #:pine/fs/node)

(defvar *waited* 5
  "How long to wait on somebody else's working-out before looking again. Not a
deadline -- the answer is looked at on waking -- only how long a wake-up that went
missing can go unnoticed.")

(defvar *waiting-on* 30
  "Seconds to wait for whoever holds a node's working-out before giving up on
them.

A READS is somebody else's code and it talks to the world: it shells out, it
reads a device, it asks another pine over a socket. One that never answers must
not take every reader of that node with it for the life of the image.")

(defvar *broke* nil
  "What to do about a working-out that threw. Filled in by whatever keeps faults,
because this layer loads before there is one.")

(defstruct (worked (:constructor worked (value from at)) (:copier nil)
                   (:predicate workedp))
  "A worked-out value, what it was worked out from, and which version it is.

FROM is every node it read while it ran, each with the version that node was at
when it was read. That list is the whole of how anybody decides afterwards whether
this value still stands -- not a mark somebody else set on it, but what it actually
looked at, checked against what those nodes say now. So a value is given up when
something it read has moved, and never when something it did not read has.

AT rides along for the same reason the other two do: one object in one slot is read
all at once. Kept in a second slot it could be read a moment late, and then
somebody holds a value labelled with a version it is not -- which looks whole for
ever after, because the label is what it would be checked against."
  value from at)

(defun %moved-on (n)
  "Say what N holds has become something else.

The version first and the count of writes second. Whoever remembers that a node
stood remembers it while that count holds still, so a version that moves while the
count does not is a memory that is wrong and stays wrong."
  (d:swap (slot-value n 'version) #'1+)
  (d:swap *writes* #'1+)
  n)

(defun %noted (n at value)
  "Say the working-out running on this thread read N at version AT, and answer
VALUE."
  (when *reading* (push (cons n at) (cdr *reading*)))
  value)

(defun %seen (n epoch)
  "Whether this walk is the first to reach N. Raises the stamp and never lowers it,
so two walks at once cannot hand a ring back and forth between them."
  (loop :for had := (stamp n)
        :do (when (>= had epoch) (return nil))
            (when (d:cas (slot-value n 'stamp) had epoch) (return t))))

(defun mark (n)
  "Which version of itself N is holding now.

For a node that is written, the version slot. For one that is worked out, the
version that came out of the slot holding the value, because those two were put
there as one object and reading that object is how the pair is got whole."
  (let ((h (and (typep n 'derived) (cached n))))
    (if (workedp h) (worked-at h) (version n))))

(defun currentp (n at)
  "Whether N is still what it was when somebody read it at version AT.

Not asked of a mark a walk set on it. A walk takes time, so a node can have
genuinely moved and not been marked yet, and anything resting on the mark would
call that whole. The version of a node that is written is raised by the write
itself, before anything else, so it is never behind -- and a node that is worked
out stands only if everything *it* read still stands, all the way down."
  (and (eql at (mark n)) (soundp n)))

(defun whole (from)
  "Whether everything a working-out read is still what it was when it read it."
  (loop :for (each . at) :in from :always (currentp each at)))

(defun soundp (n)
  "Whether what N holds still stands.

Which would be the whole graph on every read, so the answer is remembered: a node
found to stand while the count of writes was W still stands while it is W, because
nothing can have moved without that count moving. One comparison in the usual case,
where nothing is being written at all."
  (let ((h (and (typep n 'derived) (cached n))))
    (cond ((not (workedp h)) t)
          ((eql (checked n) *writes*) t)
          (t (let ((w *writes*))
               (when (whole (worked-from h))
                 (when (eql w *writes*) (setf (checked n) w))
                 t))))))

(defun freshp (n)
  (let ((h (cached n)))
    (and (workedp h) (whole (worked-from h)))))

(defun %standing (n)
  "What N holds if that still stands. One read of the one slot, so the value, what
it was worked out from, and which version it is all come back together or not at
all."
  (let ((h (cached n)))
    (when (and (workedp h) (whole (worked-from h))) h)))

(defun %waiter (n)
  (or (waiting n)
      (progn (d:cas (slot-value n 'waiting) nil
                    (cons (bordeaux-threads:make-lock "pine/working")
                          (bordeaux-threads:make-condition-variable)))
             (waiting n))))

(defun %wait (n)
  "Wait for whoever holds the claim, and hand the waking on. One waiter is woken at
a time and wakes the next itself, so a node ten threads wanted does not need the
news sent ten times by the one that did the work."
  (let ((it (%waiter n)))
    (bordeaux-threads:with-lock-held ((car it))
      (if (claim n)
          (bordeaux-threads:condition-wait (cdr it) (car it) :timeout *waited*)
          (bordeaux-threads:condition-notify (cdr it))))))

(defun %done (n)
  "Give up the claim and say so, under the lock where anybody is waiting -- so
there is no gap between letting go and telling, in which a waiter could settle down
to wait for something that has already happened."
  (let ((it (waiting n)))
    (if it
        (bordeaux-threads:with-lock-held ((car it))
          (setf (claim n) nil)
          (bordeaux-threads:condition-notify (cdr it)))
        (setf (claim n) nil))))

(defun %edges (n from)
  "Record what it read, and stop reading what it no longer reads.

What it read last time and does not read now it gives up. Without that a node that
once looked somewhere is worked out for ever after whenever that place moves, and
the thing it no longer reads holds it for as long as the image runs."
  (let ((had (saw n))
        (now (remove-duplicates (mapcar #'car from))))
    (dolist (on had) (unless (member on now) (undepend n on)))
    (setf (saw n) now)
    (dolist (on now) (unless (eq on n) (depend n on)))))

(defun %broke (n condition)
  "Say that working N out threw, to whoever keeps faults.

Told from inside the handler, before anything unwinds, because the restarts a
fault is standing in are only there while it is still standing in them. A hook
and not a call: this layer loads before there is anything that keeps faults, the
same reason COMMIT:*BROKE* is one."
  (when *broke* (funcall *broke* condition (full-name n)))
  nil)

(defun %mine (n)
  "Work it out, holding the claim. Answers the value, whether it stands, and which
version it is.

Every node it read is recorded with the version that node was at when it was read.
When the work is done those versions are looked at again, and if any of them has
moved then what was added up here is part one state and part another -- so it is
given up and nothing is put down. That check is exact: not a guess about whether
something might have moved, and not a lock stopping anything from moving, but the
list of what was actually looked at.

The version is raised only where the answer really changed, so a node worked out
from this one is left alone when this one works out to what it said before. That is
what stops a write at the bottom of a deep graph from redrawing the whole of it.

What it read is recorded whether or not it finished: a node that threw has still
read what it read, and one that keeps nothing is one nothing can ever move again.

One that threw puts nothing down, and says so with :BROKE rather than with the
NIL that means try again. Not because the fault is uninteresting but because NIL
is not what this node holds: written here it would leave the node standing on an
answer nobody worked out, given up only when something it read moves -- which,
for one that threw before it read anything, is never. Left stale, the next read
asks again, which is what a device whose command failed once needs."
  (let ((reading (cons :reading nil))
        (from nil))
    (unwind-protect
         (block mine
           (handler-bind ((error (lambda (c)
                                   (%broke n c)
                                   (return-from mine (values nil :broke nil)))))
             (let ((v (if (in-of n)
                          (funcall *elsewhere* (in-of n) (reads n))
                          (let ((*reading* reading)) (funcall (reads n))))))
               (setf from (cdr reading))
               (cond ((whole from)
                      (let* ((had (cached n))
                             (same (and (workedp had)
                                        (d:same v (worked-value had))))
                             (at (if same (worked-at had) (1+ (mark n)))))
                        (setf (cached n) (worked v from at)
                              (checked n) -1)
                        (values v t at)))
                     (t (values v nil nil))))))
      (%edges n (or from (cdr reading))))))

(defun %held (n)
  "What N last worked out to, whether or not that still stands.

What a reader is handed when the working-out cannot be had at all. It is older
than they asked for, and it is nearer what this node means than nothing is."
  (let ((h (cached n)))
    (when (workedp h) (worked-value h))))

(defun %gave-up (n)
  (%broke n (make-condition
             'simple-error
             :format-control "~a could not be worked out within ~d second~:p"
             :format-arguments (list (full-name n) *waiting-on*)))
  (values (%held n) (mark n)))

(defun work-out (n)
  "What it works out to, working it out if what it holds no longer stands.

One thread works one node out. A second that wants the same one waits for the first
rather than running the same code twice -- which matters because the code is
somebody else's and may talk to the world. Two threads wanting two nodes is two
threads working, and that is where the cores go.

What comes back was worked out from a state that stood. A node that reads sixty
others reads them one after another, and if one of those moved partway through, what
it added up is a state nobody was in -- so that answer is not handed back and it is
worked out again. Nothing is held still to arrange this: the writing goes on and the
reading tries again.

The claim is the thread that took it, so a node worked out from itself is one this
thread already holds, and says so instead of waiting for itself for ever.

Nobody waits for ever on somebody else either. A READS that never answers holds
the claim for as long as it runs, and without a deadline every reader of that
node waits behind it for the life of the image. What comes back then is what the
node last worked out to, and the fault says why it is old."
  (let ((me (bordeaux-threads:current-thread))
        (due (+ (get-universal-time) *waiting-on*)))
    (loop
      (let ((h (%standing n)))
        (when h (return (values (worked-value h) (worked-at h)))))
      (when (eq (claim n) me)
        (error "~a is worked out from itself." (full-name n)))
      (when (> (get-universal-time) due) (return (%gave-up n)))
      (if (and (null (claim n))
               (d:cas (slot-value n 'claim) nil me))
          (multiple-value-bind (v stands at)
              (unwind-protect (%mine n) (%done n))
            (cond ((eq stands :broke) (return (values (%held n) (mark n))))
                  (stands (return (values v at)))))
          (%wait n)))))

(defun stalep (n) (not (freshp n)))

(defmethod contents ((n derived))
  (let ((h (%standing n)))
    (if h
        (%noted n (worked-at h) (worked-value h))
        (multiple-value-bind (v at) (work-out n) (%noted n at v)))))

(defmethod (setf contents) (v (n derived))
  "Writing one is what its writer says it is. Without a writer it is not written: a
node that says what it is worked out from is not a node that quietly stops being
worked out because somebody wrote it once, which is what replacing READS with a
constant did -- to eleven surfaces and every read-only reading of a device."
  (unless (writes n)
    (error "~a is worked out, and takes no writing." (full-name n)))
  (funcall (writes n) v)
  v)

(defun reading (n)
  "Say the working-out running on this thread read N."
  (%noted n (mark n) n))

(defgeneric moved (node)
  (:documentation "Say NODE moved. Whatever read it is worked out again.

A node already reached by this walk is not reached again: two that read each other
are a ring, and without this walking it is the last thing the thread does. The
stamp is shared and only ever goes up, so it holds against two threads writing at
once -- a list bound on one thread does not.")
  (:method ((n node))
    (%moved-on n)
    (let ((epoch (or *walking* (d:swap *epoch* #'1+))))
      (when (%seen n epoch)
        (let ((*walking* epoch))
          (d:do-each (each (readers n)) (moved each)))))
    n)
  (:method :before ((n derived))
    "What it worked out is no longer what it would work out.

Said and not worked out: a node whose READS closes over something that is not a
node -- a list, a slot, the world -- has read nothing this could check, so being
told is the only way it can know. What it read is checked as well, and catches
what a walk cannot: a walk takes time, and during one a node can have moved and
not been reached yet."
    (setf (cached n) +unread+)))

(defun announced (n)
  "Say N moved: what read it is worked out again, and whoever is waiting for a
batch of writes to land is told. One word, because they are one event."
  (moved n)
  (commit:announce n)
  n)

(defmethod contents :around ((n node))
  "What was read, and the version it was read at, written down together.

The version is read on both sides of the value. A write moves the two one after the
other, so reading the value and then the version can hand back the old value
wearing the new version -- a pair that looks whole for ever after, because the
version it would be checked against is the one it was wrongly given.

One that is worked out writes its own down: working it out publishes the value and
the version it is as one object."
  (if (typep n 'derived)
      (call-next-method)
      (loop
        (let* ((before (version n))
               (v (call-next-method))
               (after (version n)))
          (when (eql before after) (return (%noted n before v)))))))

(defmethod (setf contents) :after (value (n node))
  (declare (ignore value))
  (announced n))
