(defpackage #:pine/kernel/graph
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:place #:pine/kernel/place)
                    (#:bt #:bordeaux-threads))
  (:export
   #:freshp #:standsp #:soundp #:*remember* #:work-out #:all-worked #:*fan-out* #:*waited*))
(in-package #:pine/kernel/graph)

(defvar *fan-out* nil
  "How to work several places out at once. Filled in by whatever keeps a pool of
threads; until it is, they are worked out one after another on whatever thread
asked, which is what the kernel does before there is a pool.")

(defvar *lucky* 8
  "How many times a working-out will try the fast way before asking for quiet.

High, because asking is expensive for everybody and being lucky is the usual case:
under any amount of writing a person can do, the first try wins. It matters only
where something is written in a loop, and there it is the difference between an
answer and none."
  )

(defvar *remember* t
  "Whether a place found to stand is taken as standing until something is
written. Off, every read walks the whole of what it rests on.")

(defvar *waited* 5
  "How long to wait on somebody else's working-out before looking again. Not a
deadline: the answer is checked again on waking, so this only decides how long a
missed wake-up can go unnoticed.")

(defun standsp (p at)
  "Whether P is still what it was when somebody read it at version AT.

Not asked of the mark a walk set on it. A walk takes time, so a place can have
genuinely moved and not been marked yet, and anything resting on the mark would
call that whole. The version of a place that is written is raised by the write
itself, before anything else, so it is never behind -- and a place that is worked
out stands only if everything *it* read still stands, all the way down to places
that are written.

Which would be the whole graph on every read, so the answer is remembered: a place
found to stand while the count of writes was W still stands while it is W. One
comparison in the usual case, where nothing is being written at all."
  (and (eql at (place:mark p)) (soundp p)))

(defun soundp (p)
  (let ((h (place:holds p)))
    (cond ((not (place:workedp h)) t)
          ((and *remember* (eql (place:checked p) place:*writes*)) t)
          (t (let ((w place:*writes*))
               (when (whole (place:worked-from h))
                 (when (eql w place:*writes*) (setf (place:checked p) w))
                 t))))))

(defun whole (from)
  "Whether everything a working-out read is still what it was when it read it."
  (loop :for (each . at) :in from :always (standsp each at)))

(defun freshp (p)
  "Whether what a worked-out place holds is still what it would work out.

Asked of what it read, not of a mark somebody else set on it. A mark says
something below may have moved; this says whether it did."
  (let ((h (place:holds p)))
    (and (place:workedp h) (whole (place:worked-from h)))))

(defun standing (p)
  "What it holds if that still stands, and nothing if it does not. One read of the
one slot, so the value, what it was worked out from, and which version it is all
come back together or not at all."
  (let ((h (place:holds p)))
    (when (and (place:workedp h) (whole (place:worked-from h)))
      h)))

(defun %waiter (p)
  "The lock and the news for this place, made the first time two threads want it
at once. A place nobody contends never carries either."
  (or (place:waiting p)
      (progn (d:cas (slot-value p 'place::waiting) nil
                    (cons (bt:make-lock) (bt:make-condition-variable)))
             (place:waiting p))))

(defun %wait (p)
  "Wait for whoever holds the claim, and hand the waking on.

One waiter is woken at a time and wakes the next itself, so a place ten threads
wanted does not need the news sent ten times by the one that did the work. The
timeout is not a deadline -- the answer is looked at again on waking -- it is only
how long a wake-up that went missing can go unnoticed."
  (let ((it (%waiter p)))
    (bt:with-lock-held ((car it))
      (if (place:claim p)
          (bt:condition-wait (cdr it) (car it) :timeout *waited*)
          (bt:condition-notify (cdr it))))))

(defun %done (p)
  "Give up the claim and say so. Under the lock where anybody is waiting, so there
is no gap between letting go and telling in which a waiter could settle down to
wait for something that has already happened."
  (let ((it (place:waiting p)))
    (if it
        (bt:with-lock-held ((car it))
          (setf (place:claim p) nil)
          (bt:condition-notify (cdr it)))
        (setf (place:claim p) nil))))

(defun %edges (p from)
  "Record what it read, and stop reading what it no longer reads.

What it read last time and does not read now it gives up. Without that, a place
that once looked somewhere is worked out for ever after whenever that place moves,
and the thing it no longer reads holds it for as long as the image runs."
  (let ((had (place:saw p))
        (now (remove-duplicates (mapcar #'car from))))
    (dolist (on had) (unless (member on now) (place:undepend p on)))
    (setf (place:saw p) now)
    (dolist (on now) (unless (eq on p) (place:depend p on)))))

(defun %mine (p)
  "Work it out, holding the claim. Answers the value, and whether it stands.

Every place it read is recorded with the version that place was at when it was
read. When the work is done those versions are looked at again, and if any of them
has moved then what was added up here is part one state and part another, so it is
thrown away and nothing is published.

That check is exact. It is not a guess about whether something might have moved,
and it is not a lock stopping anything from moving. It is the list of what was
actually looked at, and whether it is still what it was. So a place that reads the
clock is never worked out again because the volume moved.

The version is raised only where the answer really changed. A place worked out
from this one is left alone when this one works out to what it said before, which
is what stops a write at the bottom of a deep graph from redrawing the whole of it.

What it read is recorded whether or not it finished: a place that threw has still
read what it read, and one that keeps nothing is one nothing can ever mark again."
  (let ((reading (cons :reading nil))
        (from nil))
    (unwind-protect
         (let ((v (let ((place:*reading* reading)) (funcall (place:works p)))))
           (setf from (cdr reading))
           (cond ((whole from)
                  (let* ((had (place:holds p))
                         (same (and (place:workedp had)
                                    (d:same v (place:worked-value had))))
                         (at (if same (place:worked-at had) (1+ (place:mark p)))))
                    (setf (place:holds p) (place:worked v from at)
                          (place:checked p) -1
                          (place:dirty p) nil)
                    (values v t at)))
                 (t (values v nil nil))))
      (%edges p (or from (cdr reading))))))

(defun work-out (p)
  "What it works out to, working it out if what it holds no longer stands.

One thread works one place out. A second that wants the same one waits for the
first rather than running the same code twice -- which matters because the code is
somebody else's and may talk to the world. Two threads wanting two places is two
threads working, and that is where the cores go.

What comes back was worked out from a state that stood. A place that reads sixty
others reads them one after another, and if one of those moved partway through,
what it added up is a state nobody was in -- so that answer is not handed back and
it is worked out again. Nothing is held still to arrange this: the writing goes on
and the reading tries again.

Which would be for ever, against somebody writing in a loop. So after enough tries
it asks for a moment of quiet and the writers wait for one working-out. That is
the floor: optimism first, because it is nearly always right, and a way to finish
for the times it is not.

The claim is the thread that took it, so a place worked out from itself is one this
thread already holds, and says so instead of waiting for itself for ever."
  (let ((me (bt:current-thread)) (tries 0))
    (flet ((try ()
             (let ((h (standing p)))
               (when h (return-from work-out
                         (values (place:worked-value h) (place:worked-at h)))))
             (when (eq (place:claim p) me)
               (error "~a is worked out from itself." (place:full-name p)))
             (if (and (null (place:claim p))
                      (d:cas (slot-value p 'place::claim) nil me))
                 (multiple-value-bind (v stands at)
                     (unwind-protect (%mine p) (%done p))
                   (when stands (return-from work-out (values v at))))
                 (%wait p))))
      (loop
        (if (>= (incf tries) *lucky*)
            (place:quietly (try))
            (try))))))

(defmethod place:held ((p place:derived))
  "What it works out to, written down with the version that value is.

The standing value and its version come out of one slot in one read, and a
working-out answers the version it just published while it still holds the claim.
Either way the pair is got whole, which is the whole of why what is written down
here can be trusted afterwards."
  (let ((h (standing p)))
    (if h
        (place:read-at p (place:worked-at h) (place:worked-value h))
        (multiple-value-bind (v at) (work-out p)
          (place:read-at p at v)))))

(defmethod (setf place:held) (said (p place:derived))
  "Writing one is what its writer says it is. Without a writer it is not written:
a place that says what it is worked out from is not a place that quietly stops
being worked out because somebody wrote it once."
  (unless (place:puts p)
    (error "~a is worked out, and takes no writing." (place:full-name p)))
  (funcall (place:puts p) said)
  (place:moved p)
  said)

(defun all-worked (places)
  "Work every one of them out, at once where there is anything to do it with.

Places worked out from different things are different work, and nothing in one of
them waits on another, so this is where the cores are used. The order they finish
in is nobody's business, because each one publishes its own."
  (let ((need (remove-if #'freshp
                         (remove-if-not (lambda (p) (typep p 'place:derived))
                                        places))))
    (cond ((null need) places)
          ((null (rest need)) (work-out (first need)) places)
          (*fan-out*
           (funcall *fan-out* (mapcar (lambda (p) (lambda () (work-out p))) need))
           places)
          (t (mapc #'work-out need) places))))
