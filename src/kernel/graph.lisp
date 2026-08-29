(defpackage #:pine/kernel/graph
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:place #:pine/kernel/place)
                    (#:bt #:bordeaux-threads))
  (:export
   #:freshp #:work-out #:all-worked #:*fan-out* #:*waited*))
(in-package #:pine/kernel/graph)

(defvar *fan-out* nil
  "How to work several places out at once. Filled in by whatever keeps a pool of
threads; until it is, they are worked out one after another on whatever thread
asked, which is what the kernel does before there is a pool.")

(defvar *waited* 5
  "How long to wait on somebody else's working-out before looking again. Not a
deadline: the answer is checked again on waking, so this only decides how long a
missed wake-up can go unnoticed.")

(defun freshp (p)
  "Whether what a worked-out place holds is still what it would work out."
  (let ((h (place:holds p)))
    (and (place:workedp h) (eql (place:worked-age h) (place:age p)))))

(defun %waiter (p)
  "The lock and the news for this place, made the first time two threads want it
at once. A place nobody contends never carries either."
  (or (place:waiting p)
      (progn (d:cas (slot-value p 'place::waiting) nil
                    (cons (bt:make-lock) (bt:make-condition-variable)))
             (place:waiting p))))

(defun %wait (p)
  "Wait for whoever holds the claim, and hand the waking on.

One waiter is woken at a time and wakes the next itself, so a place that ten
threads wanted does not need the news sent ten times by the one that did the
work. The timeout is not a deadline -- the answer is looked at again on waking --
it is only how long a wake-up that went missing can go unnoticed."
  (let ((it (%waiter p)))
    (bt:with-lock-held ((car it))
      (if (place:claim p)
          (bt:condition-wait (cdr it) (car it) :timeout *waited*)
          (bt:condition-notify (cdr it))))))

(defun %done (p)
  "Give up the claim and say so. Under the lock where anybody is waiting, so
there is no gap between letting go and telling in which a waiter could settle
down to wait for something that has already happened."
  (let ((it (place:waiting p)))
    (if it
        (bt:with-lock-held ((car it))
          (setf (place:claim p) nil)
          (bt:condition-notify (cdr it)))
        (setf (place:claim p) nil))))

(defun %edges (p now)
  "Record what it read, and stop reading what it no longer reads.

What it read last time and does not read now it gives up. Without that, a place
that once looked somewhere is worked out for ever after whenever that place
moves, and the thing it no longer reads holds it for as long as the image runs."
  (let ((had (place:saw p)))
    (dolist (on had) (unless (member on now) (place:undepend p on)))
    (setf (place:saw p) now)
    (dolist (on now) (unless (eq on p) (place:depend p on)))))

(defun %mine (p)
  "Work it out, holding the claim.

The age is taken before the work and rides out with the value. If anything it
read moved while it ran, the age has moved too and what is put down is not
anybody's answer -- so the work is wasted and nothing else is. Nothing is thrown
away here, because the throwing away is whoever reads it deciding it is not
fresh, and that decision is one comparison.

What it read is recorded whether or not it finished: a place that threw has still
read what it read, and one that keeps nothing is one nothing can ever mark again."
  (let ((at (place:age p))
        (reading (cons :reading nil)))
    (unwind-protect
         (let ((v (let ((place:*reading* reading)) (funcall (place:works p)))))
           (setf (place:holds p) (place:worked v at))
           v)
      (%edges p (cdr reading)))))

(defun work-out (p)
  "What it works out to, working it out if what it holds no longer stands.

One thread works one place out. A second that wants the same one waits for the
first rather than running the same code twice -- which matters because the code
is somebody else's and may talk to the world. Two threads wanting two places is
two threads working, and that is where the cores go.

The claim is the thread that took it, so a place that is worked out from itself
is a place this thread already holds, and says so instead of waiting for itself
for ever."
  (let ((me (bt:current-thread)))
    (loop
      (when (freshp p) (return (place:worked-value (place:holds p))))
      (when (eq (place:claim p) me)
        (error "~a is worked out from itself." (place:full-name p)))
      (if (and (null (place:claim p))
               (d:cas (slot-value p 'place::claim) nil me))
          (return (unwind-protect (%mine p) (%done p)))
          (%wait p)))))

(defmethod place:held ((p place:derived))
  (place:reading p)
  (if (freshp p) (place:worked-value (place:holds p)) (work-out p)))

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

Places that are worked out from different things are different work, and nothing
in one of them waits on another, so this is where the cores are used. The order
they finish in is nobody's business, because each one publishes its own."
  (let ((need (remove-if #'freshp (remove-if-not (lambda (p) (typep p 'place:derived))
                                                 places))))
    (cond ((null need) places)
          ((null (rest need)) (work-out (first need)) places)
          (*fan-out*
           (funcall *fan-out*
                    (mapcar (lambda (p) (lambda () (work-out p))) need))
           places)
          (t (mapc #'work-out need) places))))
