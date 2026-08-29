(defpackage #:pine/kernel/watch
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:place #:pine/kernel/place)
                    (#:graph #:pine/kernel/graph) (#:tell #:pine/kernel/tell)
                    (#:tree #:pine/kernel/tree))
  (:export
   #:watch #:watching #:watches #:unwatch #:forget-all #:told
   #:*dispatch* #:*broke* #:attend))
(in-package #:pine/kernel/watch)

(defvar *watching* (d:table)
  "Every watch there is, by the object handed back when it was made.")

(defvar *dispatch* nil
  "How a watcher is told. Filled in by whatever keeps a pool of threads; until it
is, a watcher is told on the thread that wrote, which is what the kernel does
before there is a pool.

This is the whole of keeping somebody else's code off the writer's thread. A
watcher that talks to the world must not be able to hold up a write it has
nothing to do with.")

(defvar *broke* nil
  "What to do about a watcher that would not run.")

(defclass watch ()
  ((at    :initarg :at    :reader at)
   (tells :initarg :tells :reader tells)
   (when- :initarg :when  :reader when-of)
   (was   :initarg :was   :accessor was))
  (:documentation "Somebody to tell when a place moves.

Not a place. A watch that stood in the namespace would be worked out from what it
watches, which would put whoever's callback it is on the thread of whoever wrote
-- and a watcher that shells out would then be able to stall an unrelated write."))

(defmethod print-object ((w watch) stream)
  (print-unreadable-object (w stream :type t)
    (write-string (place:full-name (at w)) stream)))

(defun watches () (d:vals (d:all *watching*)))

(defun watching (p)
  "Every watch on this place."
  (remove-if-not (lambda (w) (eq (at w) p)) (watches)))

(defun %ran (thunk)
  (handler-case (funcall thunk)
    (error (c) (when *broke* (funcall *broke* c)) nil)))

(defun told (w)
  "Tell one watcher what stands at its place now.

The place is worked out here, on whatever thread this is running on, and not on
the thread that wrote. WHEN of :ON-CHANGE keeps the last answer and says nothing
where it has not moved; :ALWAYS says something every time the place is marked,
which is what somebody following a stream rather than a value wants."
  (let ((now (%ran (lambda () (place:held (at w))))))
    (when (or (eq (when-of w) :always) (not (d:same now (was w))))
      (setf (was w) now)
      (%ran (lambda () (funcall (tells w) (at w) now))))
    now))

(defun watch (said tells &key (when :on-change))
  "Tell TELLS when the place SAID names moves.

Takes a place, the way the other calls do. A watch on something that is worked
out is told when what it is worked out from moves, because that is when it moved."
  (let* ((p (tree:standing said))
         (w (make-instance 'watch :at p :tells tells :when when
                                  :was (%ran (lambda () (place:held p))))))
    (d:keep! *watching* w w)
    w))

(defun unwatch (w) (d:drop! *watching* w) w)

(defun forget-all () (d:clear! *watching*) nil)

(defun %told (moved)
  "Tell everybody watching anything in MOVED, once for the whole list.

The places are worked out together, so forty surfaces that went stale on one
write are forty pieces of work and not one queue. Nothing here waits on anything
else here."
  (let* ((all (remove-duplicates moved))
         (owed (remove-if-not (lambda (w) (member (at w) all)) (watches))))
    (when owed
      (graph:all-worked (mapcar #'at owed))
      (if *dispatch*
          (funcall *dispatch* (mapcar (lambda (w) (lambda () (told w))) owed))
          (mapc #'told owed)))
    owed))

(defun attend ()
  "Listen to the tree, so a watch is told when a place moves."
  (setf (tell:on-move :watch) #'%told))
