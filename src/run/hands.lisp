(defpackage #:pine/run/hands
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:graph #:pine/kernel/graph)
                    (#:watch #:pine/kernel/watch) (#:log #:pine/kernel/log))
  (:export
   #:take-up #:let-go #:hands #:*work* #:*tell*))
(in-package #:pine/run/hands)

(defvar *work* :work
  "Which dispatcher works places out. Its own, and not the shared one.

The shared dispatcher carries the image's messages. Working a place out is not a
message -- it is somebody's arithmetic, and it can be long -- so putting it there
makes every message in the image queue behind whatever a surface happened to be
adding up. Sento says as much in its own documentation, and pine has had
ACTORS:POOL for asking for one of these since before any of this.

Ask for it with :STRATEGY :ROUND-ROBIN. Sento picks a worker at random by
default, which is right for messages and measured half as fast for this: sixty
places handed out at random leave workers idle while others hold two, and in turn
they do not. 7.9x against 11.9x on thirty-one workers.")

(defvar *tell* :shared
  "Which dispatcher tells watchers. Somebody else's callback, so it is kept off
the thread that wrote and may be given a pool of its own.")

(defvar *system* nil)

(defun hands ()
  "How many workers the dispatcher that works places out has."
  (let ((sys *system*))
    (when sys
      (let ((it (getf (sento.actor-system:dispatchers sys) *work*)))
        (when it (length (sento.dispatcher:workers it)))))))

(defun %dispatcher (sys which)
  (or (getf (sento.actor-system:dispatchers sys) which)
      (getf (sento.actor-system:dispatchers sys) :shared)))

(defun %fan-out (sys thunks)
  "Work all of them out at once, and answer when all of them are done.

One message for each piece of work, and not one for each worker. Handing a worker
a share of them looks like it would save postage and measures worse: the shared
dispatcher picks a worker at random, so a few big messages land unevenly and some
workers sit while others hold two. Many small ones even out. Measured, not
reasoned -- the reasoning said the opposite.

Not a task each either. A task is an actor made and stopped again, which is the
right shape for something a person asked for once and the wrong shape for the
sixty places one write reached."
  (let ((to (%dispatcher sys *work*))
        (left (list (length thunks)))
        (lock (bordeaux-threads:make-lock "pine/hands"))
        (news (bordeaux-threads:make-condition-variable)))
    (dolist (each thunks)
      (let ((each each))
        (sento.dispatcher:dispatch-async
         to (list (lambda ()
                    (unwind-protect (funcall each)
                      (bordeaux-threads:with-lock-held (lock)
                        (decf (car left))
                        (bordeaux-threads:condition-notify news))))))))
    (bordeaux-threads:with-lock-held (lock)
      (loop :until (zerop (car left))
            :do (bordeaux-threads:condition-wait news lock :timeout 5)))
    t))

(defun %later (sys thunks)
  "Hand this over and do not wait for it. A write is finished when the news is
handed over, not when everybody who wanted it has done something about it."
  (let ((to (%dispatcher sys *tell*)))
    (dolist (each thunks t)
      (let ((each each))
        (sento.dispatcher:dispatch-async
         to (list (lambda () (ignore-errors (funcall each)))))))))

(defun take-up (sys)
  "Give the kernel this image's hands.

The kernel keeps none of its own and names no actor system: it asks for work to
be spread and for news to be carried, and this is the one place that knows what
either of those means here."
  (setf *system* sys
        graph:*fan-out* (lambda (thunks) (%fan-out sys thunks))
        watch:*dispatch* (lambda (thunks) (%later sys thunks))
        log:*later* (lambda (thunk) (%later sys (list thunk))))
  sys)

(defun let-go ()
  (setf *system* nil
        graph:*fan-out* nil
        watch:*dispatch* nil
        log:*later* nil)
  nil)
