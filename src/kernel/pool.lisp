(defpackage #:pine/kernel/pool
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:graph #:pine/kernel/graph)
                    (#:watch #:pine/kernel/watch) (#:bt #:bordeaux-threads))
  (:export
   #:pool #:start #:stop #:runningp #:hands #:fan-out #:later #:*pool*
   #:*mine* #:waiting-on #:done))
(in-package #:pine/kernel/pool)

(defvar *pool* nil
  "The hands this image works with, or nothing where it works with its own.")

(defvar *mine* nil
  "Whether this thread is one of the pool's. A worker that waited on the pool
would be a hand waiting for the hands, so one that is asked to fan work out does
it where it stands instead.")

(defstruct (pool (:constructor %pool) (:copier nil))
  "A few threads and the work waiting for them.

Not an actor system and not a scheduler. Working a place out is a call that
answers, and what this is for is calling several at once -- so it is a queue, and
threads taking from it, and nothing else."
  (lock (bt:make-lock "pine/pool"))
  (news (bt:make-condition-variable))
  (waiting nil)
  (hands nil)
  (stopping nil))

(defun cores ()
  (or (ignore-errors
       (parse-integer
        (with-output-to-string (out)
          (uiop:run-program '("nproc") :output out :ignore-error-status t))
        :junk-allowed t))
      4))

(defun %take (p)
  (bt:with-lock-held ((pool-lock p))
    (loop :until (or (pool-stopping p) (pool-waiting p))
          :do (bt:condition-wait (pool-news p) (pool-lock p) :timeout 1))
    (pop (pool-waiting p))))

(defun %hand (p)
  (bt:make-thread
   (lambda ()
     (let ((*mine* t))
       (loop :until (pool-stopping p)
             :do (let ((work (%take p)))
                   (when work (ignore-errors (funcall work)))))))
   :name "pine/hand"))

(defun runningp () (and *pool* (pool-hands *pool*) t))

(defun start (&key (hands (max 1 (1- (cores)))))
  "Take up as many hands as there are cores to work with, less the one asking.

Working a place out is where the time goes and places that read different things
are different work, so this is the whole of what pine does with a machine's
cores. Nothing here schedules anything: the work is already independent."
  (unless (runningp)
    (let ((p (%pool)))
      (setf (pool-hands p) (loop :repeat hands :collect (%hand p))
            *pool* p
            graph:*fan-out* #'fan-out
            watch:*dispatch* #'later)))
  *pool*)

(defun stop ()
  (let ((p *pool*))
    (when p
      (bt:with-lock-held ((pool-lock p))
        (setf (pool-stopping p) t)
        (bt:condition-notify (pool-news p)))
      (dolist (each (pool-hands p))
        (bt:condition-notify (pool-news p))
        (ignore-errors (bt:join-thread each)))
      (setf (pool-hands p) nil
            *pool* nil
            graph:*fan-out* nil
            watch:*dispatch* nil)))
  nil)

(defun hands () (length (and *pool* (pool-hands *pool*))))

(defun %give (p work)
  (bt:with-lock-held ((pool-lock p))
    (push work (pool-waiting p))
    (bt:condition-notify (pool-news p))))

(defun later (thunks)
  "Hand this work over and do not wait for it.

For telling watchers. A write is finished when the news is handed over, not when
everybody who wanted it has done something about it -- otherwise a watcher that
talks to the world holds up a write it has nothing to do with."
  (let ((p *pool*))
    (if (and p (not *mine*))
        (dolist (each thunks) (%give p each))
        (dolist (each thunks) (ignore-errors (funcall each)))))
  t)

(defun fan-out (thunks)
  "Do all of it at once, and answer when all of it is done.

A thread already holding a hand does the work where it stands: waiting on the
pool from inside the pool is a hand waiting for the hands, and with enough of
them nested there would be none left to do anything."
  (let ((p *pool*))
    (if (or (null p) *mine* (null (rest thunks)))
        (dolist (each thunks t) (ignore-errors (funcall each)))
        (let ((left (list (length thunks)))
              (lock (bt:make-lock "pine/fan-out"))
              (news (bt:make-condition-variable)))
          (dolist (each thunks)
            (%give p (let ((each each))
                       (lambda ()
                         (unwind-protect (funcall each)
                           (bt:with-lock-held (lock)
                             (decf (car left))
                             (bt:condition-notify news)))))))
          (bt:with-lock-held (lock)
            (loop :until (zerop (car left))
                  :do (bt:condition-wait news lock :timeout 5)))
          t))))
