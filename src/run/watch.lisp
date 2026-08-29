(defpackage #:pine/run/watch
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault))
  (:export
   #:watch #:unwatch #:forget-all))
(in-package #:pine/run/watch)

(defvar *watchers* nil)
(defparameter *every* 1)
(defparameter *workers* 4
  "Workers on the pool a watcher is told on. Enough that one that shells out does
not hold up the rest, few enough that a hundred of them cannot take the machine.")

(defclass watcher (node:node)
  ((watches :initarg :watches :reader watches)
   (tells   :initarg :tells   :reader tells)
   (only    :initarg :only :reader only :initform t)
   (was     :initform '#:unread :accessor was)
   (polling :initarg :poll :reader polling :initform nil)
   (telling :initform nil :accessor telling)
   (again   :initform nil :accessor again))
  (:documentation "Somebody waiting to hear that a node moved. Not a thread: what
has to be asked is asked on one sweep, however many there are.

TELLING and AGAIN keep one watcher's tellings in order. It is told on a worker
rather than on the thread that wrote, and two workers telling one watcher at once
would race on WAS: told twice for one move, and told the older value last."))

(defmethod print-object ((w watcher) stream)
  (print-unreadable-object (w stream :type t)
    (write-string (node:full-name (watches w)) stream)))

(defun watchers () *watchers*)

(defun fire (w)
  (let ((now (fault:attempt (lambda () (node:contents (watches w)))
                            (format nil "reading ~a" (node:full-name (watches w))))))
    (when (or (not (only w)) (not (d:same now (was w))))
      (setf (was w) now)
      (fault:attempt (lambda () (funcall (tells w) (watches w) now))
                     (format nil "telling a watcher of ~a"
                             (node:full-name (watches w)))))
    now))

(defun %told (w)
  "Tell it, and tell it again if the place moved while we were telling.

One telling at a time per watcher. FIRE reads the place rather than carrying a
value with it, so a telling asked for while another was running has nothing of
its own to say: it is enough that one more happens after that one finishes.

The claim is given up before AGAIN is read, and taken again to go round, so a
move that lands in between is somebody else's telling to make and not one this
walks away from."
  (loop
    (setf (again w) nil)
    (unwind-protect (fire w)
      (setf (telling w) nil))
    (unless (again w) (return w))
    (unless (d:cas (slot-value w 'telling) nil t) (return w))))

(defmethod node:stir ((w watcher))
  "Say the place moved. Told on a worker and not here: this runs inside the walk
a write does, and a watcher that shells out would hold up the write, everything
else that walk has still to reach, and whoever was waiting on the write."
  (cond ((d:cas (slot-value w 'telling) nil t)
         (actors:later :watch (lambda () (%told w))))
        (t (setf (again w) t)))
  w)

(defun polled () (remove-if-not #'polling (watchers)))

(defun sweep ()
  "Read every watcher that has to be asked, on one tick. A live node is one that
answers differently without anybody writing it; the ones that announce themselves
are told by whatever is behind them instead.

Through the same door as a write, so a slow one is one telling at a time here
too and the tick is not what waits for it."
  (dolist (w (polled) t) (node:stir w)))

(defun attend (&key (every *every*))
  (actors:repeat every #'sweep :as :watch :what "reading the live nodes"))

(defun watch (n tells &key (every *every*) name (only t) (poll (node:livep n)))
  (let ((w (make-instance 'watcher :watches n :tells tells :only only :poll poll
                                   :name (or name (node:full-name n)))))
    (node:depend w n)
    (d:swap *watchers* (lambda (all) (cons w all)))
    (setf (was w) (fault:or-nothing "nothing may stand there yet"
                    (node:contents n)))
    (when (and poll (actors:runningp)) (attend :every every))
    w))

(defun unwatch (w)
  (node:undepend w (watches w))
  (d:swap *watchers* (lambda (all) (remove w all)))
  (unless (polled) (actors:cancel :watch))
  w)

(defun forget-all ()
  (dolist (w (watchers)) (unwatch w))
  (setf *watchers* nil)
  (actors:cancel :watch))

(defun watching (n)
  (remove n (watchers) :key #'watches :test-not #'eq))

(pine/word:lends "watch" "unwatch")

(actors:pool :watch *workers*)
