(defpackage #:pine/run/watch
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:actors #:pine/run/actors) (#:fault #:pine/run/fault))
  (:export
   #:watcher #:watch #:unwatch #:watching #:tells
   #:forget-all #:*every*))
(in-package #:pine/run/watch)

(defvar *watchers* nil)
(defparameter *every* 1)

(defclass watcher (node:node)
  ((watches :initarg :watches :reader watches)
   (tells   :initarg :tells   :reader tells)
   (only    :initarg :only :reader only :initform t)
   (was     :initform '#:unread :accessor was)
   (polling :initarg :poll :reader polling :initform nil))
  (:documentation "Somebody waiting to hear that a node moved. Not a thread: what
has to be asked is asked on one sweep, however many there are."))

(defmethod print-object ((w watcher) stream)
  (print-unreadable-object (w stream :type t)
    (write-string (node:full-name (watches w)) stream)))

(defun watchers () *watchers*)

(defun fire (w)
  (let ((now (fault:attempt (lambda () (node:contents (watches w)))
                            (format nil "reading ~a" (node:full-name (watches w))))))
    (when (or (not (only w)) (not (equal now (was w))))
      (setf (was w) now)
      (fault:attempt (lambda () (funcall (tells w) (watches w) now))
                     (format nil "telling a watcher of ~a"
                             (node:full-name (watches w)))))
    now))

(defmethod node:stir ((w watcher))
  (fire w)
  w)

(defun polled () (remove-if-not #'polling (watchers)))

(defun sweep ()
  "Read every watcher that has to be asked, on one tick. A live node is one that
answers differently without anybody writing it; the ones that announce themselves
are told by whatever is behind them instead."
  (dolist (w (polled) t) (fire w)))

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
