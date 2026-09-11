(defpackage #:pine/run/watch
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:actors #:pine/run/actors) (#:job #:pine/run/job)
                    (#:fault #:pine/run/fault))
  (:export
   #:watch #:unwatch #:forget-all #:following #:let-go #:*streaming*
   #:watchers #:for))
(in-package #:pine/run/watch)

(defvar *watchers* nil)
(defvar *streaming* nil)
(defparameter *every* 1)

(defclass watcher ()
  ((name    :initarg :name    :reader name)
   (watches :initarg :watches :reader watches)
   (tells   :initarg :tells   :reader tells)
   (when-told :initarg :tells-when :reader tells-when :initform :on-change)
   (was     :initform '#:unread :accessor was)
   (polling :initarg :poll :reader polling :initform nil)
   (every   :initarg :every :reader every-of :initform *every*)
   (telling :initform nil :accessor telling)
   (again   :initform nil :accessor again)
   (for     :initarg :for :reader for :initform nil)))

(defmethod print-object ((w watcher) stream)
  (print-unreadable-object (w stream :type t)
    (write-string (fs:full-name (watches w)) stream)))

(defun watchers () *watchers*)

(defun fire (w)
  (let ((now (fault:attempt (lambda () (fs:contents (watches w)))
                            (format nil "reading ~a" (fs:full-name (watches w))))))
    (when (or (eq :always (tells-when w)) (not (d:same now (was w))))
      (setf (was w) now)
      (fault:attempt (lambda () (funcall (tells w) (watches w) now))
                     (format nil "telling a watcher of ~a"
                             (fs:full-name (watches w)))))
    now))

(defun %told (w)
  (loop
    (setf (again w) nil)
    (unwind-protect (fire w)
      (setf (telling w) nil))
    (unless (again w) (return w))
    (unless (d:cas-p (slot-value w 'telling) nil t) (return w))))

(defmethod fs:touch ((w watcher))
  (setf (again w) t)
  (when (d:cas-p (slot-value w 'telling) nil t)
    (actors:later :watch (lambda () (%told w))))
  w)

(defun polled () (remove-if-not #'polling (watchers)))

(defun sweep (&optional every)
  (dolist (w (polled) t)
    (when (or (null every) (eql every (every-of w))) (fs:touch w))))

(defun attend (&key (every *every*))
  (job:repeat every (lambda () (sweep every))
              :as (%tick-name every)
              :what "reading the live nodes"))

(defun %tick-name (every) (format nil "watch-~a" every))

(defun %attending ()
  (let ((wanted (mapcar #'%tick-name (remove-duplicates (mapcar #'every-of (polled))))))
    (dolist (j (job:ticks) t)
      (let ((name (job:name j)))
        (when (and (eql 0 (search "watch-" name))
                   (not (member name wanted :test #'equal)))
          (job:cancel j))))))

(defgeneric watch (n tells &key every name tells-when poll for)
  (:method (n tells &key (every *every*) name (tells-when :on-change)
                    (poll (fs:volatile-p n)) for)
    (let ((w (make-instance 'watcher :watches n :tells tells :tells-when tells-when
                                     :poll poll :every every :for for
                                     :name (or name (fs:full-name n)))))
      (fs:depend w n)
      (sb-ext:atomic-update *watchers* (lambda (all) (cons w all)))
      (setf (was w) (fault:or-nothing "nothing may stand there yet"
                      (fs:contents n)))
      (when (and poll (actors:runningp)) (attend :every every))
      w)))

(defgeneric following (n)
  (:method (n)
    (let ((heard (when *streaming*
                   (loop :for line :in (fs:notified-by n)
                         :for s := (funcall *streaming* line)
                         :when s
                           :collect (watch s (lambda (of said)
                                               (declare (ignore of said))
                                               (fault:attempt
                                                (lambda () (fs:touch n))
                                                (fs:name n)))
                                           :tells-when :always :poll nil
                                           :name (format nil "~a<-~a"
                                                         (fs:name n) line)))))
          (ticking (let ((seconds (fs:polls n)))
                     (when seconds
                       (job:repeat seconds (lambda () (fs:touch n))
                                   :as (format nil "following~a" (fs:full-name n))
                                   :what (fs:name n))))))
      (list heard ticking))))

(defun let-go (held)
  (destructuring-bind (heard ticking) held
    (mapc #'unwatch heard)
    (when ticking (job:cancel ticking)))
  t)

(defun unwatch (w)
  (fs:undepend w (watches w))
  (sb-ext:atomic-update *watchers* (lambda (all) (remove w all)))
  (%attending)
  w)

(defun forget-all ()
  (dolist (w (watchers)) (unwatch w))
  (setf *watchers* nil)
  (%attending))

(defun watching (n)
  (remove n (watchers) :key #'watches :test-not #'eq))

