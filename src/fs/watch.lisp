(defpackage #:pine/fs/watch
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:timer #:pine/run/timer) (#:fault #:pine/run/fault))
  (:export #:watcher #:watch #:unwatch #:watchers #:watching #:of #:told
           #:fire #:sweep #:polled #:attend #:forget-all #:*every*))
(in-package #:pine/fs/watch)

(defvar *watchers* (d:box nil))
(defparameter *every* 1)

(defclass watcher (node:node)
  ((of      :initarg :of   :reader of)
   (told    :initarg :told :reader told)
   (only    :initarg :only :reader only :initform t)
   (was     :initform (d:box '#:unread) :reader was)
   (polling :initarg :poll :reader polling :initform nil)))

(defmethod print-object ((w watcher) stream)
  (print-unreadable-object (w stream :type t)
    (write-string (node:full-name (of w)) stream)))

(defun watchers () (d:held *watchers*))

(defun fire (w)
  (let ((now (fault:attempt (lambda () (node:contents (of w)))
                            (format nil "reading ~a" (node:full-name (of w))))))
    (when (or (not (only w)) (not (equal now (d:held (was w)))))
      (d:put! (was w) now)
      (fault:attempt (lambda () (funcall (told w) (of w) now))
                     (format nil "telling a watcher of ~a" (node:full-name (of w)))))
    now))

(defmethod node:invalidate ((w watcher))
  (fire w)
  w)

(defun polled ()
  (remove-if-not #'polling (watchers)))

(defun sweep ()
  "Read every watcher that has to be asked, on one tick. A live node is one
that answers differently without anybody writing it; the ones that announce
themselves are told by their provider instead."
  (dolist (w (polled) t) (fire w)))

(defun attend (&key (every *every*))
  (timer:every-seconds every #'sweep :as :watch :what "reading the live nodes"))

(defun watch (n told &key (every *every*) name (only t) (poll (node:livep n)))
  (let ((w (make-instance 'watcher :of n :told told :only only :poll poll
                                   :name (or name (node:full-name n)))))
    (node:depend w n)
    (d:swap! *watchers* (lambda (all) (cons w all)))
    (d:put! (was w) (ignore-errors (node:contents n)))
    (when (and poll (timer:timing)) (attend :every every))
    w))

(defun unwatch (w)
  (d:swap! (node:dependents (of w)) (lambda (all) (d:without all w)))
  (d:swap! *watchers* (lambda (all) (remove w all)))
  (unless (polled) (timer:cancel :watch))
  w)

(defun forget-all ()
  (dolist (w (watchers)) (unwatch w))
  (d:put! *watchers* nil)
  (timer:cancel :watch))

(defun watching (n)
  (remove n (watchers) :key #'of :test-not #'eq))
