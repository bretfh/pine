(defpackage #:pine.fs.watch
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:node #:pine.fs.node)
                    (#:task #:pine.run.task) (#:fault #:pine.run.fault))
  (:export #:watcher #:watch #:unwatch #:watchers #:watching #:of #:told
           #:fire #:forget-all #:*every*))

(in-package #:pine.fs.watch)

(defvar *watchers* (c:cell nil))
(defparameter *every* 1)

(defclass watcher (node:node)
  ((of      :initarg :of   :reader of)
   (told    :initarg :told :reader told)
   (was     :initform (c:cell '#:unread) :reader was)
   (polling :initform nil  :accessor polling)))

(defmethod print-object ((w watcher) stream)
  (print-unreadable-object (w stream :type t)
    (write-string (node:full-name (of w)) stream)))

(defun watchers () (c:held *watchers*))

(defun fire (w)
  (let ((now (fault:attempt (lambda () (node:contents (of w)))
                            (format nil "reading ~a" (node:full-name (of w))))))
    (unless (equal now (c:held (was w)))
      (c:put (was w) now)
      (fault:attempt (lambda () (funcall (told w) (of w) now))
                     (format nil "telling a watcher of ~a" (node:full-name (of w)))))
    now))

(defmethod node:invalidate ((w watcher))
  (fire w)
  w)

(defun watch (n told &key (every *every*) name)
  (let ((w (make-instance 'watcher :of n :told told
                                   :name (or name (node:full-name n)))))
    (node:depend w n)
    (c:swap *watchers* (lambda (all) (cons w all)))
    (c:put (was w) (ignore-errors (node:contents n)))
    (when (node:livep n)
      (setf (polling w)
            (task:each (format nil "watch ~a" (node:name w)) every
                       (lambda () (fire w)))))
    w))

(defun unwatch (w)
  (c:swap (node:dependents (of w)) (lambda (all) (remove w all)))
  (when (polling w) (task:stop (polling w)))
  (c:swap *watchers* (lambda (all) (remove w all)))
  w)

(defun forget-all ()
  (dolist (w (watchers) (c:put *watchers* nil)) (unwatch w)))

(defun watching (n)
  (remove n (watchers) :key #'of :test-not #'eq))
