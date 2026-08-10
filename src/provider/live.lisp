(defpackage #:pine.provider.live
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node)
                    (#:watch #:pine.fs.watch) (#:task #:pine.run.task)
                    (#:sh #:pine.provider.sh) (#:fault #:pine.run.fault))
  (:export #:attend #:leave #:leave-all #:attending #:tending))

(in-package #:pine.provider.live)

(defvar *tending* (d:box nil))

(defclass tending ()
  ((of       :initarg :of      :reader of)
   (watching :initarg :watching :reader watching :initform nil)
   (ticking  :initarg :ticking  :reader ticking  :initform nil)))

(defmethod print-object ((n tending) stream)
  (print-unreadable-object (n stream :type t)
    (write-string (node:full-name (of n)) stream)))

(defun attending () (d:held *tending*))

(defun %listen (n line)
  (let ((stream (sh:streaming line)))
    (when stream
      (watch:watch stream
                   (lambda (of said)
                     (declare (ignore of said))
                     (fault:attempt (lambda () (node:stir n))
                                    (format nil "stirring ~a" (node:name n))))
                   :only nil :poll nil
                   :name (format nil "~a<-~a" (node:name n) line)))))

(defun attend (n)
  "Start what N declared: the streams whose lines say the world behind it moved,
and its interval when it has no stream to speak for it."
  (when (node:nodep n)
    (let* ((watching (remove nil (mapcar (lambda (line) (%listen n line))
                                         (node:announces n))))
           (seconds (node:every-seconds n))
           (ticking (when seconds
                      (task:each (format nil "live ~a" (node:name n)) seconds
                                 (lambda () (node:stir n)))))
           (it (make-instance 'tending :of n :watching watching :ticking ticking)))
      (d:swap! *tending* (lambda (all) (cons it all)))))
  n)

(defun leave (n)
  (let ((it (find n (attending) :key #'of)))
    (when it
      (mapc #'watch:unwatch (watching it))
      (when (ticking it) (task:stop (ticking it)))
      (d:swap! *tending* (lambda (all) (remove it all))))
    n))

(defun leave-all ()
  (dolist (it (attending) (d:put! *tending* nil))
    (mapc #'watch:unwatch (watching it))
    (when (ticking it) (task:stop (ticking it))))
  (sh:forget-all))
