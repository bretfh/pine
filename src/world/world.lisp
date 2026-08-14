(defpackage #:pine.world.world
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node)
                    (#:tree #:pine.fs.tree))
  (:export #:world #:*world* #:make-world #:root #:name #:identity-of #:idents
           #:identify #:node-for-id #:at #:place #:ensure #:with-world))

(in-package #:pine.world.world)

(defvar *world* nil)

(defclass world ()
  ((name       :initarg :name :reader name :initform "pine")
   (root       :initarg :root :reader root)
   (counter    :initform (d:box 0) :reader counter)
   (idents     :initform (make-hash-table :test 'eql) :reader idents)
   (by-node    :initform (make-hash-table :test 'eq)  :reader by-node)))

(defmethod print-object ((w world) stream)
  (print-unreadable-object (w stream :type t)
    (format stream "~a ~d" (name w) (hash-table-count (idents w)))))

(defun make-world (&key (name "pine"))
  (let ((w (make-instance 'world :name name :root (tree:root))))
    (identify w (root w))
    w))

(defun identify (w n)
  (or (gethash n (by-node w))
      (let ((id (d:swap! (counter w) #'1+)))
        (setf (gethash n (by-node w)) id
              (gethash id (idents w)) n)
        id)))

(defun identity-of (w n) (gethash n (by-node w)))

(defun node-for-id (w id) (gethash id (idents w)))

(defun at (w &rest pieces)
  (apply #'tree:at (root w) pieces))

(defun ensure (w &rest pieces)
  (let ((n (apply #'tree:ensure (root w) pieces)))
    (identify w n)
    n))

(defun place (w pieces value)
  (let ((n (apply #'ensure w (alexandria:ensure-list pieces))))
    (setf (node:contents n) value)
    n))

(defmacro with-world ((w) &body body)
  `(let ((*world* ,w)) ,@body))
