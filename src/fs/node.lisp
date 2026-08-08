(defpackage #:pine.fs.node
  (:use #:cl)
  (:shadow #:describe)
  (:local-nicknames (#:c #:pine.run.cell))
  (:export #:node #:value-node #:nodep #:name #:parent #:describes #:describe
           #:contents #:nodes #:resolve #:attach #:detach #:leafp #:persistp #:livep
           #:dependents #:depend #:invalidate #:*reading* #:reading
           #:node-named #:make-node #:full-name))

(in-package #:pine.fs.node)

(defvar *reading* nil)

(defclass node ()
  ((name       :initarg :name      :reader name)
   (parent     :initarg :parent    :accessor parent     :initform nil)
   (describes  :initarg :describes :accessor describes  :initform nil)
   (under      :initform (c:cell nil) :reader under)
   (dependents :initform (c:cell nil) :reader dependents)))

(defclass value-node (node)
  ((held :initform (c:cell nil) :reader held)))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (write-string (full-name n) stream)))

(defun nodep (x) (typep x 'node))

(defun make-node (name &rest initargs &key (class 'value-node) &allow-other-keys)
  (apply #'make-instance class :name name
         (alexandria:remove-from-plist initargs :class)))

(defun full-name (n)
  (let ((names (loop :for at := n :then (parent at)
                     :while at
                     :when (name at) :collect (name at))))
    (if names (format nil "/~{~a~^/~}" (reverse names)) "/")))

(defgeneric nodes (node)
  (:method ((n node)) (c:held (under n))))

(defgeneric resolve (node name)
  (:method ((n node) name)
    (find name (nodes n) :key #'name :test #'equal)))

(defgeneric attach (node into)
  (:method ((n node) (into node))
    (setf (parent n) into)
    (c:swap (under into)
            (lambda (all)
              (append (remove (name n) all :key #'name :test #'equal) (list n))))
    n))

(defgeneric detach (node name)
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (c:swap (under n) (lambda (all) (remove gone all)))
        (setf (parent gone) nil))
      gone)))

(defgeneric leafp (node)
  (:method ((n node)) (null (nodes n))))

(defgeneric describe (node)
  (:method ((n node)) (describes n)))

(defgeneric livep (node)
  (:method ((n node)) nil))

(defgeneric persistp (node)
  (:method ((n node)) nil)
  (:method ((n value-node)) t))

(defun reading (node)
  (when *reading* (pushnew node (cdr *reading*)))
  node)

(defgeneric depend (node on)
  (:method ((n node) (on node))
    (c:swap (dependents on) (lambda (all) (adjoin n all)))
    n))

(defgeneric invalidate (node)
  (:method ((n node))
    (dolist (d (c:held (dependents n)))
      (invalidate d))
    n))

(defgeneric contents (node)
  (:method ((n node)) nil)
  (:method ((n value-node)) (c:held (held n))))

(defmethod contents :around ((n node))
  (reading n)
  (call-next-method))

(defgeneric (setf contents) (value node)
  (:method (value (n node))
    (error "~a holds nothing that can be written." (full-name n)))
  (:method (value (n value-node))
    (c:put (held n) value)
    (invalidate n)
    value))
