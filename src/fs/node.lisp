(defpackage #:pine.fs.node
  (:use #:cl)
  (:shadow #:describe)
  (:local-nicknames (#:c #:pine.run.cell) (#:d #:pine.data))
  (:export #:node #:value-node #:nodep #:name #:parent #:describes #:describe
           #:contents #:nodes #:resolve #:attach #:detach #:leafp #:persistp #:livep
           #:dependents #:depend #:invalidate #:*reading* #:reading
           #:node-named #:make-node #:full-name #:root-of
           #:kept #:child #:children #:stir #:announces #:every-seconds
           #:verb #:verbp #:verb-name #:verb-args
           #:slot-node #:object #:slot-of #:slots))

(in-package #:pine.fs.node)

(defvar *reading* nil)

(defclass node ()
  ((name       :initarg :name      :reader name)
   (parent     :initarg :parent    :accessor parent     :initform nil)
   (describes  :initarg :describes :accessor describes  :initform nil)
   (under      :initform (c:cell nil) :reader under)
   (kept       :initform (c:cell nil) :reader kept)
   (dependents :initform (c:cell nil) :reader dependents)))

(defclass value-node (node)
  ((held :initform (c:cell nil) :reader held)))

(defclass slot-node (node)
  ((object  :initarg :object :reader object)
   (slot-of :initarg :slot   :reader slot-of)))

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

(defun child (n name builder)
  (let ((name (princ-to-string name)))
    (or (cdr (assoc name (c:held (kept n)) :test #'equal))
        (let ((made (funcall builder)))
          (cdr (assoc name
                      (c:swap (kept n)
                              (lambda (all)
                                (if (assoc name all :test #'equal)
                                    all
                                    (acons name made all))))
                      :test #'equal))))))

(defun children (n) (mapcar #'cdr (c:held (kept n))))

(defun root-of (n)
  (loop :for at := n :then (parent at)
        :while (parent at)
        :finally (return at)))

(defgeneric stir (node)
  (:method ((n node))
    (invalidate n)
    (dolist (each (children n) n) (stir each))))

(defgeneric announces (node)
  (:method ((n node)) (declare (ignore n)) nil))

(defgeneric every-seconds (node)
  (:method ((n node)) (declare (ignore n)) nil))

(defun verbp (value)
  (and (d:seqp value) (plusp (d:size value)) (keywordp (d:at value 0))))

(defun verb-name (value) (d:at value 0))

(defun verb-args (value) (d:as :list (d:rest value)))

(defgeneric verb (node name arguments)
  (:method ((n node) name arguments)
    (let ((held (contents n)))
      (setf (contents n)
            (case name
              (:set    (cl:first arguments))
              (:toggle (not held))
              (:conj   (d:with (or held (d:no-set)) (cl:first arguments)))
              (:disj   (d:without (or held (d:no-set)) (cl:first arguments)))
              (:merge  (d:merged (or held (d:no-map)) (cl:first arguments)))
              (t       (cl:first arguments)))))))

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

(defmethod (setf contents) :around (value (n node))
  (if (verbp value)
      (verb n (verb-name value) (verb-args value))
      (call-next-method)))

(defgeneric (setf contents) (value node)
  (:method (value (n node))
    (error "~a holds nothing that can be written." (full-name n)))
  (:method (value (n value-node))
    (c:put (held n) value)
    (invalidate n)
    value))

(defmethod contents ((n slot-node))
  (slot-value (object n) (slot-of n)))

(defmethod (setf contents) (value (n slot-node))
  (setf (slot-value (object n) (slot-of n)) value)
  (invalidate n)
  value)

(defmethod leafp ((n slot-node)) t)
(defmethod persistp ((n slot-node)) t)

(defun slots (of into &rest pairs)
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot-node :name (string-downcase (string name))
                                                   :object of :slot slot)
                         into)))
