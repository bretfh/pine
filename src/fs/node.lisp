(defpackage #:pine.fs.node
  (:use #:cl)
  (:shadow #:describe)
  (:local-nicknames (#:d #:pine.data))
  (:export #:node #:value-node #:nodep #:name #:parent #:describes #:describe
           #:contents #:nodes #:resolve #:attach #:detach #:leafp #:persistp #:livep
           #:dependents #:depend #:invalidate #:*reading* #:reading #:*on-write*
           #:node-named #:make-node #:full-name #:root-of
           #:kept #:child #:children #:stir #:announces #:every-seconds
           #:verb #:verbp #:verb-name #:verb-args
           #:slot-node #:object #:slot-of #:slots))

(in-package #:pine.fs.node)

(defvar *reading* nil)
(defvar *on-write* nil)

(defclass node ()
  ((name       :initarg :name      :reader name)
   (parent     :initarg :parent    :accessor parent     :initform nil)
   (describes  :initarg :describes :accessor describes  :initform nil)
   (under      :initform (d:box (d:no-seq)) :reader under)
   (kept       :initform (d:table)           :reader kept)
   (dependents :initform (d:box (d:no-set))  :reader dependents)))

(defclass value-node (node)
  ((held :initform (d:box nil) :reader held)))

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
  (:method ((n node)) (d:as :list (d:held (under n)))))

(defgeneric resolve (node name)
  (:method ((n node) name)
    (find name (nodes n) :key #'name :test #'equal)))

(defgeneric attach (node into)
  (:method ((n node) (into node))
    (setf (parent n) into)
    (d:swap! (under into)
             (lambda (all)
               (d:with (d:as :seq (cl:remove (name n) (d:as :list all)
                                             :key #'name :test #'equal))
                       n)))
    n))

(defgeneric detach (node name)
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (d:swap! (under n) (lambda (all) (d:remove gone all)))
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
  "The child N keeps under NAME, made once. Two threads asking at once both
answer the one that landed."
  (let ((name (princ-to-string name)))
    (or (d:at (d:all (kept n)) name)
        (d:claim (kept n) name (funcall builder)))))

(defun children (n) (d:vals (d:all (kept n))))

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
    (d:swap! (dependents on) (lambda (all) (d:with all n)))
    n))

(defgeneric invalidate (node)
  (:method ((n node))
    (d:do-each (each (d:held (dependents n)))
      (invalidate each))
    n))

(defgeneric contents (node)
  (:method ((n node)) nil)
  (:method ((n value-node)) (d:held (held n))))

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
    (d:put! (held n) value)
    (invalidate n)
    (when *on-write* (funcall *on-write* n))
    value))

(defmethod contents ((n slot-node))
  (slot-value (object n) (slot-of n)))

(defmethod (setf contents) (value (n slot-node))
  "Writing a slot moves the node that holds it: whether a surface is shown is
written at /surface/ctl/shown, and what is watching is the surface."
  (setf (slot-value (object n) (slot-of n)) value)
  (invalidate n)
  (let ((of (object n)))
    (when (and (nodep of) (not (eq of n))) (invalidate of)))
  value)

(defmethod leafp ((n slot-node)) t)
(defmethod persistp ((n slot-node)) t)

(defun slots (of into &rest pairs)
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot-node :name (string-downcase (string name))
                                                   :object of :slot slot)
                         into)))
