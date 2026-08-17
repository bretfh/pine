(defpackage #:pine/fs/node
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:commit #:pine/fs/commit))
  (:export #:node #:value #:derived #:slot #:nodep
           #:name #:over #:describes #:savedp #:livep #:announces #:refreshes
           #:contents #:nodes #:resolve #:stir #:verb
           #:full-name #:leafp #:stalep #:root
           #:make #:derive #:attach #:detach #:child #:children #:memo #:slots
           #:make-child #:erase-child #:reads #:writes #:object #:saw
           #:readers #:depend #:reading #:*reading* #:*on-write* #:*on-erase*))
(in-package #:pine/fs/node)

(defvar *reading* nil)
(defvar *on-write* nil)
(defvar *on-erase* nil)
(defparameter +unread+ '#:unread)

(defclass node ()
  ((name      :initarg :name      :reader name)
   (over      :initarg :over      :accessor over      :initform nil)
   (describes :initarg :describes :accessor describes :initform nil)
   (beneath   :initform (d:box (d:no-seq)) :reader beneath)
   (memo      :initform (d:table)          :reader memo)
   (readers   :initform (d:box (d:no-set)) :reader readers)
   (named     :initform (d:box nil)        :reader named)
   (savedp    :allocation :class :initform nil :reader savedp)
   (livep     :allocation :class :initform nil :reader livep))
  (:documentation "A name, what it sits under, and what is under it.

Whether a class persists and whether it answers differently without being written
are declared once as class slots, not asked of every instance."))

(defclass value (node)
  ((savedp :allocation :class :initform t :reader savedp))
  (:documentation "Holds one."))

(defclass derived (node)
  ((reads  :initarg :reads  :accessor reads)
   (writes :initarg :writes :accessor writes :initform nil)
   (cached :initform (d:box +unread+) :reader cached)
   (saw    :initform (d:box nil)      :reader saw))
  (:documentation "Works one out, and follows whatever it read.

READS answers the value; WRITES, where there is one, is what writing it means. That
second function is why a device reading is a row in a table rather than a class."))

(defclass slot (node)
  ((object :initarg :object :reader object)
   (slot   :initarg :slot   :reader slot)
   (savedp :allocation :class :initform t :reader savedp))
  (:documentation "One slot of an object, at a place."))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (write-string (full-name n) stream)))

(defun nodep (x) (typep x 'node))

(defun make (name &rest initargs &key (class 'value) &allow-other-keys)
  (apply #'make-instance class :name name
         (alexandria:remove-from-plist initargs :class)))

(defun derive (name reads &rest initargs)
  (apply #'make-instance 'derived :name name :reads reads initargs))

(defun %named (n)
  (let ((names (loop :for at := n :then (over at)
                     :while at
                     :when (name at) :collect (name at))))
    (if names (format nil "/~{~a~^/~}" (reverse names)) "/")))

(defun full-name (n)
  "The path this node is at."
  (or (d:held (named n)) (d:put! (named n) (%named n))))

(defun %renamed (n)
  (d:put! (named n) nil)
  (d:do-each (each (d:held (beneath n))) (%renamed each))
  (dolist (each (d:vals (d:all (memo n)))) (%renamed each))
  n)

(defun root (n)
  (loop :for at := n :then (over at)
        :while (over at)
        :finally (return at)))

(defgeneric nodes (node)
  (:documentation "What is under NODE, in order.

The one place a class says what it contains. RESOLVE and LEAFP are written on this,
so a class that answers here answers everywhere.")
  (:method ((n node)) (d:as :list (d:held (beneath n)))))

(defgeneric resolve (node name)
  (:documentation "What NODE has under NAME.

Derived from NODES, so the two cannot disagree. A class overrides it only where
enumerating to find one name costs what a mounted directory cannot afford.")
  (:method ((n node) name)
    (find name (nodes n) :key #'name :test #'equal)))

(defun leafp (n) (null (nodes n)))

(defun children (n) (d:vals (d:all (memo n))))

(defun child (n name builder)
  "The child N keeps under NAME, made once. Two threads asking at once both answer
the one that landed, which is what lets anything reading it be worked out again."
  (let ((name (princ-to-string name)))
    (or (d:at (d:all (memo n)) name)
        (d:claim (memo n) name (funcall builder)))))

(defgeneric attach (node into)
  (:documentation "Put NODE under INTO, replacing whatever stood at its name.")
  (:method ((n node) (into node))
    (setf (over n) into)
    (%renamed n)
    (d:swap! (beneath into)
             (lambda (all)
               (d:with (d:as :seq (cl:remove (name n) (d:as :list all)
                                             :key #'name :test #'equal))
                       n)))
    n))

(defgeneric detach (node name)
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (d:swap! (beneath n) (lambda (all) (d:remove gone all)))
        (setf (over gone) nil)
        (%renamed gone))
      gone)))

(defgeneric make-child (node name)
  (:documentation "A fresh child of NODE named NAME, made in whatever stands behind
it: a plain node keeps it here, a mounted directory makes a file on the disk. A name
that ends in / asks for a branch.")
  (:method ((n node) name)
    (attach (make (string-right-trim "/" (princ-to-string name))) n)))

(defgeneric erase-child (node name)
  (:documentation "Take NAME out of NODE, and out of whatever stands behind it.")
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (when *on-erase* (funcall *on-erase* gone))
        (commit:forget (full-name gone)))
      (d:drop! (memo n) (princ-to-string name))
      (detach n name))))

(defgeneric announces (node)
  (:documentation "The lines whose output says the world behind NODE moved.")
  (:method ((n node)) (declare (ignore n)) nil))

(defgeneric refreshes (node)
  (:documentation "How often to ask NODE again, where it has no stream to speak for
it.")
  (:method ((n node)) (declare (ignore n)) nil))

(defun reading (n)
  (when *reading* (pushnew n (cdr *reading*)))
  n)

(defgeneric depend (node on)
  (:method ((n node) (on node))
    (d:swap! (readers on) (lambda (all) (d:with all n)))
    n))

(defgeneric stir (node)
  (:documentation "Say NODE moved. Whatever read it is worked out again.")
  (:method ((n node))
    (d:do-each (each (d:held (readers n)))
      (stir each))
    n)
  (:method ((n derived))
    (d:put! (cached n) +unread+)
    (d:do-each (each (d:held (readers n)))
      (stir each))
    n))

(defun %work-out (n)
  "Work N out, and record what it read while doing it. What it read is recorded
whether or not it finished: a node that threw has still read what it read, and one
that keeps nothing is one nothing can ever stir again."
  (let ((reading (cons :reading nil)))
    (unwind-protect
         (let* ((*reading* reading)
                (v (funcall (reads n))))
           (d:put! (cached n) v)
           v)
      (d:put! (saw n) (cdr reading))
      (dolist (on (cdr reading))
        (unless (eq on n) (depend n on))))))

(defun stalep (n) (eq (d:held (cached n)) +unread+))

(defgeneric verb (node name arguments)
  (:documentation "What writing (:toggle) and its like means.")
  (:method ((n node) name arguments)
    (let ((had (contents n)))
      (setf (contents n)
            (case name
              (:set    (cl:first arguments))
              (:toggle (not had))
              (:conj   (d:with (or had (d:no-set)) (cl:first arguments)))
              (:disj   (d:without (or had (d:no-set)) (cl:first arguments)))
              (:merge  (d:merged (or had (d:no-map)) (cl:first arguments)))
              (t       (cl:first arguments)))))))

(defun %verbp (v)
  (and (d:seqp v) (plusp (d:size v)) (keywordp (d:at v 0))))

(defgeneric contents (node)
  (:documentation "What NODE holds.")
  (:method ((n node)) nil)
  (:method ((n value)) (commit:at (full-name n)))
  (:method ((n slot)) (slot-value (object n) (slot n)))
  (:method ((n derived))
    (let ((v (d:held (cached n))))
      (if (eq v +unread+) (%work-out n) v))))

(defgeneric (setf contents) (value node)
  (:method (value (n node))
    (error "~a holds nothing that can be written." (full-name n)))
  (:method (value (n value))
    (unless (d:emptyp (commit:change (d:map (full-name n) value)))
      (commit:announce n)
      (when *on-write* (funcall *on-write* n)))
    value)
  (:method (value (n slot))
    (let ((had (slot-value (object n) (slot n))))
      (setf (slot-value (object n) (slot n)) value)
      (unless (d:same had value)
        (commit:announce n)
        (when *on-write* (funcall *on-write* n))))
    (let ((it (object n)))
      (when (and (nodep it) (not (eq it n))) (stir it)))
    value)
  (:method (value (n derived))
    (cond ((writes n)
           (funcall (writes n) value)
           (commit:announce n))
          (t (setf (reads n) (constantly value))
             (d:put! (cached n) +unread+)))
    value))

(defmethod contents :around ((n node))
  (reading n)
  (call-next-method))

(defmethod (setf contents) :around (value (n node))
  (if (%verbp value)
      (verb n (d:at value 0) (d:as :list (d:rest value)))
      (call-next-method)))

(defmethod (setf contents) :after (value (n node))
  (declare (ignore value))
  (stir n))

(defun slots (object into &rest pairs)
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot
                                        :name (string-downcase (string name))
                                        :object object :slot slot)
                         into)))
