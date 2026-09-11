(in-package #:pine/ui)

(defparameter +skip+ '(:key :of :parts :hovered :pad))

(defparameter +thunks+ '(:on-click :on-change))

(defvar *known* (make-hash-table :test 'eq :synchronized t))

(defun %slots (class)
  (c2mop:ensure-finalized class)
  (let (out)
    (dolist (each (c2mop:class-precedence-list class) (nreverse out))
      (dolist (slot (c2mop:class-direct-slots each))
        (let ((key (first (c2mop:slot-definition-initargs slot)))
              (reader (first (c2mop:slot-definition-readers slot))))
          (when (and key reader (not (member key +skip+))
                     (not (find key out :key #'first)))
            (push (list key reader
                        (let ((f (c2mop:slot-definition-initfunction slot)))
                          (and f (funcall f))))
                  out)))))))

(defun %known (class)
  (let ((name (class-name class)))
    (or (gethash name *known*)
        (setf (gethash name *known*) (%slots class)))))

(defun tag (widget)
  (intern (symbol-name (class-name (class-of widget))) :keyword))

(defun %widgets ()
  (labels ((under (c) (cons c (mapcan #'under (c2mop:class-direct-subclasses c)))))
    (under (find-class 'widget))))

(defun %class (tag)
  (or (find (symbol-name tag) (%widgets)
            :key (lambda (c) (symbol-name (class-name c))) :test #'string=)
      (error "no widget crosses the wire as ~s" tag)))

(defun %ordered (props)
  (let ((pairs (loop :for (k v) :on props :by #'cddr :collect (cons k v))))
    (loop :for (k . v) :in (sort pairs #'string< :key (lambda (p)
                                                        (symbol-name (car p))))
          :append (list k v))))

(defun %widget-form-p (v)
  (and (consp v) (keywordp (first v)) (find-symbol (symbol-name (first v))
                                                   :pine/ui)
       (consp (rest v)) (listp (second v))))

(defun to-wire (widget &key on-action (at nil))
  (when widget
    (let ((props nil))
      (dolist (spec (%known (class-of widget)))
        (destructuring-bind (key reader default) spec
          (let ((v (funcall reader widget)))
            (when (member key +thunks+)
              (setf v (and v on-action (funcall on-action v widget key at))))
            (when (typep v 'widget)
              (setf v (to-wire v :on-action on-action :at at)))
            (unless (equal v default) (setf props (list* key v props))))))
      (when (placed widget)
        (setf props (list* :rect (list (top widget) (left widget)
                                       (bottom widget) (right widget))
                           props)))
      (list* (tag widget) (%ordered props)
             (loop :for p :in (parts widget)
                   :for i :from 0
                   :collect (to-wire p :on-action on-action
                                       :at (append at (list i))))))))

(defun from-wire (form &key on-action)
  (when form
    (destructuring-bind (tag props &rest parts) form
      (let* ((rect (getf props :rect))
             (args (loop :for (k v) :on props :by #'cddr
                         :unless (eq k :rect)
                           :append (list k
                                         (cond ((member k +thunks+)
                                                (and v on-action
                                                     (funcall on-action v)))
                                               ((%widget-form-p v)
                                                (from-wire v :on-action on-action))
                                               (t v)))))
             (widget (apply #'make-instance (%class tag)
                            :parts (mapcar (lambda (p)
                                             (from-wire p :on-action on-action))
                                           parts)
                            args)))
        (when rect
          (destructuring-bind (top left bottom right) rect
            (setf (top widget) top (left widget) left
                  (bottom widget) bottom (right widget) right)))
        widget))))

