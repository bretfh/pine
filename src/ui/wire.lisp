(in-package #:pine/ui)

(defparameter +skip+ '(:key :of :parts :hovered :pad)
  "What does not cross: the identity the daemon keeps, what a widget stands for,
what PARTS already says, a flag the far side sets itself, and the shorthand that
sets two others.")

(defparameter +thunks+ '(:click :changed)
  "Slots holding a function. A closure cannot cross, so it goes as an id and what
it meant stays where it was made.")

(defvar *classes* (d:table))

(defun %slots (class)
  "The slots this class carries, as (initarg reader default). Direct slots up the
precedence list rather than the effective ones: an effective slot has no readers to
ask about, and what crosses is what somebody can read back."
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
  "What this class carries, taken from its own slots. A property added to a widget
crosses because it is there, not because somebody remembered to list it."
  (let ((name (class-name class)))
    (or (d:lookup (d:all *classes*) name)
        (d:claim *classes* name (%slots class)))))

(defun tag (widget)
  (intern (symbol-name (class-name (class-of widget))) :keyword))

(defun %class (tag)
  (or (find-symbol (symbol-name tag) :pine/ui)
      (error "no widget crosses the wire as ~s" tag)))

(defun %ordered (props)
  "PROPS in key order, so two forms saying the same thing are EQUAL: whether a frame
may go as a patch is decided by comparing it with the one before."
  (let ((pairs (loop :for (k v) :on props :by #'cddr :collect (cons k v))))
    (loop :for (k . v) :in (sort pairs #'string< :key (lambda (p)
                                                        (symbol-name (car p))))
          :append (list k v))))

(defun %widget-form-p (v)
  "Whether a slot's value is a widget written down: a centerbox holds its start,
middle and end in slots of its own, and they cross the way a part does."
  (and (consp v) (keywordp (first v)) (find-symbol (symbol-name (first v))
                                                   :pine/ui)
       (consp (rest v)) (listp (second v))))

(defun to-wire (widget &key on-action)
  "A widget as plain data. ON-ACTION, given a closure, answers an id to put in its
place."
  (when widget
    (let ((props nil))
      (dolist (spec (%known (class-of widget)))
        (destructuring-bind (key reader default) spec
          (let ((v (funcall reader widget)))
            (when (member key +thunks+)
              (setf v (and v on-action (funcall on-action v))))
            (when (typep v 'widget)
              (setf v (to-wire v :on-action on-action)))
            (unless (equal v default) (setf props (list* key v props))))))
      (when (placed widget)
        (setf props (list* :rect (list (top widget) (left widget)
                                       (bottom widget) (right widget))
                           props)))
      (list* (tag widget) (%ordered props)
             (mapcar (lambda (p) (to-wire p :on-action on-action))
                     (parts widget))))))

(defun from-wire (form &key on-action)
  "Build a widget back from wire FORM, restoring the rect it was arranged at.
ON-ACTION, given an id, answers what to do about it."
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

