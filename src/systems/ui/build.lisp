(in-package #:pine/ui)

(defvar *here* nil)

(defgeneric confirming (where question thunk)
  (:method (where question thunk)
    (declare (ignore where thunk))
    (log:note "~a: nothing here can ask" question)))

(defun here ()
  *here*)

(deftype somewhere ()
  '(or fs:mount fs:value fs:derived path:path))

(defun placep (it) (typep it 'somewhere))

(defmethod held ((it path:path))
  (let ((n (fs:at it))) (and n (fs:contents n))))

(defmethod held (it) (if (fs:nodep it) (fs:contents it) it))

(defmethod (setf held) (value it) (setf (fs:contents it) value))

(defmethod (setf held) (value (it path:path))
  (setf (fs:contents (fs:mount (make-instance 'fs:value) it)) value))

(defun %shown (it)
  (let ((v (held it))) (if (null v) "" v)))

(defun %writing (m)
  (lambda ()
    (d:do-pairs (where value m) (setf (held where) value))
    t))

(defgeneric acting (on-click)
  (:method ((on-click null)) nil)
  (:method ((on-click function)) on-click)
  (:method ((on-click path:path)) (lambda () (setf (held on-click) t)))
  (:method (on-click)
    (cond ((fs:nodep on-click) (lambda () (setf (held on-click) t)))
          ((d:mapp on-click) (%writing on-click))
          (t (lambda () (command:run on-click))))))

(defun %click (props)
  (let ((thunk (acting (getf props :on-click)))
        (ask (getf props :confirm)))
    (cond ((null thunk) nil)
          ((null ask) thunk)
          (t (lambda () (confirming command:*at* ask thunk))))))

(defun %without (props &rest keys)
  (loop :for (k v) :on props :by #'cddr
        :unless (member k keys) :append (list k v)))

(defun %split (args)
  (let ((props nil) (rest args))
    (loop :while (and rest (keywordp (car rest)) (cdr rest))
          :do (let ((key (pop rest))) (setf props (append props (list key (pop rest))))))
    (values props
            (loop :for c :in rest :when c :append (if (listp c) c (list c))))))

(defun label (text &rest props)
  (apply #'make-instance 'label
         :content (let ((it (%shown text)))
                    (if (stringp it) it (princ-to-string it)))
         props))

(defun field (subject &rest props)
  (apply #'make-instance 'label
         :content (princ-to-string (%shown subject))
         :of (when (placep subject) subject)
         :on-change (when (placep subject)
                    (lambda (v) (setf (held subject) v)))
         :class (or (getf props :class) "field")
         (%without props :class)))

(defun icon (glyph &rest props)
  (let* ((raw (%shown glyph))
         (g (if (integerp raw) (string (code-char raw)) (string raw)))
         (thunk (%click props)))
    (if thunk
        (apply #'make-instance 'action
               :on-click thunk
               :parts (list (make-instance 'label :content g
                                           :class (getf props :glyph-class)
                                           :face (getf props :face)
                                           :font (getf props :font)))
               (%without props :face :font :on-click :confirm :glyph-class))
        (make-instance 'label :content g :class (getf props :class)
                               :face (getf props :face) :font (getf props :font)))))

(defun button (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'action :on-click (%click props)
           :parts (list (first parts))
           (%without props :on-click :confirm))))

(defun column (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'column :parts parts props)))

(defun row (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'row :parts parts props)))

(defun stack (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'stack :parts parts props)))

(defun box (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'box :parts (list (first parts)) props)))

(defun center (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'center :parts (list (first parts)) props)))

(defun centerbox (&key (upright :yes) class hint expand start center end)
  (make-instance 'centerbox :upright (ecase upright (:yes t) (:no nil))
                              :class class :hint hint :expand (or expand 0)
                              :start start :middle center :end end))

(defun scroll (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'scroll :parts (list (first parts)) props)))

(defun gap (&rest props) (apply #'make-instance 'gap props))

(defun rule (&rest props) (apply #'make-instance 'rule props))

(defun slider (&rest args)
  (let ((subject (first args)))
    (cond ((placep subject)
           (apply #'make-instance 'slider
                  :value (or (held subject) 0)
                  :on-change (lambda (v) (setf (held subject) v))
                  (cl:rest args)))
          ((keywordp subject) (apply #'make-instance 'slider args))
          (t (apply #'make-instance 'slider :value subject (cl:rest args))))))

(defun ring (&rest args)
  (let ((subject (first args)))
    (if (keywordp subject)
        (multiple-value-bind (props parts) (%split args)
          (apply #'make-instance 'ring :parts (list (first parts)) props))
        (multiple-value-bind (props parts) (%split (cl:rest args))
          (apply #'make-instance 'ring
                 :parts (list (first parts))
                 :value (if (placep subject) (or (held subject) 0) subject)
                 props)))))

(defun grid (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (let ((n (max 1 (or (getf props :columns) 1))))
      (apply #'make-instance 'column
             :parts (loop :for rest := parts :then (nthcdr n rest)
                          :while rest
                          :collect (apply #'make-instance 'row
                                          :parts (subseq rest 0 (min n (length rest)))
                                          (%without props :columns)))
             (%without props :columns)))))

(defun choice (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'choice :parts (list (first parts))
           :on-click (%click props)
           (%without props :on-click :confirm))))

(defun calendar (&rest props) (apply #'make-instance 'calendar props))

(defun image (where &rest props)
  (apply #'make-instance 'picture :path (princ-to-string (%shown where)) props))

(defun cells (rows &rest props)
  (apply #'make-instance 'cells :rows rows props))

(defun rows (items builder &rest props)
  (let* ((over-paths (path:pathp items))
         (all (cond ((path:patternp items)
                     (mapcar (lambda (each) (path:path (fs:full-name each)))
                             (path:matching items)))
                    (over-paths
                     (let ((n (fs:at items)))
                       (and n (mapcar (lambda (each)
                                        (path:path (fs:full-name each)))
                                      (fs:children n)))))
                    (t items))))
    (apply #'make-instance 'column
           :parts (loop :for item :in all
                        :for i :from 0
                        :collect (if over-paths
                                     (let ((*here* (if (path:pathp item) item *here*)))
                                       (let ((made (funcall builder)))
                                         (when (and made (null (of made)))
                                           (setf (of made) *here*))
                                         made))
                                     (funcall builder item i)))
           props)))

