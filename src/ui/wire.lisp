(defpackage #:pine/ui/wire
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:w #:pine/ui/widget))
  (:export #:to-wire #:from-wire #:tag #:placed #:views #:patch #:apply-patch
           #:scroll-to))
(in-package #:pine/ui/wire)

(defparameter +skip+ '(:key :of :parts :hovered :pad)
  "What does not cross: the identity the daemon keeps, what a widget stands for,
what PARTS already says, a flag the painter sets itself, and the shorthand that
sets two others.")

(defparameter +thunks+ '(:click :changed)
  "Slots holding a function. A closure cannot cross, so it goes as an id and what
it meant stays where it was made.")

(defparameter +patchable+ '(:rows :caret)
  "What a patch can carry. Two forms differing only in these can be sent as one.")

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
    (or (d:at (d:all *classes*) name)
        (d:claim *classes* name (%slots class)))))

(defun tag (widget)
  (intern (symbol-name (class-name (class-of widget))) :keyword))

(defun %class (tag)
  (or (find-symbol (symbol-name tag) :pine/ui/widget)
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
                                                   :pine/ui/widget)
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
            (when (typep v 'w:widget)
              (setf v (to-wire v :on-action on-action)))
            (unless (equal v default) (setf props (list* key v props))))))
      (when (w:placed widget)
        (setf props (list* :rect (list (w:top widget) (w:left widget)
                                       (w:bottom widget) (w:right widget))
                           props)))
      (list* (tag widget) (%ordered props)
             (mapcar (lambda (p) (to-wire p :on-action on-action))
                     (w:parts widget))))))

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
            (setf (w:top widget) top (w:left widget) left
                  (w:bottom widget) bottom (w:right widget) right)))
        widget))))

(defun placed (form)
  (and (consp form) (getf (second form) :rect) t))

(defun views (form)
  "Every cells form in FORM, in tree order."
  (let (acc)
    (labels ((walk (f)
               (when (and (consp f) (keywordp (first f)))
                 (when (eq (first f) :cells) (push f acc))
                 (dolist (part (cddr f)) (walk part)))))
      (walk form))
    (nreverse acc)))

(defun %shape (form)
  "FORM with everything a patch can carry taken out. Two forms with the same shape
differ only in what a patch can say, which is the test for sending one."
  (if (and (consp form) (keywordp (first form)))
      (let ((props (second form)))
        (when (eq (first form) :cells)
          (setf props (loop :for (k v) :on props :by #'cddr
                            :unless (member k +patchable+) :append (list k v))))
        (list* (first form) props (mapcar #'%shape (cddr form))))
      form))

(defun patch (had now)
  "What changed between two frames, or nothing when a patch cannot say it. One entry
per cells form: (INDEX CARET (LINE . ROW)...), carrying only the lines that differ."
  (when (and had now (equal (%shape had) (%shape now)))
    (let ((olds (views had)) (news (views now)))
      (when (= (length olds) (length news))
        (loop :for o :in olds
              :for n :in news
              :for index :from 0
              :for o-rows := (getf (second o) :rows)
              :for n-rows := (getf (second n) :rows)
              :unless (= (length o-rows) (length n-rows))
                :do (return-from patch nil)
              :collect (list index (getf (second n) :caret)
                             (loop :for a :in o-rows
                                   :for b :in n-rows
                                   :for line :from 0
                                   :unless (equal a b) :collect (cons line b))))))))

(defun apply-patch (form patch)
  "FORM with PATCH applied: a fresh frame carrying the lines that changed."
  (let ((index -1))
    (labels ((patched (f)
               (if (and (consp f) (keywordp (first f)))
                   (if (eq (first f) :cells)
                       (let ((entry (assoc (incf index) patch))
                             (props (second f)))
                         (if (null entry)
                             f
                             (destructuring-bind (caret lines) (rest entry)
                               (let ((rows (copy-list (getf props :rows))))
                                 (dolist (line lines)
                                   (setf (nth (car line) rows) (cdr line)))
                                 (list* :cells
                                        (%ordered
                                         (list* :rows rows :caret caret
                                                (loop :for (k v) :on props :by #'cddr
                                                      :unless (member k +patchable+)
                                                        :append (list k v))))
                                        (cddr f))))))
                       (list* (first f) (second f) (mapcar #'patched (cddr f))))
                   f)))
      (patched form))))

(defun scroll-to (chosen offset visible)
  "The offset that keeps the chosen row on screen."
  (if (minusp chosen)
      (max 0 offset)
      (let ((at offset))
        (when (>= chosen (+ at visible)) (setf at (1+ (- chosen visible))))
        (when (< chosen at) (setf at chosen))
        (max 0 at))))
