(defpackage #:pine/host
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell))
  (:import-from #:pine/host/shell #:sh)
  (:export
   #:device #:defdevice #:defbacking #:sh #:needs
   #:made #:answering #:devices #:unanswered)
  (:documentation "The machine, in the namespace: its devices, its filesystem, its
environment and what it will run.

A kind of device is a class under DEVICE. A way of answering it on one machine is a
class under that, saying what programs it NEEDS; a reading is a method on READ, a
setting a method on WRITE. Which way answers is asked of the machine when the
device is made, and where none can, every reading stands and says :ABSENT."))
(in-package #:pine/host)

(defclass device (fs:mount)
  ((arguments :initarg :arguments :reader arguments :initform nil)
   (rows      :initform nil :accessor rows-of))
  (:documentation "Something the machine may have, at /dev/<name>: every reading
any way of answering it declares, each a place, answered by the way this machine
can use."))

(defgeneric needs (device)
  (:documentation "The programs a way of answering has to find on the path.")
  (:method ((d device)) nil))

(defgeneric rows (device)
  (:documentation "Readings not known until the machine is asked: one row each of
a name, what reads it and what writes it.")
  (:method ((d device)) nil))

(defclass reading (fs:derived)
  ((row :initarg :row :reader row))
  (:documentation "One reading a row answers for."))

(defclass unanswered (fs:derived) ()
  (:documentation "A reading nothing on this machine can answer. It stands, so the
path resolves, and says :ABSENT rather than NIL."))

(defmethod fs:holding ((n unanswered)) :absent)
(defmethod fs:livep ((n unanswered) &optional name) (declare (ignore name)) t)

(defun %said (name) (string-downcase (princ-to-string name)))

(defun %symbol (name)
  "A device's name, as the class it names here: a config's AUDIO and pine's are
one class."
  (intern (string-upcase (princ-to-string name)) :pine/host))

(defun declared (name)
  "The class declared for the device called NAME, or nothing."
  (let* ((s (find-symbol (string-upcase (princ-to-string name)) :pine/host))
         (c (and s (find-class s nil))))
    (and c (subtypep c 'device) c)))

(defun devices ()
  "Every kind of device declared, by name."
  (sort (mapcar (lambda (c) (%said (class-name c)))
                (sb-mop:class-direct-subclasses (find-class 'device)))
        #'string<))

(defun %prototype (class)
  (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
  (sb-mop:class-prototype class))

(defmacro defdevice (name &key describes announces refreshes)
  "Declare a kind of device.

  (defdevice audio :describes \"the default sink\" :announces '(\"pactl subscribe\"))"
  (let ((class (%symbol name)) (d (gensym "D")))
    `(progn
       (defclass ,class (device) ()
         (:documentation ,(or describes "")))
       ,@(when announces `((defmethod fs:announces ((,d ,class)) ,announces)))
       ,@(when refreshes `((defmethod fs:refreshes ((,d ,class)) ,refreshes)))
       ',class)))

(defmacro %taking (d takes form)
  "FORM, with the device's own arguments bound to the names TAKES."
  (if takes
      `(let ,(loop :for name :in takes
                   :collect `(,name (getf (arguments ,d)
                                          ,(intern (symbol-name name) :keyword))))
         (declare (ignorable ,@takes))
         ,form)
      form))

(defmacro defbacking (name (&key needs announces refreshes takes rows) &body readings)
  "Declare one way of answering a device on one machine: a class under the device,
named for the first program it needs, or the device's own class where it needs
none. Each row is a word, a form that reads it, and a function that writes it:

  (defbacking audio (:needs \"wpctl\")
    (volume :reads  (level)
            :writes (lambda (said) (sh \"wpctl set-volume @X ~d%\" said))))

A row with no :WRITES only answers. A way that leaves out a reading another
declares does not take it away: it stands and says :ABSENT. TAKES names the
device's own arguments, and every form here is written under them. ROWS is a form
answering rows worked out when the device is made."
  (let* ((needs (if (listp needs) needs (list needs)))
         (kind (%symbol name))
         (class (if needs (%symbol (format nil "~a-~a" name (first needs))) kind))
         (d (gensym "D")) (n (gensym "NAME")) (v (gensym "VALUE")))
    `(progn
       ,@(when needs
           `((defclass ,class (,kind) ()
               (:documentation ,(format nil "~(~a~), answered with ~{~a~^ and ~}."
                                        name needs)))
             (defmethod needs ((,d ,class)) ',needs)))
       ,@(when announces
           `((defmethod fs:announces ((,d ,class)) (%taking ,d ,takes ,announces))))
       ,@(when refreshes `((defmethod fs:refreshes ((,d ,class)) ,refreshes)))
       ,@(when rows `((defmethod rows ((,d ,class)) (%taking ,d ,takes ,rows))))
       ,@(loop :for row :in readings
               :append (destructuring-bind (word &key reads writes) row
                         (let ((key (intern (string-upcase (symbol-name word)) :keyword)))
                           `((defmethod fs:read ((,d ,class) (,n (eql ,key)))
                               (declare (ignore ,n) (ignorable ,d))
                               (%taking ,d ,takes ,reads))
                             ,@(when writes
                                 `((defmethod fs:write ((,d ,class) (,n (eql ,key)) ,v)
                                     (declare (ignore ,n) (ignorable ,d))
                                     (funcall (%taking ,d ,takes ,writes) ,v))))))))
       ',class)))

(defun %kind (d)
  "The kind of device D is: the class under DEVICE it is an instance of."
  (find-if (lambda (c) (member (find-class 'device) (sb-mop:class-direct-superclasses c)))
           (sb-mop:class-precedence-list (class-of d))))

(defun %ways (kind)
  "Every way of answering KIND, in the order they were declared."
  (reverse (sb-mop:class-direct-subclasses kind)))

(defun answering (class)
  "The first way of answering this kind of device that this machine can, or
nothing where none can, in which case the kind's own readings answer."
  (and class
       (find-if (lambda (c) (every #'sh:has (needs (%prototype c)))) (%ways class))))

(defun words (d)
  "Every reading any way of answering this kind of device declares, so a path
resolves whether or not this machine is the one that can answer it."
  (append (remove-duplicates
           (loop :for c :in (cons (%kind d) (%ways (%kind d)))
                 :append (mapcar (lambda (key) (%said key))
                                 (fs:served (%prototype c))))
           :test #'equal :from-end t)
          (mapcar #'first (rows-of d))))

(defmethod initialize-instance :after ((d device) &key)
  (setf (rows-of d) (rows d)))

(defmethod fs:livep ((d device) &optional name)
  "The device answers from the world; its readings are worked out and kept until
it is told the world moved."
  (if name nil t))

(defmethod fs:read :around ((d device) name)
  "Every reading reads the device, so one told the world moved is read again."
  (declare (ignore name))
  (fs:reading d)
  (call-next-method))

(defmethod fs:works ((n reading))
  (fs:reading (fs:parent n))
  (funcall (second (row n))))

(defmethod fs:takes ((n reading) value)
  (let ((writes (third (row n))))
    (unless writes (error "~a only answers, and takes no writing." (fs:full-name n)))
    (funcall writes value)))

(defmethod fs:contents ((d device)) (words d))

(defmethod fs:entry ((d device) name)
  "Asked for exactly as it is spelled. A word written here is downcased once, as it
is read; one the machine named keeps the case the machine gave it."
  (let* ((word (princ-to-string name))
         (row (find word (rows-of d) :key #'first :test #'equal)))
    (cond (row (fs:child d word (lambda () (make-instance 'reading :name word :parent d :row row))))
          ((member word (words d) :test #'equal)
           (or (call-next-method)
               (fs:child d word (lambda () (make-instance 'unanswered :name word :parent d))))))))

(defmethod fs:entries ((d device))
  (remove nil (mapcar (lambda (word) (fs:entry d word)) (words d))))

(defun made (name &rest arguments)
  "The device NAME, standing, answering what the way this machine can use knows and
saying :ABSENT to every other reading it was declared to have. Nothing where no
such device was declared."
  (let ((kind (declared name)))
    (when kind
      (make-instance (class-name (or (answering kind) kind))
                     :name (%said (class-name kind))
                     :arguments arguments
                     :describes (documentation kind 'type)))))

(fs:mount (lambda () (make-instance 'fs:mount :describes "the machine, as devices"))
          "/dev")
