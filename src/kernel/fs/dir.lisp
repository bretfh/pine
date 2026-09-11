(defpackage #:pine/fs
  (:use #:cl)
  (:shadow #:directory #:read #:write)
  (:local-nicknames (#:d #:pine/data) (#:serial #:pine/serial))
  (:export
   #:node #:nodep #:mount #:value #:derived #:kind #:pendingp
   #:read #:write #:of #:key #:served #:names #:recompute #:on-write
   #:contents #:verb #:persistent-p #:volatile-p #:notified-by #:polls #:touch
   #:children #:child #:ensure-child #:create #:unlink #:works #:takes
   #:name #:parent #:owner #:describes #:full-name #:taking #:taken #:not-taken
   #:detach #:commit #:depend-on #:depend #:undepend #:as-value #:let-go
   #:root #:make-root #:at #:make #:erase #:walk #:paths #:split-name
   #:directory #:file #:truename-of #:node-for
   #:*owner* #:*declaring* #:absent #:not-a-place #:*root*
   #:*backing-store* #:store-get #:store-list #:store-any-p #:store-transaction
   #:store-delete #:*store-batch* #:store-flush
   #:writing #:on-commit #:on-forget #:forget-listeners
   #:*broke* #:*elsewhere* #:*slow-pool* #:*await-inline* #:*give-up-seconds* #:*retry-seconds*))
(in-package #:pine/fs)

(defvar *reading* nil)
(defvar *visiting* nil)
(defvar *visit* 0)
(defvar *revision* 0)
(defparameter +unread+ '#:unread)
(defvar *broke* nil)
(defvar *elsewhere* nil)
(defvar *slow-pool* nil)
(defvar *await-inline* nil)

(defstruct (under (:constructor %under (&optional order by-name)) (:copier nil))
  (order (d:no-seq))
  (by-name (d:no-map)))

(defclass node ()
  ((name      :initarg :name      :reader name      :initform nil)
   (parent    :initarg :parent    :accessor parent    :initform nil)
   (owner     :initarg :owner     :accessor owner     :initform nil)
   (of        :initarg :of        :reader of          :initform nil)
   (describes :initarg :describes :accessor describes :initform nil)
   (takes     :initarg :takes     :accessor taking    :initform nil)
   (dependents   :initform (d:no-set) :reader dependents)
   (depends-on       :initform nil        :accessor depends-on)
   (mtime   :initform 0          :accessor mtime)
   (visited-at     :initform 0          :accessor visited-at)
   (verified-at   :initform -1         :accessor verified-at)
   (named     :initform nil        :accessor named)))

(defgeneric persistent-p (x) (:method ((x node)) nil))

(defvar *owner* nil)

(defvar *declaring* nil)

(defvar *backing-store* nil)

(defgeneric store-get (keeper path)
  (:method (keeper path) (declare (ignore keeper path)) (values nil nil)))

(defgeneric (setf store-get) (value keeper path)
  (:method (value keeper path) (declare (ignore keeper path)) value))

(defgeneric store-list (keeper path)
  (:method (keeper path) (declare (ignore keeper path)) nil))

(defvar *store-batch* :now)

(defgeneric store-transaction (keeper thunk)
  (:method (keeper thunk) (declare (ignore keeper)) (funcall thunk)))

(defgeneric store-any-p (keeper path)
  (:method (keeper path) (declare (ignore keeper path)) nil))

(defgeneric store-delete (keeper path)
  (:method (keeper path) (declare (ignore keeper)) path))

(defgeneric volatile-p (x &optional name)
  (:method ((x node) &optional name) (declare (ignore name)) nil))
(defgeneric notified-by (x) (:method ((x node)) nil))
(defgeneric polls (x) (:method ((x node)) nil))
(defgeneric touch (x))

(defclass mount (node)
  ((under     :initform (%under)  :reader under)
   (dentries  :initform (d:no-map) :accessor dentries)
   (names     :initarg :names     :reader names-of   :initform nil)
   (each      :initarg :each      :reader each-of    :initform nil)
   (children   :initarg :entries   :reader children-of :initform nil)
   (notified-by :initarg :announces :reader notified-by  :initform nil)
   (polls :initarg :refreshes :reader polls  :initform nil)))

(defclass value (node)
  ((held :initarg :held :accessor held :initform nil)))

(defclass derived (node)
  ((recompute :initarg :recompute :accessor recompute :initform nil)
   (on-write  :initarg :on-write  :accessor on-write  :initform nil)
   (key       :initarg :key       :reader  key       :initform nil)
   (in        :initarg :in        :reader  in-of     :initform nil)
   (waits     :initarg :waits     :accessor waits-of :initform nil)
   (live      :initarg :live      :reader  live-of   :initform nil)
   (notified-by :initarg :announces :reader  notified-by :initform nil)
   (polls :initarg :refreshes :reader  polls :initform nil)
   (cached    :initform +unread+ :accessor cached)
   (last-good     :initform +unread+ :accessor last-good)
   (computing-thread     :initform nil :accessor computing-thread)
   (claimed-at   :initform nil :accessor claimed-at)
   (waiting   :initform nil :accessor waiting)
   (scheduled :initform nil :accessor scheduled)))

(defmethod persistent-p ((x value)) t)

(defmethod volatile-p ((n derived) &optional name)
  (declare (ignore name))
  (live-of n))

(defmethod volatile-p ((d mount) &optional name)
  (if name
      t
      (and (or (names-of d) (each-of d) (children-of d)) t)))

(defgeneric read (mount name)
  (:method ((d mount) name) (declare (ignore name)) nil))

(defgeneric write (mount name value)
  (:method ((d mount) name value)
    (declare (ignore value))
    (error "~a answers ~(~a~), and takes no writing there." (full-name d) name)))

(defvar *served* (make-hash-table :test 'eq :synchronized t))

(defgeneric names (d)
  (:method ((d node)) nil))

(defun %served (d)
  (let ((class (class-of d)))
    (or (gethash class *served*)
        (setf (gethash class *served*)
              (loop :for (key . nil) :in (names d)
                    :collect (cons key (string-downcase (symbol-name key))))))))

(defun served (d)
  (mapcar #'car (%served d)))

(defun %key-of (d name)
  (car (find name (%served d) :key #'cdr :test #'string=)))

(defun %doc (d key)
  (cdr (assoc key (names d))))

(defun nodep (x) (typep x 'node))

(defmethod initialize-instance :after ((x node) &key)
  (let ((said (slot-value x 'name)))
    (unless (or (null said) (stringp said))
      (setf (slot-value x 'name) (princ-to-string said)))))

(defmethod print-object ((x node) stream)
  (print-unreadable-object (x stream :type t)
    (write-string (full-name x) stream)))

(defun beneath (d) (under-order (under d)))
(defun by-name (d) (under-by-name (under d)))

(defun %named (x)
  (let ((names (loop :for at := x :then (parent at)
                     :while at
                     :when (name at) :collect (name at))))
    (if names (format nil "/~{~a~^/~}" (reverse names)) "/")))

(defun full-name (x)
  (or (named x) (setf (named x) (%named x))))

(defun %renamed (x)
  (setf (named x) nil)
  (when (typep x 'mount)
    (d:do-each (each (beneath x)) (%renamed each))
    (dolist (each (d:vals (dentries x))) (%renamed each)))
  x)

(defmethod (setf parent) :after (value (x node))
  (declare (ignore value))
  (%renamed x))

(declaim (inline %said))
(defun %said (name)
  (if (stringp name) name (princ-to-string name)))

(defun ensure-child (d name builder)
  (let ((name (%said name)))
    (or (d:lookup (dentries d) name)
        (let ((made (funcall builder)))
          (when made
            (d:lookup (sb-ext:atomic-update (slot-value d 'dentries)
                              (lambda (m)
                                (if (nth-value 1 (d:lookup m name))
                                    m
                                    (d:with m name made))))
                      name))))))

(defun %kid (d name)
  (ensure-child d name
         (lambda ()
           (let ((it (funcall (each-of d) name)))
             (when it (setf (parent it) d))
             it))))

(defun %listed (d) (mapcar #'princ-to-string (funcall (names-of d))))

(defun %answered (d key)
  (let ((name (string-downcase (symbol-name key))))
    (ensure-child d name
           (lambda ()
             (make-instance 'derived :name name :key key :of d :parent d
                                     :live (volatile-p d name)
                                     :describes (%doc d key))))))

(defun %answering (d)
  (loop :for (key . name) :in (%served d)
        :unless (d:lookup (by-name d) name)
          :collect (%answered d key)))

(defun %under-name (d name)
  (let ((at (full-name d)))
    (if (string= at "/") (format nil "/~a" name) (format nil "~a/~a" at name))))

(defun %behind (d)
  (and *backing-store*
       (not (volatile-p d))
       (not (children-of d)) (not (names-of d)) (not (each-of d))
       *backing-store*))

(defun %kept-kid (d name)
  (let ((behind (%behind d)))
    (when behind
      (ensure-child d name
             (lambda ()
               (let ((path (%under-name d name)))
                 (multiple-value-bind (value foundp) (store-get behind path)
                   (cond (foundp
                          (make-instance 'value :name (%said name) :parent d
                                                :held value))
                         ((store-any-p behind path)
                          (make-instance 'mount :name (%said name)
                                               :parent d))))))))))

(defgeneric children (x)
  (:method ((x node)) nil)
  (:method ((d mount))
    (cond ((children-of d) (funcall (children-of d)))
          ((names-of d) (remove nil (mapcar (lambda (each) (%kid d each)) (%listed d))))
          (t (let ((had (append (d:as :list (beneath d)) (%answering d)))
                   (behind (%behind d)))
               (if behind
                   (append had
                           (loop :for name :in (store-list behind (full-name d))
                                 :unless (find name had :key #'name :test #'equal)
                                   :append (let ((it (%kept-kid d name)))
                                             (when it (list it)))))
                   had))))))

(defgeneric child (x name)
  (:method ((x node) name) (declare (ignore name)) nil)
  (:method ((d mount) name)
    (let ((name (%said name)))
      (cond ((children-of d)
             (find name (funcall (children-of d)) :key #'name :test #'equal))
            ((each-of d) (%kid d name))
            (t (or (d:lookup (by-name d) name)
                   (let ((key (%key-of d name)))
                     (and key (%answered d key)))
                   (%kept-kid d name)))))))

(defgeneric depend (x on)
  (:method (x (on node))
    (sb-ext:atomic-update (slot-value on 'dependents) (lambda (all) (d:with all x)))
    x))

(defgeneric undepend (x on)
  (:method (x (on node))
    (sb-ext:atomic-update (slot-value on 'dependents) (lambda (all) (d:without all x)))
    x))

(defun %unlisted (d x)
  (sb-ext:atomic-update (slot-value d 'under)
          (lambda (all)
            (%under (fset:remove x (under-order all))
                    (d:without (under-by-name all) (%said (name x))))))
  d)

(defun %plainp (x)
  (or (eq (class-of x) (find-class 'mount))
      (and (eq (class-of x) (find-class 'value)) (null (held x)))))

(defgeneric mount (what where))

(defmethod mount ((x node) (into mount))
  (let* ((said (%said (name x)))
         (had (d:lookup (by-name into) said))
         (was (parent x)))
    (when (or (each-of into) (children-of into))
      (error "~a works out what is under it; ~a is not a place to mount at."
             (full-name into) said))
    (flet ((put ()
             (when had
               (detach into said)
               (when (eq had (d:lookup (dentries into) said))
                 (sb-ext:atomic-update (slot-value into 'dentries) (lambda (old) (d:without old said)))))
             (when (and (typep was 'mount) (not (eq was into)))
               (%unlisted was x)
               (touch was))
             (when *owner* (setf (owner x) *owner*))
             (setf (parent x) into)
             (sb-ext:atomic-update (slot-value into 'under)
                     (lambda (all)
                       (%under (d:with (d:as :seq (cl:remove said
                                                             (d:as :list (under-order all))
                                                             :key #'name :test #'equal))
                                       x)
                               (d:with (under-by-name all) said x))))
             (touch into)
             x))
      (cond ((eq had x) x)
            ((%plainp x)
             (let ((stands (or had (child into said))))
               (cond (stands
                      (when (describes x) (setf (describes stands) (describes x)))
                      stands)
                     (t (put)))))
            (t (put))))))

(defgeneric detach (d name)
  (:method ((d mount) name)
    (let ((gone (child d name)))
      (when gone
        (%unlisted d gone)
        (dolist (on (depends-on gone)) (undepend gone on))
        (setf (depends-on gone) nil)
        (setf (parent gone) nil)
        (touch d))
      gone)))

(defgeneric create (d name kind)
  (:method ((x node) name kind)
    (declare (ignore kind))
    (error "~a holds a ~(~a~); nothing goes under it, so there is no ~a there."
           (full-name x) (kind x) name))
  (:method ((d mount) name kind)
    (when (or (each-of d) (children-of d))
      (error "~a works out what is under it; ~a is not a place to make."
             (full-name d) name))
    (mount (make-instance (ecase kind (:mount 'mount) (:value 'value))
                          :name (%said name))
           d)))

(defgeneric unlink (d name)
  (:method ((d mount) name)
    (let ((gone (child d name)))
      (when gone (%went (full-name gone)))
      (let ((it (detach d name)))
        (sb-ext:atomic-update (slot-value d 'dentries) (lambda (old) (d:without old (%said name))))
        (or it gone)))))

(defgeneric let-go (x owner)
  (:method ((x node) owner) (declare (ignore owner)) nil))

(defgeneric verb (x name arguments)
  (:method ((x node) name arguments)
    (let ((had (contents x)))
      (setf (contents x)
            (case name
              (:set    (first arguments))
              (:toggle (not had))
              (:conj   (d:with (or had (d:no-set)) (first arguments)))
              (:disj   (d:without (or had (d:no-set)) (first arguments)))
              (:merge  (d:merged (or had (d:no-map)) (first arguments)))
              (t       (first arguments)))))))

(defun %verbp (v)
  (and (d:seqp v) (plusp (d:size v)) (keywordp (d:lookup v 0))
       (not (eq :quoted (d:lookup v 0)))))

(defun as-value (v)
  (if (%verbp v) (fset:concat (d:seq :quoted) v) v))

(defun %quotedp (v)
  (and (d:seqp v) (plusp (d:size v)) (eq :quoted (d:lookup v 0))))

(defgeneric works (n)
  (:method ((n derived))
    (cond ((key n) (read (of n) (key n)))
          ((recompute n) (funcall (recompute n))))))

(defgeneric takes (n value)
  (:method ((n derived) value)
    (cond ((key n) (write (of n) (key n) value))
          ((on-write n) (funcall (on-write n) value))
          (t (error "~a is worked out, and takes no writing." (full-name n))))))

(defgeneric contents (x)
  (:method ((d mount))
    (if (names-of d) (%listed d) (mapcar #'name (children d))))
  (:method ((x value)) (held x)))

(defgeneric kind (x)
  (:method ((d mount)) :dir)
  (:method ((x value)) :file)
  (:method ((n derived)) :dev))

(defgeneric (setf contents) (value x)
  (:method (v (d mount))
    (declare (ignore v))
    (error "~a is a mount; what is written is what is under it." (full-name d)))
  (:method (v (x value)) (setf (held x) v)))

(defmethod (setf contents) :after (v (x value))
  (declare (ignore v))
  (when (and *backing-store* (not *declaring*) (persistent-p x))
    (if (eq *store-batch* :now)
        (%put-down x)
        (push x (cdr *store-batch*)))))

(defun %put-down (x)
  (let ((it (contents x)))
    (when (serial:encodablep it)
      (setf (store-get *backing-store* (full-name x)) it))))

(defun store-flush (nodes)
  (when (and nodes *backing-store*)
    (store-transaction *backing-store*
                   (lambda ()
                     (dolist (each (remove-duplicates nodes))
                       (%put-down each))))))

(defun taken (takes value)
  (cond ((null takes) t)
        ((eq takes :flag) (or (null value) (eq value t)))
        ((eq takes :text) (stringp value))
        ((eq takes :number) (realp value))
        ((atom takes) t)
        ((eq (first takes) :number)
         (and (realp value)
              (or (null (second takes)) (<= (second takes) value))
              (or (null (third takes)) (<= value (third takes)))))
        ((eq (first takes) :one-of)
         (and (member value (rest takes) :test #'equal) t))
        (t t)))

(define-condition not-taken (error)
  ((where :initarg :where :reader where)
   (takes :initarg :takes :reader takes-of)
   (said  :initarg :said  :reader said-of))
  (:report
   (lambda (c s)
     (flet ((spelled (takes)
              (cond ((eq takes :flag) "yes or no")
                    ((eq takes :text) "words")
                    ((eq takes :number) "a number")
                    ((and (consp takes) (eq (first takes) :number))
                     (format nil "a number~@[ from ~a~]~@[ to ~a~]"
                             (second takes) (third takes)))
                    ((and (consp takes) (eq (first takes) :one-of))
                     (format nil "one of ~{~s~^, ~}" (rest takes)))
                    (t (princ-to-string takes)))))
       (format s "~a takes ~a, and ~s is not one." (where c) (spelled (takes-of c))
               (said-of c))))))

(defmethod (setf contents) :around (v (x node))
  (cond ((%verbp v) (verb x (d:lookup v 0) (d:as :list (fset:subseq v 1))))
        ((%quotedp v) (call-next-method (fset:subseq v 1) x))
        (t (let ((takes (taking x)))
             (unless (taken takes v)
               (error 'not-taken :where (full-name x) :takes takes :said v)))
           (call-next-method))))
