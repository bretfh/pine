(defpackage #:pine/host/devices
  (:use)
  (:documentation "Where a device's name lives: one class per kind of device, named
here whoever declared it, so a config's AUDIO and pine's are one class."))

(defpackage #:pine/host
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell))
  (:import-from #:pine/host/shell #:sh)
  (:export
   #:device #:defdevice #:defbacking #:sh
   #:made #:answering #:devices #:unanswered)
  (:documentation "The machine, in the namespace: its devices, its filesystem, its
environment and what it will run.

A device is a class: DEFDEVICE declares one, and each DEFBACKING is one way of
answering it on one machine -- the programs it needs, and a reading for each of the
device's words. Which backing answers is asked of the machine when the device is
made, and where none can, every reading stands and says :ABSENT."))
(in-package #:pine/host)

(defclass device (fs:dir)
  ((rows  :initarg :rows  :reader rows  :initform nil)
   (words :initarg :words :reader words :initform nil))
  (:documentation "Something the machine may have, at /dev/<name>: every reading any
of its backings declares, each a place, answered by the backing this machine can
use."))

(defclass backing ()
  ((needs :initarg :needs :reader needs-of :initform nil)
   (makes :initarg :makes :reader makes-of))
  (:documentation "One way of answering a device on one machine. NEEDS is what has to
be on the path. MAKES is a function of the device's own arguments answering its
rows, what says the world behind it moved, and how often to ask again."))

(defclass unanswered (fs:derived) ()
  (:documentation "A reading nothing on this machine can answer. It stands, so the
path resolves, and says :ABSENT rather than NIL."))

(defmethod fs:holding ((n unanswered)) :absent)
(defmethod fs:livep ((n unanswered)) t)

(defun %said (name) (string-downcase (princ-to-string name)))

(defun %symbol (name)
  (intern (string-upcase (princ-to-string name)) :pine/host/devices))

(defun declared (name)
  "The class declared for the device called NAME, or nothing."
  (let ((s (find-symbol (string-upcase (princ-to-string name)) :pine/host/devices)))
    (and s (find-class s nil))))

(defun devices ()
  "Every kind of device declared, by name."
  (labels ((under (c) (cons c (mapcan #'under (sb-mop:class-direct-subclasses c)))))
    (sort (mapcar (lambda (c) (%said (class-name c)))
                  (remove (find-class 'device) (under (find-class 'device))))
          #'string<)))

(defun %prototype (class)
  (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
  (sb-mop:class-prototype class))

(defun %option (class key) (getf (slot-value (%prototype class) 'declared) key))

(defun backings-of (class) (slot-value (%prototype class) 'backings))

(defmacro defdevice (name &body options)
  "Declare a kind of device. OPTIONS is a plist: :describes, :announces, :refreshes.

  (defdevice audio :describes \"the default sink\" :announces '(\"pactl subscribe\"))"
  (let ((class (%symbol name)))
    `(let ((class (defclass ,class (device)
                    ((declared :allocation :class :initform nil)
                     (backings :allocation :class :initform nil)))))
       (setf (slot-value (%prototype class) 'declared) (list ,@options))
       class)))

(defun declare-backing (name needs makes)
  "Add a way of answering the device NAME. One with the same needs replaces the one
before it, so a config read again does not answer twice."
  (let ((class (declared name)))
    (unless class (error "~a is not a device anybody declared." name))
    (setf (slot-value (%prototype class) 'backings)
          (append (remove needs (backings-of class) :key #'needs-of :test #'equal)
                  (list (make-instance 'backing :needs needs :makes makes))))
    class))

(defmacro defbacking (name (&key needs announces refreshes takes rows) &body readings)
  "Declare one way of answering a device on one machine. NEEDS is the programs that
have to be on the path. Each row is a word, a form that reads it, and a function
that writes it:

  (defbacking audio (:needs \"wpctl\")
    (volume :reads  (level)
            :writes (lambda (said) (sh \"wpctl set-volume @X ~d%\" said))))

A row with no :WRITES only answers. A backing that leaves out a reading another
declares does not take it away: it stands and says :ABSENT. TAKES names the
device's own arguments, and every form here is written under them. ROWS is a form
answering rows worked out when the device is made."
  `(declare-backing
    ',name (list ,@(if (listp needs) needs (list needs)))
    (lambda (&key ,@takes &allow-other-keys)
      (declare (ignorable ,@takes))
      (values (append
               (list ,@(loop :for row :in readings
                             :collect (destructuring-bind (word &key reads writes) row
                                        `(list ,(%said word) (lambda () ,reads)
                                               ,writes))))
               ,rows)
              ,announces ,refreshes))))

(defun answering (class)
  "The first backing this machine can answer with, or nothing."
  (and class
       (find-if (lambda (b) (every #'sh:has (needs-of b))) (backings-of class))))

(defun %words (class arguments)
  "Every reading this device has under any backing, so a path resolves whether or
not this machine is the one that can answer it."
  (remove-duplicates (loop :for b :in (backings-of class)
                           :append (mapcar #'first
                                           (apply (makes-of b) arguments)))
                     :test #'equal :from-end t))

(defun %reading (n row)
  (destructuring-bind (word reads &optional writes) row
    (make-instance 'fs:derived :name word
                               :reads (lambda () (fs:reading n) (funcall reads))
                               :parent n :writes writes)))

(defmethod fs:livep ((d device)) t)

(defmethod fs:contents ((d device)) (words d))

(defmethod fs:entry ((d device) name)
  "Asked for exactly as it is spelled. A word written here is downcased once, as it
is read; one the machine named keeps the case the machine gave it."
  (let ((word (princ-to-string name)))
    (fs:child d word
              (lambda ()
                (let ((row (find word (rows d) :key #'first :test #'equal)))
                  (cond (row (%reading d row))
                        ((member word (words d) :test #'equal)
                         (make-instance 'unanswered :name word :parent d))))))))

(defmethod fs:entries ((d device))
  (remove nil (mapcar (lambda (word) (fs:entry d word)) (words d))))

(defun made (name &rest arguments)
  "The device NAME, standing, answering what the backing this machine can use knows
and saying :ABSENT to every other reading it was declared to have. Nothing where
no such device was declared."
  (let ((class (declared name)))
    (when class
      (let ((b (answering class)))
        (multiple-value-bind (rows announces refreshes)
            (when b (apply (makes-of b) arguments))
          (make-instance (class-name class)
                         :name (%said (class-name class))
                         :rows rows
                         :words (%words class arguments)
                         :announces (or announces (%option class :announces))
                         :refreshes (or refreshes (%option class :refreshes))
                         :describes (%option class :describes)))))))
