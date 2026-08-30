(defpackage #:pine/host/declared
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:sh #:pine/host/shell))
  (:export
   #:defdevice #:defbacking #:made #:declared #:absent #:standing)
  (:documentation "Declaring a device, and binding it to whatever the host has.

A device is a name and a set of readings; a backing is one way of answering them on
one machine. Which backing answers is asked of the machine, once, when the device is
made: the first whose programs are all there wins.

That is the whole of the difference from a device written as a function. A function
names one program and reads nothing where that program is absent, so a machine with
iwd and no nmcli has a /dev/net that answers NIL to everything and cannot say why. A
declaration names as many backings as somebody has written, and where none of them
can answer it says :ABSENT -- which is the word pine already had for nothing standing
somewhere, and is not the word for a volume of zero."))
(in-package #:pine/host/declared)

(defvar *declared* (d:table)
  "Every device declared, by name. A device is a declaration and not a function, so
a config can add one and a system can bring its own.")

(defclass declared ()
  ((title     :initarg :title     :reader title-of)
   (describes :initarg :describes :reader describes-of :initform nil)
   (announces :initarg :announces :reader announces-of :initform nil)
   (refreshes :initarg :refreshes :reader refreshes-of :initform nil)
   (backings  :initarg :backings  :accessor backings-of :initform nil))
  (:documentation "Something the machine may have, and the ways of asking it."))

(defclass backing ()
  ((needs    :initarg :needs    :reader needs-of    :initform nil)
   (readings :initarg :readings :reader readings-of :initform nil))
  (:documentation "One way of answering a device on one machine. NEEDS is what has to
be on the path for this way to work."))

(defclass absent (node:place) ()
  (:documentation "A reading nothing on this machine can answer.

It stands, so the path resolves and a surface reading it is not a surface that
breaks. It holds nothing, and says :ABSENT rather than NIL, so a bar can show a dash
where there is no battery instead of a battery at zero."))

(defmethod node:holding ((n absent)) :absent)

(defun %said (name) (string-downcase (princ-to-string name)))

(defun declare-device (title &key describes announces refreshes)
  (let ((had (d:lookup (d:all *declared*) (%said title))))
    (or had
        (let ((it (make-instance 'declared :title (%said title)
                                           :describes describes
                                           :announces announces
                                           :refreshes refreshes)))
          (d:keep! *declared* (%said title) it)
          it))))

(defun declare-backing (title needs readings)
  "Add a way of answering the device TITLE. Declared later is tried later, so the
first one written is the one preferred."
  (let ((it (declare-device title)))
    (setf (backings-of it)
          (append (backings-of it)
                  (list (make-instance 'backing :needs needs :readings readings))))
    it))

(defmacro defdevice (name &body options)
  "Declare a device. OPTIONS is a plist: :describes, :announces, :refreshes.

  (defdevice audio :describes \"the default sink\" :announces '(\"pactl subscribe\"))"
  `(declare-device ',name ,@options))

(defmacro defbacking (name (&key needs) &body rows)
  "Declare one way of answering a device on one machine.

NEEDS is the programs that have to be on the path. Each row is a name, how to read
it, and what writing it means, with IT bound to the value being written:

  (defbacking audio (:needs \"wpctl\")
    (volume :reads (level) :writes (sh:sh \"wpctl set-volume @X ~d%\" it)))

A row with no :WRITES is one that only answers."
  `(declare-backing
    ',name (list ,@(if (listp needs) needs (list needs)))
    (list ,@(loop :for row :in rows
                  :collect (destructuring-bind (word &key reads writes) row
                             `(list ,(%said word)
                                    (lambda () ,reads)
                                    ,(when writes
                                       `(lambda (it)
                                          (declare (ignorable it))
                                          ,writes))))))))

(defun standing (it)
  "The first backing this machine can answer with, or nothing."
  (find-if (lambda (b) (every #'sh:has (needs-of b))) (backings-of it)))

(defun %words (it)
  "Every reading this device has under any backing, so a path resolves whether or
not this machine is the one that can answer it."
  (remove-duplicates (loop :for b :in (backings-of it)
                           :append (mapcar #'first (readings-of b)))
                     :test #'equal :from-end t))

(defun %reading (n row)
  (destructuring-bind (word reads &optional writes) row
    (node:derive word
                 (lambda () (node:reading n) (funcall reads))
                 :parent n :writes writes)))

(defun %answering (it rows)
  (let ((self (list nil)))
    (setf (first self)
          (node:lists (title-of it)
                      :announces (announces-of it)
                      :refreshes (refreshes-of it)
                      :describes (describes-of it)
                      :reads (lambda () (mapcar #'first rows))
                      :names (lambda () (mapcar #'first rows))
                      :each (lambda (want)
                              (let ((row (find (%said want) rows
                                               :key #'first :test #'equal)))
                                (when row (%reading (first self) row))))))))

(defun %nothing (it)
  "The device, standing, with nothing behind it. Every reading it could have is a
place that says :ABSENT."
  (let ((words (%words it)))
    (node:lists (title-of it)
                :describes (describes-of it)
                :names (lambda () words)
                :each (lambda (want)
                        (when (member (%said want) words :test #'equal)
                          (node:answers (%said want) :class 'absent))))))

(defun made (name)
  "The node for a declared device, bound to whatever this machine has. Nothing where
no such device was declared -- which is what lets the caller fall back."
  (let ((it (d:lookup (d:all *declared*) (%said name))))
    (when it
      (let ((b (standing it)))
        (if b (%answering it (readings-of b)) (%nothing it))))))
