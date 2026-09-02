(defpackage #:pine/host/declared
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:sh #:pine/host/shell) (#:system #:pine/run/system))
  (:export
   #:defdevice #:defbacking #:made #:device #:unanswered #:answering #:named
   #:needs-of #:backings-of)
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

(defclass device ()
  ((title     :initarg :title     :reader title-of)
   (describes :initarg :describes :reader describes-of :initform nil)
   (announces :initarg :announces :reader announces-of :initform nil)
   (refreshes :initarg :refreshes :reader refreshes-of :initform nil)
   (backings  :initarg :backings  :accessor backings-of :initform nil))
  (:documentation "Something the machine may have, and the ways of asking it."))

(defclass backing ()
  ((needs :initarg :needs :reader needs-of :initform nil)
   (makes :initarg :makes :reader makes-of))
  (:documentation "One way of answering a device on one machine. NEEDS is what has to
be on the path for this way to work.

MAKES is a function of the device's own arguments answering three things: its rows,
what says the world behind it moved, and how often to ask again where nothing does.

A function and not three slots, because all three can depend on what the device was
made with. /dev/media follows one player and the line that says it moved names that
player, so a stream said once when the backing was declared would be following
whichever player nobody asked for. It is also what lets a backing whose readings are
not known until you ask -- every variable in the environment -- say so."))

(defclass unanswered (node:place) ()
  (:documentation "A reading nothing on this machine can answer.

It stands, so the path resolves and a surface reading it is not a surface that
breaks. It holds nothing, and says :ABSENT rather than NIL, so a bar can show a dash
where there is no battery instead of a battery at zero."))

(defmethod node:holding ((n unanswered)) :absent)

(defun %said (name) (string-downcase (princ-to-string name)))

(defun declare-device (title &key describes announces refreshes)
  (let ((had (d:lookup (d:all *declared*) (%said title))))
    (or had
        (let ((it (make-instance 'device :title (%said title)
                                           :describes describes
                                           :announces announces
                                           :refreshes refreshes)))
          (d:keep! *declared* (%said title) it)
          (system:owned (list :device (%said title)))
          it))))

(defun forget-device (title)
  "Take a declaration back off. A device a system declared goes when the system does;
the ones pine ships are declared as their file loads, when nothing owns anything."
  (d:drop! *declared* (%said title))
  title)

(system:undoes :device #'forget-device)

(defun declare-backing (title needs makes)
  "Add a way of answering the device TITLE. Declared later is tried later, so the
first one written is the one preferred."
  (let ((it (declare-device title)))
    (setf (backings-of it)
          (append (backings-of it)
                  (list (make-instance 'backing :needs needs :makes makes))))
    it))

(defmacro defdevice (name &body options)
  "Declare a device. OPTIONS is a plist: :describes, :announces, :refreshes.

  (defdevice audio :describes \"the default sink\" :announces '(\"pactl subscribe\"))"
  `(declare-device ',name ,@options))

(defmacro defbacking (name (&key needs announces refreshes takes rows) &body readings)
  "Declare one way of answering a device on one machine.

NEEDS is the programs that have to be on the path. Each row is a name, a form that
reads it, and a function that writes it:

  (defbacking audio (:needs \"wpctl\")
    (volume :reads  (level)
            :writes (lambda (said)
                      (sh:sh \"wpctl set-volume @X ~d%\" said))))

:WRITES is a function and not a form with the value bound behind your back. A row
written here is read in the package the row was written in, and a name this macro
binds is a symbol in the package the macro was written in -- two symbols spelled the
same, so the binding and the use are not the same variable. It is also what
NODE:DERIVE takes, so there is one answer to what writing a place means.

A row with no :WRITES is one that only answers. A backing that leaves out a reading
the device declared does not take it away: it stands and says :ABSENT.

TAKES names the device's own arguments, and every form here is written under them:

  (defbacking media (:needs \"playerctl\" :takes (player)
                     :announces (list (format nil \"playerctl -p ~a --follow status\"
                                              player)))
    (title :reads (%meta player \"xesam:title\")))

ROWS is a form answering rows worked out when the device is made, for a backing whose
readings are not known until you ask -- every variable in the environment is one. They
come after the ones written here."
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

(defun named (title)
  "The device declared under TITLE, or nothing."
  (d:lookup (d:all *declared*) (%said title)))

(defun answering (it)
  "The first backing this machine can answer with, or nothing."
  (find-if (lambda (b) (every #'sh:has (needs-of b))) (backings-of it)))

(defun %words (it arguments)
  "Every reading this device has under any backing, so a path resolves whether or
not this machine is the one that can answer it."
  (remove-duplicates (loop :for b :in (backings-of it)
                           :append (mapcar #'first
                                           (apply (makes-of b) arguments)))
                     :test #'equal :from-end t))

(defun %reading (n row)
  (destructuring-bind (word reads &optional writes) row
    (make-instance 'node:derived :name word
                 :reads (lambda () (node:reading n) (funcall reads))
                 :parent n :writes writes)))

(defun %made (it b arguments)
  "The device, standing, answering what backing B knows and saying :ABSENT to every
other reading it was declared to have.

Every declared reading stands whether or not the backing that won answers it. A
backing that knows the connection but cannot list what is in the air leaves
/dev/net/wifi a place that says :ABSENT, rather than a path that does not resolve --
so a surface reading it is the same surface on either machine."
  (multiple-value-bind (rows announces refreshes)
      (when b (apply (makes-of b) arguments))
    (let ((self (list nil))
          (words (%words it arguments)))
      (setf (first self)
            (make-instance 'node:place :name (title-of it)
                        :announces (or announces (announces-of it))
                        :refreshes (or refreshes (refreshes-of it))
                        :describes (describes-of it)
                        :reads (lambda () words)
                        :names (lambda () words)
                        :each (lambda (want)
                                "Asked for exactly as it is spelled. A row written
here is downcased once, as it is read; one the machine named keeps the case the
machine gave it, and PATH is not path."
                                (let* ((word (princ-to-string want))
                                       (row (find word rows
                                                  :key #'first :test #'equal)))
                                  (cond (row (%reading (first self) row))
                                        ((member word words :test #'equal)
                                         (make-instance 'unanswered
                                                        :name word))))))))))

(defun made (name &rest arguments)
  "The node for a declared device, bound to whatever this machine has, and made with
ARGUMENTS. Nothing where no such device was declared."
  (let ((it (named name)))
    (when it (%made it (answering it) arguments))))
