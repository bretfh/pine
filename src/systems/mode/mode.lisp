(defpackage #:pine/mode
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:fs #:pine/fs) (#:command #:pine/run/command)
                    (#:fault #:pine/run/fault) (#:system #:pine/run/system))
  (:export
   #:mode #:text #:prose #:code #:lisp
   #:pine #:scheme #:org #:press #:typing
   #:indent #:complete #:saving #:regions #:setting #:says
   #:covering #:name-of #:from-of #:to-of #:inside-of
   #:handles #:mode-for #:bind #:unbind #:binding #:bindings
   #:dispatch #:modes #:mode-node))
(in-package #:pine/mode)

(defclass mode () ()
  (:documentation "How a document is understood. The chain is class inheritance:
CALL-NEXT-METHOD is the fallback, and precedence costs nothing.

This is what replaces a parent named by a string, a handler table keyed by verbs,
and seven generics reimplementing method combination."))

(defclass fundamental (mode) ())
(defclass text (mode) ())
(defclass prose (text) ())
(defclass org (prose) ())
(defclass code (text) ())
(defclass lisp (code) ())
(defclass pine (lisp) ())
(defclass scheme (code) ())

(defgeneric press (mode document key)
  (:documentation "What a key means here. Nothing by default, so the keymap has it.")
  (:method ((m mode) d k) (declare (ignore d k)) nil))

(defgeneric typing (mode document string)
  (:documentation "What typing means here, beside PRESS, which says what a key
means. Not INSERT: putting text in a document is the document's, and a mode says
what typing is before anything is put anywhere.")
  (:method ((m mode) d s) (declare (ignore d s)) nil))

(defgeneric indent (mode document line)
  (:documentation "What column LINE should start at, or nothing to leave it.")
  (:method ((m mode) d line) (declare (ignore d line)) nil))

(defgeneric complete (mode document prefix)
  (:documentation "What PREFIX could be finished as.")
  (:method ((m mode) d prefix) (declare (ignore d prefix)) nil))

(defgeneric saving (mode document)
  (:documentation "What saving means here, beside PRESS and TYPING. Not SAVE:
writing a document back where it came from is the document's, and a mode says what
saving is before anything is written.")
  (:method ((m mode) d) (declare (ignore d)) nil))

(defclass covering ()
  ((name   :initarg :name   :reader name-of)
   (from   :initarg :from   :reader from-of)
   (to     :initarg :to     :reader to-of)
   (inside :initarg :inside :reader inside-of :initform nil))
  (:documentation "One stretch a mode says its text divides into: what to call it,
where it starts and ends as (LINE . COLUMN), and the coverings inside it.

Not SPAN: PINE/TEXT:SPAN is (LINE FROM TO FACE), the few numbers a run of cells is
painted with. This is a structural claim about the text and not a colour, and one
word cannot be both.

Said and not kept. A COVERING is what a mode answers; the node standing for it is
RESTRUCTURE's, and that node outlives an edit so a watcher on one goes on watching.
Answering nodes instead would make a mode mint a new one on every keystroke and
every watcher would be watching something nothing else can reach.

A class because it was (NAME START END . CHILDREN), and a mode that put its three
in another order made regions covering text they were never standing for -- with
nothing to catch it, because every one of those shapes is a list."))

(defun covering (name from to &optional inside)
  "One. FROM and TO are (LINE . COLUMN) in the document's own lines."
  (make-instance 'covering :name (princ-to-string name) :from from :to to
                           :inside inside))

(defgeneric regions (mode document)
  (:documentation "What this text divides into, as a tree of COVERING.

What comes back is put in the namespace under the document, so a form or a heading
is a place anything can read, write and watch -- inside this image and outside it.")
  (:method ((m mode) d) (declare (ignore d)) nil))

(defgeneric setting (of key)
  (:documentation "What OF says about KEY. A mode answers for every document it is
for; a document answers for itself first and asks its mode after. One question,
because that is what it is.

CALL-NEXT-METHOD is the fallback, so a mode that says nothing gets what its parent
says. Saying nothing is :DEFAULT and not NIL, because NIL is an answer a setting
can have: with the two spelled the same, turning one off wrote NIL and reading it
back said nobody had ever set it.")
  (:method ((m mode) key) (declare (ignore key)) :default))

(defun says (of key else)
  "What OF says about KEY, or ELSE where it says nothing."
  (let ((said (setting of key)))
    (if (eq said :default) else said)))

(defgeneric (setf setting) (value of key)
  (:documentation "Say what OF holds for KEY."))

(defgeneric handles (mode)
  (:documentation "The globs of paths and names this mode is for.")
  (:method ((m mode)) nil))

(defmethod fs:name ((m mode))
  (string-downcase (symbol-name (class-name (class-of m)))))

(defmethod setting ((m text) key)
  (case key (:tab-width 8) (t (call-next-method))))

(defmethod setting ((m code) key)
  (case key (:indent 2) (:comment ";") (t (call-next-method))))

(defmethod setting ((m lisp) key)
  (case key (:grammar :commonlisp) (t (call-next-method))))

(defmethod setting ((m pine) key)
  (case key (:grammar :pine) (t (call-next-method))))

(defmethod setting ((m scheme) key)
  (case key (:grammar :scheme) (t (call-next-method))))

(defmethod setting ((m org) key)
  (case key (:comment "#") (t (call-next-method))))

(defmethod handles ((m lisp)) '("*.lisp" "*.asd" "*.cl"))
(defmethod handles ((m scheme)) '("*.scm" "*.ss"))
(defmethod handles ((m org)) '("*.org"))

(defun glob (pattern text)
  (labels ((walk (p n)
             (cond ((and (null p) (null n)) t)
                   ((null p) nil)
                   ((char= (first p) #\*)
                    (or (walk (rest p) n) (and n (walk p (rest n)))))
                   ((null n) nil)
                   ((char-equal (first p) (first n)) (walk (rest p) (rest n)))
                   (t nil))))
    (walk (coerce pattern 'list) (coerce text 'list))))

(defun modes ()
  "Every mode class there is, most particular first. A class somebody defined and
has not made an instance of yet is finalized here: it is a mode whether or not
anything has asked for one.

A class two modes both lead to is here once, and two of one depth are ordered by
name, so which mode claims a path is the answer twice running and the same answer
in another image. The order the classes were defined in is not one pine is given."
  (labels ((under (class)
             (c2mop:ensure-finalized class)
             (cons class (mapcan #'under (c2mop:class-direct-subclasses class))))
           (depth (c) (length (c2mop:class-precedence-list c))))
    (sort (remove-duplicates (remove (find-class 'mode) (under (find-class 'mode))))
          (lambda (a b)
            (let ((da (depth a)) (db (depth b)))
              (if (= da db)
                  (string< (symbol-name (class-name a)) (symbol-name (class-name b)))
                  (> da db)))))))

(defun %class (name)
  "The mode class this name stands for, whichever package it was written in."
  (find (princ-to-string name) (modes)
        :key (lambda (c) (string-downcase (symbol-name (class-name c))))
        :test #'string-equal))

(defun mode (name)
  (let ((class (%class name)))
    (when class (fault:or-nothing "a mode class may take initargs nobody gave"
                  (make-instance (class-name class))))))

(defun claimsp (m path)
  (let ((leaf (file-namestring (pathname path)))
        (full (namestring (pathname path))))
    (some (lambda (p) (or (glob p leaf) (glob p full))) (handles m))))

(defun mode-for (path)
  "The mode for a place: the most particular class that claims it.

What a class claims is asked of the class, through the prototype the metaobject
protocol already keeps, so answering the question costs nothing and only the mode
that won is made."
  (loop :for class :in (modes)
        :for it := (c2mop:class-prototype class)
        :when (and (handles it) (claimsp it path))
          :do (return (fault:or-nothing "a mode class may take initargs nobody gave"
                        (make-instance (class-name class))))))

(defun %named-as (class)
  "The one name a mode's chords are kept under, spelled the way the mode is.

TEXT written in one package and TEXT written in another are one mode here, which
is what the class lookup already says. Spelling it means a chord bound before its
class is loaded is under the same name when the class arrives, rather than under a
string nothing ever reads again."
  (string-downcase (if (symbolp class) (symbol-name class) (princ-to-string class))))

(defclass keys (fs:value)
  ((owners :initform (d:no-map) :accessor owners))
  (:documentation "The chords bound by hand in one mode: chord to command. Who
bound each is kept beside, so a system's go when it does; nobody saves it, so a
restore cannot overwrite what this run's config bound."))

(defmethod fs:savedp ((k keys)) nil)

(defmethod fs:let-go ((k keys) owner)
  (let ((mine (loop :for (chord . who) :in (d:pairs (owners k))
                    :when (equal who owner) :collect chord)))
    (when mine
      (setf (owners k) (reduce #'d:without mine :initial-value (owners k)))
      (setf (fs:contents k) (reduce #'d:without mine :initial-value (fs:contents k))))
    mine))

(defclass modes (fs:mount) ()
  (:documentation "/mode: every mode class there is, and any name a chord was
bound under before its class arrived."))

(defmethod fs:entries ((d modes))
  (dolist (name (%names)) (fs:entry d name))
  (call-next-method))

(defmethod fs:entry ((d modes) name)
  (or (call-next-method)
      (and (%class name) (%make-mode-dir d (%named-as name)))))

(defun mode-node () (make-instance 'modes :name "mode"
                                          :describes "every mode there is, and its chords"))

(defun %root () (fs:at "/mode"))

(defun %walked (name)
  "The chords the commands themselves carry for this mode."
  (let ((cmd (fs:at "/cmd")) (out (d:no-map)))
    (when cmd (fs:reading cmd))
    (dolist (c (command:commands) out)
      (let ((on (command:on c)))
        (when (and on (string-equal name (string (first on))))
          (dolist (chord (rest on))
            (setf out (d:with out chord (command:name c)))))))))

(defclass mode-dir (fs:mount) ()
  (:documentation "One mode at /mode/<name>: its bound chords, the keymap in force,
and what the mode says about itself."))

(defun %make-mode-dir (root name)
  (let ((d (fs:mount (make-instance 'mode-dir :name name) root)))
    (let ((k (fs:mount (make-instance 'keys :name "keys" :held (d:no-map)) d)))
      (fs:mount (make-instance 'fs:derived :name "keymap"
                                :reads (lambda ()
                                         (d:merged (%walked name) (fs:contents k)))
                                :describes "every chord in force here")
                 d))
    (fs:mount (make-instance 'fs:derived :name "said" :live t
                              :reads (lambda () (%said name)))
               d)
    d))

(defun %mode-dir (root name)
  "The dir for one mode, made once -- for a class there is, or for a name a chord
was bound under before its class arrived."
  (or (fs:entry root name) (%make-mode-dir root name)))

(defun %keymap (class)
  (let ((d (%mode-dir (%root) (%named-as class))))
    (fs:contents (fs:entry d "keymap"))))

(defun keys (class) (%keymap class))

(defun %chain (m)
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :when (subtypep class 'mode) :collect (class-name class)))

(defun binding (m chord)
  "What CHORD runs for this mode: its own keymap, then up the class precedence list,
so a mode inherits bindings exactly as it inherits methods.

A chord bound to a command that has since gone is not unbound: it answers the name
it was bound to, so whoever asked can say so rather than take the key for text."
  (loop :for class :in (%chain m)
        :for found := (d:lookup (%keymap class) chord)
        :when found :do (return (values (command:named found) found))))

(defun bindings (m)
  "Every chord in force for a mode: its own, and its parents', nearest first."
  (loop :for class :in (%chain m)
        :append (d:pairs (%keymap class))))

(defun %keys (class)
  (fs:entry (%mode-dir (%root) (%named-as class)) "keys"))

(defun bind (class chord command)
  "Bind a chord in a mode. A config binds one the way pine does. A chord bound
while a system starts is that system's, and goes when it does; one a config binds
is nobody's and stands."
  (let ((k (%keys class)))
    (when fs:*owner*
      (setf (owners k) (d:with (owners k) chord fs:*owner*)))
    (setf (fs:contents k) (d:with (fs:contents k) chord command))
    chord))

(defun unbind (class chord)
  (let ((k (%keys class)))
    (setf (owners k) (d:without (owners k) chord))
    (setf (fs:contents k) (d:without (fs:contents k) chord))
    chord))

(defun dispatch (m subject k &optional (pending (ui:pending)))
  "What a key means to a mode. Answers :taken, what the command answered,
:pending, (:insert . string) or :unbound, and the chord standing so far.

SUBJECT is what the mode is understanding -- a document, or nothing where the mode
is a compositor's and there is no document in it at all. PENDING is the chord
already accumulated, so two keyboards, or a window manager and an editor, keep
their own place in a chord and cannot take each other's.

PRESS is asked first and the keymap only after, because a mode that takes the key
itself has no use for the chord this would otherwise spell out for every keystroke
whether anything wanted it or not."
  (if (press m subject k)
      (values :taken nil)
      (let* ((typed (append pending (list k)))
             (chord (ui:spelled typed)))
        (multiple-value-bind (found named) (binding m chord)
          (cond (found (values (fault:attempt (lambda () (command:run found))
                                              (command:name found))
                               nil
                               (command:name found)))
                (named (values :unbound nil named))
                ((prefixp m chord) (values :pending typed))
                ((and (null pending) (ui:typed k))
                 (values (cons :insert (ui:typed k)) nil))
                (t (values :unbound nil)))))))

(defun prefixp (m chord)
  "Whether CHORD is the beginning of something longer bound in this mode."
  (block found
    (dolist (class (%chain m) nil)
      (dolist (had (d:keys (%keymap class)))
        (when (and (> (length had) (length chord))
                   (string= chord had :end2 (length chord))
                   (char= #\Space (char had (length chord))))
          (return-from found t))))))

(defun %names ()
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c)))) (modes)))

(defun %said (name)
  (let ((m (mode name)))
    (when m (list :type (fs:name m) :handles (handles m)))))

(fs:mount #'mode-node "/mode")
