(defpackage #:pine/mode
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:command #:pine/run/command)
                    (#:fault #:pine/run/fault) (#:system #:pine/run/system))
  (:export
   #:mode #:text #:prose #:code #:lisp
   #:pine #:scheme #:org #:press #:typing
   #:indent #:complete #:saving #:regions #:setting #:says
   #:covering #:name-of #:from-of #:to-of #:inside-of
   #:handles #:mode-for #:bind #:unbind #:binding #:bindings
   #:dispatch #:modes #:mode-node))
(in-package #:pine/mode)

(defvar *keys* (d:table)
  "Chords, by mode class name. A mode's own keymap; what a chord means comes from
the class precedence list, so a mode inherits its parent's bindings the way it
inherits its parent's methods.")
(defvar *carried* (d:table)
  "What the commands themselves carry, by mode class name, and the turn of the
command table it was read off. A keystroke asks for this once per class in the
precedence list, and the commands do not move between two keystrokes.")

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

(defmethod node:name ((m mode))
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

(defun %walked (class)
  (let ((out (d:no-map)))
    (dolist (c (command:commands) out)
      (let ((on (command:on c)))
        (when (and on (string-equal class (string (first on))))
          (dolist (chord (rest on))
            (setf out (d:with out chord (command:name c)))))))))

(defun %carried (class)
  "The chords the commands themselves carry for this mode class. A command says
what chord means it; nothing tells this file, and a chord goes when the command
that named it does.

Kept until the commands turn over, because this is asked once per class in the
precedence list for every key that arrives."
  (let ((had (d:lookup (d:all *carried*) class))
        (now (command:turned)))
    (if (and had (eql (car had) now))
        (cdr had)
        (let ((made (%walked class)))
          (d:keep! *carried* class (cons now made))
          made))))

(defun keys (class)
  "Every chord in force for a mode class: what its commands carry, and what
somebody bound by hand on top of that."
  (let ((class (%named-as class)))
    (d:merged (%carried class)
              (or (d:lookup (d:all *keys*) class) (d:no-map)))))

(defun bind (class chord command)
  "Bind a chord in a mode. A config binds one the way pine does.

A chord bound while a system starts is that system's, and goes when it does. One a
config binds is nobody's and stands, which is the difference between a system you
can drop and a machine you set up."
  (let ((class (%named-as class)))
    (d:update! *keys* class
               (lambda (had) (d:with (or had (d:no-map)) chord command)))
    (system:owned (list :chord class chord))
    chord))

(defun unbind (class chord)
  "Take a chord off a mode."
  (let ((class (%named-as class)))
    (d:update! *keys* class
               (lambda (had) (if had (d:without had chord) (d:no-map))))
    chord))

(system:undoes :chord #'unbind)

(defun binding (m chord)
  "What CHORD runs for this mode: its own keymap, then up the class precedence list,
so a mode inherits bindings exactly as it inherits methods.

A chord bound to a command that has since gone is not unbound: it answers the name
it was bound to, so whoever asked can say so rather than take the key for text."
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :for found := (d:lookup (keys (class-name class)) chord)
        :when found :do (return (values (command:named found) found))))

(defun bindings (m)
  "Every chord in force for a mode: its own, and its parents', nearest first."
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :append (d:pairs (keys (class-name class)))))

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
    (dolist (class (c2mop:class-precedence-list (class-of m)) nil)
      (dolist (had (d:keys (keys (class-name class))))
        (when (and (> (length had) (length chord))
                   (string= chord had :end2 (length chord))
                   (char= #\Space (char had (length chord))))
          (return-from found t))))))

(defun %names ()
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c)))) (modes)))

(defun %said (name)
  (let ((m (mode name)))
    (when m (list :type (node:name m) :handles (handles m)))))

(defun %chords (name)
  "What a mode is bound to, as it stands. A chord a config added is here without
anything having to be told about it.

The class is looked for among the modes rather than in one package, because a
mode is a class anybody can write and most of them are not written here."
  (let ((class (%class name)))
    (when class
      (sort (d:pairs (keys (class-name class))) #'string< :key #'car))))

(defun %mode (name)
  (when (%class name)
    (make-instance 'node:place :name name
                :names (constantly '("keys"))
                :each (lambda (field)
                        (when (equal field "keys")
                          (make-instance 'node:place :name field
                                      :reads (lambda () (%chords name)))))
                :reads (lambda () (%said name)))))

(defun mode-node ()
  "Every mode there is, and its chords, as a place. Made here and attached by
whoever is putting it up, the way any other node is."
  (make-instance 'node:place :name "mode"
              :names #'%names
              :each #'%mode
              :describes "every mode there is, and its chords"))

