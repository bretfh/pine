(defpackage #:pine/mode
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:command #:pine/run/command)
                    (#:fault #:pine/run/fault) (#:key #:pine/ui/key))
  (:shadow #:type #:structure)
  (:export #:mode #:text #:prose #:code #:lisp #:pine #:scheme #:org #:fundamental
           #:press #:insert #:indent #:complete #:save #:structure
           #:setting #:claims #:claimsp #:mode-for #:keys #:bind #:binding
           #:bindings #:dispatch
           #:named #:modes #:prefixp #:type #:mode-node #:glob))
(in-package #:pine/mode)

(defvar *keys* (d:table)
  "Chords, by mode class name. A mode's own keymap; what a chord means comes from
the class precedence list, so a mode inherits its parent's bindings the way it
inherits its parent's methods.")

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

(defgeneric insert (mode document string)
  (:documentation "What typing means here.")
  (:method ((m mode) d s) (declare (ignore d s)) nil))

(defgeneric indent (mode document line)
  (:documentation "What column LINE should start at, or nothing to leave it.")
  (:method ((m mode) d line) (declare (ignore d line)) nil))

(defgeneric complete (mode document prefix)
  (:documentation "What PREFIX could be finished as.")
  (:method ((m mode) d prefix) (declare (ignore d prefix)) nil))

(defgeneric save (mode document)
  (:documentation "What saving means here.")
  (:method ((m mode) d) (declare (ignore d)) nil))

(defgeneric structure (mode document)
  (:documentation "The regions this text divides into, as a tree.

Each is (NAME START END . CHILDREN) in the document's own lines. What comes back is
put in the namespace under the document, so a form or a heading is a place anything
can read, write and watch -- inside this image and outside it.")
  (:method ((m mode) d) (declare (ignore d)) nil))

(defgeneric setting (mode key)
  (:documentation "What this mode says about KEY. CALL-NEXT-METHOD is the fallback,
so a mode that says nothing gets what its parent says.")
  (:method ((m mode) key) (declare (ignore key)) nil))

(defgeneric claims (mode)
  (:documentation "The globs of paths and names this mode is for.")
  (:method ((m mode)) nil))

(defgeneric type (mode)
  (:documentation "What the modeline calls it.")
  (:method ((m mode)) (string-downcase (symbol-name (class-name (class-of m))))))

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

(defmethod claims ((m lisp)) '("*.lisp" "*.asd" "*.cl"))
(defmethod claims ((m scheme)) '("*.scm" "*.ss"))
(defmethod claims ((m org)) '("*.org"))

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
anything has asked for one."
  (labels ((under (class)
             (c2mop:ensure-finalized class)
             (cons class (mapcan #'under (c2mop:class-direct-subclasses class)))))
    (sort (remove (find-class 'mode) (under (find-class 'mode)))
          #'> :key (lambda (c) (length (c2mop:class-precedence-list c))))))

(defun %class (name)
  "The mode class this name stands for, whichever package it was written in."
  (find (princ-to-string name) (modes)
        :key (lambda (c) (string-downcase (symbol-name (class-name c))))
        :test #'string-equal))

(defun named (name)
  (let ((class (%class name)))
    (when class (fault:or-nothing "a mode class may take initargs nobody gave"
                  (make-instance (class-name class))))))

(defun claimsp (m path)
  (let ((leaf (file-namestring (pathname path)))
        (full (namestring (pathname path))))
    (some (lambda (p) (or (glob p leaf) (glob p full))) (claims m))))

(defun mode-for (path)
  "The mode for a place: the most particular class that claims it."
  (loop :for class :in (modes)
        :for m := (fault:or-nothing "a mode class may take initargs nobody gave"
                     (make-instance (class-name class)))
        :when (and m (claims m) (claimsp m path)) :do (return m)))

(defun %carried (class)
  "The chords the commands themselves carry for this mode class. A command says
what chord means it; nothing tells this file, and a chord goes when the command
that named it does."
  (let ((wanted (symbol-name class))
        (out (d:no-map)))
    (dolist (c (command:commands) out)
      (let ((on (command:on c)))
        (when (and on (string-equal wanted (string (first on))))
          (dolist (chord (rest on))
            (setf out (d:with out chord (command:name c)))))))))

(defun keys (class)
  "Every chord in force for a mode class: what its commands carry, and what
somebody bound by hand on top of that."
  (d:merged (%carried class) (or (d:lookup (d:all *keys*) class) (d:no-map))))

(defun bind (class chord command)
  "Bind a chord in a mode. A config binds one the way pine does.

The mode is named rather than identified: TEXT written in one package and TEXT
written in another are two symbols and one class, and a keymap that told them apart
would be two keymaps nobody asked for."
  (let* ((found (%class class))
         (class (if found (class-name found) class)))
    (d:keep! *keys* class
             (d:with (or (d:lookup (d:all *keys*) class) (d:no-map)) chord command))
    chord))

(defun binding (m chord)
  "What CHORD runs for this mode: its own keymap, then up the class precedence list,
so a mode inherits bindings exactly as it inherits methods."
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :for found := (d:lookup (keys (class-name class)) chord)
        :when found :do (return (command:named found))))

(defun bindings (m)
  "Every chord in force for a mode: its own, and its parents', nearest first."
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :append (d:pairs (keys (class-name class)))))

(defun dispatch (m subject k &optional (pending (key:pending)))
  "What a key means to a mode. Answers :taken, what the command answered,
:pending, (:insert . string) or :unbound, and the chord standing so far.

SUBJECT is what the mode is understanding -- a document, or nothing where the mode
is a compositor's and there is no document in it at all. PENDING is the chord
already accumulated, so two keyboards, or a window manager and an editor, keep
their own place in a chord and cannot take each other's."
  (let* ((so-far (append pending (list k)))
         (chord (key:text so-far))
         (found (binding m chord)))
    (cond ((press m subject k) (values :taken nil))
          (found (values (fault:attempt (lambda () (command:run found))
                                        (command:name found))
                         nil
                         (command:name found)))
          ((prefixp m chord) (values :pending so-far))
          ((and (null pending) (key:typed k))
           (values (cons :insert (key:typed k)) nil))
          (t (values :unbound nil)))))

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
  (let ((m (named name)))
    (when m (list :type (type m) :claims (claims m)))))

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
    (node:place name
                :names (constantly '("keys"))
                :each (lambda (field)
                        (when (equal field "keys")
                          (node:place field
                                      :reads (lambda () (%chords name)))))
                :reads (lambda () (%said name)))))

(defun mode-node ()
  "Every mode there is, and its chords, as a place. Made here and attached by
whoever is putting it up, the way any other node is."
  (node:place "mode"
              :names #'%names
              :each #'%mode
              :describes "every mode there is, and its chords"))

(pine/word:lends "mode" "text" "prose" "code" "lisp" "pine" "scheme" "org"
                "press" "insert" "indent" "complete" "save" "structure" "setting"
                "bind" "claims")
