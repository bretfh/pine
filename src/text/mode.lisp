(defpackage #:pine/text/mode
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:command #:pine/run/command)
                    (#:key #:pine/ui/key))
  (:shadow #:type #:structure)
  (:export #:mode #:text #:prose #:code #:lisp #:pine #:scheme #:org #:fundamental
           #:press #:insert #:indent #:complete #:save #:structure
           #:setting #:claims #:claimsp #:mode-for #:keys #:bind #:binding
           #:named #:modes #:prefixp #:type #:attach #:glob))
(in-package #:pine/text/mode)

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

(defun named (name)
  (let ((class (find-symbol (string-upcase (princ-to-string name))
                            :pine/text/mode)))
    (when (and class (find-class class nil)
               (subtypep class 'mode))
      (make-instance class))))

(defun claimsp (m path)
  (let ((leaf (file-namestring (pathname path)))
        (full (namestring (pathname path))))
    (some (lambda (p) (or (glob p leaf) (glob p full))) (claims m))))

(defun mode-for (path)
  "The mode for a place: the most particular class that claims it."
  (loop :for class :in (modes)
        :for m := (ignore-errors (make-instance (class-name class)))
        :when (and m (claims m) (claimsp m path)) :do (return m)))

(defun keys (class)
  (or (d:at (d:all *keys*) class) (d:no-map)))

(defun bind (class chord command)
  "Bind a chord in a mode. A config binds one the way pine does."
  (d:keep! *keys* class (d:with (keys class) chord command))
  chord)

(defun binding (m chord)
  "What CHORD runs for this mode: its own keymap, then up the class precedence list,
so a mode inherits bindings exactly as it inherits methods."
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :for found := (d:at (keys (class-name class)) chord)
        :when found :do (return (command:named found))))

(defun prefixp (m chord)
  "Whether CHORD is the beginning of something longer bound in this mode."
  (block found
    (dolist (class (c2mop:class-precedence-list (class-of m)) nil)
      (dolist (had (d:keys (keys (class-name class))))
        (when (and (> (length had) (length chord))
                   (string= chord had :end2 (length chord))
                   (char= #\Space (char had (length chord))))
          (return-from found t))))))

(defclass modes-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defclass mode-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defclass keys-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defun %names ()
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c)))) (modes)))

(defun %mode-node (n name)
  (node:child n name (lambda () (make-instance 'mode-node :name name :over n))))

(defmethod node:nodes ((n modes-node))
  (mapcar (lambda (name) (%mode-node n name)) (%names)))

(defmethod node:resolve ((n modes-node) name)
  (when (member (princ-to-string name) (%names) :test #'equal)
    (%mode-node n (princ-to-string name))))

(defmethod node:contents ((n modes-node)) (%names))

(defmethod node:contents ((n mode-node))
  (let ((m (named (node:name n))))
    (when m (list :type (type m) :claims (claims m)))))

(defmethod node:nodes ((n mode-node))
  (list (node:child n "keys"
                    (lambda () (make-instance 'keys-node :name "keys" :over n)))))

(defmethod node:contents ((n keys-node))
  "What this mode is bound to, as it stands. A chord a config added is here without
anything having to be told about it."
  (let ((class (find-symbol (string-upcase (node:name (node:over n)))
                            :pine/text/mode)))
    (and class (keys class))))

(defun attach (root)
  (node:attach (make-instance 'modes-node :name "mode"
                              :describes "every mode there is, and its chords")
               root))
