(in-package #:pine/ui)

(defvar *acts* (d:table)
  "What clicking a widget means, by the id it crossed the wire as. A closure
cannot cross, so what it meant stays here.")

(defclass role () ()
  (:documentation "Where a surface of this kind goes, and whether it is up already.

A role is a class and that is the whole of what any kind of surface means. Writing a
new one is one ANCHOR method and a default; what shows it needs no knowledge of it,
because the role crosses the wire with the surface it is on."))

(defclass bar (role) ())
(defclass panel (role) ())
(defclass overlay (role) ())
(defclass background (role) ())
(defclass window (role) ())
(defclass tile (role) ())

(defgeneric shows (role)
  (:documentation "When a surface of this role comes up: :ALWAYS as soon as it is
declared, or :WHEN-ASKED and not before. Furniture is the first; a panel is the
second.

Two words rather than a yes and a no, because the no was the one that put a
surface on the screen -- a config saying its role does not wait to be asked was
writing NIL, and reading that method afterwards told you nothing about which way
round it went.")
  (:method ((r role)) :always)
  (:method ((r panel)) :when-asked)
  (:method ((r overlay)) :when-asked))

(defclass placing ()
  ((edges   :initarg :edges   :reader edges-of   :initform nil)
   (wide    :initarg :wide    :reader wide-of    :initform 0)
   (tall    :initarg :tall    :reader tall-of    :initform 0)
   (reserve :initarg :reserve :reader reserve-of :initform 0)
   (margin  :initarg :margin  :reader margin-of  :initform '(0 0 0 0)))
  (:documentation "Where a surface sits and how big: which edges it is anchored to,
how wide and how tall, what strip it keeps for itself, and its margin.

A class and not a map, because it is what ANCHOR answers and every kind of surface
answers it. A map made a misspelled key a surface that quietly sat at the origin;
a slot that does not exist is a build that fails. What crosses the wire is still a
plist -- the far side may be another image -- and %PLAINLY is where it becomes one.

The words are wayland's because that is what the placement is; nothing about the
content is here."))

(defmethod print-object ((p placing) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~{~(~a~)~^ ~} ~dx~d" (edges-of p) (wide-of p) (tall-of p))))

(defun placing (&key edges (wide 0) (tall 0) (reserve 0) (margin '(0 0 0 0)))
  (make-instance 'placing :edges edges :wide wide :tall tall
                          :reserve reserve :margin margin))

(defun inset (&key (top 0) (right 0) (bottom 0) (left 0))
  "A margin, in the order wayland reads one. Not MARGIN: that is the widget slot,
and a surface's margin is not a widget's."
  (list top right bottom left))

(defgeneric anchor (role width height)
  (:documentation "Where a surface of this role sits and how big, given what it
measured to. Answers a PLACING.

The words are wayland's because that is what the placement is; nothing about the
content is here.")
  (:method ((r role) width height)
    (placing :edges '(:top :left) :wide width :tall height))
  (:method ((r bar) width height)
    (declare (ignore height))
    (placing :edges '(:top :left :bottom) :wide width :tall 0 :reserve width))
  (:method ((r background) width height)
    (declare (ignore width height))
    (placing :edges '(:top :left :bottom :right)))
  (:method ((r overlay) width height)
    (placing :edges '(:top :right) :wide width :tall height
             :margin (inset :top 8 :right 8)))
  (:method ((r panel) width height)
    (placing :edges '(:top :left) :wide width :tall height
             :margin (inset :top 8 :left 8)))
  (:method ((r window) width height)
    "A window of its own: the compositor sizes it, so nothing is anchored."
    (declare (ignore width height))
    (placing))
  (:method ((r tile) width height)
    (declare (ignore width height))
    (placing)))

(defclass surface (node:derived)
  ((role  :initarg :role  :accessor role)
   (shown :initarg :shown :accessor shown)
   (size  :initarg :size  :accessor size :initform nil))
  (:documentation "A widget tree that is worked out, so it follows whatever it read.
SHOWN is a node under it: writing /surface/audio/shown '(:toggle)' is the whole of
putting a panel up. So is SIZE: what shows it says how big it came out there, and
what the surface builds follows it like anything else it read."))

(defmethod print-object ((s surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~(~a~)~:[~; shown~]" (node:name s)
            (class-name (class-of (role s))) (shown s))))

(defun root () (tree:ensure "/surface"))

(defun surfaces ()
  (remove-if-not (lambda (n) (typep n 'surface)) (node:nodes (root))))

(defun named (name)
  (let ((n (tree:at (root) (princ-to-string name))))
    (and (typep n 'surface) n)))

(defun %id (name at) (format nil "~a/~d" name at))

(defun %plainly (said)
  "A PLACING as a plist. What crosses a wire is plain lisp data: the far side may
be another image, and what it reads has to be something a reader can read."
  (list :edges (edges-of said) :wide (wide-of said) :tall (tall-of said)
        :reserve (reserve-of said) :margin (margin-of said)))

(defun act (said)
  "Do what the widget that crossed as this id meant, with whatever the far side
says it was given. Nothing where it means nothing, because a pine showing this one
can be a frame behind."
  (let* ((all (alexandria:ensure-list said))
         (id (princ-to-string (first all)))
         (thunk (d:lookup (d:all *acts*) id)))
    (when thunk
      (fault:attempt (lambda () (apply thunk (rest all)))
                     (format nil "the widget at ~a" id)))))

(defun %wire (s)
  "This surface's tree written down, with every closure in it left here under the
id it crossed as."
  (let ((name (node:name s))
        (index -1)
        (tree (node:contents s)))
    (when tree
      (to-wire tree
                    :on-action (lambda (thunk)
                                 (let ((id (%id name (incf index))))
                                   (d:keep! *acts* id thunk)
                                   id))))))

(defgeneric declared (surface)
  (:documentation "Say a surface was declared. Whatever paints surfaces puts this
one up; with nothing painting, a declared surface is a node in the tree and
nothing more, which is exactly what it is in a test.")
  (:method (surface) (declare (ignore surface)) nil))

(defun make-surface (name reads &key (as 'panel) (starts :as-the-role-says))
  "STARTS is :UP, :DOWN, or :AS-THE-ROLE-SAYS -- which asks the role's SHOWS. It is
three words because it is three answers: DEFSURFACE and a direct call used to
disagree about what leaving it out meant."
  (let* ((r (make-instance as))
         (s (make-instance 'surface :name (princ-to-string name) :reads reads
                                    :role r
                                    :shown (ecase starts
                                             (:up t)
                                             (:down nil)
                                             (:as-the-role-says
                                              (eq :always (shows r))))
                                    :describes "a widget tree, and where it goes")))
    (node:attach s (root))
    (let ((size (second (node:slots s s "shown" 'shown "size" 'size))))
      (node:attach (node:derive
                    "role"
                    (lambda () (string-downcase (class-name (class-of (role s)))))
                    :parent s
                    :describes "which kind of surface this is")
                   s)
      (node:attach (node:derive "wire" (lambda () (%wire s)) :parent s
                                :describes "the tree, as it crosses to another pine")
                   s)
      (node:attach (node:derive
                    "where"
                    (lambda ()
                      (let ((said (node:contents size)))
                        (%plainly (anchor (role s)
                                          (or (getf said :wide) 0)
                                          (or (getf said :tall) 0)))))
                    :parent s
                    :describes "where the role says this goes")
                   s))
    (node:attach (node:answers "click"
                             :writes #'act
                             :describes "what another pine says was clicked")
                 s)
    (system:owned (list :surface (node:name s)))
    (declared s)
    s))

(defun forget-surface (name)
  "Take a surface off, and let go the closures its widgets crossed as.

Erasing the node is not the whole of it. What a widget meant stays in *ACTS* under
the id it crossed as, so a surface that has gone leaves closures nothing can reach
and a click on one of its old ids still runs what it used to mean."
  (let ((under (concatenate 'string (princ-to-string name) "/")))
    (dolist (id (d:keys (d:all *acts*)))
      (when (and (> (length id) (length under))
                 (string= under id :end2 (length under)))
        (d:drop! *acts* id))))
  (tree:erase (format nil "/surface/~a" name))
  name)

(system:undoes :surface #'forget-surface)

(defmacro defsurface (name options &body body)
  "Declare a surface. OPTIONS is :as and a role class."
  `(make-surface ,(string-downcase (string name)) (lambda () ,@body)
           ,@options :starts :as-the-role-says))

