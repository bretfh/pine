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

(defgeneric anchor (role width height)
  (:documentation "Where a surface of this role sits and how big, given what it
measured to. Answers a map: which edges it is anchored to, how wide and how tall,
what strip it keeps for itself, and its margin.

The words are wayland's because that is what the placement is; nothing about the
content is here.")
  (:method ((r role) width height)
    (d:map :edges '(:top :left) :wide width :tall height :reserve 0
           :margin '(0 0 0 0)))
  (:method ((r bar) width height)
    (declare (ignore height))
    (d:map :edges '(:top :left :bottom) :wide width :tall 0 :reserve width
           :margin '(0 0 0 0)))
  (:method ((r background) width height)
    (declare (ignore width height))
    (d:map :edges '(:top :left :bottom :right) :wide 0 :tall 0 :reserve 0
           :margin '(0 0 0 0)))
  (:method ((r overlay) width height)
    (d:map :edges '(:top :right) :wide width :tall height :reserve 0
           :margin '(8 8 0 0)))
  (:method ((r panel) width height)
    (d:map :edges '(:top :left) :wide width :tall height :reserve 0
           :margin '(8 0 0 8)))
  (:method ((r window) width height)
    "A window of its own: the compositor sizes it, so nothing is anchored."
    (declare (ignore width height))
    (d:map :edges nil :wide 0 :tall 0 :reserve 0 :margin '(0 0 0 0)))
  (:method ((r tile) width height)
    (declare (ignore width height))
    (d:map :edges nil :wide 0 :tall 0 :reserve 0 :margin '(0 0 0 0))))

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
  "A map as a plist. What crosses a wire is plain lisp data: the far side may
be another image, and what it reads has to be something a reader can read."
  (loop :for (key . value) :in (d:pairs said) :append (list key value)))

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

(defun builds (name reads &key (as 'panel) (starts :as-the-role-says))
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
    (node:attach (node:place "click"
                             :writes #'act
                             :describes "what another pine says was clicked")
                 s)
    (declared s)
    s))

(defmacro defsurface (name options &body body)
  "Declare a surface. OPTIONS is :as and a role class."
  `(builds ,(string-downcase (string name)) (lambda () ,@body)
           ,@options :starts :as-the-role-says))

