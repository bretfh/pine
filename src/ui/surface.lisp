(defpackage #:pine/ui/surface
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:wire #:pine/ui/wire)
                    (#:fault #:pine/run/fault))
  (:export #:surface #:defsurface #:surfaces #:named #:root #:builds
           #:role #:anchor #:shown #:asks #:size #:act
           #:bar #:panel #:overlay #:background #:window #:tile
           #:declared))
(in-package #:pine/ui/surface)

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

(defgeneric asks (role)
  (:documentation "Whether a surface of this role waits to be asked for. Furniture
is up as soon as it is declared; a panel is not.")
  (:method ((r role)) nil)
  (:method ((r panel)) t)
  (:method ((r overlay)) t))

(defgeneric anchor (role width height)
  (:documentation "Where a surface of this role sits and how big, given what it
measured to. Answers a map: which edges it is anchored to, how wide and how tall,
what strip it keeps for itself, and its margin.

The words are wayland's because that is what the placement is; nothing about the
content is here.")
  (:method ((r role) width height)
    (d:map :edges '(:top :left) :wide width :tall height :keeps 0
           :margin '(0 0 0 0)))
  (:method ((r bar) width height)
    (declare (ignore height))
    (d:map :edges '(:top :left :bottom) :wide width :tall 0 :keeps width
           :margin '(0 0 0 0)))
  (:method ((r background) width height)
    (declare (ignore width height))
    (d:map :edges '(:top :left :bottom :right) :wide 0 :tall 0 :keeps 0
           :margin '(0 0 0 0)))
  (:method ((r overlay) width height)
    (d:map :edges '(:top :right) :wide width :tall height :keeps 0
           :margin '(8 8 0 0)))
  (:method ((r panel) width height)
    (d:map :edges '(:top :left) :wide width :tall height :keeps 0
           :margin '(8 0 0 8)))
  (:method ((r window) width height)
    "A window of its own: the compositor sizes it, so nothing is anchored."
    (declare (ignore width height))
    (d:map :edges nil :wide 0 :tall 0 :keeps 0 :margin '(0 0 0 0)))
  (:method ((r tile) width height)
    (declare (ignore width height))
    (d:map :edges nil :wide 0 :tall 0 :keeps 0 :margin '(0 0 0 0))))

(defclass surface (node:node)
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

(defun root () (tree:ensure nil "surface"))

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
         (thunk (d:at (d:all *acts*) id)))
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
      (wire:to-wire tree
                    :on-action (lambda (thunk)
                                 (let ((id (%id name (incf index))))
                                   (d:keep! *acts* id thunk)
                                   id))))))

(defgeneric declared (surface)
  (:documentation "Say a surface was declared. Whatever paints surfaces puts this
one up; with nothing painting, a declared surface is a node in the tree and
nothing more, which is exactly what it is in a test.")
  (:method (surface) (declare (ignore surface)) nil))

(defun builds (name reads &key (as 'panel) shown)
  (let* ((r (make-instance as))
         (s (make-instance 'surface :name (princ-to-string name) :reads reads
                                    :cachedp t :role r
                                    :shown (if (eq shown :default) (not (asks r))
                                               shown)
                                    :describes "a widget tree, and where it goes")))
    (node:attach s (root))
    (let ((size (second (node:slots s s "shown" 'shown "size" 'size))))
      (node:attach (node:derive
                    "role"
                    (lambda () (string-downcase (class-name (class-of (role s)))))
                    :over s
                    :describes "which kind of surface this is")
                   s)
      (node:attach (node:derive "wire" (lambda () (%wire s)) :over s
                                :describes "the tree, as it crosses to another pine")
                   s)
      (node:attach (node:derive
                    "where"
                    (lambda ()
                      (let ((said (node:contents size)))
                        (%plainly (anchor (role s)
                                          (or (getf said :wide) 0)
                                          (or (getf said :tall) 0)))))
                    :over s
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
           ,@options :shown :default))

(pine/word:lends "surface" "defsurface" "builds" "role" "anchor" "asks"
                "shown" "bar" "panel" "overlay" "background" "window" "tile")
