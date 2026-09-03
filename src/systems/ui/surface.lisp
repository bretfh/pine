(in-package #:pine/ui)

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

(defclass surface (fs:dir)
  ((role  :initarg :role  :accessor role)
   (shown :initarg :shown :accessor shown)
   (size  :initarg :size  :accessor size :initform nil)
   (acts  :initform (d:no-map) :accessor acts))
  (:documentation "A surface: under it TREE, the widget tree worked out from what it
read; SHOWN, which writing puts it up or down; SIZE, what shows it says it came out
at; ROLE, WHERE and WIRE."))

(defun tree (s)
  (let ((n (fs:entry s "tree"))) (and n (fs:contents n))))

(defmethod print-object ((s surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~(~a~)~:[~; shown~]" (fs:name s)
            (class-name (class-of (role s))) (shown s))))

(defun root () (fs:ensure "/ui/surface"))

(defun surfaces ()
  (remove-if-not (lambda (n) (typep n 'surface)) (fs:entries (root))))

(defgeneric spelled-place (it)
  (:documentation "What a widget stands for, as the one word it crosses as.")
  (:method ((it path:path)) (path:whole it))
  (:method (it) (if (fs:kind it) (fs:full-name it) (princ-to-string it))))

(defun %id (widget slot at)
  "What one closure crosses as.

What the widget stands for, where it stands for anything. OF is the path a row was
built for and the place a field is over -- /proc/editor, /dev/audio/sink -- and it
does not move when the list gains a row above it.

Where it sits otherwise, which is the answer for the ones that stand for nothing:
a bar's mute button is the third thing in the second row, and for one of those the
shape is the identity.

Counted instead -- one number over the whole walk -- every id after an inserted
row was the id of a different widget, so a click that crossed during a repaint ran
whatever had slid into the place it was looking at."
  (format nil "~a/~(~a~)"
          (let ((stands-for (of widget)))
            (if stands-for
                (spelled-place stands-for)
                (format nil "@~{~d~^.~}" at)))
          slot))

(defun %plainly (said)
  "A PLACING as a plist. What crosses a wire is plain lisp data: the far side may
be another image, and what it reads has to be something a reader can read."
  (list :edges (edges-of said) :wide (wide-of said) :tall (tall-of said)
        :reserve (reserve-of said) :margin (margin-of said)))

(defun act (name said)
  "Do what the widget that crossed as this id meant, with whatever the far side
says it was given. Nothing where it means nothing, because a pine showing this one
can be a frame behind."
  (let* ((all (alexandria:ensure-list said))
         (id (princ-to-string (first all)))
         (s (fs:at "/ui/surface" (princ-to-string name)))
         (thunk (and s (d:lookup (acts s) id))))
    (when thunk
      (fault:attempt (lambda () (apply thunk (rest all)))
                     (format nil "the widget at ~a" id)))))

(defun %wire (s)
  "This surface's tree written down, with every closure in it kept on the surface
under the id it crossed as. Replaced whole: one whose row has gone no longer
answers."
  (let ((mine (d:no-map))
        (tree (tree s)))
    (when tree
      (let ((said (to-wire tree
                           :on-action (lambda (thunk widget slot at)
                                        (let ((id (%id widget slot at)))
                                          (setf mine (d:with mine id thunk))
                                          id)))))
        (setf (acts s) mine)
        said))))

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
         (s (make-instance 'surface :name (princ-to-string name)
                                    :role r
                                    :shown (ecase starts
                                             (:up t)
                                             (:down nil)
                                             (:as-the-role-says
                                              (eq :always (shows r))))
                                    :describes "a widget tree, and where it goes")))
    (fs:attach s (root))
    (fs:attach (make-instance 'fs:derived :name "tree" :reads reads :parent s
                              :describes "the widget tree, worked out from what it read")
               s)
    (let ((size (second (fs:slots s s "shown" 'shown "size" 'size))))
      (fs:attach (make-instance
                    'fs:derived :name "role"
                    :reads (lambda () (string-downcase (class-name (class-of (role s)))))
                    :parent s
                    :describes "which kind of surface this is")
                   s)
      (fs:attach (make-instance 'fs:derived :name "wire"
                                :reads (lambda () (%wire s)) :parent s
                                :describes "the tree, as it crosses to another pine")
                   s)
      (fs:attach (make-instance
                    'fs:derived :name "where"
                    :reads (lambda ()
                      (let ((said (fs:contents size)))
                        (%plainly (anchor (role s)
                                          (or (getf said :wide) 0)
                                          (or (getf said :tall) 0)))))
                    :parent s
                    :describes "where the role says this goes")
                   s))
    (fs:attach (make-instance 'fs:derived :name "click" :live t
                             :writes (lambda (said) (act (fs:name s) said))
                             :describes "what another pine says was clicked")
                 s)
    (setf (fs:owner s) fs:*owner*)
    (declared s)
    s))

(defun forget-surface (name)
  (fs:erase (format nil "/ui/surface/~a" name))
  name)

(defmacro defsurface (name options &body body)
  "Declare a surface. OPTIONS is :as and a role class."
  `(make-surface ,(string-downcase (string name)) (lambda () ,@body)
           ,@options :starts :as-the-role-says))

