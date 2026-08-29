(defpackage #:pine/kernel/tree
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:name #:pine/kernel/name)
                    (#:place #:pine/kernel/place) (#:tell #:pine/kernel/tell))
  (:export
   #:root #:make-root #:*root* #:reach #:standing #:standsp #:walk
   #:make-under #:take-away #:builders #:on-build #:builds))
(in-package #:pine/kernel/tree)

(defvar *root* nil
  "The namespace this image is.")

(defvar *builders* nil
  "What each system puts on the tree, in the order the systems were loaded. They
build separate branches and none of them reads another, so that order is the only
one there is and nothing has to state it.")

(defun make-root ()
  (place:make-place :value "" :describes "the namespace this image is"))

(defun root ()
  (or *root* (setf *root* (make-root))))

(defun builders () (reverse *builders*))

(defun on-build (builds) (pushnew builds *builders*) builds)

(defun builds ()
  (dolist (each (builders) (root)) (funcall each (root))))

(defgeneric said-as (said)
  (:documentation "SAID as a name. One question with one answer per kind of thing
you can say a place with, and every one of them is from the root: a name is where
something stands, and there is nowhere else to measure that from.")
  (:method ((said name:name)) said)
  (:method ((said string)) (name:parse said))
  (:method ((said null)) (name:parse))
  (:method ((said cons)) (apply #'name:parse said))
  (:method (said) (name:parse (princ-to-string said))))

(defun %walked (text)
  "Walk a written name without making anything out of it.

The hot path of the whole kernel: everything anybody does begins by saying where.
Making a name here -- pieces, a class for each kind of piece, a list of them --
would be a handful of objects for every read of every place, which is most of what
a read would cost and all of what it would leave behind for the collector. So this
walks the string where it lies and allocates one short string for each piece,
because that is what the beneath map is keyed by."
  (declare (type string text) (optimize (speed 3) (safety 1)))
  (let ((at (root)) (n (length text)) (i 0))
    (loop
      (loop :while (and (< i n) (char= #\/ (char text i))) :do (incf i))
      (when (>= i n) (return at))
      (let ((j i))
        (loop :while (and (< j n) (not (char= #\/ (char text j)))) :do (incf j))
        (setf at (place:resolve at (subseq text i j)))
        (when (null at) (return nil))
        (setf i j)))))

(defun reach (said)
  "The place SAID names, or nothing where nothing stands there.

A place is itself: handed one, this is what hands it back, so everything above can
take either and none of it has to ask which it got."
  (typecase said
    (place:place said)
    (string (%walked said))
    (null (root))
    (name:name (loop :with at := (root)
                     :for piece :in (name:spelled said)
                     :do (setf at (and at (place:resolve at piece)))
                     :finally (return at)))
    (t (%walked (princ-to-string said)))))

(defun standsp (said) (and (reach said) t))

(defun standing (said)
  "The place SAID names, or a complaint naming what was asked for. For whoever
would rather stop than carry a nothing along."
  (or (reach said) (error "nothing stands at ~a" (name:whole (said-as said)))))

(defun make-under (said kind &rest initargs)
  "Bring a place of KIND into being at SAID, making the branches above it.

What is above a place is a place, so a name three deep makes three. They hold
nothing until somebody writes them, which is the difference between a branch and
a leaf and the whole of it."
  (let* ((n (said-as said))
         (pieces (name:spelled n)))
    (when (null pieces) (error "the root is already there"))
    (tell:together
      (loop :with at := (root)
            :for (piece . more) :on pieces
            :do (setf at
                      (if more
                          (or (place:resolve at piece)
                              (let ((made (place:make-place :value piece)))
                                (place:attach made at)
                                (place:moved at)
                                made))
                          (let ((made (apply #'place:make-place kind piece
                                             initargs)))
                            (place:attach made at)
                            (place:moved at)
                            made)))
            :finally (return at)))))

(defun take-away (said)
  "Take the place SAID names off the tree, and everything under it with it.

What is under it goes first, so nothing is left holding a parent that is no
longer anywhere. Saying the name went is told once for the whole of it, because
one erasure is one piece of news however deep it reached."
  (let ((it (reach said)))
    (when it
      (tell:together
        (let ((gone (place:full-name it))
              (over (place:under it)))
          (labels ((down (p)
                     (dolist (each (d:vals (place:beneath p))) (down each))
                     (place:detach p)))
            (dolist (each (d:vals (place:beneath it))) (down each))
            (place:detach it))
          (when over (place:moved over))
          (tell:went gone)
          it)))))

(defun walk (p function &key (depth -1) (into (complement #'place:livep)))
  "FUNCTION over P and everything under it.

P itself is always walked; INTO says which places are walked *into*. A live one
is not, by default: what is under one is whatever the world says is under it
right now, and asking the world for the whole of that is not what somebody
counting the tree meant to do. INTO of nothing walks into everything, and says so
by being the only way to say it."
  (when p
    (funcall function p)
    (when (and (not (zerop depth)) (or (null into) (funcall into p)))
      (dolist (each (place:kids p))
        (walk each function :depth (1- depth) :into into)))
    p))
