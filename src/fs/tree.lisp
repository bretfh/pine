(defpackage #:pine/fs/tree
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export
   #:*root* #:root #:make-root #:at #:ensure
   #:put #:erase #:walk #:listing #:paths
   #:split-name #:pieces #:builder
   #:absent #:not-a-place #:where #:given
   #:built))
(in-package #:pine/fs/tree)

(defvar *root* nil
  "The namespace this image is. One per image: a second one is another pine, and it
is reached by mounting it rather than by holding two here.")

(defvar *builders* nil
  "What each package puts on the tree, in the order the packages were loaded. They
build separate branches and none of them reads another, so that order is the only
one there is and nothing has to state it.")

(defvar *spelled* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "What a name spells, kept against the string itself.

Held by EQ and not by what the string says, because the point is the string a
compiled call site hands over every time it runs: /dev/audio/volume in a file is
one object, and cutting it into three again on every read is the only thing left
that a read allocates. A name built for the occasion misses, and is cut the way it
always was.

Weak on the key, so a name built for the occasion is not remembered for ever.")

(define-condition absent (error)
  ((where :initarg :where :reader where))
  (:report (lambda (c s) (format s "nothing at ~a" (where c)))))

(define-condition not-a-place (error)
  ((given :initarg :given :reader given))
  (:report
   (lambda (c s)
     (format s "~s is not a place. A place is a name, a path or a node; nothing ~
is what a place answers when there is none, so it cannot also be one."
             (given c)))))

(defun make-root ()
  "The namespace this image is. A branch: it holds nothing itself, and what a
fresh one has is nothing, because a value lives in the node that holds it."
  (setf *root* (make-instance 'node:node :name nil)))

(defun root () *root*)

(defun builder (thunk)
  "Say this package puts nodes on the tree, and how.

Said in the file the nodes are defined in. What the namespace has at boot is the
sum of what pine loaded, not a list somewhere else naming twelve packages in an
order nobody can check."
  (setf *builders* (append (cl:remove thunk *builders*) (list thunk)))
  thunk)

(defun built (&optional (root (root)))
  "Put on the tree what every package said it puts there."
  (dolist (thunk *builders* root) (funcall thunk root)))

(defun split-name (text)
  (let ((names nil)
        (piece (make-string-output-stream)))
    (flet ((finish ()
             (let ((s (get-output-stream-string piece)))
               (when (plusp (length s)) (push s names)))))
      (loop :for ch :across text
            :if (char= ch #\/) :do (finish)
              :else :do (write-char ch piece))
      (finish))
    (nreverse names)))

(defun %names (pieces)
  (loop :for p :in pieces
        :append (typecase p
                  (string (split-name p))
                  (symbol (list (string-downcase (symbol-name p))))
                  (t (list (princ-to-string p))))))

(defun %step (at name makep)
  (and at (or (node:resolve at name)
              (and makep (node:make-child at name)))))

(defun %cut (text)
  "The pieces TEXT spells, cut where it lies.

SPLIT-NAME is the same walk with a stream to show for it. This keeps what it cut,
so the next read of the same name cuts nothing."
  (declare (type string text) (optimize (speed 3) (safety 1)))
  (let ((n (length text)) (i 0) (out nil))
    (loop
      (loop :while (and (< i n) (char= #\/ (char text i))) :do (incf i))
      (when (>= i n) (return (nreverse out)))
      (let ((j i))
        (loop :while (and (< j n) (not (char= #\/ (char text j)))) :do (incf j))
        (push (subseq text i j) out)
        (setf i j)))))

(defun pieces (text)
  "What TEXT spells, cut once and kept against the string itself."
  (or (gethash text *spelled*)
      (setf (gethash text *spelled*) (%cut text))))

(defun %spelled (at text makep)
  "Walk what TEXT spells, a piece at a time.

Every read and every write goes through here. The pieces are cut once for a given
string and kept, so a name a compiled call site hands over walks without
allocating anything at all -- which is the whole of what a namespace lookup has to
cost where everything on the machine is a name."
  (loop :for name :in (pieces text)
        :do (setf at (%step at name makep))
        :while at
        :finally (return at)))

(defun %walk (n pieces makep)
  (loop :with at := n
        :for p :in pieces
        :do (setf at (typecase p
                       (string (%spelled at p makep))
                       (symbol (%step at (string-downcase (symbol-name p)) makep))
                       (t (%step at (princ-to-string p) makep))))
            (when (null at) (return nil))
        :finally (return at)))

(defgeneric at (where &rest names)
  (:documentation "The node WHERE names, and NAMES on from there, or nothing where
none stands.

One question with one answer per kind of thing you can name a place with: a node
is itself, a name is from the root, and a path is what it spells. Nothing is not
one of them -- it is what this answers when there is nothing there, and a value
that means absence cannot also mean somewhere.")
  (:method ((where node:node) &rest names)
    (declare (dynamic-extent names))
    (%walk where names nil))
  (:method ((where null) &rest names)
    (declare (ignore names))
    (error 'not-a-place :given where))
  (:method ((where string) &rest names)
    (declare (dynamic-extent names))
    (if names
        (%walk (%spelled *root* where nil) names nil)
        (%spelled *root* where nil))))

(defgeneric ensure (where &rest names)
  (:documentation "The same, making what is not there. That is the whole of the
difference between a read and a write: a read finds what stands and a write makes
what does not.")
  (:method ((where node:node) &rest names)
    (declare (dynamic-extent names))
    (%walk where names t))
  (:method ((where null) &rest names)
    (declare (ignore names))
    (error 'not-a-place :given where))
  (:method ((where string) &rest names)
    (declare (dynamic-extent names))
    (if names
        (%walk (%spelled *root* where t) names t)
        (%spelled *root* where t))))

(defun put (where pieces value)
  (let ((n (apply #'ensure where (alexandria:ensure-list pieces))))
    (setf (node:contents n) value)
    n))

(defun %erase (from names)
  (let* ((gone (car (last names)))
         (holder (and gone (%walk from (butlast names) nil))))
    (when holder (node:erase-child holder gone))))

(defgeneric erase (where &rest pieces)
  (:documentation "Take off the last name in what WHERE and PIECES spell between
them.

WHERE names a place the way AT does, so the name that goes may be the end of it:
(erase \"/a/b\") and (erase \"/a\" \"b\") take off the same node.")
  (:method ((where node:node) &rest pieces) (%erase where (%names pieces)))
  (:method ((where string) &rest pieces)
    (%erase *root* (%names (cons where pieces))))
  (:method ((where null) &rest pieces)
    (declare (ignore pieces))
    (error 'not-a-place :given where)))

(defun walk (n function &key (depth -1) (into (complement #'node:livep)))
  (funcall function n)
  (when (and (not (zerop depth)) (or (null into) (funcall into n)))
    (dolist (each (node:nodes n))
      (walk each function :depth (1- depth) :into into)))
  n)

(defun listing (n)
  "The names directly under N."
  (mapcar #'node:name (node:nodes n)))

(defun paths (n)
  "Every path below N that this image keeps, N's own first.

Not into a live one. What is under a mounted directory or a device belongs to the
world rather than to pine, and walking it here would be walking the disk."
  (let (acc)
    (walk n (lambda (each) (push (node:full-name each) acc)))
    (nreverse acc)))

