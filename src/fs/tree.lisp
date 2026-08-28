(defpackage #:pine/fs/tree
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export
   #:*root* #:*here* #:here #:root #:make-root #:at #:ensure
   #:put #:erase #:walk #:listing #:paths
   #:split-name #:builder
   #:built))
(in-package #:pine/fs/tree)

(defvar *root* nil
  "The namespace this image is. One per image: a second one is another pine, and it
is reached by mounting it rather than by holding two here.")

(defvar *here* nil
  "Where a name with no leading / is measured from. Nothing is the root; a session
binds it to wherever it stands, which is what makes a relative name mean something
in one and not leak into another.")

(defvar *builders* nil
  "What each package puts on the tree, in the order the packages were loaded. They
build separate branches and none of them reads another, so that order is the only
one there is and nothing has to state it.")

(define-condition absent (error)
  ((where :initarg :where :reader where))
  (:report (lambda (c s) (format s "nothing at ~a" (where c)))))

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

(defun here () (or *here* *root*))

(defun %rootedp (text)
  (and (plusp (length text)) (char= #\/ (char text 0))))

(defun %walk (n pieces makep)
  (loop :with at := n
        :for name :in (%names pieces)
        :do (setf at (and at (or (node:resolve at name)
                                 (and makep (node:make-child at name)))))
        :finally (return at)))

(defgeneric at (where &rest names)
  (:documentation "The node WHERE names, and NAMES on from there.

One question with one answer per kind of thing you can name a place with: a node
is itself, nothing is where you are, a string beginning with / is from the root
and one that does not is from where you are, and a path is what it spells.")
  (:method ((where node:node) &rest names) (%walk where names nil))
  (:method ((where null) &rest names) (%walk (here) names nil))
  (:method ((where string) &rest names)
    (%walk (if (%rootedp where) *root* (here)) (cons where names) nil)))

(defgeneric ensure (where &rest names)
  (:documentation "The same, making what is not there. That is the whole of the
difference between a read and a write: a read finds what stands and a write makes
what does not.")
  (:method ((where node:node) &rest names) (%walk where names t))
  (:method ((where null) &rest names) (%walk (here) names t))
  (:method ((where string) &rest names)
    (%walk (if (%rootedp where) *root* (here)) (cons where names) t)))

(defun put (where pieces value)
  (let ((n (apply #'ensure where (alexandria:ensure-list pieces))))
    (setf (node:contents n) value)
    n))

(defun erase (where &rest pieces)
  (let* ((names (%names pieces))
         (holder (apply #'at where (butlast names))))
    (when holder (node:erase-child holder (car (last names))))))

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
  "Every path below N, N's own first."
  (let (acc)
    (walk n (lambda (each) (push (node:full-name each) acc)))
    (nreverse acc)))

(pine/word:lends "ensure" "erase" "listing" "root")
