(in-package #:pine/fs)

(defvar *root* (make-instance 'dir :name nil)
  "The namespace this image is, there from the moment this loads. One per image: a
second one is another pine, and it is reached by mounting it rather than by holding
two here.")

(defvar *builders* nil)

(defvar *spelled* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "What a name spells, kept against the string itself: a name a compiled call site
hands over every time it runs is cut once.")

(define-condition absent (error)
  ((where :initarg :where :reader where))
  (:report (lambda (c s) (format s "nothing at ~a" (where c)))))

(define-condition not-a-place (error)
  ((given :initarg :given :reader given))
  (:report
   (lambda (c s)
     (format s "~s is not a place. A place is a name, a path or an entry; nothing ~
is what a place answers when there is none, so it cannot also be one."
             (given c)))))

(defun make-root ()
  (setf *root* (make-instance 'dir :name nil)))

(defun root () *root*)

(defun builder (thunk &key as)
  "Say this package puts something on the tree, and how: done on the root now, and
again on any root built later. AS names the builder, so saying it again replaces it."
  (let ((key (or as thunk)))
    (setf *builders* (append (cl:remove key *builders* :key #'car :test #'equal)
                             (list (cons key thunk))))
    (funcall thunk (root))
    thunk))

(defun built (&optional (root (root)))
  (dolist (each *builders* root) (funcall (cdr each) root)))

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

(defun %step (at name make)
  (and at (or (entry at name)
              (and make (make-entry at name make)))))

(defun %cut (text)
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
  (or (gethash text *spelled*)
      (setf (gethash text *spelled*) (%cut text))))

(defun %spelled (at text make last)
  "Walk what TEXT spells. Every piece but the last is made as a dir where MAKE says
to make at all; the last as LAST."
  (loop :for (name . more) :on (pieces text)
        :do (setf at (%step at name (if more (and make :dir) last)))
        :while at
        :finally (return at)))

(defun %walk (at names make)
  (loop :for (p . more) :on names
        :for last := (if more (and make :dir) make)
        :do (setf at (typecase p
                       (string (%spelled at p make last))
                       (symbol (%step at (string-downcase (symbol-name p)) last))
                       (t (%step at (princ-to-string p) last))))
            (when (null at) (return nil))
        :finally (return at)))

(defgeneric at (where &rest names)
  (:documentation "What WHERE names, and NAMES on from there, or nothing where none
stands. A name is from the root, a path is what it spells, an entry is itself.")
  (:method ((where standing) &rest names)
    (declare (dynamic-extent names))
    (%walk where names nil))
  (:method ((where null) &rest names)
    (declare (ignore names))
    (error 'not-a-place :given where))
  (:method ((where string) &rest names)
    (declare (dynamic-extent names))
    (if names
        (%walk (%spelled *root* where nil nil) names nil)
        (%spelled *root* where nil nil))))

(defgeneric ensure (where &rest names)
  (:documentation "The dir WHERE names, making dirs where none stand.")
  (:method ((where standing) &rest names)
    (declare (dynamic-extent names))
    (%walk where names :dir))
  (:method ((where null) &rest names)
    (declare (ignore names))
    (error 'not-a-place :given where))
  (:method ((where string) &rest names)
    (declare (dynamic-extent names))
    (if names
        (%walk (%spelled *root* where :dir :dir) names :dir)
        (%spelled *root* where :dir :dir))))

(defgeneric leaf (where &rest names)
  (:documentation "What WHERE names as somewhere to write: what stands there, or a
value made there, under dirs made on the way.")
  (:method ((where standing) &rest names)
    (declare (dynamic-extent names))
    (%walk where names :value))
  (:method ((where null) &rest names)
    (declare (ignore names))
    (error 'not-a-place :given where))
  (:method ((where string) &rest names)
    (declare (dynamic-extent names))
    (if names
        (%walk (%spelled *root* where :dir :dir) names :value)
        (%spelled *root* where :dir :value))))

(defun %erase (from names)
  (let* ((gone (car (last names)))
         (holder (and gone (%walk from (butlast names) nil))))
    (when holder (erase-entry holder gone))))

(defgeneric erase (where &rest pieces)
  (:documentation "Take off the last name in what WHERE and PIECES spell.")
  (:method ((where standing) &rest pieces) (%erase where (%names pieces)))
  (:method ((where string) &rest pieces)
    (%erase *root* (%names (cons where pieces))))
  (:method ((where null) &rest pieces)
    (declare (ignore pieces))
    (error 'not-a-place :given where)))

(defun walk (x function &key (depth -1) (into (complement #'livep)))
  "Depth first from X. Not into a live dir: what is under one belongs to the world."
  (funcall function x)
  (when (and (not (zerop depth)) (or (null into) (funcall into x)))
    (dolist (each (entries x))
      (walk each function :depth (1- depth) :into into)))
  x)

(defun paths (x)
  "Every path below X that this image keeps, X's own first."
  (let (acc)
    (walk x (lambda (each) (push (full-name each) acc)))
    (nreverse acc)))
