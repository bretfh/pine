(defpackage #:pine/fs/tree
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node))
  (:export
   #:*root* #:root #:make-root #:at #:ensure
   #:put #:erase #:walk #:listing #:paths
   #:split-name #:builder
   #:built))
(in-package #:pine/fs/tree)

(defvar *root* nil
  "The namespace this image is. One per image: a second one is another pine, and it
is reached by mounting it rather than by holding two here.")

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

(defun %from (where)
  (etypecase where
    (node:node where)
    (null *root*)))

(defun at (where &rest pieces)
  (loop :with n := (%from where)
        :for name :in (%names pieces)
        :do (setf n (and n (node:resolve n name)))
        :finally (return n)))

(defun ensure (where &rest pieces)
  (loop :with n := (%from where)
        :for name :in (%names pieces)
        :do (setf n (or (node:resolve n name) (node:make-child n name)))
        :finally (return n)))

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
