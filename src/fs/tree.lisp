(defpackage #:pine/fs/tree
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:commit #:pine/fs/commit))
  (:export #:*root* #:root #:make-root #:at #:ensure #:put #:erase #:walk
           #:listing #:paths #:split-name #:absent
           #:change #:was #:revert #:diff
           #:id-of #:node-for-id #:identify #:forget-ids))
(in-package #:pine/fs/tree)

(defvar *root* nil
  "The namespace this image is. One per image: a second one is another pine, and it
is reached by mounting it rather than by holding two here.")

(defvar *ids* (make-hash-table :test 'eql))
(defvar *by-node* (make-hash-table :test 'eq))
(defvar *counter* 0)
(defvar *naming* (bordeaux-threads:make-lock "pine-ids"))

(define-condition absent (error)
  ((where :initarg :where :reader where))
  (:report (lambda (c s) (format s "nothing at ~a" (where c)))))

(defun make-root ()
  (let ((n (node:make nil :savedp nil)))
    (setf *root* n)
    (commit:clear)
    (forget-ids)
    (identify n)
    n))

(defun root () *root*)

(defun identify (n)
  "The number this node crosses the wire as. Under a lock: two hash tables cannot be
swapped as one, and a counter that hands the same number to two nodes is a frontend
clicking the wrong one."
  (bordeaux-threads:with-lock-held (*naming*)
    (or (gethash n *by-node*)
        (let ((it (d:swap *counter* #'1+)))
          (setf (gethash n *by-node*) it
                (gethash it *ids*) n)
          it))))

(defun id-of (n)
  (bordeaux-threads:with-lock-held (*naming*) (gethash n *by-node*)))

(defun node-for-id (it)
  (bordeaux-threads:with-lock-held (*naming*) (gethash it *ids*)))

(defun forget-ids ()
  (bordeaux-threads:with-lock-held (*naming*)
    (clrhash *ids*)
    (clrhash *by-node*)
    (setf *counter* 0))
  t)

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
        :finally (progn (identify n) (return n))))

(defun put (where pieces value)
  (let ((n (apply #'ensure where (alexandria:ensure-list pieces))))
    (setf (node:contents n) value)
    n))

(defun %placed (pairs)
  (loop :for (place . value) :in (d:pairs pairs)
        :collect (cons (ensure nil (alexandria:ensure-list place)) value)))

(defun %by-name (placed)
  (loop :with out := (d:no-map)
        :for (n . value) :in placed
        :do (setf out (d:with out (node:full-name n) value))
        :finally (return out)))

(defun change (pairs &key when)
  (let ((placed (%placed pairs)))
    (cond ((every (lambda (each) (node:storedp (car each))) placed)
           (let ((moved (commit:change (%by-name placed)
                                       :when (and when (%by-name (%placed when))))))
             (when (and moved (not (d:emptyp moved)))
               (commit:writing
                 (loop :for (n . nil) :in placed
                       :when (d:contains moved (node:full-name n))
                         :do (node:moved n))))
             moved))
          (t (commit:writing
               (loop :for (n . value) :in placed
                     :do (setf (node:contents n) value))
               (%by-name placed))))))

(defun was (place n)
  (commit:was (node:full-name (ensure nil (alexandria:ensure-list place))) n))

(defun revert (place &optional (n 0))
  (let ((it (ensure nil (alexandria:ensure-list place))))
    (setf (node:contents it) (commit:was (node:full-name it) n))))

(defun diff (n)
  (let ((then (commit:root-at n))
        (now (commit:held))
        (out (d:no-map)))
    (when then
      (d:do-map (place value now)
        (unless (d:same value (d:at then place))
          (setf out (d:with out place (list (d:at then place) value)))))
      (d:do-map (place value then)
        (unless (d:contains now place)
          (setf out (d:with out place (list value nil))))))
    out))

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
