(in-package #:pine/fs)

(defvar *root* (make-instance 'mount :name nil))

(defvar *mounted* nil)

(defvar *spelled* (make-hash-table :test 'eq :weakness :key :synchronized t))

(define-condition absent (error)
  ((where :initarg :where :reader where))
  (:report (lambda (c s) (format s "nothing at ~a" (where c)))))

(define-condition not-a-place (error)
  ((given :initarg :given :reader given))
  (:report
   (lambda (c s)
     (format s "~s is not a place. A place is a name, a path or an child; nothing ~
is what a place answers when there is none, so it cannot also be one."
             (given c)))))

(defun root () *root*)

(defun %remember (where make)
  (let ((had (assoc where *mounted* :test #'equal)))
    (if had
        (setf (cdr had) make)
        (setf *mounted* (append *mounted* (list (cons where make)))))))

(defun make-root ()
  (setf *root* (make-instance 'mount :name nil))
  (loop :for (where . make) :in (copy-list *mounted*)
        :do (mount (funcall make) where))
  *root*)

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
  (and at (or (child at name)
              (and make (create at name make)))))

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
  (loop :for (name . more) :on (pieces text)
        :do (setf at (%step at name (if more (and make :mount) last)))
        :while at
        :finally (return at)))

(defun %walk (at names make)
  (loop :for (p . more) :on names
        :for last := (if more (and make :mount) make)
        :do (setf at (typecase p
                       (string (%spelled at p make last))
                       (symbol (%step at (string-downcase (symbol-name p)) last))
                       (t (%step at (princ-to-string p) last))))
            (when (null at) (return nil))
        :finally (return at)))

(defgeneric at (where &rest names)
  (:method ((where node) &rest names)
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

(defgeneric make (where kind &rest names)
  (:method ((where node) kind &rest names)
    (%walk where names kind))
  (:method ((where null) kind &rest names)
    (declare (ignore kind names))
    (error 'not-a-place :given where))
  (:method ((where string) kind &rest names)
    (if names
        (%walk (%spelled *root* where kind :mount) names kind)
        (%spelled *root* where kind kind))))

(defmethod mount ((x node) (where string))
  (let* ((names (pieces where))
         (last (car (last names))))
    (unless last (error 'not-a-place :given where))
    (let ((into (if (rest names) (%walk *root* (butlast names) :mount) *root*)))
      (setf (slot-value x 'name) last)
      (mount x into))))

(defmethod mount (what (where null))
  (declare (ignore what))
  (error 'not-a-place :given where))

(defmethod mount ((make function) where)
  (let ((x (mount (funcall make) where)))
    (unless *owner* (%remember (full-name x) make))
    x))

(defun %erase (from names)
  (let* ((gone (car (last names)))
         (holder (and gone (%walk from (butlast names) nil))))
    (when holder
      (let ((it (child holder gone)))
        (when it
          (setf *mounted* (cl:remove (full-name it) *mounted* :key #'car :test #'equal))))
      (unlink holder gone))))

(defgeneric erase (where &rest pieces)
  (:method ((where node) &rest pieces) (%erase where (%names pieces)))
  (:method ((where string) &rest pieces)
    (%erase *root* (%names (cons where pieces))))
  (:method ((where null) &rest pieces)
    (declare (ignore pieces))
    (error 'not-a-place :given where)))

(defun walk (x function &key (depth -1) (into (complement #'volatile-p)))
  (funcall function x)
  (when (and (not (zerop depth)) (or (null into) (funcall into x)))
    (dolist (each (children x))
      (walk each function :depth (1- depth) :into into)))
  x)

(defun paths (x)
  (let (acc)
    (walk x (lambda (each) (push (full-name each) acc)))
    (nreverse acc)))
