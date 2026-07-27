(defpackage #:pine.data
  (:use #:cl)
  (:export #:syntax #:data
           #:fn
           #:keys #:vals #:fold
           #:serialize #:deserialize))

(in-package #:pine.data)

;;;; Every structure pine holds is a persistent map, seq or set. The engine's
;;;; one question is whether a value changed, and on these an unchanged subtree
;;;; is the same object, so the answer is a pointer compare and a walk descends
;;;; only into what moved. A plist answers it in linear time and is blind to
;;;; sharing.
;;;;
;;;; The readtable is scoped: a file says (named-readtables:in-readtable
;;;; pine.data:syntax) to get the literals, and nothing loaded beside pine sees
;;;; them.

(defun %read-until (stream close)
  "Read forms from STREAM until CLOSE, which is consumed."
  (loop :for ch = (peek-char t stream t nil t)
        :until (char= ch close)
        :collect (read stream t nil t)
        :finally (read-char stream t nil t)))

(defun %pairs (forms what)
  "FORMS as a list of (key value), for an fset:map construction form."
  (when (oddp (length forms))
    (error "Odd number of forms in ~a: ~s has no value." what (car (last forms))))
  (loop :for (k v) :on forms :by #'cddr :collect (list k v)))

(defun read-map (stream char)
  "{k v ...} -> a persistent map. Keys and values are evaluated."
  (declare (ignore char))
  (cons 'fset:map (%pairs (%read-until stream #\}) "a map literal")))

(defun read-seq (stream char)
  "[a b ...] -> a persistent seq. Elements are evaluated."
  (declare (ignore char))
  (cons 'fset:seq (%read-until stream #\])))

(defun read-set (stream char arg)
  "#{a b ...} -> a persistent set. Elements are evaluated."
  (declare (ignore char arg))
  (cons 'fset:set (%read-until stream #\})))

(defun read-close (stream char)
  "Signal on a closing delimiter with no opener, as the standard reader does
for a stray right parenthesis."
  (declare (ignore stream))
  (error "Unmatched ~c." char))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char #\{ #'read-map)
  (:macro-char #\} #'read-close)
  (:macro-char #\[ #'read-seq)
  (:macro-char #\] #'read-close)
  (:dispatch-macro-char #\# #\{ #'read-set))

;;;; Serialization reads with its own rules: a literal builds its object at read
;;;; time and every other form is data. Nothing the store holds is ever
;;;; evaluated to be read back.

(defun read-map-data (stream char)
  (declare (ignore char))
  (let ((forms (%read-until stream #\})))
    (when (oddp (length forms))
      (error "Odd number of forms in a serialized map."))
    (loop :with m = (fset:empty-map)
          :for (k v) :on forms :by #'cddr
          :do (setf m (fset:with m k v))
          :finally (return m))))

(defun read-seq-data (stream char)
  (declare (ignore char))
  (fset:convert 'fset:seq (%read-until stream #\])))

(defun read-set-data (stream char arg)
  (declare (ignore char arg))
  (fset:convert 'fset:set (%read-until stream #\})))

(named-readtables:defreadtable data
  (:merge :standard)
  (:macro-char #\{ #'read-map-data)
  (:macro-char #\} #'read-close)
  (:macro-char #\[ #'read-seq-data)
  (:macro-char #\] #'read-close)
  (:dispatch-macro-char #\# #\{ #'read-set-data))

;;;; A literal read into a fasl has to be dumpable, which is what a path with a
;;;; constraint asks for: the constraint is a map, built when the file is read.

(defmethod make-load-form ((m fset:map) &optional environment)
  (declare (ignore environment))
  `(fset:convert 'fset:map ',(fset:convert 'list m)))

(defmethod make-load-form ((s fset:seq) &optional environment)
  (declare (ignore environment))
  `(fset:convert 'fset:seq ',(fset:convert 'list s)))

(defmethod make-load-form ((s fset:set) &optional environment)
  (declare (ignore environment))
  `(fset:convert 'fset:set ',(fset:convert 'list s)))

(defun %emit (value stream)
  (cond
    ((fset:map? value)
     (write-char #\{ stream)
     (let ((first t))
       (fset:do-map (k v value)
         (if first (setf first nil) (write-char #\Space stream))
         (%emit k stream)
         (write-char #\Space stream)
         (%emit v stream)))
     (write-char #\} stream))
    ((fset:seq? value)
     (write-char #\[ stream)
     (let ((first t))
       (fset:do-seq (x value)
         (if first (setf first nil) (write-char #\Space stream))
         (%emit x stream)))
     (write-char #\] stream))
    ((fset:set? value)
     (write-string "#{" stream)
     (let ((first t))
       (fset:do-set (x value)
         (if first (setf first nil) (write-char #\Space stream))
         (%emit x stream)))
     (write-char #\} stream))
    (t (prin1 value stream))))

(defun serialize (value)
  "VALUE as a string that DESERIALIZE reads back to an equal value.

Printed with *PACKAGE* the keyword package, where no other package's symbols
are accessible, so every symbol but a keyword carries its package and reads
back as itself whatever package is current later."
  (with-output-to-string (out)
    (let ((*print-readably* t)
          (*print-circle* t)
          (*package* (find-package :keyword)))
      (%emit value out))))

(defun deserialize (string)
  "The value SERIALIZE wrote. Reads only: nothing in STRING is evaluated."
  (let ((*read-eval* nil)
        (*package* (find-package :cl-user))
        (*readtable* (named-readtables:find-readtable 'data)))
    (values (read-from-string string))))

;;;; The vocabulary a caller needs that fset does not already give in this
;;;; shape. Everything else is fset's own: with, less, lookup, contains?, size.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %arglist (args)
    "ARGS as a lambda list. A seq literal reads as a construction form, so
[a b] arrives here as (fset:seq a b)."
    (if (and (consp args) (eq (first args) 'fset:seq))
        (rest args)
        args)))

(defmacro fn (args &body body)
  "LAMBDA with a seq arglist: (fn [buf text] ...). An ordinary lambda list
also works, so (fn () ...) reads as it looks."
  `(lambda ,(%arglist args) ,@body))

(defun keys (collection)
  "A list of COLLECTION's keys: a map's domain, or a set's elements."
  (cond ((fset:map? collection) (fset:convert 'list (fset:domain collection)))
        ((fset:set? collection) (fset:convert 'list collection))
        (t (error "~s has no keys." collection))))

(defun vals (collection)
  "A list of COLLECTION's values, in the order KEYS answers for a map."
  (cond ((fset:map? collection)
         (mapcar (lambda (k) (fset:lookup collection k)) (keys collection)))
        ((fset:seq? collection) (fset:convert 'list collection))
        ((fset:set? collection) (fset:convert 'list collection))
        (t (error "~s has no values." collection))))

(defun fold (collection initial function)
  "Reduce over COLLECTION from INITIAL. FUNCTION takes (acc key value) for a
map and (acc element) for a seq or a set."
  (let ((acc initial))
    (cond ((fset:map? collection)
           (fset:do-map (k v collection)
             (setf acc (funcall function acc k v))))
          ((fset:seq? collection)
           (fset:do-seq (x collection)
             (setf acc (funcall function acc x))))
          ((fset:set? collection)
           (fset:do-set (x collection)
             (setf acc (funcall function acc x))))
          (t (error "~s cannot be folded." collection)))
    acc))
