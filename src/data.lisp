(defpackage #:pine.data
  (:use #:cl)
  (:export #:syntax #:data
           #:fn
           #:keys #:vals #:fold #:emit
           #:pairs #:cells #:plist #:split #:joined
           #:table #:at #:put #:claim #:clear #:all
           #:serialize #:deserialize))

(in-package #:pine.data)

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

(defmethod make-load-form ((m fset:map) &optional environment)
  (declare (ignore environment))
  `(fset:convert 'fset:map ',(fset:convert 'list m)))

(defmethod make-load-form ((s fset:seq) &optional environment)
  (declare (ignore environment))
  `(fset:convert 'fset:seq ',(fset:convert 'list s)))

(defmethod make-load-form ((s fset:set) &optional environment)
  (declare (ignore environment))
  `(fset:convert 'fset:set ',(fset:convert 'list s)))

(defgeneric emit (value stream)
  (:documentation "Write VALUE to STREAM in the syntax DESERIALIZE reads back."))

(defmethod emit ((value t) stream)
  (prin1 value stream))

(defun %between (stream first)
  "A space before every element but the first. Answers whether the next one is
still the first."
  (unless first (write-char #\Space stream))
  nil)

(defmethod emit ((value fset:map) stream)
  (write-char #\{ stream)
  (let ((first t))
    (fset:do-map (key inner value)
      (setf first (%between stream first))
      (emit key stream)
      (write-char #\Space stream)
      (emit inner stream)))
  (write-char #\} stream))

(defmethod emit ((value fset:seq) stream)
  (write-char #\[ stream)
  (let ((first t))
    (fset:do-seq (element value)
      (setf first (%between stream first))
      (emit element stream)))
  (write-char #\] stream))

(defmethod emit ((value fset:set) stream)
  (write-string "#{" stream)
  (let ((first t))
    (fset:do-set (element value)
      (setf first (%between stream first))
      (emit element stream)))
  (write-char #\} stream))

(defun serialize (value)
  "VALUE as a string that DESERIALIZE reads back to an equal value.

Escaped but not *PRINT-READABLY*: under that, SBCL writes a base-string as
#A((2) BASE-CHAR . \"16\"), which reads back but is what a person sees when the
shell prints an answer. Escaping is what the round trip actually needs."
  (with-output-to-string (out)
    (let ((*print-readably* nil)
          (*print-escape* t)
          (*print-circle* t)
          (*package* (find-package :keyword)))
      (emit value out))))

(defun deserialize (string &optional (readtable 'data))
  "The value SERIALIZE wrote. Reads only: nothing in STRING is evaluated."
  (let ((*read-eval* nil)
        (*package* (find-package :cl-user))
        (*readtable* (named-readtables:find-readtable readtable)))
    (values (read-from-string string))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %arglist (args)
    (if (and (consp args) (eq (first args) 'fset:seq))
        (rest args)
        args)))

(defmacro fn (args &body body)
  "LAMBDA with a seq arglist: (fn [buf text] ...)."
  `(lambda ,(%arglist args) ,@body))

(defgeneric keys (collection)
  (:documentation "COLLECTION's keys, as a list."))

(defmethod keys ((collection t)) (error "~s has no keys." collection))
(defmethod keys ((collection fset:map))
  (fset:convert 'list (fset:domain collection)))
(defmethod keys ((collection fset:set))
  (fset:convert 'list collection))

(defgeneric vals (collection)
  (:documentation "COLLECTION's values, as a list, in KEYS order."))

(defmethod vals ((collection t)) (error "~s has no values." collection))
(defmethod vals ((collection fset:map))
  (mapcar (lambda (key) (fset:lookup collection key)) (keys collection)))
(defmethod vals ((collection fset:seq)) (fset:convert 'list collection))
(defmethod vals ((collection fset:set)) (fset:convert 'list collection))

(defgeneric fold (collection initial function)
  (:documentation "Reduce over COLLECTION from INITIAL. FUNCTION takes
(acc key value) for a map and (acc element) for a seq or a set."))

(defmethod fold ((collection t) initial function)
  (declare (ignore initial function))
  (error "~s cannot be folded." collection))

(defmethod fold ((collection fset:map) initial function)
  (let ((acc initial))
    (fset:do-map (key value collection)
      (setf acc (funcall function acc key value)))
    acc))

(defmethod fold ((collection fset:seq) initial function)
  (let ((acc initial))
    (fset:do-seq (element collection)
      (setf acc (funcall function acc element)))
    acc))

(defmethod fold ((collection fset:set) initial function)
  (let ((acc initial))
    (fset:do-set (element collection)
      (setf acc (funcall function acc element)))
    acc))

(defun pairs (plist &key (key #'identity))
  "A map from a flat key/value list. KEY names each key on the way in."
  (loop :with out = (fset:empty-map)
        :for (this value) :on plist :by #'cddr
        :do (setf out (fset:with out (funcall key this) value))
        :finally (return out)))

(defun cells (alist &key (key #'identity))
  "A map from a list of (key . value)."
  (loop :with out = (fset:empty-map)
        :for cell :in alist
        :do (setf out (fset:with out (funcall key (car cell)) (cdr cell)))
        :finally (return out)))

(defun plist (map)
  "MAP as a flat key/value list."
  (let ((out nil))
    (fset:do-map (key value map)
      (push key out)
      (push value out))
    (nreverse out)))

(defun split (text &key (on #\Newline) limit (empties t))
  "TEXT in pieces, cut at every ON.

ON is a character or a sequence of them, any of which cuts. LIMIT stops cutting
after that many, leaving the rest of TEXT whole. EMPTIES decides whether a cut
beside another answers an empty piece."
  (let ((cuts (if (characterp on) (list on) (coerce on 'list)))
        (pieces nil)
        (start 0)
        (made 0))
    (dotimes (i (length text))
      (when (and (member (char text i) cuts)
                 (or (null limit) (< made limit)))
        (push (subseq text start i) pieces)
        (setf start (1+ i))
        (incf made)))
    (push (subseq text start) pieces)
    (let ((all (nreverse pieces)))
      (if empties all (remove "" all :test #'string=)))))

(defun joined (pieces &key (with #\Newline))
  "PIECES as one string, WITH between them."
  (with-output-to-string (out)
    (loop :for piece :in pieces
          :for first = t :then nil
          :do (unless first (write-char with out))
              (write-string piece out))))

(defun table ()
  "A name-to-thing table nothing has to lock to touch: a persistent map in an
atomic cell."
  (sento.atomic:make-atomic-reference :value (fset:empty-map)))

(defun all (table)
  "Everything in TABLE, as the map it is."
  (sento.atomic:atomic-get table))

(defun at (table key &optional default)
  (let ((value (fset:lookup (all table) key)))
    (if (null value) default value)))

(defun put (table key value)
  "Put VALUE in TABLE at KEY, or take KEY out when VALUE is nothing."
  (sento.atomic:atomic-swap table
                            (lambda (map)
                              (if (null value)
                                  (fset:less map key)
                                  (fset:with map key value))))
  value)

(defun claim (table key value)
  "Put VALUE at KEY unless something is there already, and answer whatever is
there afterwards, so the loser of a race gets the winner's object."
  (fset:lookup (sento.atomic:atomic-swap
                table
                (lambda (map)
                  (if (fset:lookup map key) map (fset:with map key value))))
               key))

(defun clear (table)
  "Empty TABLE."
  (sento.atomic:atomic-swap table (lambda (map) (declare (ignore map))
                                    (fset:empty-map)))
  nil)
