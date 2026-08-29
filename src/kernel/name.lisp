(defpackage #:pine/kernel/name
  (:use #:cl)
  (:shadow #:parse)
  (:export
   #:name #:namep #:whole #:pieces #:spelled #:leaf #:under
   #:rootp #:beneath-p #:matches #:parse #:split))
(in-package #:pine/kernel/name)

(defvar +separator+ #\/)

(defclass piece ()
  ((said :initarg :said :reader said)))

(defclass plain (piece) ())
(defclass any (piece) ())
(defclass deep (piece) ())
(defclass bound (piece) ())

(defclass name ()
  ((pieces :initarg :pieces :reader pieces))
  (:documentation "Where something stands, as a value.

A name is not a place and holds no reference to one: it is what you say to reach
one, so the same name means the same thing in this image, in a config, on a
socket, and against a machine that is not running yet."))

(defmethod print-object ((n name) stream)
  (print-unreadable-object (n stream :type nil)
    (write-string (whole n) stream)))

(defun namep (x) (typep x 'name))

(defun split (text)
  "TEXT cut at every separator, with nothing between two of them dropped. A name
written with a trailing or a doubled separator is the name without it, because
/dev/ and /dev are the same place and answering differently would make the
spelling matter."
  (loop :with out := nil
        :with piece := (make-string-output-stream)
        :for ch :across (princ-to-string text)
        :do (if (char= ch +separator+)
                (let ((said (get-output-stream-string piece)))
                  (when (plusp (length said)) (push said out)))
                (write-char ch piece))
        :finally (let ((said (get-output-stream-string piece)))
                   (when (plusp (length said)) (push said out))
                   (return (nreverse out)))))

(defun %piece (text)
  (cond ((string= text "**") (make-instance 'deep :said text))
        ((string= text "*") (make-instance 'any :said text))
        ((and (> (length text) 1) (char= #\? (char text 0)))
         (make-instance 'bound :said (subseq text 1)))
        (t (make-instance 'plain :said text))))

(defun parse (&rest said)
  "A name out of whatever it was written as: strings, other names, or anything
that prints as one piece. Every name is from the root, because there is nowhere
else to be measured from."
  (make-instance
   'name
   :pieces (loop :for each :in said
                 :append (typecase each
                           (name (pieces each))
                           (piece (list each))
                           (null nil)
                           (string (mapcar #'%piece (split each)))
                           (t (mapcar #'%piece
                                      (split (princ-to-string each))))))))

(defun name (&rest said) (apply #'parse said))

(defgeneric written (piece)
  (:documentation "A piece as it was said, so a name prints as it reads.")
  (:method ((p piece)) (said p))
  (:method ((p bound)) (concatenate 'string "?" (said p))))

(defun rootp (n) (null (pieces n)))

(defun whole (n)
  (if (rootp n) "/" (format nil "~{/~a~}" (mapcar #'written (pieces n)))))

(defun spelled (n) (mapcar #'said (pieces n)))

(defun leaf (n) (let ((last (car (last (pieces n))))) (and last (said last))))

(defun under (n)
  "The name this one stands under. The root stands under nothing and answers
itself, so walking upward always ends."
  (if (rootp n) n (make-instance 'name :pieces (butlast (pieces n)))))

(defun beneath-p (over n)
  (let ((a (pieces over)) (b (pieces n)))
    (and (<= (length a) (length b))
         (every (lambda (x y) (equal (said x) (said y)))
                a (subseq b 0 (length a))))))

(defun matches (pattern n)
  "Whether N is one of the names PATTERN spells, and what its ?pieces were bound
to. * is one piece, ** is any number of them, and ?x is one piece under a name.

Answers the bindings where there are any and T where the match took none, so a
match is never mistaken for a miss by whoever only asked whether."
  (let ((bound nil))
    (labels ((walk (ps ss)
               (cond ((and (null ps) (null ss)) t)
                     ((null ps) nil)
                     ((typep (first ps) 'deep)
                      (or (walk (rest ps) ss) (and ss (walk ps (rest ss)))))
                     ((null ss) nil)
                     ((typep (first ps) 'any) (walk (rest ps) (rest ss)))
                     ((typep (first ps) 'bound)
                      (push (cons (intern (string-upcase (said (first ps))) :keyword)
                                  (said (first ss)))
                            bound)
                      (walk (rest ps) (rest ss)))
                     ((equal (said (first ps)) (said (first ss)))
                      (walk (rest ps) (rest ss)))
                     (t nil))))
      (and (walk (pieces pattern) (pieces n))
           (or (nreverse bound) t)))))
