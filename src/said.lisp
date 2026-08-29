(defpackage #:pine/said
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export
   #:said #:took #:sayablep #:tags))
(in-package #:pine/said)

(defparameter +tags+ '(:map :seq :set :quoted)
  "The words a collection is written down under. A list of somebody's own that
begins with one of them is written under :QUOTED, so what comes back is the list
that went in.")

(defun tags () +tags+)

(defun sayablep (value)
  "Whether VALUE has a spelling. What has none is an object standing for itself --
a widget, a node, a condition -- and the answer to one of those is a place under
it that does, not a nil where a value was expected."
  (typecase value
    ((or null number string character keyword) t)
    (symbol t)
    (cons (and (sayablep (car value)) (sayablep (cdr value))))
    ((and vector (not string)) (every #'sayablep value))
    (t (and (d:collectionp value)
            (every #'sayablep (d:as :list (d:keys value)))
            (every #'sayablep (d:as :list (d:vals value)))))))

(defun said (value)
  "VALUE as plain data: lists, and the words above for what is not a list.

The shape a value takes when it leaves this image, whether it is going to a file
or to somebody on the other end of a socket. What that shape is written as is
theirs -- the store prints it, a wire renders it -- and neither of them has to
know what an fset map is."
  (cond ((d:mapp value)
         (list* :map (loop :for (k . v) :in (d:pairs value)
                           :append (list (said k) (said v)))))
        ((d:setp value) (list* :set (mapcar #'said (d:as :list value))))
        ((d:seqp value) (list* :seq (mapcar #'said (d:as :list value))))
        ((and (consp value) (member (car value) +tags+))
         (list :quoted (cons (car value) (said (cdr value)))))
        ((consp value) (cons (said (car value)) (said (cdr value))))
        (t value)))

(defun took (form)
  "The value FORM spells. The other half of SAID: what comes back is what went
out, so a map is a map again and a list that looked like one is still a list."
  (cond ((and (consp form) (eq :map (car form)))
         (loop :with m := (d:no-map)
               :for (k v) :on (rest form) :by #'cddr
               :do (setf m (d:with m (took k) (took v)))
               :finally (return m)))
        ((and (consp form) (eq :seq (car form)))
         (d:as :seq (mapcar #'took (rest form))))
        ((and (consp form) (eq :set (car form)))
         (d:as :set (mapcar #'took (rest form))))
        ((and (consp form) (eq :quoted (car form)))
         (let ((it (second form)))
           (cons (car it) (took (cdr it)))))
        ((consp form) (cons (took (car form)) (took (cdr form))))
        (t form)))
