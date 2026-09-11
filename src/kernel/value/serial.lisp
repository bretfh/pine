(defpackage #:pine/serial
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export
   #:encode #:decode #:encodablep #:tags))
(in-package #:pine/serial)

(defparameter +tags+ '(:map :seq :set :quoted))

(defun tags () +tags+)

(defun encodablep (value)
  (typecase value
    ((or null number string character keyword) t)
    (symbol t)
    (cons (and (encodablep (car value)) (encodablep (cdr value))))
    ((and vector (not string)) (every #'encodablep value))
    (t (and (d:collectionp value)
            (every #'encodablep (d:as :list (d:keys value)))
            (every #'encodablep (d:as :list (d:vals value)))))))

(defun encode (value)
  (cond ((d:mapp value)
         (list* :map (loop :for (k . v) :in (d:pairs value)
                           :append (list (encode k) (encode v)))))
        ((d:setp value) (list* :set (mapcar #'encode (d:as :list value))))
        ((d:seqp value) (list* :seq (mapcar #'encode (d:as :list value))))
        ((and (consp value) (member (car value) +tags+))
         (list :quoted (cons (car value) (encode (cdr value)))))
        ((consp value) (cons (encode (car value)) (encode (cdr value))))
        (t value)))

(defun decode (form)
  (cond ((and (consp form) (eq :map (car form)))
         (loop :with m := (d:no-map)
               :for (k v) :on (rest form) :by #'cddr
               :do (setf m (d:with m (decode k) (decode v)))
               :finally (return m)))
        ((and (consp form) (eq :seq (car form)))
         (d:as :seq (mapcar #'decode (rest form))))
        ((and (consp form) (eq :set (car form)))
         (d:as :set (mapcar #'decode (rest form))))
        ((and (consp form) (eq :quoted (car form)))
         (let ((it (second form)))
           (cons (car it) (decode (cdr it)))))
        ((consp form) (cons (decode (car form)) (decode (cdr form))))
        (t form)))
