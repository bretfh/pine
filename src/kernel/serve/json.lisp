(defpackage #:pine/serve/json
  (:use #:cl)
  (:local-nicknames (#:serial #:pine/serial))
  (:export
   #:as-json #:from-json #:as-verb #:render #:parse))
(in-package #:pine/serve/json)

(defparameter +kinds+ '(("map" . :map) ("seq" . :seq) ("set" . :set)))

(defun %keyword-text (k) (format nil ":~a" (string-downcase (symbol-name k))))

(defun %keywordp (text)
  (and (stringp text) (plusp (length text)) (char= #\: (char text 0))))

(defun %as-keyword (text)
  (intern (string-upcase (subseq text 1)) :keyword))

(defun as-json (form)
  (cond ((null form) 'null)
        ((eq form t) t)
        ((keywordp form) (%keyword-text form))
        ((symbolp form) (error "~s is a symbol; there is no spelling for one here."
                               form))
        ((characterp form) (string form))
        ((or (numberp form) (stringp form)) form)
        ((and (consp form) (eq :map (car form)))
         (let ((out (make-hash-table :test 'equal)))
           (setf (gethash "map" out)
                 (coerce (loop :for (k v) :on (rest form) :by #'cddr
                               :collect (vector (as-json k) (as-json v)))
                         'vector))
           out))
        ((and (consp form) (member (car form) '(:seq :set)))
         (let ((out (make-hash-table :test 'equal)))
           (setf (gethash (string-downcase (symbol-name (car form))) out)
                 (coerce (mapcar #'as-json (rest form)) 'vector))
           out))
        ((and (consp form) (eq :quoted (car form)))
         (coerce (mapcar #'as-json (second form)) 'vector))
        ((consp form) (coerce (mapcar #'as-json form) 'vector))
        (t (error "~s has no spelling here." form))))

(defun %tagged (it)
  (when (and (hash-table-p it) (= 1 (hash-table-count it)))
    (loop :for (word . kind) :in +kinds+
          :for found := (nth-value 1 (gethash word it))
          :when found :do (return (values kind (gethash word it))))))

(defun from-json (it)
  (cond ((eq it 'null) nil)
        ((null it) nil)
        ((eq it t) t)
        ((%keywordp it) (%as-keyword it))
        ((or (numberp it) (stringp it)) it)
        ((hash-table-p it)
         (multiple-value-bind (kind held) (%tagged it)
           (unless kind
             (error "~s names no kind: a map, a seq or a set is an object of one ~
                     word, and anything else is an array."
                    (loop :for k :being :the :hash-keys :of it :collect k)))
           (case kind
             (:map (list* :map (loop :for pair :across held
                                     :append (list (from-json (aref pair 0))
                                                   (from-json (aref pair 1))))))
             (t (list* kind (map 'list #'from-json held))))))
        ((vectorp it)
         (let ((all (map 'list #'from-json it)))
           (if (member (car all) (serial:tags))
               (list :quoted all)
               all)))
        (t (error "~s is not something this speaks." it))))

(defun as-verb (word)
  (when word
    (let ((text (string-downcase (princ-to-string word))))
      (intern (string-upcase (string-left-trim ":" text)) :keyword))))

(defun render (value)
  (com.inuoe.jzon:stringify (as-json (serial:encode value))))

(defun parse (text)
  (serial:decode (from-json (com.inuoe.jzon:parse text))))
