(defpackage #:pine/fs/path
  (:use #:cl)
  (:shadow #:parse)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree))
  (:export #:path #:pathp #:segments #:text #:parse #:leaf #:parent #:rootp #:match #:prefixp
           #:literal #:binding #:any #:deep #:segment #:segment-text #:kind
           #:value #:at #:ensure #:contents #:matching #:erase))
(in-package #:pine/fs/path)

(defclass segment ()
  ((value :initarg :value :reader value)))

(defclass literal (segment) ())
(defclass binding (segment) ())
(defclass any (segment) ())
(defclass deep (segment) ())

(defclass path ()
  ((segments :initarg :segments :reader segments)))

(defmethod print-object ((p path) stream)
  (print-unreadable-object (p stream :type nil)
    (write-string (text p) stream)))

(defun pathp (x) (typep x 'path))

(defgeneric kind (segment)
  (:method ((s literal)) :literal)
  (:method ((s binding)) :binding)
  (:method ((s any)) :any)
  (:method ((s deep)) :deep))

(defun %segment (text)
  (cond ((string= text "**") (make-instance 'deep :value text))
        ((string= text "*") (make-instance 'any :value text))
        ((and (> (length text) 1) (char= #\? (char text 0)))
         (make-instance 'binding :value (subseq text 1)))
        (t (make-instance 'literal :value text))))

(defun path (&rest pieces)
  (make-instance 'path
                 :segments (loop :for p :in pieces
                                 :append (typecase p
                                           (path (segments p))
                                           (segment (list p))
                                           (string (mapcar #'%segment
                                                           (tree:split-name p)))
                                           (t (list (%segment (princ-to-string p))))))))

(defun parse (text) (path text))

(defgeneric segment-text (segment)
  (:documentation "A segment as it was written, so a path prints as it reads.")
  (:method ((s segment)) (value s))
  (:method ((s binding)) (concatenate 'string "?" (value s))))

(defun text (p)
  (if (rootp p)
      "/"
      (format nil "~{/~a~}" (mapcar #'segment-text (segments p)))))

(defun rootp (p) (null (segments p)))

(defun leaf (p) (let ((s (car (last (segments p))))) (and s (value s))))

(defun parent (p)
  (make-instance 'path :segments (butlast (segments p))))

(defun prefixp (prefix p)
  (let ((a (segments prefix)) (b (segments p)))
    (and (<= (length a) (length b))
         (every (lambda (x y) (equal (value x) (value y))) a (subseq b 0 (length a))))))

(defun match (pattern subject)
  (let ((bound nil))
    (labels ((walk (ps ss)
               (cond ((and (null ps) (null ss)) t)
                     ((null ps) nil)
                     ((typep (first ps) 'deep)
                      (or (walk (rest ps) ss)
                          (and ss (walk ps (rest ss)))))
                     ((null ss) nil)
                     ((typep (first ps) 'any) (walk (rest ps) (rest ss)))
                     ((typep (first ps) 'binding)
                      (push (cons (intern (string-upcase (value (first ps))))
                                  (value (first ss)))
                            bound)
                      (walk (rest ps) (rest ss)))
                     ((equal (value (first ps)) (value (first ss)))
                      (walk (rest ps) (rest ss)))
                     (t nil))))
      (and (walk (segments pattern) (segments subject))
           (or (nreverse bound) t)))))

(defun at (p &optional (where (tree:root)))
  (apply #'tree:at where (mapcar #'value (segments p))))

(defun ensure (p &optional (where (tree:root)))
  (apply #'tree:ensure where (mapcar #'value (segments p))))

(defun erase (p &optional (where (tree:root)))
  (apply #'tree:erase where (mapcar #'value (segments p))))

(defun contents (p &optional (where (tree:root)))
  (let ((n (at p where)))
    (and n (node:contents n))))

(defun (setf contents) (value p &optional (where (tree:root)))
  (setf (node:contents (ensure p where)) value))

(defun matching (pattern &optional (where (tree:root)))
  (let ((found nil))
    (tree:walk where
               (lambda (each)
                 (when (match pattern (parse (node:full-name each)))
                   (push each found))))
    (nreverse found)))

(pine/word:lends "leaf" '("path" "parse"))
