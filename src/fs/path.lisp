(defpackage #:pine/fs/path
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree))
  (:export
   #:path #:pathp #:whole #:leaf #:patternp #:matching))
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
    (write-string (whole p) stream)))

(defun pathp (x) (typep x 'path))

(defun patternp (p)
  "Whether this path names one place or a shape of them: * is any one name, ** is
any run of them, and ?name is one that is captured."
  (and (pathp p) (some (lambda (s) (not (typep s 'literal))) (segments p))))

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

(defgeneric segment-text (segment)
  (:documentation "A segment as it was written, so a path prints as it reads.")
  (:method ((s segment)) (value s))
  (:method ((s binding)) (concatenate 'string "?" (value s))))

(defun whole (p)
  "The path as it is written: /a/b/c."
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

(defun %spelled (p) (mapcar #'value (segments p)))

(defmethod tree:at ((p path) &rest names)
  "A path is one more thing you can name a place with, so it is one more method
rather than a second way to walk the tree."
  (apply #'tree:at (tree:root) (append (%spelled p) names)))

(defmethod tree:ensure ((p path) &rest names)
  (apply #'tree:ensure (tree:root) (append (%spelled p) names)))

(defmethod tree:erase ((p path) &rest pieces)
  (apply #'tree:erase (tree:root) (append (%spelled p) pieces)))

(defun matching (pattern &optional (where (tree:root)))
  (let ((found nil))
    (tree:walk where
               (lambda (each)
                 (when (match pattern (path (node:full-name each)))
                   (push each found))))
    (nreverse found)))

