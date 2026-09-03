(defpackage #:pine/fs/path
  (:use #:cl)
  (:local-nicknames (#:fs #:pine/fs))
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
                                                           (fs:split-name p)))
                                           (t (list (%segment (princ-to-string p))))))))

(defgeneric segment-text (segment)
  (:method ((s segment)) (value s))
  (:method ((s binding)) (concatenate 'string "?" (value s))))

(defun whole (p)
  "The path as it is written: /a/b/c."
  (if (rootp p)
      "/"
      (format nil "~{/~a~}" (mapcar #'segment-text (segments p)))))

(defun rootp (p) (null (segments p)))

(defun leaf (p) (let ((s (car (last (segments p))))) (and s (value s))))

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

(defmethod fs:at ((p path) &rest names)
  (apply #'fs:at (fs:root) (append (%spelled p) names)))

(defmethod fs:ensure ((p path) &rest names)
  (apply #'fs:ensure (fs:root) (append (%spelled p) names)))

(defmethod fs:leaf ((p path) &rest names)
  (apply #'fs:leaf (fs:root) (append (%spelled p) names)))

(defmethod fs:erase ((p path) &rest pieces)
  (apply #'fs:erase (fs:root) (append (%spelled p) pieces)))

(defun matching (pattern &optional (where (fs:root)))
  (let ((found nil))
    (fs:walk where
             (lambda (each)
               (when (match pattern (path (fs:full-name each)))
                 (push each found))))
    (nreverse found)))
