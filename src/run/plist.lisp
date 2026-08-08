(defpackage #:pine.run.plist
  (:use #:cl)
  (:shadow #:map #:keys #:count)
  (:export #:at #:with #:without #:keys #:vals #:count #:mapp #:do-map #:merged
           #:map))

(in-package #:pine.run.plist)

(defun at (plist key &optional default)
  (loop :for (k v) :on plist :by #'cddr
        :when (eql k key) :do (return v)
        :finally (return default)))

(defun with (plist key value)
  (append (without plist key) (list key value)))

(defun without (plist key)
  (loop :for (k v) :on plist :by #'cddr
        :unless (eql k key) :append (list k v)))

(defun keys (plist)
  (loop :for (k nil) :on plist :by #'cddr :collect k))

(defun vals (plist)
  (loop :for (nil v) :on plist :by #'cddr :collect v))

(defun count (plist) (floor (length plist) 2))

(defun mapp (x)
  (and (consp x) (evenp (length x))
       (loop :for (k nil) :on x :by #'cddr
             :always (or (keywordp k) (integerp k) (stringp k)))))

(defun merged (&rest plists)
  (let ((out nil))
    (dolist (p plists out)
      (loop :for (k v) :on p :by #'cddr :do (setf out (with out k v))))))

(defun map (&rest pairs) pairs)

(defmacro do-map ((key value plist &optional result) &body body)
  (let ((rest (gensym)))
    `(loop :for ,rest :on ,plist :by #'cddr
           :for ,key := (first ,rest)
           :for ,value := (second ,rest)
           :do (locally ,@body)
           :finally (return ,result))))
