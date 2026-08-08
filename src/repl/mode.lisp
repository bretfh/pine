(in-package #:pine.repl)

(defvar *modes* (make-hash-table :test 'equal))

(defclass mode ()
  ((name      :initarg :name      :reader name)
   (parent    :initarg :parent    :accessor parent    :initform nil)
   (indicator :initarg :indicator :accessor indicator :initform nil)
   (settings  :initarg :settings  :accessor settings  :initform nil)
   (handlers  :initform (make-hash-table :test 'eq) :reader handlers)
   (keys      :initform (make-hash-table :test 'equal) :reader keys)
   (claims    :initarg :claims    :accessor claims    :initform nil)))

(defclass minor-mode (mode)
  ((precedence :initarg :precedence :accessor precedence :initform 0)))

(defmethod print-object ((m mode) stream)
  (print-unreadable-object (m stream :type t)
    (write-string (name m) stream)))

(defun mode (name &rest initargs &key (class 'mode) &allow-other-keys)
  (let ((m (apply #'make-instance class :name name
                  (alexandria:remove-from-plist initargs :class))))
    (setf (gethash name *modes*) m)))

(defun minor (name &rest initargs)
  (apply #'mode name :class 'minor-mode initargs))

(defun mode-named (name)
  (etypecase name
    (null nil)
    (mode name)
    (string (gethash name *modes*))
    (symbol (gethash (string-downcase (symbol-name name)) *modes*))))

(defun modes ()
  (sort (loop :for m :being :the :hash-values :of *modes* :collect m)
        #'string< :key #'name))

(defun unmode (name)
  (remhash name *modes*)
  name)

(defgeneric chain (mode)
  (:method ((name string)) (chain (mode-named name)))
  (:method ((m null)) nil)
  (:method ((m mode))
    (loop :with seen := nil
          :for at := m :then (mode-named (parent at))
          :while (and at (not (member at seen)))
          :do (push at seen)
          :collect at)))

(defgeneric setting (mode key &optional default)
  (:method ((name string) key &optional default)
    (setting (mode-named name) key default))
  (:method ((m null) key &optional default)
    (declare (ignore key))
    default)
  (:method ((m mode) key &optional default)
    (loop :for link :in (chain m)
          :for value := (getf (settings link) key)
          :when value :do (return value)
          :finally (return default))))

(defgeneric claimsp (mode path)
  (:method ((m mode) path)
    (let ((full (namestring (pathname path)))
          (leaf (file-namestring (pathname path))))
      (loop :for (kind . patterns) :in (claims m)
            :thereis (some (lambda (pattern)
                             (globp pattern (if (eq kind :paths) full leaf)))
                           patterns)))))

(defun globp (pattern text)
  (labels ((walk (p n)
             (cond ((and (null p) (null n)) t)
                   ((null p) nil)
                   ((char= (first p) #\*)
                    (or (walk (rest p) n) (and n (walk p (rest n)))))
                   ((null n) nil)
                   ((char-equal (first p) (first n)) (walk (rest p) (rest n)))
                   (t nil))))
    (walk (coerce pattern 'list) (coerce text 'list))))

(defun mode-for (path)
  (find-if (lambda (m) (claimsp m path)) (modes)))

(defgeneric handle (mode verb function)
  (:method ((name string) verb function) (handle (mode-named name) verb function))
  (:method ((m mode) verb function)
    (setf (gethash verb (handlers m)) function)))

(defgeneric bind (mode chord command)
  (:method ((name string) chord command) (bind (mode-named name) chord command))
  (:method ((m mode) chord command)
    (setf (gethash chord (keys m)) command)))

(defgeneric in-force (of)
  (:documentation nil)
  (:method ((m mode)) (chain m)))

(defgeneric claimants (of verb)
  (:method (of verb)
    (loop :for link :in (in-force of)
          :for fn := (gethash verb (handlers link))
          :when fn :collect fn)))

(defgeneric handler (of verb)
  (:method (of verb) (first (claimants of verb))))

(defgeneric binding (of chord)
  (:method (of chord)
    (loop :for link :in (in-force of)
          :for c := (gethash chord (keys link))
          :when c :do (return (command-named c)))))
