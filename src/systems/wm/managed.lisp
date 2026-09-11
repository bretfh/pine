(in-package #:pine/wm)

(defparameter +verbs+ '("close" "exit" "next" "previous"))

(defclass managed (compositor)
  ((said  :initform nil :accessor said)
   (wants :initform nil :accessor wants)
   (where :initform nil :accessor where)))

(defun told (c)
  (said c))

(defun windows-of (c) (getf (told c) :windows))

(defun %window (c id)
  (find (princ-to-string id) (windows-of c)
        :key (lambda (w) (princ-to-string (getf w :id)))
        :test #'equal))

(defun placement (c) (where c))

(defun asked (c said)
  (sb-ext:atomic-update (slot-value c 'wants) (lambda (all) (append all (list said))))
  said)

(defun take (c)
  (loop :for had := (wants c)
        :when (d:cas-p (slot-value c 'wants) had nil) :do (return had)))

(defmethod outputs ((c managed)) (getf (told c) :outputs))

(defmethod ids ((c managed))
  (mapcar (lambda (w) (getf w :id)) (windows-of c)))

(defmethod windows ((c managed))
  (loop :for w :in (windows-of c)
        :collect (let ((h (make-hash-table :test 'equal)))
                   (setf (gethash "id" h) (princ-to-string (getf w :id))
                         (gethash "title" h) (or (getf w :title) "")
                         (gethash "app_id" h) (or (getf w :app) "")
                         (gethash "is_focused" h)
                         (equal (getf w :id) (getf (told c) :focused)))
                   h)))

(defmethod focused ((c managed))
  (let ((id (getf (told c) :focused)))
    (and id (princ-to-string id))))

(defmethod titled ((c managed) id)
  (getf (%window c id) :title))

(defmethod rect ((c managed) id)
  (let ((each (find (princ-to-string id) (placement c)
                    :key (lambda (e) (princ-to-string (first e)))
                    :test #'equal)))
    (when each (subseq each 1 5))))

(defmethod hidden ((c managed) id)
  (and (getf (%window c id) :hidden) t))

(defmethod hide ((c managed) id)
  (asked c (list :hide (princ-to-string id))))

(defmethod show ((c managed) id)
  (asked c (list :show (princ-to-string id))))

(defmethod focus ((c managed) id)
  (asked c (list :focus (princ-to-string id))))

(defmethod verbs ((c managed)) +verbs+)

(defmethod act ((c managed) verb &rest arguments)
  (let ((verb (princ-to-string verb)))
    (when (member verb +verbs+ :test #'equal)
      (asked c (list* (intern (string-upcase verb) :keyword) arguments))
      t)))

(defmethod initialize-instance :after ((c managed) &key)
  (setf (parts c)
        (append (parts c)
                (list (make-instance 'handed :name "said" :of c
                                     :describes "what the compositor handed over")
                      (make-instance 'placement :name "placement" :of c
                                     :describes "where each window goes")
                      (make-instance 'wanted :name "wants" :of c
                                     :describes "what pine wants done about it")))))

(defclass handed (fs:derived) ())

(defmethod fs:volatile-p ((n handed) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n handed)) (told (fs:of n)))

(defmethod fs:takes ((n handed) value)
  (setf (said (fs:of n)) value)
  (fs:touch (fs:of n)))

(defclass placement (fs:derived) ())

(defmethod fs:volatile-p ((n placement) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n placement)) (where (fs:of n)))

(defmethod fs:takes ((n placement) value)
  (setf (where (fs:of n)) (d:as :list value))
  (fs:touch (fs:of n)))

(defclass wanted (fs:derived) ())

(defmethod fs:volatile-p ((n wanted) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n wanted)) (take (fs:of n)))

(defmethod fs:takes ((n wanted) value) (asked (fs:of n) value))
