(defpackage #:pine/wm/managed
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:compositor #:pine/wm/compositor))
  (:export
   #:managed))
(in-package #:pine/wm/managed)

(defparameter +verbs+ '("close" "exit" "next" "previous")
  "What this compositor takes. A verb is a node: writing /wm/close closes the
focused window, and that is the whole of the protocol for it. Nothing here is
about arrangement -- that is whatever writes /wm/placement.")

(defclass managed (compositor:compositor)
  ((said  :initform nil :accessor said)
   (wants :initform nil :accessor wants)
   (where :initform nil :accessor where))
  (:documentation "A compositor pine is the window manager of.

The other subclass talks to one; this one is told what there is and says where it
goes. Same protocol, because from the namespace they are the same thing: outputs,
windows with titles, one of them focused, and verbs it takes.

Where they go is not decided here. PLACEMENT is a place a system writes, and
writing it is the whole of being a window manager."))

(defun told (c)
  "What the wayland side last said: (:windows ((:id .. :title .. :app .. :size ..
:hidden ..) ...) :outputs ((:name .. :position .. :size .. :area ..) ...)
:focused id)."
  (said c))

(defun windows-of (c) (getf (told c) :windows))

(defun %window (c id)
  (find (princ-to-string id) (windows-of c)
        :key (lambda (w) (princ-to-string (getf w :id)))
        :test #'equal))

(defun placement (c) (where c))

(defun asked (c said)
  "Ask the wayland side for something. What it takes it takes once."
  (d:swap (slot-value c 'wants) (lambda (all) (append all (list said))))
  said)

(defun take (c)
  "Everything asked for since the last time, and forget it."
  (loop :for had := (wants c)
        :when (d:cas (slot-value c 'wants) had nil) :do (return had)))

(defmethod compositor:outputs ((c managed)) (getf (told c) :outputs))

(defmethod compositor:ids ((c managed))
  (mapcar (lambda (w) (getf w :id)) (windows-of c)))

(defmethod compositor:windows ((c managed))
  (loop :for w :in (windows-of c)
        :collect (let ((h (make-hash-table :test 'equal)))
                   (setf (gethash "id" h) (princ-to-string (getf w :id))
                         (gethash "title" h) (or (getf w :title) "")
                         (gethash "app_id" h) (or (getf w :app) "")
                         (gethash "is_focused" h)
                         (equal (getf w :id) (getf (told c) :focused)))
                   h)))

(defmethod compositor:focused ((c managed))
  (let ((id (getf (told c) :focused)))
    (and id (princ-to-string id))))

(defmethod compositor:titled ((c managed) id)
  (getf (%window c id) :title))

(defmethod compositor:rect ((c managed) id)
  "Where a window is: what was last placed for it, which is what it was told to
be. Until something places it there is nothing to say."
  (let ((each (find (princ-to-string id) (placement c)
                    :key (lambda (e) (princ-to-string (first e)))
                    :test #'equal)))
    (when each (subseq each 1 5))))

(defmethod compositor:hidden ((c managed) id)
  (and (getf (%window c id) :hidden) t))

(defmethod compositor:hide ((c managed) id)
  (asked c (list :hide (princ-to-string id))))

(defmethod compositor:show ((c managed) id)
  (asked c (list :show (princ-to-string id))))

(defmethod compositor:focus ((c managed) id)
  (asked c (list :focus (princ-to-string id))))

(defmethod compositor:verbs ((c managed)) +verbs+)

(defmethod compositor:act ((c managed) verb &rest arguments)
  (let ((verb (princ-to-string verb)))
    (when (member verb +verbs+ :test #'equal)
      (asked c (list* (intern (string-upcase verb) :keyword) arguments))
      t)))

(defmethod initialize-instance :after ((c managed) &key)
  "What the process holding the connection last saw, where each window goes, and
what pine wants done about it. Nothing under src/ writes the placement: a window
manager is a system that does, and the answer is total -- a window it does not name
is hidden. What is wanted is taken once, so nothing is done twice."
  (setf (compositor:parts c)
        (append (compositor:parts c)
                (list (make-instance 'handed :name "said" :of c
                                     :describes "what the compositor handed over")
                      (make-instance 'placement :name "placement" :of c
                                     :describes "where each window goes")
                      (make-instance 'wanted :name "wants" :of c
                                     :describes "what pine wants done about it")))))

(defclass handed (fs:derived) ()
  (:documentation "/wm/said: what the process holding the connection last saw."))

(defmethod fs:livep ((n handed) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n handed)) (told (fs:of n)))

(defmethod fs:takes ((n handed) value)
  (setf (said (fs:of n)) value)
  (fs:moved (fs:of n)))

(defclass placement (fs:derived) ()
  (:documentation "/wm/placement: where each window goes. What a window manager
writes; a window it does not name is hidden."))

(defmethod fs:livep ((n placement) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n placement)) (where (fs:of n)))

(defmethod fs:takes ((n placement) value)
  (setf (where (fs:of n)) (d:as :list value))
  (fs:moved (fs:of n)))

(defclass wanted (fs:derived) ()
  (:documentation "/wm/wants: what pine wants done about the windows, taken once."))

(defmethod fs:livep ((n wanted) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n wanted)) (take (fs:of n)))

(defmethod fs:takes ((n wanted) value) (asked (fs:of n) value))
