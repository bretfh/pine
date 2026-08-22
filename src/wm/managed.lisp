(defpackage #:pine/wm/managed
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:compositor #:pine/wm/compositor))
  (:export #:managed #:said #:wants #:told #:asked #:take #:placement))
(in-package #:pine/wm/managed)

(defparameter +verbs+ '("close" "exit" "next" "previous")
  "What this compositor takes. A verb is a node: writing /wm/close closes the
focused window, and that is the whole of the protocol for it. Nothing here is
about arrangement -- that is whatever writes /wm/placement.")

(defclass managed (compositor:compositor)
  ((said  :initform (d:box nil) :reader said)
   (wants :initform (d:box nil) :accessor wants)
   (kids  :initform nil :accessor kids))
  (:documentation "A compositor pine is the window manager of.

The other subclass talks to one; this one is told what there is and says where it
goes. Same protocol, because from the namespace they are the same thing: outputs,
windows with titles, one of them focused, and verbs it takes.

Where they go is not decided here. PLACEMENT is a place a system writes, and
writing it is the whole of being a window manager."))

(defclass said-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "What the process holding the compositor connection last saw:
its windows, its outputs and what has the keyboard. Writing it is that process
saying so."))

(defclass placement-node (node:node)
  ((where  :initform (d:box nil) :reader where)
   (livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Where each window goes: (ID X Y WIDTH HEIGHT &key CLIP STACK),
one per window that is to be on screen. The answer is total -- a window it does
not name is hidden.

Nothing under src/ writes this. A window manager is a system that does."))

(defclass wants-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "What pine wants done about it, for that process to take. It
takes one and the node is empty again, so nothing is done twice."))

(defun told (c)
  "What the wayland side last said: (:windows ((:id .. :title .. :app .. :size ..
:hidden ..) ...) :outputs ((:name .. :position .. :size .. :area ..) ...)
:focused id)."
  (d:held (said c)))

(defun windows-of (c) (getf (told c) :windows))

(defun %window (c id)
  (find (princ-to-string id) (windows-of c)
        :key (lambda (w) (princ-to-string (getf w :id)))
        :test #'equal))

(defun placement (c)
  (let ((n (find "placement" (kids c) :key #'node:name :test #'equal)))
    (and n (d:held (where n)))))

(defun asked (c said)
  "Ask the wayland side for something. What it takes it takes once."
  (d:swap! (wants c) (lambda (all) (append all (list said))))
  said)

(defun take (c)
  "Everything asked for since the last time, and forget it."
  (loop :for had := (d:held (wants c))
        :when (d:cas (wants c) had nil) :do (return had)))

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

(defmethod node:contents ((n said-node)) (told (node:over n)))

(defmethod (setf node:contents) (value (n said-node))
  (let ((c (node:over n)))
    (d:put! (said c) value)
    (node:stir c))
  value)

(defmethod node:contents ((n placement-node)) (d:held (where n)))

(defmethod (setf node:contents) (value (n placement-node))
  (d:put! (where n) (d:as :list value))
  (node:stir (node:over n))
  value)

(defmethod node:contents ((n wants-node)) (take (node:over n)))

(defmethod (setf node:contents) (value (n wants-node))
  (asked (node:over n) value)
  value)

(defmethod initialize-instance :after ((c managed) &key)
  (setf (kids c)
        (list (make-instance 'said-node :name "said" :over c
                             :describes "what the compositor handed over")
              (make-instance 'placement-node :name "placement" :over c
                             :describes "where each window goes")
              (make-instance 'wants-node :name "wants" :over c
                             :describes "what pine wants done about it"))))

(defmethod node:nodes ((c managed))
  (append (call-next-method) (kids c)))

(defmethod node:resolve ((c managed) name)
  (let ((name (princ-to-string name)))
    (or (find name (kids c) :key #'node:name :test #'equal)
        (call-next-method))))
