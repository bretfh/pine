(defpackage #:pine/wm/managed
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:compositor #:pine/wm/compositor) (#:tiles #:pine/wm/tiles))
  (:export #:managed #:said #:wants #:layout-of #:told #:asked #:take
           #:placement))
(in-package #:pine/wm/managed)

(defparameter +verbs+ '("close" "exit" "next" "previous" "full" "tall" "wide"
                        "stacked")
  "What this compositor takes. A verb is a node: writing /wm/close closes the
focused window, and that is the whole of the protocol for it.")

(defclass managed (compositor:compositor)
  ((said       :initform (d:box nil) :reader said)
   (wants      :initform (d:box nil) :accessor wants)
   (layout-of  :initarg :layout :accessor layout-of
               :initform (make-instance 'tiles:tall))
   (kids       :initform nil :accessor kids))
  (:documentation "A compositor pine is the window manager of.

The other subclass talks to one; this one is told what there is and says where it
goes. Same protocol, because from the namespace they are the same thing: windows
with titles, one of them focused, and verbs it takes."))

(defclass said-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "What the process holding the compositor connection last saw:
its windows, its outputs and what has the keyboard. Writing it is that process
saying so."))

(defclass layout-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Which layout is in force, by name. Writing one puts it there:
pine write /wm/layout wide."))

(defclass wants-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "What pine wants done about it, for that process to take. It
takes one and the node is empty again, so nothing is done twice."))

(defun told (c)
  "What the wayland side last said: (:windows ((id title app) ...) :outputs
((name x y w h) ...) :focused id)."
  (d:held (said c)))

(defun windows-of (c) (getf (told c) :windows))

(defun %ids (c) (mapcar #'first (windows-of c)))

(defun %area (c)
  (let ((out (first (getf (told c) :outputs))))
    (if out (cdr out) (list 0 0 1920 1080))))

(defun placement (c)
  "Where each window goes, worked out from what the wayland side said. A derived
node reads this, so it follows what it was told the way anything else does."
  (tiles:arrange (layout-of c) (%ids c) (%area c)))

(defun asked (c said)
  "Ask the wayland side for something. What it takes it takes once."
  (d:swap! (wants c) (lambda (all) (append all (list said))))
  said)

(defun take (c)
  "Everything asked for since the last time, and forget it."
  (loop :for had := (d:held (wants c))
        :when (d:cas (wants c) had nil) :do (return had)))

(defmethod compositor:windows ((c managed))
  (loop :for (id title app) :in (windows-of c)
        :collect (let ((h (make-hash-table :test 'equal)))
                   (setf (gethash "id" h) (princ-to-string id)
                         (gethash "title" h) (or title "")
                         (gethash "app_id" h) (or app "")
                         (gethash "is_focused" h)
                         (equal id (getf (told c) :focused)))
                   h)))

(defmethod compositor:workspaces ((c managed)) nil)

(defmethod compositor:focused ((c managed))
  (let ((id (getf (told c) :focused)))
    (and id (princ-to-string id))))

(defmethod compositor:titled ((c managed) id)
  (second (find (princ-to-string id) (windows-of c)
                :key (lambda (w) (princ-to-string (first w)))
                :test #'equal)))

(defmethod compositor:focus ((c managed) id)
  (asked c (list :focus (princ-to-string id))))

(defmethod compositor:verbs ((c managed)) +verbs+)

(defmethod compositor:act ((c managed) verb &rest arguments)
  (let ((verb (princ-to-string verb)))
    (cond ((member verb '("tall" "wide" "full" "stacked") :test #'equal)
           (let ((n (node:resolve c "layout")))
             (and n (setf (node:contents n) verb) t)))
          ((member verb +verbs+ :test #'equal)
           (asked c (list* (intern (string-upcase verb) :keyword) arguments))
           t))))

(defmethod node:contents ((n said-node)) (told (node:over n)))

(defmethod (setf node:contents) (value (n said-node))
  (let ((c (node:over n)))
    (d:put! (said c) value)
    (node:stir c))
  value)

(defmethod node:contents ((n layout-node))
  (string-downcase (class-name (class-of (layout-of (node:over n))))))

(defmethod (setf node:contents) (value (n layout-node))
  (let ((c (node:over n))
        (l (tiles:named value)))
    (when l (setf (layout-of c) l) (node:stir c)))
  value)

(defmethod node:contents ((n wants-node)) (take (node:over n)))

(defmethod (setf node:contents) (value (n wants-node))
  (asked (node:over n) value)
  value)

(defmethod initialize-instance :after ((c managed) &key)
  "Its own places, made once. PLACEMENT reads the other two through the namespace
rather than reaching into the object, so writing either one works it out again."
  (let* ((said (make-instance 'said-node :name "said" :over c
                              :describes "what the compositor handed over"))
         (layout (make-instance 'layout-node :name "layout" :over c
                                :describes "which layout is in force"))
         (wants (make-instance 'wants-node :name "wants" :over c
                               :describes "what pine wants done about it"))
         (placement (node:derive "placement"
                                 (lambda ()
                                   (node:contents said)
                                   (node:contents layout)
                                   (placement c))
                                 :over c
                                 :describes "where each window goes")))
    (setf (kids c) (list said layout wants placement))))

(defmethod node:nodes ((c managed))
  (append (call-next-method) (kids c)))

(defmethod node:resolve ((c managed) name)
  (let ((name (princ-to-string name)))
    (or (find name (kids c) :key #'node:name :test #'equal)
        (call-next-method))))
