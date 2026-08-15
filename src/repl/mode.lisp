(defpackage #:pine/repl/mode
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:cmd #:pine/repl/command))
  (:export #:install #:modes-node
           #:mode #:minor #:minor-mode #:mode-named #:modes #:unmode #:remode
           #:mode-for
           #:name #:parent #:indicator #:settings #:claims #:claimsp
           #:precedence #:chain #:setting #:handle #:handler #:claimants
           #:bind #:binding #:in-force #:globp #:keys #:handlers
           #:claimed #:merged))
(in-package #:pine/repl/mode)

(defvar *modes* (d:table))

(defclass mode ()
  ((name      :initarg :name      :reader name)
   (parent    :initarg :parent    :accessor parent    :initform nil)
   (indicator :initarg :indicator :accessor indicator :initform nil)
   (settings  :initarg :settings  :accessor settings  :initform nil)
   (handlers  :initform (d:table) :reader handlers)
   (keys      :initform (d:table) :reader keys)
   (claims    :initarg :claims    :accessor claims    :initform nil)))

(defclass minor-mode (mode)
  ((precedence :initarg :precedence :accessor precedence :initform 0)))

(defmethod print-object ((m mode) stream)
  (print-unreadable-object (m stream :type t)
    (write-string (name m) stream)))

(defun mode (name &rest initargs &key (class 'mode) &allow-other-keys)
  (let ((m (apply #'make-instance class :name name
                  (alexandria:remove-from-plist initargs :class))))
    (d:keep! *modes* name m)))

(defun minor (name &rest initargs)
  (apply #'mode name :class 'minor-mode initargs))

(defun mode-named (name)
  (etypecase name
    (null nil)
    (mode name)
    (string (d:at (d:all *modes*) name))
    (symbol (d:at (d:all *modes*) (string-downcase (symbol-name name))))))

(defun modes ()
  (sort (d:vals (d:all *modes*)) #'string< :key #'name))

(defun unmode (name)
  (d:drop! *modes* name)
  name)

(defun remode (m)
  "Put a mode back, with the keys and handlers it already had."
  (d:keep! *modes* (name m) m))

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

(defgeneric chain (mode)
  (:method ((name string)) (chain (mode-named name)))
  (:method ((m null)) nil)
  (:method ((m mode))
    (loop :with seen := nil
          :for at := m :then (mode-named (parent at))
          :while (and at (not (member at seen)))
          :do (cl:push at seen)
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

(defun mode-for (path)
  (or (find-if (lambda (m) (find :paths (claims m) :key #'first))
               (remove-if-not (lambda (m) (claimsp m path)) (modes)))
      (find-if (lambda (m) (claimsp m path)) (modes))))

(defgeneric handle (mode verb function)
  (:method ((name string) verb function) (handle (mode-named name) verb function))
  (:method ((m mode) verb function)
    (d:keep! (handlers m) verb function)))

(defgeneric bind (mode chord command)
  (:method ((name string) chord command) (bind (mode-named name) chord command))
  (:method ((m mode) chord command)
    (d:keep! (keys m) chord command)))

(defgeneric in-force (of)
  (:method ((m null)) nil)
  (:method ((m mode)) (chain m)))

(defgeneric claimants (of verb)
  (:method (of verb)
    (loop :for link :in (in-force of)
          :for fn := (d:at (d:all (handlers link)) verb)
          :when fn :collect fn)))

(defgeneric handler (of verb)
  (:method (of verb) (first (claimants of verb))))

(defun claimed (of verb &rest arguments)
  "Offer VERB to every mode in force, nearest first, and answer the first that
takes it. A handler that answers nil has not taken it, so the next mode sees it
and finally whoever asked does it itself."
  (loop :for fn :in (claimants of verb)
        :for said := (apply fn of arguments)
        :when said :do (return said)))

(defun merged (of verb &rest arguments)
  "What every mode in force says about VERB, appended."
  (loop :for fn :in (claimants of verb)
        :append (alexandria:ensure-list (apply fn of arguments))))

(defgeneric binding (of chord)
  (:method (of chord)
    (loop :for link :in (in-force of)
          :for c := (d:at (d:all (keys link)) chord)
          :when c :do (return (cmd:command-named c)))))

(defclass modes-node (node:node) ())
(defclass mode-node (node:node) ())
(defclass keys-node (node:node) ())

(defun %mode-node (n name)
  (node:child n name
              (lambda () (make-instance 'mode-node :name name :parent n))))

(defmethod node:nodes ((n modes-node))
  (loop :for m :in (modes) :collect (%mode-node n (name m))))

(defmethod node:resolve ((n modes-node) name)
  (when (mode-named name) (%mode-node n (princ-to-string name))))

(defmethod node:contents ((n modes-node)) (mapcar #'name (modes)))

(defmethod node:contents ((n mode-node))
  (let ((m (mode-named (node:name n))))
    (and m (settings m))))

(defmethod node:nodes ((n mode-node))
  (list (node:child n "keys"
                    (lambda () (make-instance 'keys-node :name "keys" :parent n)))))

(defmethod node:contents ((n keys-node))
  "What this mode is bound to, as it stands. A chord a config added is here
without anything having to be told about it."
  (let ((m (mode-named (node:name (node:parent n)))))
    (and m (d:all (keys m)))))

(defmethod node:leafp ((n keys-node)) t)
(defmethod node:livep ((n modes-node)) t)
(defmethod node:livep ((n mode-node)) t)
(defmethod node:livep ((n keys-node)) t)
(defmethod node:persistp ((n modes-node)) nil)
(defmethod node:persistp ((n mode-node)) nil)
(defmethod node:persistp ((n keys-node)) nil)

(defun install (root)
  "Every mode there is, and what each is bound to. A mode is what an end user
implements to make pine understand something new, so it belongs where it can be
read rather than only in a registry."
  (node:attach (make-instance 'modes-node :name "mode"
                                          :describes "every mode there is")
               root))
