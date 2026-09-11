(in-package #:pine/ui)

(defclass role () ())

(defclass bar (role) ())
(defclass panel (role) ())
(defclass overlay (role) ())
(defclass background (role) ())
(defclass toplevel (role) ())
(defclass tile (role) ())

(defgeneric shows (role)
  (:method ((r role)) :always)
  (:method ((r panel)) :when-asked)
  (:method ((r overlay)) :when-asked))

(defclass placing ()
  ((edges   :initarg :edges   :reader edges-of   :initform nil)
   (width    :initarg :width    :reader width-of    :initform 0)
   (height    :initarg :height    :reader height-of    :initform 0)
   (reserve :initarg :reserve :reader reserve-of :initform 0)
   (margin  :initarg :margin  :reader margin-of  :initform '(0 0 0 0))))

(defmethod print-object ((p placing) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~{~(~a~)~^ ~} ~dx~d" (edges-of p) (width-of p) (height-of p))))

(defun placing (&key edges (width 0) (height 0) (reserve 0) (margin '(0 0 0 0)))
  (make-instance 'placing :edges edges :width width :height height
                          :reserve reserve :margin margin))

(defun inset (&key (top 0) (right 0) (bottom 0) (left 0))
  (list top right bottom left))

(defgeneric anchor (role width height)
  (:method ((r role) width height)
    (placing :edges '(:top :left) :width width :height height))
  (:method ((r bar) width height)
    (declare (ignore height))
    (placing :edges '(:top :left :bottom) :width width :height 0 :reserve width))
  (:method ((r background) width height)
    (declare (ignore width height))
    (placing :edges '(:top :left :bottom :right)))
  (:method ((r overlay) width height)
    (placing :edges '(:top :right) :width width :height height
             :margin (inset :top 8 :right 8)))
  (:method ((r panel) width height)
    (placing :edges '(:top :left) :width width :height height
             :margin (inset :top 8 :left 8)))
  (:method ((r toplevel) width height)
    (declare (ignore width height))
    (placing))
  (:method ((r tile) width height)
    (declare (ignore width height))
    (placing)))

(defclass surface (fs:mount)
  ((role  :initarg :role  :accessor role)
   (shown :initarg :shown :accessor shown)
   (size  :initarg :size  :accessor size :initform nil)
   (builds :initarg :builds :reader builds)
   (acts  :initform (d:no-map) :accessor acts)))

(defun tree (s)
  (let ((n (fs:child s "tree"))) (and n (fs:contents n))))

(defmethod print-object ((s surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~(~a~)~:[~; shown~]" (fs:name s)
            (class-name (class-of (role s))) (shown s))))

(defun root () (fs:at "/ui/surface"))

(defun surfaces ()
  (remove-if-not (lambda (n) (typep n 'surface)) (fs:children (root))))

(defgeneric spelled-place (it)
  (:method ((it path:path)) (path:whole it))
  (:method (it) (if (fs:nodep it) (fs:full-name it) (princ-to-string it))))

(defun %id (widget slot at)
  (format nil "~a/~(~a~)"
          (let ((stands-for (of widget)))
            (if stands-for
                (spelled-place stands-for)
                (format nil "@~{~d~^.~}" at)))
          slot))

(defun %plainly (said)
  (list :edges (edges-of said) :width (width-of said) :height (height-of said)
        :reserve (reserve-of said) :margin (margin-of said)))

(defun act (name said)
  (let* ((all (alexandria:ensure-list said))
         (id (princ-to-string (first all)))
         (s (fs:at "/ui/surface" (princ-to-string name)))
         (thunk (and s (d:lookup (acts s) id))))
    (when thunk
      (fault:attempt (lambda () (apply thunk (rest all)))
                     (format nil "the widget at ~a" id)))))

(defun %wire (s)
  (let ((mine (d:no-map))
        (tree (tree s)))
    (when tree
      (let ((said (to-wire tree
                           :on-action (lambda (thunk widget slot at)
                                        (let ((id (%id widget slot at)))
                                          (setf mine (d:with mine id thunk))
                                          id)))))
        (setf (acts s) mine)
        said))))

(defgeneric declared (surface)
  (:method (surface) (declare (ignore surface)) nil))

(defun make-surface (name builds &key (as 'panel) (starts :as-the-role-says))
  (let* ((r (make-instance as))
         (s (make-instance 'surface :name (princ-to-string name)
                                    :role r :builds builds
                                    :shown (ecase starts
                                             (:up t)
                                             (:down nil)
                                             (:as-the-role-says
                                              (eq :always (shows r))))
                                    :describes "a widget tree, and where it goes")))
    (fs:mount s (root))
    (declared s)
    s))

(defmethod fs:names ((s surface))
  '((:tree     . "the widget tree, worked out from what it read")
    (:role     . "which kind of surface this is")
    (:wire     . "the tree, as it crosses to another pine")
    (:where    . "where the role says this goes")
    (:shown    . "whether it is up; writing puts it up or down")
    (:size     . "what shows it says it came out at")
    (:on-click . "what another pine says was clicked")))

(defmethod fs:read ((s surface) (name (eql :tree)))
  (funcall (builds s)))

(defmethod fs:read ((s surface) (name (eql :role)))
  (string-downcase (class-name (class-of (role s)))))

(defmethod fs:read ((s surface) (name (eql :wire)))
  (%wire s))

(defmethod fs:read ((s surface) (name (eql :where)))
  (let ((said (fs:contents (fs:child s "size"))))
    (%plainly (anchor (role s) (or (getf said :width) 0) (or (getf said :height) 0)))))

(defmethod fs:read ((s surface) (name (eql :shown)))
  (shown s))

(defmethod fs:write ((s surface) (name (eql :shown)) value)
  (setf (shown s) value))

(defmethod fs:read ((s surface) (name (eql :size)))
  (size s))

(defmethod fs:write ((s surface) (name (eql :size)) value)
  (setf (size s) value))

(defmethod fs:write ((s surface) (name (eql :on-click)) said)
  (act (fs:name s) said))

(defmethod fs:volatile-p ((s surface) &optional name)
  (if name
      (not (member name '("tree" "wire" "where") :test #'equal))
      (call-next-method)))

(defun forget-surface (name)
  (fs:erase (format nil "/ui/surface/~a" name))
  name)

(defmacro defsurface (name options &body body)
  `(make-surface ,(string-downcase (string name)) (lambda () ,@body)
           ,@options :starts :as-the-role-says))

