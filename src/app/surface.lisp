(defpackage #:pine.app.surface
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:computed #:pine.fs.computed)
                    (#:world #:pine.world.world) (#:tree #:pine.fs.tree))
  (:export #:surface #:defsurface #:surfaces #:surface-named #:surface-node #:root
           #:as #:shownp #:builds #:show! #:hide! #:toggle! #:only!
           #:panelp #:forget))

(in-package #:pine.app.surface)

(defparameter +roles+ '(:bar :echo :panel :overlay :background :toplevel))

(defclass surface (computed:computed-node)
  ((as     :initarg :as    :accessor as     :initform :panel)
   (shownp :initarg :shown :accessor shownp :initform nil)))

(defmethod print-object ((s surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~(~a~)~:[~; shown~]" (node:name s) (as s) (shownp s))))

(defun root (&optional (w world:*world*))
  (world:ensure w "surface"))

(defun surfaces (&optional (w world:*world*))
  (remove-if-not (lambda (n) (typep n 'surface)) (node:nodes (root w))))

(defun surface-named (name &optional (w world:*world*))
  (let ((n (tree:at (root w) (princ-to-string name))))
    (and (typep n 'surface) n)))

(defun panelp (s) (member (as s) '(:panel :overlay)))

(defun surface-node (name builds &key (as :panel) (shown (not (member as '(:panel :overlay)))))
  (let ((s (make-instance 'surface :name (princ-to-string name) :thunk builds
                                   :as as :shown shown
                                   :describes "a widget tree wayland shows")))
    (node:attach s (root))
    (node:slots s s "as" 'as "shown" 'shownp)
    s))

(defun surface (name builds &rest initargs)
  (apply #'surface-node name builds initargs))

(defmacro defsurface (name options &body body)
  `(surface-node ,(string-downcase (string name)) (lambda () ,@body) ,@options))

(defun forget (name)
  (node:detach (root) (princ-to-string name)))

(defun show! (name)
  (let ((s (surface-named name)))
    (when s
      (setf (shownp s) t)
      (node:invalidate s)
      s)))

(defun hide! (name)
  (let ((s (surface-named name)))
    (when s
      (setf (shownp s) nil)
      (node:invalidate s)
      s)))

(defun only! (name)
  (dolist (other (surfaces))
    (when (and (panelp other) (not (equal (node:name other) (princ-to-string name))))
      (hide! (node:name other))))
  name)

(defun toggle! (name)
  (let ((s (surface-named name)))
    (when s
      (if (shownp s)
          (hide! name)
          (progn (only! name) (show! name))))))
