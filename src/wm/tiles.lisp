(defpackage #:pine/wm/tiles
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:system #:pine/run/system) (#:command #:pine/run/command)
                    (#:watch #:pine/run/watch) (#:fault #:pine/run/fault)
                    (#:compositor #:pine/wm/compositor))
  (:export #:tiles #:layout #:tall #:wide #:full #:stacked #:arrange
           #:named #:layouts #:share #:gaps #:*layout*))
(in-package #:pine/wm/tiles)

(defvar *layout* nil
  "The layout in force. A config puts its own class here and that is the whole of
changing how windows are laid out.")

(defclass layout ()
  ((share :initarg :share :accessor share :initform 1/2)
   (gaps  :initarg :gaps  :accessor gaps  :initform 0))
  (:documentation "How windows are laid out on an output. A class, so a config
writes one ARRANGE method and pine has a new layout: nothing else knows the names
of these."))

(defclass tall (layout) ()
  (:documentation "One window down the left, the rest stacked down the right."))

(defclass wide (layout) ()
  (:documentation "One window across the top, the rest side by side under it."))

(defclass full (layout) ()
  (:documentation "One window at a time, filling the output."))

(defclass stacked (layout) ()
  (:documentation "Every window filling the output, in the order they arrived."))

(defun layouts ()
  "Every layout class there is, so a config's own is offered like the rest."
  (labels ((under (class)
             (c2mop:ensure-finalized class)
             (cons class (mapcan #'under (c2mop:class-direct-subclasses class)))))
    (remove (find-class 'layout) (under (find-class 'layout)))))

(defun named (name)
  (let ((found (find (string-downcase (princ-to-string name)) (layouts)
                     :key (lambda (c) (string-downcase (symbol-name (class-name c))))
                     :test #'equal)))
    (when found (make-instance (class-name found)))))

(defun %rect (l x y width height)
  "One window's rect, with the layout's gaps taken out of it."
  (let ((g (gaps l)))
    (list (+ x g) (+ y g) (max 1 (- width (* 2 g))) (max 1 (- height (* 2 g))))))

(defgeneric arrange (layout windows area)
  (:documentation "Where each window goes: a list of (ID X Y WIDTH HEIGHT), one
per window, in the order they were given. AREA is (X Y WIDTH HEIGHT).")
  (:method ((l layout) windows area)
    (destructuring-bind (x y width height) area
      (loop :for id :in windows :collect (cons id (%rect l x y width height))))))

(defmethod arrange ((l stacked) windows area)
  (destructuring-bind (x y width height) area
    (loop :for id :in windows :collect (cons id (%rect l x y width height)))))

(defmethod arrange ((l full) windows area)
  (destructuring-bind (x y width height) area
    (loop :for id :in windows
          :for first := t :then nil
          :when first :collect (cons id (%rect l x y width height)))))

(defmethod arrange ((l tall) windows area)
  (destructuring-bind (x y width height) area
    (let ((n (length windows)))
      (cond ((zerop n) nil)
            ((= n 1) (list (cons (first windows) (%rect l x y width height))))
            (t (let* ((main (max 1 (round (* width (share l)))))
                      (rest (max 1 (- width main)))
                      (each (max 1 (floor height (1- n)))))
                 (cons (cons (first windows) (%rect l x y main height))
                       (loop :for id :in (cdr windows)
                             :for i :from 0
                             :collect (cons id (%rect l (+ x main) (+ y (* i each))
                                                      rest
                                                      (if (= i (- n 2))
                                                          (- height (* i each))
                                                          each)))))))))))

(defmethod arrange ((l wide) windows area)
  (destructuring-bind (x y width height) area
    (let ((n (length windows)))
      (cond ((zerop n) nil)
            ((= n 1) (list (cons (first windows) (%rect l x y width height))))
            (t (let* ((main (max 1 (round (* height (share l)))))
                      (rest (max 1 (- height main)))
                      (each (max 1 (floor width (1- n)))))
                 (cons (cons (first windows) (%rect l x y width main))
                       (loop :for id :in (cdr windows)
                             :for i :from 0
                             :collect (cons id (%rect l (+ x (* i each)) (+ y main)
                                                      (if (= i (- n 2))
                                                          (- width (* i each))
                                                          each)
                                                      rest))))))))))

(defclass tiles (system:system)
  ((layout-of :initarg :layout :accessor layout-of
              :initform (make-instance 'tall))
   (watching  :initform nil :accessor watching))
  (:documentation "One of the window managers pine ships: it reads what the
compositor handed over and writes where each window goes.

Nothing in the substrate knows this is here. Dropping it takes /wm/layout and its
commands away, and pine places nothing again."))

(system:offers 'tiles)

(defclass layout-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Which layout is in force, by name: pine write /wm/layout wide."))

(defun %system () (system:named "tiles"))

(defmethod node:contents ((n layout-node))
  (let ((s (%system)))
    (when s (string-downcase (class-name (class-of (layout-of s)))))))

(defmethod (setf node:contents) (value (n layout-node))
  (let ((s (%system))
        (l (named value)))
    (when (and s l) (setf (layout-of s) l) (place s)))
  value)

(defun %area (s)
  (let ((c (tree:at nil "wm")))
    (or (getf (first (compositor:outputs c)) :area)
        (progn s (list 0 0 1920 1080)))))

(defun %ids (s)
  (declare (ignore s))
  (compositor:ids (tree:at nil "wm")))

(defun place (s)
  "Work out where the windows go and say so. Writing /wm/placement is the whole
of being the window manager here."
  (let ((n (tree:at nil "wm" "placement")))
    (when n
      (setf (node:contents n)
            (loop :for (id x y wide tall) :in (arrange (layout-of s) (%ids s)
                                                       (%area s))
                  :collect (list id x y wide tall))))))

(defmethod job:start ((s tiles))
  (let ((c (tree:at nil "wm")))
    (unless c (error "no /wm: use the wm system before this one."))
    (node:attach (make-instance 'layout-node :name "layout"
                                :describes "which layout is in force")
                 c)
    (let ((said (node:resolve c "said")))
      (when said
        (setf (watching s)
              (list (watch:watch said
                                 (lambda (of value)
                                   (declare (ignore of value))
                                   (fault:attempt (lambda () (place s)) "tiles"))
                                 :only nil :poll nil :name "tiles<-wm/said")))))
    (command:defcommand "wm-layout" (&optional name)
        (:describes "how windows are laid out")
      (let ((n (tree:at nil "wm" "layout")))
        (when (and n name) (setf (node:contents n) (princ-to-string name)))
        (and n (node:contents n))))
    (command:defcommand "wm-layouts" () (:describes "every layout there is")
      (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
              (layouts)))
    (place s))
  s)

(defmethod job:stop ((s tiles))
  (dolist (w (watching s)) (ignore-errors (watch:unwatch w)))
  (setf (watching s) nil)
  (dolist (name '("wm-layout" "wm-layouts")) (command:forget name))
  (tree:erase nil "wm" "layout")
  s)
