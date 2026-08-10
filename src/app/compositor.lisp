(defpackage #:pine.app.compositor
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:world #:pine.world.world) (#:window #:pine.edit.window)
                    (#:build #:pine.ui.build) (#:layout #:pine.ui.layout)
                    (#:face #:pine.ui.face) (#:mode #:pine.repl.mode)
                    (#:attach #:pine.net.attach) (#:wm #:pine.app.wm)
                    (#:fault #:pine.run.fault) (#:log #:pine.run.log))
  (:export #:compositor #:install #:pine-wm #:output-of #:splits #:arrange
           #:rects #:add-window #:forget-window #:bindings #:received
           #:push-arrangement))

(in-package #:pine.app.compositor)

(defparameter +chords+
  '(("s-Return" . "wm-terminal")
    ("s-q"      . "wm-close-window")
    ("s-j"      . "wm-focus-next")
    ("s-k"      . "wm-focus-previous")
    ("s-2"      . "wm-split-below")
    ("s-3"      . "wm-split-beside")
    ("s-S-e"    . "wm-exit")))

(defclass compositor (node:node)
  ((output-of :initform nil :accessor output-of)
   (titles    :initform (d:no-map) :accessor titles)
   (splits    :initform :column :accessor splits)))

(defmethod print-object ((n compositor) stream)
  (print-unreadable-object (n stream :type t)
    (format stream "~d window~:p~@[ on ~a~]"
            (length (window:windows n)) (output-of n))))

(defmethod node:contents ((n compositor))
  (list :windows (length (window:windows n))
        :focused (wm:focused-of n)
        :output (output-of n)))

(defmethod node:persistp ((n compositor)) nil)

(defun pine-wm (&optional (w world:*world*))
  (let ((n (make-instance 'compositor :name "wm"
                                      :describes "the windows this pine manages")))
    (node:attach n (world:root w))
    n))

(defun %id (w) (window:buffer-of w))

(defun %window-at (n id)
  (find id (window:windows n) :key #'%id :test #'equal))

(defmethod wm:windows-of ((n compositor))
  (remove nil (mapcar #'%id (window:windows n))))

(defmethod wm:titled ((n compositor) id)
  (d:at (titles n) id))

(defmethod wm:focused-of ((n compositor))
  (let ((w (window:focused n)))
    (and w (%id w))))

(defmethod wm:focus! ((n compositor) id)
  (let ((w (%window-at n id)))
    (when w
      (window:focus! w n)
      (push-arrangement n)
      id)))

(defmethod wm:step! ((n compositor) by)
  (let* ((all (window:windows n))
         (at (position (window:focused n) all)))
    (when (and all at)
      (window:focus! (nth (mod (+ at by) (length all)) all) n)
      (push-arrangement n)
      (wm:focused-of n))))

(defmethod wm:close-window! ((n compositor))
  (let ((id (wm:focused-of n)))
    (when id (%tell n :wm :action :close :id id))
    id))

(defmethod wm:exit! ((n compositor))
  (%tell n :wm :action :exit)
  t)

(defmethod wm:overview! ((n compositor)) nil)

(defmethod wm:split! ((n compositor) side)
  (setf (splits n) (if (member side '(:beside :right :row)) :row :column)))

(defun %app ()
  (find :wm (attach:clients) :key #'attach:client-kind))

(defun %tell (n verb &rest message)
  (declare (ignore n))
  (let ((to (%app)))
    (cond (to (apply #'attach:push-to to verb message))
          (t (log:note "no wm frontend attached, dropped ~a" verb)
             nil))))

(defun %margin ()
  (let ((border (face:metric :border 2)))
    (list border border border border)))

(defun %leaf (w)
  (build:cells nil :of w :as 'pine.ui.node:os-window-view
                   :expand (max 1 (window:weight-of w)) :margin (%margin)))

(defun %node (w)
  (if (window:splitp w)
      (let ((row (eq :row (window:runs-of w))))
        (apply (if row #'build:row #'build:column)
               :align :stretch :expand (max 1 (window:weight-of w))
               (loop :for part :in (window:parts w)
                     :for first := t :then nil
                     :append (if first
                                 (list (%node part))
                                 (list (build:rule :vertical row
                                                   :face :border-inactive)
                                       (%node part))))))
      (%leaf w)))

(defun %tree (n)
  (let ((parts (window:parts n)))
    (cond ((null parts) nil)
          ((null (rest parts)) (%node (first parts)))
          (t (apply #'build:column :align :stretch :expand 1
                    (mapcar #'%node parts))))))

(defun rects (n)
  (let ((tree (%tree n))
        (output (output-of n)))
    (when (and tree output)
      (destructuring-bind (&key x y width height) output
        (let ((layout:*text-size* (lambda (text font-px)
                                    (declare (ignore font-px))
                                    (values (length text) 1))))
          (layout:measure tree width height)
          (layout:arrange tree (or x 0) (or y 0) width height))
        (let (acc)
          (labels ((walk (node)
                     (when (typep node 'pine.ui.node:os-window-view)
                       (let ((w (pine.ui.node:of node)))
                         (push (list (%id w)
                                     (pine.ui.node:start-col node)
                                     (pine.ui.node:start-line node)
                                     (- (pine.ui.node:end-col node)
                                        (pine.ui.node:start-col node))
                                     (1+ (- (pine.ui.node:end-line node)
                                            (pine.ui.node:start-line node))))
                               acc)))
                     (mapc #'walk (layout:nodes-of node))))
            (walk tree))
          (nreverse acc))))))

(defun %border ()
  (list :width (face:metric :border 2)
        :active (face:face-fg :border-active)
        :inactive (face:face-fg :border-inactive)))

(defun arrange (n) (rects n))

(defun push-arrangement (n)
  (let ((found (rects n)))
    (when found
      (%tell n :arrangement :rects found
                            :focus (wm:focused-of n)
                            :border (%border)))))

(defun add-window (n id title app)
  (let ((focus (window:focused n)))
    (cond ((null focus)
           (setf (window:buffer-of (window:make-window :into n)) id)
           (window:focus! (first (window:windows n)) n))
          (t
           (window:split! focus (if (eq :row (splits n)) :beside :below))
           (setf (window:buffer-of (window:focused n)) id)))
    (setf (titles n) (d:with (titles n) id (or title app)))
    (push-arrangement n)
    id))

(defun forget-window (n id)
  (setf (titles n) (d:without (titles n) id))
  (let ((w (%window-at n id)))
    (when w
      (if (rest (window:windows n))
          (window:close! w n)
          (setf (window:buffer-of w) nil))
      (push-arrangement n))
    id))

(defun bindings ()
  (let ((m (mode:mode-named "wm")))
    (when m
      (d:pairs (d:all (mode:keys m))))))

(defun %run-chord (chord)
  (let ((command (and (mode:mode-named "wm")
                      (gethash chord (mode:keys (mode:mode-named "wm"))))))
    (if command
        (fault:attempt (lambda () (pine.repl.command:run command)) chord)
        (log:note "no command bound to ~a" chord))))

(defun received (client message)
  (declare (ignore client))
  (let ((n (wm:current)))
    (when (typep n 'compositor)
      (case (first message)
        (:binding
         (destructuring-bind (&key keys &allow-other-keys) (rest message)
           (%run-chord keys)))
        (:output
         (destructuring-bind (&key x y width height &allow-other-keys) (rest message)
           (setf (output-of n) (list :x (or x 0) :y (or y 0)
                                     :width width :height height))
           (push-arrangement n)))
        (:window-added
         (destructuring-bind (&key id title app-id &allow-other-keys) (rest message)
           (add-window n id title app-id)))
        (:window-closed
         (destructuring-bind (&key id &allow-other-keys) (rest message)
           (forget-window n id)))
        (:window-focused
         (destructuring-bind (&key id &allow-other-keys) (rest message)
           (wm:focus! n id)))
        (t nil)))))

(defun %attached (client)
  (attach:push-to client :bindings :table (bindings))
  (let ((n (wm:current)))
    (when (typep n 'compositor) (push-arrangement n)))
  client)

(defun install ()
  (mode:mode "wm" :settings '(:indicator "WM"))
  (dolist (pair +chords+)
    (mode:bind "wm" (car pair) (cdr pair)))
  (pine.repl.command:defcommand "wm-split-below" ()
      (:describes "the next window opens below")
    (wm:split! (wm:current) :below))
  (pine.repl.command:defcommand "wm-split-beside" ()
      (:describes "the next window opens beside")
    (wm:split! (wm:current) :beside))
  (attach:app :wm
              (d:map :doc "the compositor's windows, and the chords that move them"
                     :attached #'%attached
                     :received #'received))
  t)
