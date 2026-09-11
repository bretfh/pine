(in-package #:pine/wayland)

(defconstant +left+ #x110)
(defparameter +settling+ 10)

(defgeneric pointing (s sh &rest event))
(defgeneric keyboard-said (s sh &rest event))
(defgeneric typed (s said))
(defgeneric chorded (s said))
(defgeneric chords-wanted (s))

(defclass screen (job:thread)
  ((display :initform nil     :accessor display-of)
   (shell   :initform nil     :accessor shell-of)
   (pump    :initform nil     :accessor pump)
   (says    :initform nil     :accessor says)
   (up      :initform (d:no-map) :accessor up)
   (watching :initform (d:no-map) :accessor watching)
   (wm      :initform nil     :accessor wm-of)
   (keys    :initform (make-keys) :reader keys)
   (pointer :initform (make-pointer) :reader pointer)
   (turns   :initform 0       :accessor turns)
   (done    :initform nil     :accessor done)))

(defun availablep () (and (uiop:getenv "WAYLAND_DISPLAY") t))

(defun %at (name &rest under)
  (apply #'fs:at "/ui/surface" name under))

(defun tell (s thunk)
  (let ((to (says s)))
    (when to (job:tell to thunk) t)))

(defun %named (name) (fs:at "/ui/surface" name))

(defun %tree (name)
  (let ((it (%named name))) (when it (ui:tree it))))

(defun %shownp (name)
  (let ((it (%named name))) (and it (ui:shown it) t)))

(defun %windowp (name)
  (let ((it (%named name)))
    (and it (typep (ui:role it) '(or ui:toplevel ui:tile)) t)))

(defun %where (name)
  (let ((n (%at name "where")))
    (and n (fs:contents n))))

(defun %sizing (p cell-w cell-h)
  (list :width (width p) :height (height p)
        :cols (max 1 (floor (width p) (max 1 (or cell-w 9))))
        :lines (max 1 (floor (height p) (max 1 (or cell-h 18))))
        :font *font-size*))

(defun %say-size (s name p &key cell-w cell-h now)
  (let ((said (%sizing p cell-w cell-h))
        (n (%at name "size")))
    (flet ((put ()
             (when (and n (not (equal said (fs:contents n))))
               (setf (fs:contents n) said))))
      (if now (put) (tell s #'put)))))

(defun open-one (s name)
  (let ((tree (%tree name)))
    (when (and tree (null (d:lookup (up s) name)))
      (let ((p (make-instance 'pane
                              :name name :shell (shell-of s) :tree tree
                              :on-resize
                              (lambda (p)
                                (multiple-value-bind (cw ch) (cell p)
                                  (%say-size s name p :cell-w cw :cell-h ch))))))
        (when (eq p (d:lookup (sb-ext:atomic-update (slot-value s 'up) (lambda (m) (if (nth-value 1 (d:lookup m name)) m (d:with m name p)))) name))
          (multiple-value-bind (width height) (measure p)
            (setf (width p) width (height p) height)
            (multiple-value-bind (cw ch) (cell p)
              (%say-size s name p :cell-w cw :cell-h ch :now t)))
          (let ((where (%where name))
                (windowp (%windowp name)))
            (hand (pump s)
                       (lambda ()
                         (open-pane p where :windowp windowp)
                         (%shows s p)
                         (log:note "~a is up" name)))))))))

(defun moved (s name)
  (let ((p (d:lookup (up s) name))
        (tree (%tree name)))
    (cond ((and p tree)
           (hand (pump s)
                      (lambda () (setf (tree p) tree) (%shows s p))))
          ((and (null p) tree (%shownp name)) (open-one s name))
          (t nil))))

(defun toggled (s name)
  (if (%shownp name)
      (unless (d:lookup (up s) name) (open-one s name))
      (let ((p (d:lookup (up s) name)))
        (when p
          (sb-ext:atomic-update (slot-value s 'up) (lambda (old) (d:without old name)))
          (hand (pump s) (lambda () (close-pane p)))))))

(defun %unlisten (s name)
  (dolist (w (d:lookup (watching s) name))
    (fault:or-nothing "a watch already let go of is let go of"
      (watch:unwatch w)))
  (sb-ext:atomic-update (slot-value s 'watching) (lambda (old) (d:without old name))))

(defun follows (name)
  (%at name "tree"))

(defun %listen (s name)
  (%unlisten s name)
  (let ((it (follows name)))
    (when it
      (let ((on-tree (watch:watch it (lambda (of said)
                                       (declare (ignore of said))
                                       (moved s name))
                                  :tells-when :always :poll nil
                                  :name (format nil "screen<-~a" name)))
            (shown (%at name "shown")))
        (sb-ext:atomic-update (slot-value s 'watching) (lambda (old) (d:with old name (list* on-tree
                        (when shown
                          (list (watch:watch shown
                                             (lambda (of said)
                                               (declare (ignore of said))
                                               (toggled s name))
                                             :poll nil
                                             :name (format nil "screen<-~a/shown"
                                                            name))))))))))))

(defun %settle (s)
  (d:do-each (name (d:keys (up s)))
    (unless (fs:at "/ui/surface" name)
      (let ((p (d:lookup (up s) name)))
        (%unlisten s name)
        (sb-ext:atomic-update (slot-value s 'up) (lambda (old) (d:without old name)))
        (when p
          (close-pane p)
          (log:note "~a is gone" name))))))

(defun %tick (s)
  (let ((again (repeating (keys s))))
    (when again (typed s again)))
  (when (zerop (mod (incf (turns s)) +settling+)) (%settle s)))

(defun %loop (s)
  (let ((d (display-of s)))
    (loop :until (or (done s) (job:stopping s))
          :do (%tick s)
              (dispatch d)
              (drain (pump s))
              (let ((woke (wait d (pump s)
                                        (or (deadline (keys s)) 100))))
                (when woke (drain-wake (pump s)))))))

(defun %managing (s said)
  (tell s
        (lambda ()
          (let ((where (fs:at "/wm/said")))
            (when where (setf (fs:contents where) said)))
          (let ((wants (let ((n (fs:at "/wm/wants")))
                         (and n (fs:contents n))))
                (layout (let ((n (fs:at "/wm/placement")))
                          (and n (fs:contents n)))))
            (hand (pump s)
                       (lambda ()
                         (let ((it (wm-of s)))
                           (when it
                             (when wants (take it wants))
                             (laid it layout)))))))))

(defun %rendering (s)
  (d:do-each (p (d:vals (up s)))
    (when (and (chromep p) (dirty p))
      (fault:attempt (lambda () (render p))
                     (format nil "rendering ~a" (name-of p))))))

(defun %shows (s p)
  (paint p)
  (when (and (chromep p) (dirty p)) (cycle (wm-of s))))

(defun %names ()
  (let ((n (fs:at "/ui/surface")))
    (and n (mapcar #'fs:name (fs:children n)))))

(defun %managing-windows (s)
  (when (wm-of s)
    (fault:attempt
     (lambda ()
       (setf (fs:contents (fs:mount (make-instance 'fs:value) "/wm/manages")) :pine)
       (when (module:named "wm") (module:drop "wm"))
       (module:use "wm")
       (chords-wanted s))
     "taking the windows over")))

(defun %rebound (s name p)
  (multiple-value-bind (cw ch) (cell p)
    (%say-size s name p :cell-w cw :cell-h ch :now t))
  (let ((tree (%tree name)))
    (when tree
      (hand (pump s) (lambda () (setf (tree p) tree) (%shows s p))))))

(defun took-up (s name)
  (%listen s name)
  (let ((p (d:lookup (up s) name)))
    (cond ((not (%shownp name))
           (when p
             (sb-ext:atomic-update (slot-value s 'up) (lambda (old) (d:without old name)))
             (hand (pump s) (lambda () (close-pane p)))))
          (p (%rebound s name p))
          (t (open-one s name)))))

(defun %take-up (s)
  (%managing-windows s)
  (dolist (name (%names)) (took-up s name))
  (log:note "~d surface~:p up, ~d watched" (d:size (up s))
            (length (%names)))
  s)

(defun %attend (s)
  (let ((d (connect)))
    (setf (display-of s) d
          (pump s) (make-pump))
    (setf (shell-of s) (open-shell d
                                   :on-pointer (lambda (sh &rest e)
                                                 (apply #'pointing s sh e))
                                   :on-keyboard (lambda (sh &rest e)
                                                  (apply #'keyboard-said s sh e))))
    (setf (wm-of s) (open-manager d :on-said (lambda (said) (%managing s said))
                               :on-render (lambda () (%rendering s))
                               :on-chord (lambda (said) (chorded s said))))
    (when (wm-of s)
      (setf (chrome (shell-of s)) (manager (wm-of s))))
    (setf (says s)
          (job:start (make-instance 'job:actor
                                    :name (format nil "~a-work" (fs:name s))
                                    :on-fault :leave :dispatcher :pinned
                                    :receive (lambda (thunk)
                                               (fault:attempt thunk "the screen")))))
    (%take-up s)
    s))

(defun %shut (s)
  (d:do-each (name (d:keys (watching s)))
    (%unlisten s name))
  (d:do-each (p (d:vals (up s)))
    (fault:or-nothing "a pane whose compositor has gone cannot be told"
      (close-pane p)))
  (d:do-each (name (d:keys (up s)))
    (sb-ext:atomic-update (slot-value s 'up) (lambda (old) (d:without old name))))
  (when (says s) (fault:or-nothing "one already stopped stays stopped"
                   (job:stop (says s))))
  (when (pump s) (close-pump (pump s)))
  (when (display-of s) (disconnect (display-of s)))
  (setf (display-of s) nil (shell-of s) nil (wm-of s) nil)
  s)

(defmethod job:start :before ((s screen))
  (setf (job:body s)
        (lambda ()
          (unwind-protect (progn (%attend s) (%loop s))
            (%shut s)))))

(defun open-screen (&key (name "screen"))
  (let ((s (make-instance 'screen :name name :on-fault :restart)))
    (job:supervise s)
    (job:start s)
    s))

(defmethod ui:declared ((it ui:surface))
  (let ((s (job:named "screen")))
    (when (and (typep s 'screen) (job:alivep s))
      (tell s (lambda () (took-up s (fs:name it)))))))

(defun close-screen (&optional (name "screen"))
  (let ((s (job:named name)))
    (when (typep s 'screen)
      (setf (done s) t)
      (when (pump s) (wake (pump s)))
      (job:stop s))
    s))

(defmethod pine:opening ((what (eql :display)))
  (when (availablep) (open-screen)))
