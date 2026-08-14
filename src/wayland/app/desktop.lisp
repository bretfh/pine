(defpackage #:pine.wayland.app.desktop
  (:use #:cl #:wayflan-client #:pine.wayland.protocol #:pine.wayland.connection
        #:pine.wayland.surface #:pine.wayland.input)
  (:local-nicknames (#:a #:alexandria) (#:node #:pine.ui.node)
                    (#:uiw #:pine.ui.wire) (#:d #:pine.data))
  (:export #:desktop #:run-desktop #:received #:open-for #:close-for
           #:placement #:role-for #:tree-fn #:on-widgets #:on-panel))

(in-package #:pine.wayland.app.desktop)

(defclass desktop ()
  ((sys      :initarg :sys  :accessor ds-sys :initform nil)
   (conn     :initarg :conn :reader ds-conn)
   (ref      :initform nil  :accessor ds-ref)
   (surfaces :initform (make-hash-table :test 'equal) :reader ds-surfaces)
   (wire     :initform (make-hash-table :test 'equal) :reader ds-wire)
   (roles    :initform (make-hash-table :test 'equal) :reader ds-roles)
   (pump     :initarg :pump :reader ds-pump)
   (done     :initform nil  :accessor ds-done)))

(defun send (ds message)
  (a:when-let ((ref (ds-ref ds))) (ignore-errors (sento.actor:tell ref message))))

(defun acting (ds)
  (lambda (id) (lambda (&rest args) (send ds (list :widget-action :id id :args args)))))

(defun tree-fn (ds name)
  (lambda () (uiw:wire->node (gethash name (ds-wire ds)) :on-action (acting ds))))

(defun placement (role)
  (flet ((spec (layer anchor axis avail exclusive margin)
           (d:map :layer layer :anchor anchor :axis axis :avail avail
                  :exclusive exclusive :margin margin)))
    (case role
      (:background (spec :background '(:top :left :bottom :right) :wh 3840 0
                         '(0 0 0 0)))
      (:bar     (spec :top '(:top :left :bottom) :w 160 :w '(0 0 0 0)))
      (:echo    (spec :top '(:bottom :left :right) :h 3840 :h '(0 0 0 0)))
      (:overlay (spec :overlay '(:top :right) :wh 460 0 '(8 8 0 0)))
      (t        (spec :overlay '(:top :left) :wh 460 0 '(8 0 0 8))))))

(defun role-for (ds name)
  (or (gethash name (ds-roles ds))
      (cond ((string= name "bar") :bar)
            ((string= name "echo") :echo)
            (t :panel))))

(defun open-for (ds name)
  (let* ((spec (placement (role-for ds name)))
         (builds (tree-fn ds name)))
    (multiple-value-bind (mw mh) (measure-panel builds :avail-w (d:at spec :avail))
      (let* ((axis (d:at spec :axis))
             (w (if (member axis '(:w :wh)) mw 0))
             (h (if (member axis '(:h :wh)) mh 0))
             (keeps (d:at spec :exclusive))
             (ls (open-layer-surface (ds-conn ds) builds
                                     :layer (d:at spec :layer)
                                     :anchor (d:at spec :anchor)
                                     :width w :height h
                                     :exclusive (case keeps (:w w) (:h h) (t keeps))
                                     :margin (d:at spec :margin)
                                     :on-closed (lambda () (close-for ds name)))))
        (setf (gethash name (ds-surfaces ds)) ls)))))

(defun close-for (ds name)
  (a:when-let ((ls (gethash name (ds-surfaces ds))))
    (ignore-errors (zwlr-layer-surface-v1.destroy (ls-layer-surf ls)))
    (ignore-errors (wl-surface.destroy (ls-wl-surface ls)))
    (setf (wl-conn-surfaces (ds-conn ds))
          (remove ls (wl-conn-surfaces (ds-conn ds)) :key #'cdr))
    (remhash name (ds-surfaces ds))))

(defun on-widgets (ds name tree as)
  (setf (gethash name (ds-wire ds)) tree)
  (when as (setf (gethash name (ds-roles ds)) as))
  (let ((ls (gethash name (ds-surfaces ds))))
    (cond (ls (build-tree ls) (paint-surface ls))
          ((member (role-for ds name) '(:bar :echo :background))
           (open-for ds name)))))

(defun on-panel (ds name show)
  (if show
      (unless (gethash name (ds-surfaces ds))
        (when (gethash name (ds-wire ds)) (open-for ds name)))
      (close-for ds name)))

(defun repaint-all (ds)
  (maphash (lambda (name ls)
             (declare (ignore name))
             (build-tree ls)
             (paint-surface ls))
           (ds-surfaces ds)))

(defun received (ds message)
  (case (first message)
    (:attached
     (setf (ds-ref ds) (pine.net.attach:accept-attached (ds-sys ds) message))
     (send ds (list :refresh)))
    (:widgets
     (destructuring-bind (&key surface tree as &allow-other-keys) (rest message)
       (pine.frontend:enqueue (ds-pump ds)
                              (lambda () (on-widgets ds surface tree as)))))
    (:panel
     (destructuring-bind (&key name show &allow-other-keys) (rest message)
       (pine.frontend:enqueue (ds-pump ds) (lambda () (on-panel ds name show)))))
    (:style
     (destructuring-bind (&key styles &allow-other-keys) (rest message)
       (pine.ui.css:install styles)
       (pine.frontend:enqueue (ds-pump ds) (lambda () (repaint-all ds)))))
    (t nil)))

(defun run-desktop (&key (host pine.net.server:*host*) (port pine.net.server:*port*))
  (let* ((conn (connect-desktop))
         (ds (make-instance 'desktop :conn conn :pump (pine.frontend:make-pump))))
    (setf *on-hover*
          (lambda (n) (send ds (list :hint :text (or (and n (node:hint n)) "")))))
    (setf (ds-sys ds)
          (pine.frontend:attach :desktop (lambda (message) (received ds message))
                                :host host :port port))
    (unwind-protect
         (pine.frontend:run (wl-conn-backing conn) (ds-pump ds)
                            :done (lambda () (ds-done ds)))
      (pine.frontend:close-pump (ds-pump ds))
      (pine.frontend:shutdown (wl-conn-backing conn)))))

(pine.net.attach:frontend :desktop #'run-desktop)
