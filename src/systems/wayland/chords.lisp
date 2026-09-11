(in-package #:pine/wayland)

(defparameter +river-modifiers+
  '((:shift . :shift) (:ctrl . :ctrl) (:meta . :mod1) (:super . :mod4)))

(defclass chords ()
  ((of     :initarg :of   :reader of)
   (seat   :initform nil  :accessor seat)
   (eating :initform nil  :accessor eating)
   (bound  :initform nil  :accessor bound)
   (told   :initarg :told :accessor told :initform nil)))

(defun make-chords (of &key told) (make-instance 'chords :of of :told told))

(defun usablep (c) (and c (of c) t))

(defun mask (k)
  (loop :for (mine . theirs) :in +river-modifiers+
        :when (ecase mine
                (:shift (ui:shift k)) (:ctrl (ui:ctrl k))
                (:meta (ui:meta k)) (:super (ui:super k)))
          :collect theirs))

(defun keysym (k)
  (let ((said (xkb:xkb-keysym-from-name (ui:keysym-name (ui:sym k)) '())))
    (when (and said (plusp said)) said)))

(defun every-key (chords)
  (let ((all nil))
    (dolist (chord chords (nreverse all))
      (dolist (k (ui:chord chord))
        (pushnew k all :test #'eq)))))

(defun attend (c proxy)
  (when (and (usablep c) (null (seat c)))
    (setf (seat c) proxy)
    (setf (eating c)
          (fault:or-nothing "the compositor may not offer chord binding"
            (river-xkb-bindings-v1.get-seat (of c) proxy)))
    (when (eating c)
      (push (evlambda (:ate-unbound-key () (%said c nil)))
            (wl-proxy-hooks (eating c)))))
  c)

(defun %said (c k)
  (when (told c) (funcall (told c) (and k (ui:spelled (list k))))))

(defun forget (c)
  (dolist (each (bound c))
    (fault:or-nothing "a binding the compositor dropped is dropped"
      (river-xkb-binding-v1.destroy (cdr each))))
  (setf (bound c) nil))

(defun ask-for (c chords)
  (when (and (usablep c) (seat c))
    (forget c)
    (dolist (k (every-key chords))
      (let ((sym (keysym k)))
        (when sym
          (let ((it (river-xkb-bindings-v1.get-xkb-binding
                     (of c) (seat c) sym (mask k))))
            (push (cons k it) (bound c))
            (push (evlambda
                    (:pressed () (%said c k))
                    (:released () nil)
                    (:stop-repeat () nil))
                  (wl-proxy-hooks it))
            (river-xkb-binding-v1.enable it)))))
    (log:note "~d chord~:p asked of the compositor" (length (bound c))))
  c)

(defgeneric eat-next (it)
  (:method ((c chords))
    (when (and (usablep c) (eating c))
      (fault:or-nothing "the compositor may have taken the seat back"
        (river-xkb-bindings-seat-v1.ensure-next-key-eaten (eating c))))
    c))
