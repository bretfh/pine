(defpackage #:pine/wayland/chords
  (:use #:cl #:wayflan-client #:pine/wayland/protocol)
  (:local-nicknames (#:log #:pine/run/log) (#:key #:pine/ui/key))
  (:export #:chords #:make-chords #:availablep #:attend #:ask-for #:forget
           #:eat-next #:every-key #:mask #:keysym))
(in-package #:pine/wayland/chords)

(defparameter +modifiers+
  '((:shift . :shift) (:ctrl . :ctrl) (:meta . :mod1) (:super . :mod4))
  "What pine calls a modifier and what river does. Its mod1 is what a keyboard
calls alt and pine calls meta; its mod4 is super. The protocol spells a bitfield
as the list of what is set, so that is what a chord comes to.")

(defclass chords ()
  ((of     :initarg :of   :reader of)
   (seat   :initform nil  :accessor seat)
   (eating :initform nil  :accessor eating)
   (bound  :initform nil  :accessor bound)
   (told   :initarg :told :accessor told :initform nil))
  (:documentation "The chords a compositor is holding for pine.

A key it was never asked for is one it gives to whatever has focus; a key it was
asked for it hands here instead, whatever is focused. That is the whole of why
this exists: it is the only way a window manager hears a key that was not typed
at it."))

(defun make-chords (of &key told) (make-instance 'chords :of of :told told))

(defun availablep (c) (and c (of c) t))

(defun mask (k)
  "The modifiers a key is held with, as the protocol spells them."
  (loop :for (mine . theirs) :in +modifiers+
        :when (ecase mine
                (:shift (key:shift k)) (:ctrl (key:ctrl k))
                (:meta (key:meta k)) (:super (key:super k)))
          :collect theirs))

(defun keysym (k)
  "The keysym a key is, as xkb numbers it. A name it does not know is no chord."
  (let ((said (xkb:xkb-keysym-from-name (key:keysym-name (key:sym k)) '())))
    (when (and said (plusp said)) said)))

(defun every-key (chords)
  "Every distinct key any of these chords is spelled with. A compositor says which
key was pressed only for a key it was asked for, so the second key of a chord has
to be asked for as much as the first."
  (let ((all nil))
    (dolist (chord chords (nreverse all))
      (dolist (k (key:chord chord))
        (pushnew k all :test #'eq)))))

(defun attend (c proxy)
  "Take this seat, and the object that lets the next key be taken with it."
  (when (and (availablep c) (null (seat c)))
    (setf (seat c) proxy)
    (setf (eating c)
          (ignore-errors (river-xkb-bindings-v1.get-seat (of c) proxy)))
    (when (eating c)
      (push (evlambda (:ate-unbound-key () (%said c nil)))
            (wl-proxy-hooks (eating c)))))
  c)

(defun %said (c k)
  (when (told c) (funcall (told c) (and k (key:text (list k))))))

(defun forget (c)
  (dolist (each (bound c))
    (ignore-errors (river-xkb-binding-v1.destroy (cdr each))))
  (setf (bound c) nil))

(defun ask-for (c chords)
  "Ask the compositor for every key these chords are spelled with, and enable
each. Only inside a manage sequence: that is where the protocol allows it."
  (when (and (availablep c) (seat c))
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

(defun eat-next (c)
  "Take the next key from whatever has focus too: pine is part way through a chord
and the rest of it is not the focused window's to see."
  (when (and (availablep c) (eating c))
    (ignore-errors
     (river-xkb-bindings-seat-v1.ensure-next-key-eaten (eating c))))
  c)
