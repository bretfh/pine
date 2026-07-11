(defpackage #:pine.surface
  (:use #:cl)
  (:export #:surface #:surface-on-key #:surface-on-resize
           #:present #:paint-frame #:request-redraw #:surface-metrics))

(in-package #:pine.surface)

(defclass surface ()
  ((on-key    :initarg :on-key    :accessor surface-on-key    :initform nil)
   (on-resize :initarg :on-resize :accessor surface-on-resize :initform nil)))

(defgeneric present (surface))
(defgeneric paint-frame (surface frame))
(defgeneric request-redraw (surface))
(defgeneric surface-metrics (surface)
  (:documentation "Values: cell-width cell-height cols rows."))
