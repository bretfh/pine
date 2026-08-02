(defpackage #:pine.editor.view-state
  (:use #:cl)
  (:export #:frame #:frame-cols #:frame-rows))

(in-package #:pine.editor.view-state)

;;;; The surface a frontend gave this client, in cells. It is the only geometry
;;;; the editor holds: how big the thing it is painting into is.
;;;;
;;;; What each pane shows, where it is scrolled, how tall it was arranged and
;;;; which one has the keyboard are paths, so none of them are here. A pane is a
;;;; place, and what a place holds is the tree's to say.

(defclass frame ()
  ((cols :initarg :cols :accessor frame-cols :initform 80)
   (rows :initarg :rows :accessor frame-rows :initform 30)))
