(require :asdf)
(require :sb-introspect)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :pine/all))

(defpackage #:pine/bench/shot (:use #:cl))
(in-package #:pine/bench/shot)

(defparameter +into+ (or (uiop:getenv "PINE_SHOT") "/tmp/"))

(defun main ()
  "Every surface a running pine would be showing, as a PNG each. No display and
no compositor: this is the painter's own drawing, in a file you can look at."
  (pine:start)
  (pine:use :text)
  (pine:use :edit)
  (pine:use :desk)
  (pine/edit:type-text "(defun hello (who) (format t \"hi ~a\" who))")
  (dolist (each (pine/paint/shot:surfaces :into +into+))
    (when each (format t "~&~a~%" each)))
  (pine:stop))

(main)
(sb-ext:exit)
