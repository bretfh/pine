(require :asdf)
(require :sb-introspect)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :pine/all))

(defpackage #:pine/bench/shot (:use #:cl))
(in-package #:pine/bench/shot)

(defparameter +into+ (or (uiop:getenv "PINE_SHOT") "/tmp/"))

(defparameter +config+
  (let ((said (uiop:getenv "PINE_CONFIG")))
    (when (and said (plusp (length said))) said)))

(defun main ()
  "Every surface a running pine would be showing, as a PNG each. No display and
no compositor: this is the same drawing, in a file you can look at.

PINE_CONFIG names a config to read, and then it is that config's surfaces."
  (pine:start)
  (if +config+
      (progn (pine:load-config +config+)
             (dolist (f (pine/run/fault:faults))
               (format t "~&~a: ~a~%" (pine/run/fault:label f)
                       (pine/run/fault:condition-of f))))
      (progn (pine:use :text) (pine:use :edit) (pine:use :desk)))
  (pine:use :edit)
  (dolist (each (pine/ui/surface:surfaces))
    (setf (pine/ui/surface:shown each) t))
  (pine/edit:type-text "(defun hello (who) (format t \"hi ~a\" who))")
  (dolist (each (pine/paint/shot:surfaces :into +into+ :width 1200 :height 1400))
    (when each (format t "~&~a~%" each)))
  (pine:stop))

(main)
(sb-ext:exit)
