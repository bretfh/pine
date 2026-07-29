(defpackage #:pine.provider.clock
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:mount #:unmount #:tick))

(in-package #:pine.provider.clock)
(named-readtables:in-readtable pine.path:syntax)

;;;; Time, as paths. A surface that shows the hour reads /clock/hour and is
;;;; rebuilt when it changes, which is once an hour rather than once a second.

(defvar *timer* nil)
(defvar *key* :pine-clock)

(defun tick ()
  "Put the time where anything watching it will see it."
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time (get-universal-time))
    (ns:write /clock (fset:map (:at (get-universal-time))
                               (:second second)
                               (:minute (format nil "~2,'0d" minute))
                               (:hour (format nil "~2,'0d" hour))
                               (:day day)
                               (:month month)
                               (:year year)
                               (:weekday weekday)))))

(defun provider ()
  "What /clock is. A clause that says only what a path is for leaves the value
in the tree, so the time is still written, watched and read like anything else."
  (ns:provider
   (/clock {:doc "the time: :at :second :minute :hour :day :month :year :weekday"})))

(defun mount (&key system (every 1))
  "Keep /clock current on SYSTEM's wheel timer."
  (unmount)
  (ns:write /clock (provider))
  (tick)
  (when system
    (let ((timer (sento.actor-system:scheduler system)))
      (setf *timer* timer)
      (sento.wheel-timer:schedule-recurring
       timer every every (lambda () (pine.err:attempt #'tick "the clock")) *key*)))
  nil)

(defun unmount ()
  (when *timer*
    (sento.wheel-timer:cancel *timer* *key*)
    (setf *timer* nil))
  nil)
