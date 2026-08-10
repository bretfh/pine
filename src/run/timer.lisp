(defpackage #:pine.run.timer
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:fault #:pine.run.fault))
  (:export #:timing #:*soonest* #:attend #:leave #:every-seconds #:once #:cancel #:names))

(in-package #:pine.run.timer)

(defvar *wheel* nil)
(defparameter *soonest* 0.05)
(defvar *system* nil)
(defvar *named* (d:table))

(defun attend (system)
  "Every interval in pine runs on this system's wheel timer. The image has one
clock, not a thread per thing that repeats."
  (setf *system* system
        *wheel* (sento.actor-system:scheduler system)))

(defun timing () *wheel*)

(defun names () (d:keys (d:all *named*)))

(defun %away (thunk what)
  "Off the timer thread: the wheel is one thread for the whole image, and these
thunks shell out, read files and paint."
  (lambda ()
    (let ((system *system*))
      (flet ((run () (fault:attempt thunk what)))
        (if system
            (sento.tasks:with-context (system)
              (sento.tasks:task-start #'run))
            (run))))))

(defun cancel (name)
  (let ((had (d:at (d:all *named*) name)))
    (when (and had *wheel*)
      (ignore-errors (sento.wheel-timer:cancel *wheel* had))
      (d:drop! *named* name))
    name))

(defun every-seconds (seconds thunk &key (as (gensym "EVERY-")) (what "a tick"))
  "Run THUNK every SECONDS, under the name AS. Asking again under one name
replaces what was there."
  (when *wheel*
    (cancel as)
    (let ((signature (gensym "PINE-EVERY-"))
          (seconds (max seconds *soonest*)))
      (sento.wheel-timer:schedule-recurring *wheel* seconds seconds
                                            (%away thunk what) signature)
      (d:keep! *named* as signature)
      as)))

(defun once (seconds thunk &key (what "a tick"))
  (when *wheel*
    (sento.wheel-timer:schedule-once *wheel* (max seconds *soonest*)
                                     (%away thunk what))))

(defun leave ()
  (dolist (name (names)) (cancel name))
  (setf *wheel* nil *system* nil)
  t)
