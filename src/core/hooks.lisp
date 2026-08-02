(defpackage #:pine.core.hooks
  (:use :cl)
  (:export
   #:add-init-hook
   #:add-shutdown-hook
   #:run-init-hooks
   #:run-shutdown-hooks))

(in-package #:pine.core.hooks)

;;;; What runs when the image comes up and goes down. Named, so declaring the
;;;; same hook again replaces it and re-loading a config is safe. A table and
;;;; not a list, because a hook is added from whatever thread it is on; a map
;;;; has no order of its own, so each hook carries the count it was added at.

(defvar *hooks* (pine.data:table)
  "(WHEN . NAME) to (ORDER . FUNCTION), for WHEN :INIT or :SHUTDOWN.")

(defvar *added* (sento.atomic:make-atomic-reference :value 0)
  "How many hooks have been added, which is the order they run in.")

(defun %add (when name fn)
  (pine.data:put *hooks* (cons when name)
                 (cons (sento.atomic:atomic-swap *added* #'1+) fn))
  name)

(defun %hooks (when)
  "The hooks for WHEN, as (NAME . FUNCTION), in the order they were added."
  (let ((acc nil))
    (fset:do-map (key entry (pine.data:all *hooks*))
      (when (eq when (car key))
        (push (list (car entry) (cdr key) (cdr entry)) acc)))
    (loop :for (nil name fn) :in (sort acc #'< :key #'first)
          :collect (cons name fn))))

(defun %run (when hooks)
  (loop :for (name . fn) :in hooks
        :do (handler-case (funcall fn)
              (error (c)
                (format *error-output* "~(~a~) hook ~a failed: ~a~%" when name c)))))

(defun add-init-hook (name fn) (%add :init name fn))

(defun add-shutdown-hook (name fn) (%add :shutdown name fn))

(defun run-init-hooks ()
  (%run :init (%hooks :init)))

(defun run-shutdown-hooks ()
  "Shutdown runs in the reverse of the order things were added, so what came up
last comes down first."
  (%run :shutdown (reverse (%hooks :shutdown))))
