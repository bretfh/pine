(defpackage #:pine/edit/keys
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:meter #:pine/run/meter)
                    (#:command #:pine/run/command)
                    (#:mode #:pine/mode) (#:doc #:pine/text/document)
                    (#:fault #:pine/run/fault))
  (:export #:dispatch #:bindings))
(in-package #:pine/edit/keys)

(defun bindings (m) (mode:bindings m))

(defun %dispatch (document k)
  (let ((m (doc:mode-of document)))
    (multiple-value-bind (said so-far ran) (mode:dispatch m document k
                                                          (ui:pending))
      (cond ((eq said :pending)
             (setf (ui:pending) so-far)
             :pending)
            ((and (consp said) (eq :insert (car said)))
             (setf (ui:pending) nil)
             (setf (ui:last-said) "insert")
             (unless (mode:insert m document (cdr said))
               (doc:insert document (cdr said)))
             :inserted)
            (t
             (setf (ui:pending) nil)
             (setf (ui:last-said) (cond (ran ran)
                                    ((eq said :taken) "the mode took it")
                                    (t (ui:last-said))))
             said)))))

(defun dispatch (k)
  "What a key means: whoever asked to read the next one, else this document's mode
and the chords its class inherits."
  (meter:timing (:key)
    (or (ui:reading k)
        (let ((document (doc:current)))
          (and document (%dispatch document k))))))
