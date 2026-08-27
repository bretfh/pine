(defpackage #:pine/edit/keys
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:meter #:pine/run/meter)
                    (#:command #:pine/run/command) (#:key #:pine/ui/key)
                    (#:mode #:pine/mode) (#:doc #:pine/text/document)
                    (#:fault #:pine/run/fault))
  (:export #:dispatch #:bindings))
(in-package #:pine/edit/keys)

(defun bindings (m) (mode:bindings m))

(defun %dispatch (document k)
  (let ((m (doc:mode-of document)))
    (multiple-value-bind (said so-far ran) (mode:dispatch m document k
                                                          (key:pending))
      (cond ((eq said :pending)
             (setf (key:pending) so-far)
             :pending)
            ((and (consp said) (eq :insert (car said)))
             (setf (key:pending) nil)
             (setf (key:last) "insert")
             (unless (mode:insert m document (cdr said))
               (doc:insert document (cdr said)))
             :inserted)
            (t
             (setf (key:pending) nil)
             (setf (key:last) (cond (ran ran)
                                    ((eq said :taken) "the mode took it")
                                    (t (key:last))))
             said)))))

(defun dispatch (k)
  "What a key means: whoever asked to read the next one, else this document's mode
and the chords its class inherits."
  (meter:timing (:key)
    (or (key:reading k)
        (let ((document (doc:current)))
          (and document (%dispatch document k))))))
