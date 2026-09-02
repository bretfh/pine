(in-package #:pine/edit)

(defun bindings (m) (mode:bindings m))

(defun %dispatch (document k)
  (let ((m (text:mode-of document)))
    (multiple-value-bind (said typed ran) (mode:dispatch m document k
                                                          (ui:pending))
      (cond ((eq said :pending)
             (setf (ui:pending) typed)
             :pending)
            ((and (consp said) (eq :insert (car said)))
             (setf (ui:pending) nil)
             (setf (ui:last-said) "insert")
             (unless (mode:typing m document (cdr said))
               (text:insert document (cdr said)))
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
        (let ((document (text:current)))
          (and document (%dispatch document k))))))
