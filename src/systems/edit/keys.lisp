(in-package #:pine/edit)

(defun bindings (m) (mode:bindings m))

(defun %dispatch (buffer k)
  (let ((m (text:mode-of buffer)))
    (multiple-value-bind (said typed ran) (mode:dispatch m buffer k
                                                          (ui:pending))
      (cond ((eq said :pending)
             (setf (ui:pending) typed)
             :pending)
            ((and (consp said) (eq :insert (car said)))
             (setf (ui:pending) nil)
             (setf (ui:last-said) "insert")
             (unless (mode:typing m buffer (cdr said))
               (text:insert buffer (cdr said)))
             :inserted)
            (t
             (setf (ui:pending) nil)
             (setf (ui:last-said) (cond (ran ran)
                                    ((eq said :taken) "the mode took it")
                                    (t (ui:last-said))))
             said)))))

(defun dispatch (k)
  (meter:timing (:key)
    (or (ui:reading k)
        (let ((buffer (text:current)))
          (and buffer (%dispatch buffer k))))))
