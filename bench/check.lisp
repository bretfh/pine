(require :asdf)

(defvar *said* nil)

(defun minep (condition)
  (let ((*package* (find-package :cl-user))
        (*print-pretty* nil))
    (and (search "PINE" (princ-to-string condition)) t)))

(handler-bind ((warning (lambda (c)
                          (when (minep c)
                            (push (list (type-of c) (princ-to-string c)) *said*)))))
  (asdf:load-system :pine/all))

(let ((said (reverse *said*)))
  (cond ((null said)
         (princ :loaded)
         (terpri)
         (sb-ext:exit :code 0))
        (t
         (format t "~&~d warning~:p from pine's own code:~%" (length said))
         (loop :for (kind text) :in said
               :do (format t "  ~(~a~): ~a~%" kind text))
         (finish-output)
         (sb-ext:exit :code 1))))
