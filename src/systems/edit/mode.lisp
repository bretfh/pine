(in-package #:pine/edit)

(defclass prompt (mode:text) ())

(defclass listing (mode:text)
  ((shown-rows :initarg :shown-rows :accessor shown-rows :initform nil)
   (on-enter   :initarg :on-enter   :accessor on-enter   :initform nil)))

(defclass debugger (mode:text) ())

(defmethod mode:setting ((m prompt) key)
  (case key (:aside t) (t (call-next-method))))

(defmethod mode:setting ((m listing) key)
  (case key (:aside t) (t (call-next-method))))

(defmethod mode:typing ((m mode:text) buffer string)
  (when (mode:says buffer :overwrite nil)
    (let ((line (text:at-line buffer)) (col (text:at-col buffer)))
      (when (< col (length (text:line buffer line)))
        (text:delete-region buffer line col line
                           (min (length (text:line buffer line))
                                (+ col (length string)))))
      (text:insert buffer string)
      t)))
