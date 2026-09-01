(in-package #:pine/ui)

(defun %covers (w line col)
  (and (<= (top w) line) (<= line (bottom w))
       (<= (left w) col) (< col (right w))))

(defgeneric under (widget line col)
  (:documentation "The deepest widget at this place that answers interaction: an
action, a choice, a slider, a field. Nothing where none does.

The default descends, so a container needs no method and a widget says only what it
does with a hit.")
  (:method ((w widget) line col)
    "The last part that answers, and not the first. A STACK paints its parts in
the order they were written and the last is on top, so the first one to cover a
place was the one underneath -- and a click landed on what was behind."
    (let ((found nil))
      (dolist (part (parts w) found)
        (let ((it (under part line col)))
          (when it (setf found it))))))
  (:method ((w action) line col)
    (when (%covers w line col) (or (call-next-method) w)))
  (:method ((w choice) line col)
    (when (%covers w line col) (or (call-next-method) w)))
  (:method ((w slider) line col) (when (%covers w line col) w))
  (:method ((w scroll) line col) (when (%covers w line col) (call-next-method)))
  (:method ((w label) line col)
    "A run of text answers a hit only when it is a field. One that took every hit
over its own text would swallow the clicks of whatever it sits inside."
    (when (and (changed w) (%covers w line col)) w)))

(defun value-at (w col)
  "What a click at COL means for a slider, from the width it was arranged at."
  (let* ((n (max 1 (width w)))
         (rel (max 0 (min n (- col (left w)))))
         (span (- (high w) (low w))))
    (+ (low w) (round (* (/ rel n) span)))))

(defgeneric clicked (widget col)
  (:documentation "The nullary thunk clicking this widget at COL means.")
  (:method ((w widget) col) (declare (ignore col)) nil)
  (:method ((w action) col) (declare (ignore col)) (click w))
  (:method ((w choice) col) (declare (ignore col)) (click w))
  (:method ((w slider) col)
    (let ((fn (changed w)) (v (value-at w col)))
      (when fn (lambda () (funcall fn v))))))

(defun clicked-at (root line col)
  (let ((hit (under root line col)))
    (and hit (clicked hit col))))
