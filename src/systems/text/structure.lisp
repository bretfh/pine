(in-package #:pine/text)

(defun covered (r)
  (let ((doc (%buffer r)))
    (when doc
      (destructuring-bind (from to) (covers r)
        (region (lines doc) (car from) (cdr from)
                      (car to) (cdr to))))))

(defun (setf covered) (value r)
  (let ((doc (%buffer r)))
    (when doc
      (destructuring-bind (from to) (covers r)
        (delete-region doc (car from) (cdr from) (car to) (cdr to))
        (goto doc (car from) (cdr from))
        (insert doc (princ-to-string value))
        (restructure doc))))
  value)

(defun %buffer (r)
  (loop :for at := r :then (fs:parent at)
        :while at
        :when (typep at 'buffer) :do (return at)))

(defmethod fs:names ((r region))
  '((:text . "what it covers")))

(defmethod fs:read ((r region) (name (eql :text)))
  (covered r))

(defmethod fs:write ((r region) (name (eql :text)) value)
  (setf (covered r) value))

(defun %region (under name covers)
  (let ((r (fs:ensure-child under name
                     (lambda ()
                       (make-instance 'region :name name :parent under :covers covers)))))
    (setf (covers r) covers)
    r))

(defun %cleared (under)
  (dolist (each (fs:children under) under)
    (when (typep each 'region) (fs:detach under (fs:name each)))))

(defun %forgotten (under kept)
  (dolist (name (d:keys (fs::dentries under)) under)
    (unless (member name kept :test #'equal)
      (sb-ext:atomic-update (slot-value under 'fs::dentries) (lambda (old) (d:without old name))))))

(defun %build (under said)
  (%cleared under)
  (let ((seen (d:no-map))
        (kept nil))
    (dolist (each said)
      (let* ((base (mode:name-of each))
             (had (or (d:lookup seen base) 0))
             (name (if (plusp had) (format nil "~a<~d>" base (1+ had)) base)))
        (setf seen (d:with seen base (1+ had)))
        (push name kept)
        (let ((r (%region under name (list (mode:from-of each) (mode:to-of each)))))
          (fs:mount r under)
          (when (mode:inside-of each) (%build r (mode:inside-of each))))))
    (%forgotten under kept)))

(defun restructure (doc)
  (let ((said (mode:regions (mode-of doc) doc)))
    (%build doc said)
    said))

(defun fresh-structure (doc)
  (unless (eql (structured doc) (tick doc))
    (setf (structured doc) (tick doc))
    (restructure doc))
  doc)

(defmethod fs:children ((doc buffer))
  (fresh-structure doc)
  (call-next-method))

(defmethod fs:child ((doc buffer) name)
  (fresh-structure doc)
  (call-next-method))

(defun regions (doc)
  (remove-if-not (lambda (n) (typep n 'region)) (fs:children doc)))
