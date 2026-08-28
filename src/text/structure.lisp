(in-package #:pine/text)

(defmethod node:contents ((r region))
  (let ((doc (%document r)))
    (when doc
      (destructuring-bind (from to) (covers r)
        (region (lines doc) (car from) (cdr from)
                      (car to) (cdr to))))))

(defmethod (setf node:contents) (value (r region))
  "Writing a region replaces the text it covers."
  (let ((doc (%document r)))
    (when doc
      (destructuring-bind (from to) (covers r)
        (delete-region doc (car from) (cdr from) (car to) (cdr to))
        (goto doc (car from) (cdr from))
        (insert doc (princ-to-string value))
        (restructure doc))))
  value)

(defun %document (r)
  (loop :for at := r :then (node:over at)
        :while at
        :when (typep at 'document) :do (return at)))

(defun %region (under name covers)
  (let ((r (node:child under name
                       (lambda () (make-instance 'region :name name :over under
                                                         :covers covers)))))
    (setf (covers r) covers)
    r))

(defun %build (under said)
  "Put what the mode said into the namespace under UNDER, keeping the node that was
already at each name so anything watching one keeps watching it."
  (dolist (each said)
    (destructuring-bind (name from to &rest children) each
      (let ((r (%region under (princ-to-string name) (list from to))))
        (node:attach r under)
        (when children (%build r children))))))

(defun restructure (doc)
  "Ask the mode what this text divides into, and put it in the namespace. Regions
are nodes with identity, so one that is still there is the same node it was and a
watcher on it goes on watching."
  (let ((said (mode:structure (mode-of doc) doc)))
    (dolist (each (node:nodes doc))
      (when (typep each 'region) (node:detach doc (node:name each))))
    (%build doc said)
    said))

(defun fresh-structure (doc)
  "Build the regions again where the text has moved since they were built. A region
worked out before an edit covers the wrong span, and writing one replaces text it
was never standing for."
  (unless (eql (structured doc) (tick doc))
    (setf (structured doc) (tick doc))
    (restructure doc))
  doc)

(defmethod node:nodes ((doc document))
  (fresh-structure doc)
  (call-next-method))

(defmethod node:resolve ((doc document) name)
  (fresh-structure doc)
  (call-next-method))

(defun regions (doc)
  (remove-if-not (lambda (n) (typep n 'region)) (node:nodes doc)))
