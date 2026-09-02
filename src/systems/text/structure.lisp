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
  (loop :for at := r :then (node:parent at)
        :while at
        :when (typep at 'document) :do (return at)))

(defun %region (under name covers)
  (let ((r (node:child under name
                       (lambda () (make-instance 'region :name name :parent under
                                                         :covers covers)))))
    (setf (covers r) covers)
    r))

(defun %cleared (under)
  "Take the regions off UNDER. What the mode says now is the whole answer, so one
it no longer says is one that stands for nothing."
  (dolist (each (node:nodes under) under)
    (when (typep each 'region) (node:detach under (node:name each)))))

(defun %forgotten (under kept)
  "Let go of the regions UNDER no longer has.

A region is kept under its name so that one still there is the same node it was
and a watcher on it goes on watching. One the mode has stopped naming is not still
there: typing a name a character at a time says a different one on every key, and
every one of them stayed for as long as the image ran."
  (dolist (name (d:keys (d:all (node:memo under))) under)
    (unless (member name kept :test #'equal)
      (d:drop! (node:memo under) name))))

(defun %build (under said)
  "Put the spans the mode said into the namespace under UNDER, keeping the node that
was already at each name so anything watching one keeps watching it.

Two spans a mode gives one name are two places, and the second takes NAME<2>. One
node standing for both would cover only the last of them, and writing it would
replace text it was never standing for.

Every level is cleared and every level is forgotten. Only the top was, so a span
inside one that the mode stopped saying stayed where it was with the extent it had
before the edit -- and writing it replaced text it was never standing for, which
is the one thing this is written to stop."
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
          (node:attach r under)
          (when (mode:inside-of each) (%build r (mode:inside-of each))))))
    (%forgotten under kept)))

(defun restructure (doc)
  "Ask the mode what this text divides into, and put it in the namespace. Regions
are nodes with identity, so one that is still there is the same node it was and a
watcher on it goes on watching."
  (let ((said (mode:structure (mode-of doc) doc)))
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
