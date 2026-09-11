(in-package #:pine/text)

(defun %after (line from)
  (let* ((start (position-if-not (lambda (ch) (member ch '(#\Space #\Tab)))
                                 line :start from))
         (end (and start (or (position-if
                              (lambda (ch) (member ch '(#\Space #\Tab #\( #\))))
                              line :start start)
                             (length line)))))
    (when (and start end (< start end)) (subseq line start end))))

(defun %named-after (buffer word)
  (loop :for n :from (1- (line-count buffer)) :downto 0
        :for line := (line buffer n)
        :for at := (search word line :from-end t :test #'char-equal)
        :when at
          :do (return (values (or (%after line (+ at (length word)))
                                  (and (< (1+ n) (line-count buffer))
                                       (%after (line buffer (1+ n)) 0)))
                              n))))

(defun %package-of (buffer)
  (multiple-value-bind (said at) (%named-after buffer "in-package")
    (values (or (and said (find-package (string-upcase (string-left-trim "#:" said))))
                (find-package :pine/user)
                (find-package :cl-user))
            at)))

(defun %readtable-of (buffer)
  (multiple-value-bind (said at) (%named-after buffer "in-readtable")
    (values (when said
              (or (fault:or-nothing "what the file says may name no readtable"
                    (named-readtables:find-readtable
                     (let ((*package* (find-package :cl-user)) (*read-eval* nil))
                       (read-from-string said))))
                  (fault:or-nothing "nor as a keyword"
                    (named-readtables:find-readtable
                     (intern (string-upcase (string-left-trim "#:" said))
                             :keyword)))))
            at)))

(defun %says (buffer key word worker)
  (let ((had (declared buffer))
        (now (tick buffer)))
    (unless (and had (eql (first had) now))
      (setf had (list now))
      (setf (declared buffer) had))
    (let ((cell (assoc key (rest had))))
      (cond (cell (second cell))
            (t (multiple-value-bind (v at) (funcall worker buffer)
                 (setf (cdr had)
                       (cons (list key v (or at -1) word) (cdr had)))
                 v))))))

(defun package-of (buffer)
  (%says buffer :package "in-package" #'%package-of))

(defgeneric readtable-of (of)
  (:method (of) (declare (ignore of)) nil))

(defmethod readtable-of ((buffer buffer))
  (%says buffer :readtable "in-readtable" #'%readtable-of))

(defun reading (buffer)
  (values (package-of buffer) (or (readtable-of buffer) *readtable*)))
