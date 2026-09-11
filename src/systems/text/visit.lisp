(in-package #:pine/text)

(defun recent () *recent*)

(defun %recently (name)
  (setf *recent* (cons name (remove name *recent* :test #'equal)))
  (when (> (length *recent*) +recent-kept+)
    (setf *recent* (subseq *recent* 0 +recent-kept+)))
  name)

(defun %on-the-host (where)
  (let ((at (fs:at "/file"))
        (names (fs:split-name (namestring where))))
    (loop :while (and at (rest names))
          :do (setf at (fs:child at (pop names))))
    (and at names (fs:node-for at (first names)))))

(defun %place (where)
  (or (fs:at where) (%on-the-host where)))

(defun visit (buffer where)
  (let ((n (%place where)))
    (when n
      (setf (source buffer) n)
      (%recently (origin buffer))
      (setf (text buffer) (or (fs:contents n) ""))
      (let ((m (mode:mode-for (origin buffer))))
        (when m (setf (mode-of buffer) m)))
      (let ((had (visited buffer)))
        (if had (goto buffer (first had) (second had))
            (goto buffer 0 0)))
      (restructure buffer)
      (setf (modified buffer) nil))
    buffer))

(defmethod visiting ((buffer buffer) where)
  (visit buffer where))

(defun save (buffer &optional where)
  (when where (setf (source buffer) (%place where)))
  (let ((n (source buffer)))
    (when n
      (mode:saving (mode-of buffer) buffer)
      (setf (fs:contents n) (text buffer))
      (setf (modified buffer) nil)
      (origin buffer))))

(defun revert (buffer)
  (let ((n (source buffer)))
    (when (and n (fs:contents n))
      (leaving buffer)
      (visit buffer n))))

(defun %syntax ()
  (let ((it (make-ts-runtime)))
    (fault:attempt (lambda () (ensure-ts it)) "loading tree-sitter")
    (when (ts-loaded-p it)
      (setf *runtime* it))))

(command:defcommand "buffers" () (:describes "every buffer there is")
  (mapcar #'fs:name (buffers)))

(command:defcommand "structure" (&optional name)
    (:describes "what this buffer's mode makes of it")
  (let ((d (if name (fs:at (root) name) (current))))
    (when d (mapcar #'fs:name (regions d)))))

(defmethod job:start ((s text))
  (%syntax)
  (root)
  (let ((scratch (make-buffer "scratch" :mode (make-instance 'mode:lisp))))
    (setf (current) scratch))
  s)

(defmethod job:stop ((s text))
  (forget-all)
  (dolist (d (buffers)) (kill (fs:name d)))
  s)

