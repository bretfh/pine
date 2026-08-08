(defpackage #:pine.fs.tree
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:computed #:pine.fs.computed))
  (:export #:root #:at #:ensure #:place #:erase #:walk #:listing #:names
           #:split-name #:absent))

(in-package #:pine.fs.tree)

(define-condition absent (error)
  ((where :initarg :where :reader where))
  (:report (lambda (c s) (format s "nothing at ~a" (where c)))))

(defun root (&optional (name nil))
  (node:make-node name :class 'node:node))

(defun split-name (text)
  (let ((names nil)
        (piece (make-string-output-stream)))
    (flet ((finish ()
             (let ((s (get-output-stream-string piece)))
               (when (plusp (length s)) (push s names)))))
      (loop :for ch :across text
            :if (char= ch #\/) :do (finish)
              :else :do (write-char ch piece))
      (finish))
    (nreverse names)))

(defun %names (pieces)
  (loop :for p :in pieces
        :append (typecase p
                  (string (split-name p))
                  (symbol (list (string-downcase (symbol-name p))))
                  (t (list (princ-to-string p))))))

(defun at (from &rest pieces)
  (loop :with n := from
        :for name :in (%names pieces)
        :do (setf n (and n (node:resolve n name)))
        :finally (return n)))

(defun ensure (from &rest pieces)
  (loop :with n := from
        :for name :in (%names pieces)
        :do (setf n (or (node:resolve n name)
                        (node:attach (node:make-node name) n)))
        :finally (return n)))

(defun place (from pieces value)
  (let ((n (apply #'ensure from (alexandria:ensure-list pieces))))
    (setf (node:contents n) value)
    n))

(defun erase (from &rest pieces)
  (let* ((names (%names pieces))
         (holder (apply #'at from (butlast names))))
    (when holder (node:detach holder (car (last names))))))

(defun walk (n function &key (depth -1))
  (funcall function n)
  (unless (zerop depth)
    (dolist (under (node:nodes n))
      (walk under function :depth (1- depth))))
  n)

(defun listing (n)
  (mapcar #'node:name (node:nodes n)))

(defun names (n)
  (let (acc)
    (walk n (lambda (each) (push (node:full-name each) acc)))
    (nreverse acc)))
