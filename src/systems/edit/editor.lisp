(in-package #:pine/edit)

(defmethod text:reparsed ((buffer text:buffer))
  (fs:touch buffer))

(defclass typed (fs:derived) ())

(defmethod fs:volatile-p ((n typed) &optional name) (declare (ignore name)) t)

(defmethod fs:works ((n typed)) (ui:spelled (ui:pending)))

(defmethod fs:takes ((n typed) value)
  (fs:writing
    (dolist (k (ui:chord (princ-to-string value)))
      (dispatch k))))

(defun %key ()
  (make-instance 'typed :name "key" :describes "write a chord here to type it"))

(defun %sources ()
  (completes :command
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (cons (command:name c)
                                             (command:describes c)))
                           (command:commands))))
  (completes :buffer
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (d) (cons (fs:name d)
                                             (or (text:file-of d) "")))
                           (text:buffers))))
  (completes :mode
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
                           (mode:modes))))
  (completes :setting
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (each)
                             (cons (string-downcase (string (car each))) (cdr each)))
                           +settings+)))
  (completes :window
                 (lambda (typed)
                   (declare (ignore typed))
                   (let ((n (fs:at "/wm/windows")))
                     (when n
                       (flet ((field (id what)
                                (let ((it (fs:at n (princ-to-string id) what)))
                                  (or (and it (fs:contents it)) ""))))
                         (loop :for id :in (fs:contents n)
                               :collect (cons (format nil "~a ~a" id (field id "title"))
                                              (field id "app"))))))))
  (completes :file #'files))

(defun %asking (c)
  (let* ((spec (first (command:asks c)))
         (initial (getf spec :initial)))
    (ask (or (getf spec :prompt) (format nil "~a: " (command:name c)))
                :category (getf spec :category)
                :history (getf spec :history)
                :candidates (getf spec :candidates)
                :must-match (getf spec :must-match)
                :initial (if (functionp initial) (funcall initial) initial)
                :then (lambda (answer) (command:run c (list answer))))
    :asking))

(defmethod command:asking ((s edit) c)
  (%asking c))

(defmethod ui:confirming ((s edit) question thunk)
  (ask (format nil "~a " question)
              :candidates (list "yes" "no") :must-match t
              :then (lambda (said) (when (equal "yes" said) (funcall thunk))))
  :asking)

(defun %editor ()
  (let* ((s (fs:at "/ui/surface" "editor"))
         (size (and s (ui:size s)))
         (*font* (getf size :font)))
    (frame :cols (or (getf size :cols) *cols*)
                  :lines (or (getf size :lines) *lines*))))

(defun type-text (text)
  (loop :for ch :across text
        :do (dispatch (ui:make-key (string ch))))
  (text:point (text:current)))

(defmethod job:start ((s edit))
  (%sources)
  (setf command:*at* s)
  (fs:mount (%key) "/edit/key")
  (let ((scratch (or (fs:at "/text" "scratch")
                     (text:make-buffer "scratch"
                                        :mode (make-instance 'mode:lisp)))))
    (setf (text:current) scratch)
    (seed scratch))
  (ui:make-surface "editor" #'%editor :as 'ui:toplevel :starts :up)
  s)

(defmethod job:stop ((s edit))
  (setf command:*at* nil)
  (ui:take-next nil)
  (took-all)
  (text:forget-all)
  (dolist (win (panes)) (fs:detach (fs:parent win) (fs:name win)))
  s)
