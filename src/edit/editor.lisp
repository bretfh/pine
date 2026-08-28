(in-package #:pine/edit)

(defun %drew (moved)
  "Work the editor's frame out again.

Everything that moves anywhere goes through here, which is a sledgehammer where
an edge belongs: the frame is a derived node and would follow what it read by
itself, if the renderer read documents and windows as nodes rather than through
their accessors. It does not, so this stands in for the edges that are missing."
  (declare (ignore moved))
  (let ((s (ui:named "editor")))
    (when s (fault:attempt (lambda () (node:stir s)) "the frame"))))

(defmethod text:reparsed ((document text:document))
  (%drew nil))

(defun %key ()
  "Where a key arrives. Writing a chord here is typing it, so a keyboard, a test and
another pine all press keys the same way."
  (node:place "key"
              :reads (lambda () (ui:spelled (ui:pending)))
              :writes (lambda (value)
                        (commit:writing
                          (dolist (k (ui:chord (princ-to-string value)))
                            (dispatch k))))
              :describes "write a chord here to type it"))

(defun %sources ()
  (source :command
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (cons (command:name c)
                                             (command:describes c)))
                           (command:commands))))
  (source :document
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (d) (cons (node:name d)
                                             (or (text:file-of d) "")))
                           (text:documents))))
  (source :mode
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
                           (mode:modes))))
  (source :setting
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (each)
                             (cons (string-downcase (string (car each))) (cdr each)))
                           +settings+)))
  (source :window
                 (lambda (typed)
                   (declare (ignore typed))
                   "What the compositor says there is. Read through the namespace:
the editor has never heard of a window manager, and does not have to."
                   (let ((n (tree:at nil "wm" "windows")))
                     (when n
                       (loop :for id :in (node:contents n)
                             :for each := (tree:at n (princ-to-string id))
                             :for said := (and each (node:contents each))
                             :collect (cons (format nil "~a ~a" id
                                                    (or (getf said :title) ""))
                                            (or (getf said :app) "")))))))
  (source :file #'files))

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
  "An editor has somebody looking at it, so the question goes on the screen and
the command runs again when they answer."
  (%asking c))

(defmethod ui:confirming ((s edit) question thunk)
  (ask (format nil "~a " question)
              :candidates (list "yes" "no") :must-match t
              :then (lambda (said) (when (equal "yes" said) (funcall thunk))))
  :asking)

(defun %editor ()
  "The editor, laid out for whatever is showing it. The screen says how big it is by
writing /surface/editor/size, and this follows that the way it follows anything
else it read."
  (let* ((s (ui:named "editor"))
         (size (and s (ui:size s)))
         (*font* (getf size :font)))
    (frame :cols (or (getf size :cols) *cols*)
                  :lines (or (getf size :lines) *lines*))))

(defun type-text (text)
  "Type TEXT a character at a time, the way a keyboard does."
  (loop :for ch :across text
        :do (dispatch (ui:make-key (string ch))))
  (text:point (text:current)))

(defmethod job:start ((s edit))
  (%sources)
  (setf command:*at* s
        (commit:on-commit :edit) #'%drew)
  (node:attach (%key) (tree:root))
  (let ((scratch (or (text:named "scratch")
                     (text:make-document "scratch"
                                        :mode (make-instance 'mode:lisp)))))
    (setf (text:current) scratch)
    (seed scratch))
  (ui:builds "editor" #'%editor :as 'ui:window :shown t)
  s)

(defmethod job:stop ((s edit))
  (setf command:*at* nil
        (commit:on-commit :edit) nil)
  (ui:take-next nil)
  (took-all)
  (text:forget-all)
  (dolist (win (windows)) (node:detach (node:over win) (node:name win)))
  (tree:erase nil "surface/editor")
  s)
