(defpackage #:pine/edit
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:commit #:pine/fs/commit)
                    (#:job #:pine/run/job)
                    (#:system #:pine/run/system) (#:command #:pine/run/command)
                    (#:fault #:pine/run/fault) (#:log #:pine/run/log)
                    (#:key #:pine/ui/key) (#:surface #:pine/ui/surface)
                    (#:build #:pine/ui/build)
                    (#:mode #:pine/mode) (#:doc #:pine/text/document)
                    (#:parser #:pine/text/ts/parser)
                    (#:window #:pine/edit/window)
                    (#:keys #:pine/edit/keys) (#:render #:pine/edit/render)
                    (#:prompt #:pine/edit/prompt) (#:match #:pine/edit/matching)
                    (#:isearch #:pine/edit/isearch)
                    (#:help #:pine/edit/help)
                    (#:debugger #:pine/edit/debugger))
  (:export #:edit #:type-text))
(in-package #:pine/edit)

(defclass edit (system:system) ()
  (:documentation "Windows onto documents, the chords that act on them, and the
surface pine shows. A system like any other: nothing in the substrate names
it."))

(system:offers 'edit)

(defun %drew (moved)
  (declare (ignore moved))
  (let ((s (surface:named "editor")))
    (when s (fault:attempt (lambda () (node:stir s)) "the frame"))))

(defun %key ()
  "Where a key arrives. Writing a chord here is typing it, so a keyboard, a test and
another pine all press keys the same way."
  (node:place "key"
              :reads (lambda () (key:text (key:pending)))
              :writes (lambda (value)
                        (commit:writing
                          (dolist (k (key:chord (princ-to-string value)))
                            (keys:dispatch k))))
              :describes "write a chord here to type it"))

(defun %sources ()
  (prompt:source :command
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (cons (command:name c)
                                             (command:describes c)))
                           (command:commands))))
  (prompt:source :document
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (d) (cons (node:name d)
                                             (or (doc:file-of d) "")))
                           (doc:documents))))
  (prompt:source :mode
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
                           (mode:modes))))
  (prompt:source :setting
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (each)
                             (cons (string-downcase (string (car each))) (cdr each)))
                           help:+settings+)))
  (prompt:source :window
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
  (prompt:source :file #'match:files))

(defun %asking (c)
  (let* ((spec (first (command:asks c)))
         (initial (getf spec :initial)))
    (prompt:ask (or (getf spec :prompt) (format nil "~a: " (command:name c)))
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

(defmethod build:confirming ((s edit) question thunk)
  (prompt:ask (format nil "~a " question)
              :candidates (list "yes" "no") :must-match t
              :then (lambda (said) (when (equal "yes" said) (funcall thunk))))
  :asking)

(defun %frame ()
  "The editor, laid out for whatever is showing it. The screen says how big it is by
writing /surface/editor/size, and this follows that the way it follows anything
else it read."
  (let* ((s (surface:named "editor"))
         (size (and s (surface:size s)))
         (render:*font* (getf size :font)))
    (render:frame :cols (or (getf size :cols) render:*cols*)
                  :lines (or (getf size :lines) render:*lines*))))

(defun type-text (text)
  "Type TEXT a character at a time, the way a keyboard does."
  (loop :for ch :across text
        :do (keys:dispatch (key:make-key (string ch))))
  (doc:point (doc:current)))

(defmethod job:start ((s edit))
  (%sources)
  (setf command:*at* s
        doc:*on-current* #'window:follow
        parser:*showing* #'render:showing
        parser:*on-parse* (lambda (document) (declare (ignore document)) (%drew nil))
        (commit:on-commit :edit) #'%drew
        fault:*on-fault*
        (lambda (f)
          (if (fault:standingp f)
              (ignore-errors (debugger:show (fault:condition-of f) :fault f))
              (log:note "~a" (fault:condition-of f)))))
  (node:attach (%key) (tree:root))
  (let ((scratch (or (doc:named "scratch")
                     (doc:make-document "scratch"
                                        :mode (make-instance 'mode:lisp)))))
    (setf (doc:current) scratch)
    (window:seed scratch))
  (surface:builds "editor" #'%frame :as 'surface:window :shown t)
  s)

(defmethod job:stop ((s edit))
  (setf command:*at* nil
        doc:*on-current* nil
        parser:*showing* nil
        parser:*on-parse* nil
        (commit:on-commit :edit) nil
        fault:*on-fault* nil)
  (key:take-next nil)
  (isearch:took-all)
  (parser:forget-all)
  (dolist (win (window:windows)) (node:detach (node:over win) (node:name win)))
  (tree:erase nil "surface/editor")
  s)
