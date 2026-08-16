(in-package :pine/test)

(def-suite* :pine/term :in :pine)

(defun %terminal ()
  (editing)
  (pine:use :term)
  (pine/run/command:run "terminal" (list "/bin/sh"))
  (pine/term:current))

(defmacro with-terminal ((it) &body body)
  `(let ((,it (%terminal)))
     (unwind-protect (progn ,@body)
       (ignore-errors (pine/run/command:run "terminal-close")))))

(test a-terminal-is-a-document-and-a-job-at-once
  (with-terminal (term)
    (is (typep term 'doc:document))
    (is (typep term 'job:job))
    (is (job:alivep term))
    (is (typep (doc:mode-of term) 'pine/term/mode:shell))
    (is (eq term (doc:current)))))

(test writing-a-terminal-is-typing-at-the-program
  (with-terminal (term)
    (setf (node:contents term) (format nil "echo the-pty-answered~%"))
    (is (until (lambda () (search "the-pty-answered" (doc:text term)))
               :seconds 5))))

(test what-a-key-means-here-is-a-method-not-an-edit
  (with-terminal (term)
    (let ((was (doc:text term)))
      (pine/edit:type-text "echo two")
      (is (until (lambda () (search "echo two" (doc:text term))) :seconds 5)
          "it reached the program, and the program echoed it")
      (is (not (equal was (doc:text term)))))))

(test a-terminals-size-is-a-node
  (with-terminal (term)
    (is (eql (pine/term/terminal:wide term)
             (node:contents (tree:at term "wide"))))
    (pine/term/terminal:resize term 100 30)
    (is (eql 100 (node:contents (tree:at term "wide"))))
    (is (eql 30 (node:contents (tree:at term "tall"))))))

(test the-frame-draws-a-terminal-like-any-document
  (with-terminal (term)
    (setf (node:contents term) (format nil "echo drawn-in-the-frame~%"))
    (is (until (lambda () (search "drawn-in-the-frame" (doc:text term)))
               :seconds 5))
    (window:show (window:focused) term)
    (is (somewhere (render:rows :cols 80 :lines 24) "drawn-in-the-frame"))))

(test closing-one-takes-its-job-and-its-document-with-it
  (let ((term (%terminal)))
    (let ((name (node:name term)))
      (pine/run/command:run "terminal-close")
      (is (null (doc:named name)))
      (is (null (job:named name)))
      (is (null (pine/term/terminal:terminals))))))
