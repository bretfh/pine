(in-package :pine/test)

(def-suite* :pine/term :in :pine)

(defun %terminal ()
  (editing)
  (pine:use :term)
  (pine/run/command:run "terminal" (list "/bin/sh"))
  (pine/term:current))

(defun %printf (said)
  "A line the shell will print, with printf's own escape before every [. Typed at
a shell, so it carries no escape of its own: what comes back from the program is
what the terminal is asked to colour."
  (with-output-to-string (out)
    (write-string "printf '" out)
    (loop :for ch :across said
          :do (when (char= ch #\[) (write-char #\\ out) (write-string "033" out))
              (write-char ch out))
    (write-string "'" out)
    (terpri out)))

(defmacro with-terminal ((it) &body body)
  `(let ((,it (%terminal)))
     (unwind-protect (progn ,@body)
       (ignore-errors (pine/run/command:run "terminal-close")))))

(test a-terminal-is-a-document-and-a-job-at-once
  (with-terminal (term)
    (is (typep term 'doc:document))
    (is (typep term 'job:job))
    (is (job:alivep term))
    (is (typep (doc:mode-of term) 'pine/term/terminal:shell))
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

(test the-colour-a-program-asked-for-is-spans-on-the-document
  "A terminal's colour is not a thing of its own: it is spans over the text, which
is what a search that has just landed says and what a parse says. One kind of
thing, painted one way."
  (with-terminal (term)
    (setf (node:contents term) (%printf "[31mred[0m [1;32mgreen[0m"))
    (is (until (lambda () (search "red green" (doc:text term))) :seconds 5))
    (let* ((spans (doc:spans term))
           (red (find-if (lambda (each) (equal '(172 66 66) (first (fourth each))))
                         spans))
           (green (find-if (lambda (each)
                             (equal '(144 169 89) (first (fourth each))))
                           spans)))
      (is (not (null red)) "what the program asked for, as numbers")
      (is (not (null green)))
      (is (eql 1 (third (fourth green))) "and bold is bold"))))

(test what-the-grid-paints-is-what-the-program-asked-for
  (with-terminal (term)
    (setf (node:contents term) (%printf "[31mred[0m"))
    (is (until (lambda () (search "red" (doc:text term))) :seconds 5))
    (window:show (window:focused) term)
    (let* ((rows (render:rows :cols 60 :lines 8))
           (runs (loop :for row :in rows :append (cdr row))))
      (is (find-if (lambda (run) (equal '(172 66 66) (subseq run 1 4))) runs)
          "the cell the grid painted carries the program's own red"))))

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

(test a-terminal-runs-a-program-and-its-screen-is-the-document
  "The whole of what a terminal is for, through the pty: what is typed reaches the
program and what it wrote is text a window shows."
  (with-terminal (term)
    (is (job:alivep term))
    (setf (node:contents term) (format nil "echo from-the-program~%"))
    (is (until (lambda () (search "from-the-program" (doc:text term)))
               :seconds 5))
    (pine/term/terminal:resize term 100 30)
    (is (eql 100 (node:contents (tree:at term "wide"))))
    (is (eql 30 (node:contents (tree:at term "tall"))))))
