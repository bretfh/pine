(in-package :pine/test)

(def-suite* :pine/edit :in :pine)

(defvar *editing* nil)

(defun editing ()
  "A pine with text and the editor up, made once. Starting it twice would be two
images, and there is one."
  (unless *editing*
    (pine:start)
    (pine:use :text)
    (pine:use :edit)
    (setf *editing* t))
  (let ((scratch (or (doc:named "scratch")
                     (doc:make-document "scratch"
                                        :mode (make-instance 'mode:lisp)))))
    (setf (doc:current) scratch)
    (setf (node:contents scratch) "")
    (doc:goto scratch 0 0)
    (window:show (window:focused) scratch)
    scratch))

(test the-editor-starts-with-a-document-in-a-window
  (let ((scratch (editing)))
    (is (eq scratch (doc:current)))
    (is (typep (doc:mode-of scratch) 'mode:lisp))
    (is (eq scratch (window:shows (window:focused))))
    (is (tree:at nil "system/edit") "and it is a job you can see")))

(test typing-lands-in-the-document-and-in-the-frame
  (editing)
  (pine/edit:type-text "(defun hello () 42)")
  (is (equal "(defun hello () 42)" (doc:text (doc:current))))
  (let ((rows (render:rows :cols 60 :lines 10)))
    (is (somewhere rows "(defun hello"))
    (is (somewhere rows "scratch") "the modeline says which document")))

(test a-chord-written-to-key-is-a-chord-typed
  (editing)
  (pine/edit:type-text "hello")
  (setf (node:contents (tree:at nil "key")) "C-a")
  (is (zerop (doc:at-col (doc:current))))
  (setf (node:contents (tree:at nil "key")) "C-e")
  (is (= 5 (doc:at-col (doc:current))))
  (setf (node:contents (tree:at nil "key")) "C-a C-k")
  (is (equal "" (doc:text (doc:current))))
  (command:run "yank")
  (is (equal "hello" (doc:text (doc:current)))))

(test a-prefix-chord-waits-for-the-rest-of-itself
  (editing)
  (keys:dispatch (key:parse "C-x"))
  (is (key:pending) "C-x on its own is pending")
  (keys:dispatch (key:parse "C-g"))
  (is (null (key:pending))))

(test the-prompt-is-a-mode-and-narrows-as-you-type
  (editing)
  (command:run "run-command")
  (is (prompt:askingp))
  (is (typep (doc:mode-of (doc:current)) 'emode:prompt))
  (pine/edit:type-text "beginning-of-doc")
  (is (member "beginning-of-document" (prompt:matching)
              :key #'prompt:name-of :test #'equal))
  (is (somewhere (render:rows :cols 60 :lines 12) "M-x")
      "the frame shows the question")
  (command:run "cancel")
  (is (not (prompt:askingp))))

(test what-a-prompt-answers-is-what-it-does
  (editing)
  (let ((said nil))
    (prompt:ask "Probe: " :then (lambda (answer) (setf said answer)))
    (pine/edit:type-text "yes")
    (command:run "answer")
    (is (equal "yes" said))
    (is (not (prompt:askingp)))))

(test a-search-lands-and-steps
  (let ((doc (editing)))
    (setf (node:contents doc) (format nil "one~%two~%three~%two again"))
    (doc:goto doc 0 0)
    (isearch:start)
    (pine/edit:type-text "two")
    (is (= 1 (doc:at-line doc)))
    (is (search "I-search" (or (isearch:said) "")))
    (isearch:step-search (isearch:searching) t)
    (is (= 3 (doc:at-line doc)))
    (isearch:took (isearch:searching))
    (is (null (isearch:searching)))))

(test a-listing-row-stands-for-a-thing
  (editing)
  (command:run "list-documents")
  (is (equal "*documents*" (node:name (doc:current))))
  (is (typep (listing:place) 'doc:document))
  (is (typep (doc:mode-of (doc:current)) 'emode:listing)))

(test windows-split-and-close
  (editing)
  (command:run "split-window-below")
  (is (= 2 (length (window:windows))))
  (is (> (length (render:rows :cols 40 :lines 20)) 10) "the frame draws both")
  (command:run "other-window")
  (command:run "delete-other-windows")
  (is (= 1 (length (window:windows)))))

(test evaluating-a-form-answers-beside-it
  (let ((doc (editing)))
    (setf (node:contents doc) "(+ 2 2)")
    (doc:move doc :text 1)
    (command:run "eval-last-expression")
    (is (render:overlays doc) "what it answered is shown beside the line")
    (is (search "4" (second (first (render:overlays doc)))))))

(test what-is-at-point-is-a-symbol-the-mode-knows
  (let ((doc (editing)))
    (setf (node:contents doc) "(car nil)")
    (doc:goto doc 0 2)
    (is (search "car" (or (pine/edit/eval:arglist (doc:mode-of doc) doc) "")))
    (is (member "car" (mode:complete (doc:mode-of doc) doc "ca")
                :test #'equal))))

(test a-system-stops-and-takes-its-surface-with-it
  (editing)
  (is (tree:at nil "surface/editor"))
  (pine:drop :edit)
  (is (null (system:named "edit")))
  (is (null (tree:at nil "surface/editor")))
  (setf *editing* nil))
