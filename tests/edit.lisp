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
  (when (prompt:askingp) (command:run "cancel"))
  (when (isearch:searching) (isearch:took (isearch:searching)))
  (key:take-next nil)
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

(defun typed (&rest chords)
  "Type at pine the way a keyboard does: one write to /key each. Nothing about the
editor is reached around, so what this proves is what a keyboard would get."
  (dolist (c chords chords)
    (setf (node:contents (tree:at nil "key")) c)))

(test space-is-a-key-like-any-other
  "Every name a keyboard hands over has to be one pine can spell. A space arriving
as itself is a chord nothing can parse, so it lands nowhere."
  (let ((doc (editing)))
    (typed "a" "space" "b" "SPC" "c")
    (is (equal "a b c" (doc:text doc)))))

(test typing-says-nothing-in-the-log
  "A key is not news. The echo line shows the last thing the log said, so a note
per keystroke is the editor talking over itself."
  (editing)
  (let ((before (log:last-said)))
    (typed "x" "y" "C-a")
    (is (equal before (log:last-said))
        "the log said ~s" (log:last-said))))

(test what-a-keyboard-can-send-the-editor-takes
  "Every chord a keyboard hands over, through the one place they arrive. This is
the path a person is on; a test that reaches past /key proves nothing about it."
  (let ((doc (editing)))
    (flet ((typed-into (want &rest chords)
             (setf (node:contents doc) "")
             (doc:goto doc 0 0)
             (apply #'typed chords)
             (is (equal want (doc:text doc)) "~{ ~a~}" chords)))
      (typed-into "abc" "a" "b" "c")
      (typed-into "a b" "a" "space" "b")
      (typed-into "123" "1" "2" "3")
      (typed-into "A" "S-a")
      (typed-into "(hi)" "(" "h" "i" ")")
      (typed-into (format nil "a~%b") "a" "Return" "b")
      (typed-into "ab" "a" "b" "c" "BackSpace")
      (typed-into "xabc" "a" "b" "c" "C-a" "x")
      (typed-into "abx" "a" "b" "C-a" "End" "x")
      (typed-into "acb" "a" "b" "Left" "c")
      (typed-into "bc" "a" "b" "c" "C-a" "C-d")
      (typed-into "a" "a" "b" "c" "C-a" "Right" "C-k")
      (typed-into "ba" "a" "b" "C-a" "Right" "C-t")
      (typed-into "ab " "a" "b" "space" "c" "d" "M-BackSpace")
      (typed-into "abab" "a" "b" "C-a" "C-space" "C-e" "M-w" "C-e" "C-y")
      (typed-into "" "a" "b" "C-a" "C-space" "C-e" "C-w")
      (typed-into "ab" "a" "b" "c" "C-/"))))

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
              :key #'match:name-of :test #'equal))
  (is (somewhere (render:rows :cols 60 :lines 12) "M-x")
      "the frame shows the question")
  (command:run "cancel")
  (is (not (prompt:askingp))))

(test the-completing-read-is-what-m-x-and-c-x-c-f-are
  "The one facility every question goes through: what is offered, how typing
narrows it, what TAB fills in, what C-n chooses and what RET does with it."
  (editing)
  (flet ((typed-in (text) (dolist (c (coerce text 'list)) (typed (string c))))
         (names () (mapcar #'match:name-of (prompt:matching)))
         (clear () (loop :repeat 3 :while (prompt:askingp)
                         :do (command:run "cancel"))))
    (clear)
    (typed "M-x")
    (is (prompt:askingp))
    (is (> (length (names)) 100) "every command is offered before you type")
    (typed-in "begin")
    (is (member "beginning-of-line" (names) :test #'equal) "typing narrows")
    (is (not (member "save-document" (names) :test #'equal)))
    (is (somewhere (render:rows :cols 90 :lines 24) "beginning-of-line")
        "and the candidates are on screen")
    (clear)
    (typed "M-x")
    (typed-in "doc begin")
    (is (member "beginning-of-document" (names) :test #'equal)
        "words in any order, in any place")
    (clear)
    (typed "M-x")
    (typed-in "beginning-of-docu")
    (typed "TAB")
    (is (equal "beginning-of-document" (prompt:so-far)) "TAB fills in the rest")
    (clear)
    (typed "M-x")
    (typed-in "list-")
    (typed "C-n")
    (is (eql 1 (prompt:chosen)))
    (typed "C-p")
    (is (eql 0 (prompt:chosen)))
    (clear)
    (typed "M-x")
    (typed-in "list-documents")
    (typed "Return")
    (is (equal "*documents*" (node:name (doc:current))) "and RET runs it")
    (clear)
    (typed "C-x" "C-f")
    (is (prompt:filep prompt::*prompt*) "a file question knows it is one")
    (is (and (plusp (length (prompt:so-far)))
             (eql #\/ (char (prompt:so-far) 0)))
        "and starts where you are")
    (clear)))

(test a-space-typed-at-key-is-the-space-key
  "A chord written down separates keys with spaces, so a space on its own has to
still be the space key. /key is the one door everything types through."
  (let ((doc (editing)))
    (typed "a" " " "b")
    (is (equal "a b" (doc:text doc)))))

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
    (is (search "I-search" (or (isearch:banner) "")))
    (isearch:step-search (isearch:searching) t)
    (is (= 3 (doc:at-line doc)))
    (isearch:took (isearch:searching))
    (is (null (isearch:searching)))))

(test a-long-file-scrolls-under-the-window
  "A window shows part of a document. Point going out of it has to bring it along,
or everything past the first screenful is unreachable."
  (let ((doc (editing))
        (render:*cols* 60)
        (render:*lines* 10))
    (setf (node:contents doc)
          (format nil "~{line-~d~^~%~}" (loop :for i :below 200 :collect i)))
    (doc:goto doc 0 0)
    (flet ((shows (what)
             (and (somewhere (render:rows :cols 60 :lines 10) what) t)))
      (render:rows :cols 60 :lines 10)
      (is (shows "line-0"))
      (is (not (shows "line-150")))
      (typed "M->")
      (render:rows :cols 60 :lines 10)
      (is (shows "line-199") "point at the end brought the window with it")
      (is (not (shows "line-0")))
      (typed "M-<")
      (render:rows :cols 60 :lines 10)
      (is (shows "line-0") "and back")
      (typed "C-v")
      (render:rows :cols 60 :lines 10)
      (is (not (shows "line-0")) "a page down moved it")
      (typed "M-v")
      (render:rows :cols 60 :lines 10)
      (is (shows "line-0") "and a page back returned it"))))

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
    (is (doc:overlays doc) "what it answered is shown beside the line")
    (is (search "4" (second (first (doc:overlays doc)))))))

(test a-name-written-with-its-package-completes
  "Pine's own source is written in package-qualified names. A completion that only
knew the document's package would be no use in the thing it is written in."
  (let ((doc (editing)))
    (flet ((completing (text col)
             (setf (node:contents doc) text)
             (doc:goto doc 0 col)
             (command:run "complete-symbol")
             (setf (doc:current) doc)
             (doc:text doc)))
      (is (equal "(pine/text/document:make-document"
                 (completing "(pine/text/document:make-docu" 29)))
      (is (search "prefix-at" (completing "(pine/edit/eval::prefix-a" 25))
          "and two colons reach what a package keeps to itself")
      (is (equal "(nosuchpackage:thi" (completing "(nosuchpackage:thi" 18))
          "a package that is not there leaves the text alone"))
    (is (member "pine/fs/node:contents"
                (mode:complete (doc:mode-of doc) doc "pine/fs/node:conten")
                :test #'equal))))

(test what-is-at-point-is-a-symbol-the-mode-knows
  (let ((doc (editing)))
    (setf (node:contents doc) "(car nil)")
    (doc:goto doc 0 2)
    (is (search "car" (or (pine/edit/eval:arglist (doc:mode-of doc) doc) "")))
    (is (member "car" (mode:complete (doc:mode-of doc) doc "ca")
                :test #'equal))))

(test a-file-opens-saves-reverts-and-goes
  "The whole of what a person does with a file, through the commands and the
prompt. A command that asks a question has to be able to take the answer."
  (editing)
  (let ((file (merge-pathnames "pine-file-probe.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(defun one () 1)~%(defun two () 2)~%"))
           (command:run "find-file" (list (namestring file)))
           (let ((d (doc:current)))
             (is (search "(defun one () 1)" (doc:text d)))
             (is (equal '("one" "two")
                        (mapcar #'node:name (node:nodes (tree:at d "defun"))))
                 "and its mode gave it regions")
             (doc:goto d 0 0)
             (typed "x")
             (is (doc:modified d))
             (command:run "save-document")
             (is (not (doc:modified d)))
             (is (search "x(defun one" (uiop:read-file-string file)))
             (doc:goto d 0 0)
             (typed "y" "y")
             (command:run "revert-document" '("yes"))
             (setf (doc:current) d)
             (is (not (search "yy" (doc:text d))) "reverted")
             (doc:goto d 0 0)
             (typed "z")
             (command:run "revert-document" '("no"))
             (setf (doc:current) d)
             (is (search "z" (doc:text d)) "and no means no")
             (doc:kill (node:name d))))
      (ignore-errors (delete-file file)))))

(test a-system-stops-and-takes-its-surface-with-it
  (editing)
  (is (tree:at nil "surface/editor"))
  (pine:drop :edit)
  (is (null (system:named "edit")))
  (is (null (tree:at nil "surface/editor")))
  (setf *editing* nil))
