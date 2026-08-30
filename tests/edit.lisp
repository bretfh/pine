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
  (when (edit:askingp) (command:run "cancel"))
  (when (edit:searching) (edit:took (edit:searching)))
  (ui:take-next nil)
  (let ((scratch (or (text:named "scratch")
                     (text:make-document "scratch"
                                        :mode (make-instance 'mode:lisp)))))
    (setf (text:current) scratch)
    (setf (node:contents scratch) "")
    (text:goto scratch 0 0)
    (edit:show (edit:focused) scratch)
    scratch))

(test the-editor-starts-with-a-document-in-a-window
  (let ((scratch (editing)))
    (is (eq scratch (text:current)))
    (is (typep (text:mode-of scratch) 'mode:lisp))
    (is (eq scratch (edit:shows (edit:focused))))
    (is (tree:at "/system/edit") "and it is a job you can see")))

(test typing-lands-in-the-document-and-in-the-frame
  (editing)
  (pine/edit:type-text "(defun hello () 42)")
  (is (equal "(defun hello () 42)" (text:text (text:current))))
  (let ((rows (edit:rows :cols 60 :lines 10)))
    (is (somewhere rows "(defun hello"))
    (is (somewhere rows "scratch") "the modeline says which document")))

(test a-chord-written-to-key-is-a-chord-typed
  (editing)
  (pine/edit:type-text "hello")
  (setf (node:contents (tree:at "/key")) "C-a")
  (is (zerop (text:at-col (text:current))))
  (setf (node:contents (tree:at "/key")) "C-e")
  (is (= 5 (text:at-col (text:current))))
  (setf (node:contents (tree:at "/key")) "C-a C-k")
  (is (equal "" (text:text (text:current))))
  (command:run "yank")
  (is (equal "hello" (text:text (text:current)))))

(defun typed (&rest chords)
  "Type at pine the way a keyboard does: one write to /key each. Nothing about the
editor is reached around, so what this proves is what a keyboard would get."
  (dolist (c chords chords)
    (setf (node:contents (tree:at "/key")) c)))

(test space-is-a-key-like-any-other
  "Every name a keyboard hands over has to be one pine can spell. A space arriving
as itself is a chord nothing can parse, so it lands nowhere."
  (let ((doc (editing)))
    (typed "a" "space" "b" "SPC" "c")
    (is (equal "a b c" (text:text doc)))))

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
             (text:goto doc 0 0)
             (apply #'typed chords)
             (is (equal want (text:text doc)) "~{ ~a~}" chords)))
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
  (edit:dispatch (ui:parse "C-x"))
  (is (ui:pending) "C-x on its own is pending")
  (edit:dispatch (ui:parse "C-g"))
  (is (null (ui:pending))))

(test the-prompt-is-a-mode-and-narrows-as-you-type
  (editing)
  (command:run "run-command")
  (is (edit:askingp))
  (is (typep (text:mode-of (text:current)) 'edit:prompt))
  (pine/edit:type-text "beginning-of-doc")
  (is (member "beginning-of-document" (edit:matching)
              :key #'edit:name-of :test #'equal))
  (is (somewhere (edit:rows :cols 60 :lines 12) "M-x")
      "the frame shows the question")
  (command:run "cancel")
  (is (not (edit:askingp))))

(test the-completing-read-is-what-m-x-and-c-x-c-f-are
  "The one facility every question goes through: what is offered, how typing
narrows it, what TAB fills in, what C-n chooses and what RET does with it."
  (editing)
  (flet ((typed-in (text) (dolist (c (coerce text 'list)) (typed (string c))))
         (names () (mapcar #'edit:name-of (edit:matching)))
         (clear () (loop :repeat 3 :while (edit:askingp)
                         :do (command:run "cancel"))))
    (clear)
    (typed "M-x")
    (is (edit:askingp))
    (is (> (length (names)) 100) "every command is offered before you type")
    (typed-in "begin")
    (is (member "beginning-of-line" (names) :test #'equal) "typing narrows")
    (is (not (member "save-document" (names) :test #'equal)))
    (is (somewhere (edit:rows :cols 90 :lines 24) "beginning-of-line")
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
    (is (equal "beginning-of-document" (edit:so-far)) "TAB fills in the rest")
    (clear)
    (typed "M-x")
    (typed-in "list-")
    (typed "C-n")
    (is (eql 1 (edit:chosen)))
    (typed "C-p")
    (is (eql 0 (edit:chosen)))
    (clear)
    (typed "M-x")
    (typed-in "list-documents")
    (typed "Return")
    (is (equal "*documents*" (node:name (text:current))) "and RET runs it")
    (clear)
    (typed "C-x" "C-f")
    (is (edit:filep edit::*prompt*) "a file question knows it is one")
    (is (and (plusp (length (edit:so-far)))
             (eql #\/ (char (edit:so-far) 0)))
        "and starts where you are")
    (clear)))

(test a-space-typed-at-key-is-the-space-key
  "A chord written down separates keys with spaces, so a space on its own has to
still be the space key. /key is the one door everything types through."
  (let ((doc (editing)))
    (typed "a" " " "b")
    (is (equal "a b" (text:text doc)))))

(test what-a-prompt-answers-is-what-it-does
  (editing)
  (let ((said nil))
    (edit:ask "Probe: " :then (lambda (answer) (setf said answer)))
    (pine/edit:type-text "yes")
    (command:run "answer")
    (is (equal "yes" said))
    (is (not (edit:askingp)))))

(test a-search-lands-and-steps
  (let ((doc (editing)))
    (setf (node:contents doc) (format nil "one~%two~%three~%two again"))
    (text:goto doc 0 0)
    (edit:start)
    (pine/edit:type-text "two")
    (is (= 1 (text:at-line doc)))
    (is (search "I-search" (or (edit:banner) "")))
    (edit:step-search (edit:searching) t)
    (is (= 3 (text:at-line doc)))
    (edit:took (edit:searching))
    (is (null (edit:searching)))))

(test a-long-file-scrolls-under-the-window
  "A window shows part of a document. Point going out of it has to bring it along,
or everything past the first screenful is unreachable."
  (let ((doc (editing))
        (edit:*cols* 60)
        (edit:*lines* 10))
    (setf (node:contents doc)
          (format nil "~{line-~d~^~%~}" (loop :for i :below 200 :collect i)))
    (text:goto doc 0 0)
    (flet ((shows (what)
             (and (somewhere (edit:rows :cols 60 :lines 10) what) t)))
      (edit:rows :cols 60 :lines 10)
      (is (shows "line-0"))
      (is (not (shows "line-150")))
      (typed "M->")
      (edit:rows :cols 60 :lines 10)
      (is (shows "line-199") "point at the end brought the window with it")
      (is (not (shows "line-0")))
      (typed "M-<")
      (edit:rows :cols 60 :lines 10)
      (is (shows "line-0") "and back")
      (typed "C-v")
      (edit:rows :cols 60 :lines 10)
      (is (not (shows "line-0")) "a page down moved it")
      (typed "M-v")
      (edit:rows :cols 60 :lines 10)
      (is (shows "line-0") "and a page back returned it"))))

(test a-listing-row-stands-for-a-thing
  (editing)
  (command:run "list-documents")
  (is (equal "*documents*" (node:name (text:current))))
  (is (typep (edit:place) 'text:document))
  (is (typep (text:mode-of (text:current)) 'edit:listing)))

(test windows-split-and-close
  (editing)
  (command:run "split-window-below")
  (is (= 2 (length (edit:windows))))
  (is (> (length (edit:rows :cols 40 :lines 20)) 10) "the frame draws both")
  (command:run "other-window")
  (command:run "delete-other-windows")
  (is (= 1 (length (edit:windows)))))

(test evaluating-a-form-answers-beside-it
  (let ((doc (editing)))
    (setf (node:contents doc) "(+ 2 2)")
    (text:move doc :text 1)
    (command:run "eval-last-expression")
    (is (text:overlays doc) "what it answered is shown beside the line")
    (is (search "4" (second (first (text:overlays doc)))))))

(test a-name-written-with-its-package-completes
  "Pine's own source is written in package-qualified names. A completion that only
knew the document's package would be no use in the thing it is written in."
  (let ((doc (editing)))
    (flet ((completing (text col)
             (setf (node:contents doc) text)
             (text:goto doc 0 col)
             (command:run "complete-symbol")
             (setf (text:current) doc)
             (text:text doc)))
      (is (equal "(pine/text:make-document"
                 (completing "(pine/text:make-docu" 29)))
      (is (search "prefix-at" (completing "(pine/edit::prefix-a" 25))
          "and two colons reach what a package keeps to itself")
      (is (equal "(nosuchpackage:thi" (completing "(nosuchpackage:thi" 18))
          "a package that is not there leaves the text alone"))
    (is (member "pine/fs/node:contents"
                (mode:complete (text:mode-of doc) doc "pine/fs/node:conten")
                :test #'equal))))

(test what-is-at-point-is-a-symbol-the-mode-knows
  (let ((doc (editing)))
    (setf (node:contents doc) "(car nil)")
    (text:goto doc 0 2)
    (is (search "car" (or (pine/edit:arglist (text:mode-of doc) doc) "")))
    (is (member "car" (mode:complete (text:mode-of doc) doc "ca")
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
           (let ((d (text:current)))
             (is (search "(defun one () 1)" (text:text d)))
             (is (equal '("one" "two")
                        (mapcar #'node:name (node:nodes (tree:at d "defun"))))
                 "and its mode gave it regions")
             (text:goto d 0 0)
             (typed "x")
             (is (text:modified d))
             (command:run "save-document")
             (is (not (text:modified d)))
             (is (search "x(defun one" (uiop:read-file-string file)))
             (text:goto d 0 0)
             (typed "y" "y")
             (command:run "revert-document" '("yes"))
             (setf (text:current) d)
             (is (not (search "yy" (text:text d))) "reverted")
             (text:goto d 0 0)
             (typed "z")
             (command:run "revert-document" '("no"))
             (setf (text:current) d)
             (is (search "z" (text:text d)) "and no means no")
             (text:kill (node:name d))))
      (ignore-errors (delete-file file)))))

(test a-system-stops-and-takes-its-surface-with-it
  (editing)
  (is (tree:at "/surface/editor"))
  (pine:drop :edit)
  (is (null (system:named "edit")))
  (is (null (tree:at "/surface/editor")))
  (setf *editing* nil))

(test two-files-with-one-name-are-two-documents
  "A document was named by the file's own name and any document already at that
name was reused, so opening src/ui/system.lisp after src/wm/system.lisp pointed
the first one at the second file and the first was gone."
  (let ((a #p"/tmp/pine-test-a/") (b #p"/tmp/pine-test-b/"))
    (ensure-directories-exist a)
    (ensure-directories-exist b)
    (with-open-file (s (merge-pathnames "system.lisp" a)
                       :direction :output :if-exists :supersede)
      (write-string "(this is A)" s))
    (with-open-file (s (merge-pathnames "system.lisp" b)
                       :direction :output :if-exists :supersede)
      (write-string "(this is B)" s))
    (with-tree
      (tree:built (tree:root))
      (mount:mount #p"/" (tree:root) "file")
      (pine/text::root)
      (let* ((path-a (namestring (merge-pathnames "system.lisp" a)))
             (path-b (namestring (merge-pathnames "system.lisp" b)))
             (name-a (pine/edit::%document-name path-a))
             (doc-a (text:make-document name-a)))
        (text:visit doc-a path-a)
        (let* ((name-b (pine/edit::%document-name path-b))
               (doc-b (or (text:named name-b) (text:make-document name-b))))
          (text:visit doc-b path-b)
          (is (not (eq doc-a doc-b)) "they are two")
          (is (equal "(this is A)" (node:contents doc-a)) "and the first still is")
          (is (equal "(this is B)" (node:contents doc-b)))
          (is (equal name-a (pine/edit::%document-name path-a))
              "while opening the same file again is still one"))))))

(test a-chord-bound-before-its-mode-loads-is-there-when-it-arrives
  "Bound by the name as written and read by the class's own name, the two never
met and the binding was never found again."
  (mode:bind "text" "C-q test-a" "pwd")
  (is (equal "pwd" (d:lookup (mode::keys 'pine/mode:text) "C-q test-a")))
  (is (equal "pwd" (d:lookup (mode::keys "text") "C-q test-a"))
      "named either way, it is one keymap")
  (is (every #'stringp (d:keys (d:all mode::*keys*)))
      "and they are all kept under one kind of name"))

(test a-chord-whose-command-has-gone-does-not-type-itself
  "BINDING answered NIL for a chord bound to a command that had been withdrawn,
which DISPATCH could not tell from unbound, so dropping a system turned its chords
into text."
  (let ((m (make-instance 'pine/mode:text)))
    (mode:bind "text" "C-M-F9" "no-such-command-at-all")
    (multiple-value-bind (said typed named)
        (mode:dispatch m nil (first (ui:chord "C-M-F9")) nil)
      (declare (ignore typed))
      (is (eq :unbound said) "it is not taken")
      (is (equal "no-such-command-at-all" named) "and it says what it wanted"))))

(test an-unbound-printable-key-still-types-itself
  (let ((m (make-instance 'pine/mode:text)))
    (let ((said (mode:dispatch m nil (first (ui:chord "q")) nil)))
      (is (equal '(:insert . "q") said)))))

(test every-mode-is-offered-once-and-in-one-order
  "MODES walked the subclasses and sorted by how deep each was, so a class two
modes both led to was there twice and two of one depth came back in whatever order
the metaobject protocol happened to give."
  (eval '(defclass test-diamond-mode (pine/mode:pine pine/mode:org) ()))
  (let ((a (mapcar #'class-name (mode::modes)))
        (b (mapcar #'class-name (mode::modes))))
    (is (equal a b) "twice running, the same order")
    (is (= 1 (count 'test-diamond-mode a)) "and each of them once")
    (is (= (length (mode::%names))
           (length (remove-duplicates (mode::%names) :test #'equal))))))

(test a-keymap-is-not-built-from-every-command-on-every-keystroke
  "COMMANDS is asked once per class in the precedence list by BINDING and again by
PREFIXP, so a key cost a list of every command that stands, several times over."
  (let ((m (make-instance 'pine/mode:pine)))
    (mode:binding m "C-f")
    (let ((before (sb-ext:get-bytes-consed)))
      (dotimes (i 100) (mode:binding m "C-f"))
      (let ((each (round (- (sb-ext:get-bytes-consed) before) 100)))
        (is (< each 8000) "~d bytes a look, which was twenty-two thousand" each)))))

(test a-keymap-follows-the-commands-that-turn-over
  (let ((m (make-instance 'pine/mode:text)))
    (is (null (mode:binding m "C-q test-fresh")))
    (command:command "test-fresh" (lambda () :ran) :on '(text "C-q test-fresh"))
    (is (not (null (mode:binding m "C-q test-fresh")))
        "what was kept is worked out again when the commands move")
    (command:forget "test-fresh")))
