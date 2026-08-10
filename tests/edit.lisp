(in-package :pine.test)

(def-suite* :pine.edit :in :pine)

(defmacro with-editor ((&key (text "")) &body body)
  `(unwind-protect
        (progn
          (pine:start)
          (setf (pine.fs.node:contents (pine.edit.buffer:current)) ,text)
          (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 0)
          ,@body)
     (pine:stop)))

(defun b () (pine.edit.buffer:current))

(test text-is-a-vector-of-lines-and-an-insert-answers-where-point-lands
  (let ((lines (pine.edit.text:lines-of "hello")))
    (multiple-value-bind (fresh line col) (pine.edit.text:insert lines 0 5 " there")
      (is (equal "hello there" (pine.edit.text:line-at fresh 0)))
      (is (eql 0 line))
      (is (eql 11 col)))))

(test inserting-a-newline-splits-the-line
  (multiple-value-bind (fresh line col)
      (pine.edit.text:insert (pine.edit.text:lines-of "abcd") 0 2 (string #\Newline))
    (is (equal "ab" (pine.edit.text:line-at fresh 0)))
    (is (equal "cd" (pine.edit.text:line-at fresh 1)))
    (is (eql 1 line))
    (is (eql 0 col))))

(test a-region-spans-lines-and-deleting-it-joins-them
  (let ((lines (pine.edit.text:lines-of "one
two
three")))
    (is (equal "ne
two
th" (pine.edit.text:region lines 0 1 2 2)))
    (multiple-value-bind (fresh line col taken)
        (pine.edit.text:delete lines 0 1 2 2)
      (is (equal "oree" (pine.edit.text:line-at fresh 0)))
      (is (eql 1 (pine.edit.text:line-count fresh)))
      (is (eql 0 line))
      (is (eql 1 col))
      (is (search "two" taken)))))

(test motion-by-word-steps-over-a-word-and-the-space-before-it
  (let ((lines (pine.edit.text:lines-of "one two  three")))
    (multiple-value-bind (line col) (pine.edit.text:move-by :word lines 0 0 1)
      (is (eql 0 line))
      (is (eql 3 col)))
    (multiple-value-bind (line col) (pine.edit.text:move-by :word lines 0 14 -1)
      (is (eql 9 col))
      (is (eql 0 line)))))

(test a-buffer-is-a-node-and-its-contents-is-its-text
  (with-editor (:text "hello")
    (is (typep (b) 'pine.edit.buffer:buffer))
    (is (equal "hello" (pine.fs.node:contents (b))))
    (is (equal "/buf/scratch" (pine.fs.node:full-name (b))))
    (is (eq (b) (pine.world.world:at pine.world.world:*world* "buf/scratch")))))

(test a-buffers-slots-are-nodes-and-writing-one-moves-the-buffer
  (with-editor (:text "hello
there")
    (setf (pine.fs.node:contents
           (pine.world.world:at pine.world.world:*world* "buf/scratch/point-line"))
          1)
    (is (eql 1 (pine.edit.buffer:point-line (b)))
        "a slot node is the slot, not a copy of it")))

(test typing-reaches-the-buffer-through-the-keymap
  (with-editor ()
    (pine:type! "hello")
    (is (equal "hello" (pine.fs.node:contents (b))))
    (is (equal '(0 5) (pine.edit.buffer:point (b))))))

(test a-chord-runs-the-command-the-mode-binds-it-to
  (with-editor (:text "hello")
    (pine.edit.buffer:goto! (b) 0 0)
    (pine.edit.key:dispatch nil (pine.edit.key:parse-key "C-e"))
    (is (equal '(0 5) (pine.edit.buffer:point (b))))
    (pine.edit.key:dispatch nil (pine.edit.key:parse-key "C-a"))
    (is (equal '(0 0) (pine.edit.buffer:point (b))))))

(test a-prefix-chord-waits-for-the-rest-of-itself
  (with-editor ()
    (is (eq :pending (pine.edit.key:dispatch nil (pine.edit.key:parse-key "C-x"))))
    (is (pine.edit.key:pending))
    (pine.edit.key:dispatch nil (pine.edit.key:parse-key "2"))
    (is (null (pine.edit.key:pending)))
    (is (= 2 (length (pine.edit.window:windows)))
        "C-x 2 split the window, and the 2 was not self-inserted")))

(test killing-and-yanking-a-region
  (with-editor (:text "hello there")
    (pine.edit.buffer:goto! (b) 0 0)
    (pine.edit.buffer:mark! (b))
    (pine.edit.buffer:goto! (b) 0 5)
    (is (equal "hello" (pine.repl.command:run "kill-region")))
    (is (equal " there" (pine.fs.node:contents (b))))
    (pine.edit.buffer:goto! (b) 0 6)
    (pine.repl.command:run "yank")
    (is (equal " therehello" (pine.fs.node:contents (b))))))

(test a-window-shows-a-buffer-and-splitting-makes-two
  (with-editor ()
    (is (= 1 (length (pine.edit.window:windows))))
    (pine.repl.command:run "split-window-below")
    (is (= 2 (length (pine.edit.window:windows))))
    (let ((first (pine.edit.window:focused)))
      (pine.repl.command:run "other-window")
      (is (not (eq first (pine.edit.window:focused)))))
    (pine.repl.command:run "delete-other-windows")
    (is (= 1 (length (pine.edit.window:windows))))))

(defun %classed (tree class)
  (labels ((walk (n)
             (if (equal class (pine.ui.node:css-class n))
                 n
                 (some #'walk (pine.ui.layout:nodes-of n)))))
    (walk tree)))

(defun %all-classed (tree class)
  (let (acc)
    (labels ((walk (n)
               (when (equal class (pine.ui.node:css-class n)) (push n acc))
               (mapc #'walk (pine.ui.layout:nodes-of n))))
      (walk tree))
    (nreverse acc)))

(test the-frame-fills-the-height-it-is-given
  (with-editor (:text "hello")
    (let ((tree (pine.edit.render:frame-tree)))
      (pine.ui.layout:measure tree 80 24)
      (pine.ui.layout:arrange tree 0 0 80 24)
      (let ((view (%classed tree "editor-view"))
            (modeline (%classed tree "modeline"))
            (echo (%classed tree "echo")))
        (is (= 23 (pine.ui.node:end-line echo))
            "the echo line sits on the last row, not under the first")
        (is (= 22 (pine.ui.node:start-line modeline))
            "the modeline sits above it")
        (is (= 0 (pine.ui.node:start-line view)))
        (is (= 21 (pine.ui.node:end-line view))
            "the buffer view takes every row down to the modeline")))))

(test the-frame-carries-the-caret-where-point-is
  (with-editor (:text "hello
there")
    (pine.edit.buffer:goto! (b) 1 3)
    (let ((view (%classed (pine.edit.render:frame-tree :cols 40 :rows 8)
                          "editor-view")))
      (is (= 1 (pine.ui.node:view-crow view)))
      (is (= 3 (pine.ui.node:view-ccol view))
          "without this the window paints text with no cursor in it"))))

(test the-caret-follows-the-window-that-has-the-keyboard
  (with-editor (:text "hello")
    (pine.repl.command:run "split-window-below")
    (let* ((tree (pine.edit.render:frame-tree :cols 40 :rows 12))
           (views (%all-classed tree "editor-view"))
           (carets (remove -1 (mapcar #'pine.ui.node:view-crow views))))
      (is (= 2 (length views)))
      (is (= 1 (length carets)) "only the focused window shows a caret"))))

(test the-region-is-painted-with-the-selection-background
  (with-editor (:text "hello there")
    (pine.edit.buffer:mark! (b) 0 0)
    (pine.edit.buffer:goto! (b) 0 5)
    (let* ((view (%classed (pine.edit.render:frame-tree :cols 40 :rows 8)
                           "editor-view"))
           (runs (cdr (first (pine.ui.node:view-rows view))))
           (bg (pine.ui.face:face-bg :selection)))
      (is (equal bg (subseq (first runs) 4 7))
          "the marked run carries the selection background")
      (is (not (equal bg (subseq (second runs) 4 7)))
          "and the text past point does not"))))

(test a-file-opens-into-a-buffer-and-saves-back
  (let ((file (merge-pathnames "pine-probe-edit.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out file :direction :output :if-exists :supersede)
             (write-string "(defun f () 1)" out))
           (with-editor ()
             (pine.repl.command:run "find-file" (list (namestring file)))
             (let ((opened (pine.edit.buffer:buffer-named "pine-probe-edit.lisp")))
               (is (equal "(defun f () 1)" (pine.fs.node:contents opened)))
               (is (equal "lisp" (pine.edit.buffer:mode-of opened))
                   "the mode came from what the file is called")
               (setf (pine.edit.buffer:current) opened)
               (pine.edit.buffer:move! opened :buffer 1)
               (pine.edit.buffer:insert! opened " ")
               (pine.edit.buffer:save! opened)
               (is (equal "(defun f () 1) " (uiop:read-file-string file))))))
      (ignore-errors (delete-file file)))))

(test the-frame-renders-the-buffer-and-a-modeline
  (with-editor (:text "hello
there")
    (let ((rows (pine:frame :width 40 :height 6)))
      (is (search "hello" (first rows)))
      (is (search "there" (second rows)))
      (is (find-if (lambda (row) (search "scratch" row)) rows)
          "the modeline names the buffer")
      (is (find-if (lambda (row) (search "L1 C0" row)) rows)
          "and says where point is"))))

(test a-lisp-buffer-renders-with-its-faces
  (with-editor (:text "(defun f (x) x)")
    (pine.ts.parser:wait (b))
    (let ((found (pine.edit.render:highlights-for (b))))
      (is (plusp (hash-table-count found))
          "lisp mode names a grammar, so the buffer is parsed")
      (is (find :keyword (gethash 0 found) :key #'third)))))

(test a-large-buffer-edits-by-sharing-what-it-did-not-touch
  (let* ((text (with-output-to-string (s)
                 (dotimes (i 100000) (format s "(defun f~d (x) x)~%" i))))
         (lines (pine.edit.text:lines-of text)))
    (is (eql 100001 (pine.edit.text:line-count lines)))
    (multiple-value-bind (fresh line col) (pine.edit.text:insert lines 50000 0 "probe ")
      (is (eql 50000 line))
      (is (eql 6 col))
      (is (eq (pine.data:at lines 5) (pine.data:at fresh 5))
          "an edit shares every line it did not touch")
      (is (eq (pine.data:at lines 99999) (pine.data:at fresh 99999)))
      (is (not (equal (pine.edit.text:line-at lines 50000)
                      (pine.edit.text:line-at fresh 50000)))))))

(test undo-puts-back-what-an-edit-changed-and-redo-does-it-again
  (with-editor (:text "hello")
    (pine.edit.buffer:goto! (b) 0 5)
    (pine.edit.buffer:insert! (b) " there")
    (is (equal "hello there" (pine.fs.node:contents (b))))
    (is-true (pine.edit.buffer:undoable (b)))
    (pine.edit.buffer:undo! (b))
    (is (equal "hello" (pine.fs.node:contents (b))))
    (is (equal '(0 5) (pine.edit.buffer:point (b))) "point comes back too")
    (pine.edit.buffer:redo! (b))
    (is (equal "hello there" (pine.fs.node:contents (b))))))

(test undo-is-cheap-because-the-seqs-share
  (with-editor ()
    (setf (pine.fs.node:contents (b))
          (with-output-to-string (s) (dotimes (i 20000) (format s "line ~d~%" i))))
    (pine.edit.buffer:goto! (b) 10000 0)
    (let ((before (pine.run.cell:held (pine.edit.buffer:lines (b)))))
      (pine.edit.buffer:insert! (b) "x")
      (let ((after (pine.run.cell:held (pine.edit.buffer:lines (b)))))
        (is (eq (pine.data:at before 0) (pine.data:at after 0)))
        (pine.edit.buffer:undo! (b))
        (is (eq before (pine.run.cell:held (pine.edit.buffer:lines (b))))
            "undo is the seq it had, not a rebuilt one")))))

(test a-named-mark-is-a-place-that-outlives-point
  (with-editor (:text "one
two
three")
    (pine.edit.buffer:goto! (b) 1 2)
    (is (equal '(1 2) (pine.edit.buffer:put-mark! (b) :probe)))
    (pine.edit.buffer:goto! (b) 2 0)
    (is (equal '(1 2) (pine.edit.buffer:mark-at (b) :probe)))
    (pine.edit.buffer:drop-mark! (b) :probe)
    (is (null (pine.edit.buffer:mark-at (b) :probe)))))

(test a-region-carries-properties-and-they-read-back-by-place
  (with-editor (:text "hello there")
    (pine.edit.buffer:propertize! (b) 0 0 5 '(:face :match))
    (is (equal '((:face :match)) (pine.edit.buffer:properties-at (b) 0 2)))
    (is (null (pine.edit.buffer:properties-at (b) 0 7)))
    (pine.edit.buffer:clear-properties! (b))
    (is (null (pine.edit.buffer:properties-at (b) 0 2)))))

(test a-line-in-lisp-indents-by-what-the-parse-says
  (with-editor (:text "(defun f (x)
x)")
    (unwind-protect
         (progn
           (is (eql 2 (pine.edit.render:indent-for (b) 1))
               "a defun's body indents by two, said by the parse and not a rule here")
           (pine.edit.buffer:goto! (b) 1 0)
           (pine.repl.command:run "indent-line")
           (is (equal "  x)" (pine.edit.buffer:line (b) 1)))
           (is (eql 2 (pine.edit.buffer:point-col (b)))
               "point moved with the text it was sitting in"))
      (pine.ts.parser:forget (b)))))

(test killing-a-buffer-leaves-something-to-type-into
  (with-editor (:text "hello")
    (pine.repl.command:run "kill-buffer" (list "scratch"))
    (is-true (pine.edit.buffer:current) "there is always a buffer")
    (is (eq (pine.edit.buffer:current)
            (pine.edit.window:buffer-of (pine.edit.window:focused)))
        "and the window is showing it, not the one that was killed")
    (pine.edit.key:dispatch nil (pine.edit.key:make-key "x"))
    (is (equal "x" (pine.fs.node:contents (pine.edit.buffer:current)))
        "so typing still reaches a buffer")))

(test the-window-follows-what-is-current
  (with-editor ()
    (let ((other (pine.edit.buffer:make-buffer "other")))
      (setf (pine.edit.buffer:current) other)
      (is (eq other (pine.edit.window:buffer-of (pine.edit.window:focused)))
          "a command that switches buffers switches what is on the screen"))))

(test a-buffer-with-no-file-is-asked-where-to-write-it
  (with-editor (:text "hello")
    (is (eq :asking (pine.repl.command:run "save-buffer")))
    (is (equal "Write file: "
               (pine.edit.prompt:question (pine.edit.prompt:asking))))
    (pine.edit.prompt:cancel!)))

(test a-directory-is-not-a-file-to-open
  (with-editor ()
    (pine.repl.command:run "find-file" (list "/tmp"))
    (is (equal "scratch" (pine.fs.node:name (pine.edit.buffer:current)))
        "opening a directory says so rather than filling a buffer with a fault")))

(test a-listing-is-a-buffer-you-can-act-on
  (with-editor ()
    (pine.edit.buffer:make-buffer "probe")
    (pine.repl.command:run "list-buffers")
    (let ((b (pine.edit.buffer:current)))
      (is (equal "*buffers*" (pine.fs.node:name b)))
      (is (member "list" (pine.edit.buffer:minors-of b) :test #'equal)
          "a listing is in list mode, so RET means open this line")
      (loop :for n :from 0 :below (pine.edit.buffer:line-count b)
            :when (search "probe" (pine.edit.buffer:line b n))
              :do (pine.edit.buffer:goto! b n 0) (return))
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "RET"))
      (is (equal "probe" (pine.fs.node:name (pine.edit.buffer:current)))
          "RET on the line opened the buffer it named"))))

(test one-string-becomes-another
  (with-editor (:text "one two one")
    (pine.edit.buffer:goto! (b) 0 0)
    (pine.repl.command:run "query-replace" (list "one"))
    (pine.edit.prompt:answer! "ONE")
    (is (equal "ONE two ONE" (pine.fs.node:contents (b))))))

(test a-buffer-says-when-it-has-been-changed-and-stops-when-it-is-saved
  (let ((file (merge-pathnames "pine-probe-modified.txt" (uiop:temporary-directory))))
    (unwind-protect
         (with-editor ()
           (let ((b (b)))
             (pine.edit.buffer:save! b file)
             (is-false (pine.edit.buffer:modified b) "saving settles it")
             (pine.edit.buffer:insert! b "x")
             (is-true (pine.edit.buffer:modified b) "and typing dirties it again")
             (is (find-if (lambda (row) (search "**" row))
                          (pine:frame :width 40 :height 6))
                 "the modeline says so")
             (pine.edit.buffer:save! b file)
             (is-false (pine.edit.buffer:modified b) "saving settles it")))
      (ignore-errors (delete-file file)))))

(test a-file-comes-back-with-point-where-it-was-left
  (let ((file (merge-pathnames "pine-probe-place.txt" (uiop:temporary-directory))))
    (unwind-protect
         (with-editor ()
           (with-open-file (out file :direction :output :if-exists :supersede)
             (format out "one~%two~%three~%"))
           (pine.repl.command:run "find-file" (list (namestring file)))
           (pine.edit.buffer:goto! (pine.edit.buffer:current) 2 1)
           (pine.repl.command:run "switch-to-buffer" (list "scratch"))
           (pine.repl.command:run "find-file" (list (namestring file)))
           (is (equal '(2 1) (pine.edit.buffer:point (pine.edit.buffer:current)))
               "coming back lands where you left, not at the top"))
      (ignore-errors (delete-file file)))))

(test the-file-again-as-it-is-on-disk
  (let ((file (merge-pathnames "pine-probe-revert.txt" (uiop:temporary-directory))))
    (unwind-protect
         (with-editor ()
           (with-open-file (out file :direction :output :if-exists :supersede)
             (write-string "from disk" out))
           (pine.repl.command:run "find-file" (list (namestring file)))
           (pine.edit.buffer:insert! (pine.edit.buffer:current) "typed ")
           (is (search "typed" (pine.fs.node:contents (pine.edit.buffer:current))))
           (pine.edit.buffer:revert! (pine.edit.buffer:current))
           (is (equal "from disk"
                      (string-right-trim (string #\Newline)
                                         (pine.fs.node:contents
                                          (pine.edit.buffer:current)))))
           (is-false (pine.edit.buffer:modified (pine.edit.buffer:current))))
      (ignore-errors (delete-file file)))))

(test a-setting-is-the-modes-until-this-buffer-says-otherwise
  (with-editor ()
    (let ((b (b)))
      (is (eql 2 (pine.edit.buffer:setting b :indent))
          "lisp mode says two")
      (setf (pine.edit.buffer:setting b :indent) 8)
      (is (eql 8 (pine.edit.buffer:setting b :indent))
          "and this buffer says eight")
      (is (eql 2 (pine.repl.mode:setting "lisp" :indent))
          "without the mode changing for every other buffer"))))

(test what-this-buffer-reads-for-every-setting
  (with-editor ()
    (setf (pine.edit.buffer:setting (b) :tab-width) 4)
    (pine.repl.command:run "describe-variables")
    (let ((help (pine.edit.buffer:buffer-named "*help*")))
      (is-true help)
      (is (search "tab-width" (pine.fs.node:contents help)))
      (is (search "4" (pine.fs.node:contents help))))))

(test what-is-marked-on-the-text-moves-with-it
  (with-editor (:text "one
two
three")
    (let ((b (b)))
      (pine.edit.buffer:propertize! b 2 0 5 '(:face :match))
      (pine.edit.buffer:goto! b 0 0)
      (pine.edit.buffer:insert! b (format nil "zero~%"))
      (is (equal '((:face :match)) (pine.edit.buffer:properties-at b 3 1))
          "a mark below an insert moves down with the line it was on")
      (is (null (pine.edit.buffer:properties-at b 2 1))
          "and is no longer on the line it used to be"))))

(test a-mark-the-edit-ran-through-is-dropped-rather-than-left-lying
  (with-editor (:text "one
two
three")
    (let ((b (b)))
      (pine.edit.buffer:propertize! b 1 0 3 '(:face :match))
      (pine.edit.buffer:delete-region! b 1 0 2 0)
      (is (null (pine.edit.buffer:properties-at b 1 1))
          "the text it described is gone, so the mark is too"))))

(test a-tab-takes-you-to-the-next-stop
  (with-editor ()
    (multiple-value-bind (drawn where)
        (pine.edit.render:shown-line (format nil "a~cb" #\Tab) 8)
      (is (equal "a       b" drawn) "one tab, eight columns")
      (is (eql 8 (pine.edit.render:shown-col where 2))
          "and the character after it is at column eight, not two"))))

(test an-evaluation-says-its-answer-beside-the-form
  (with-editor (:text "(+ 1 2)")
    (let ((b (b)))
      (pine.edit.buffer:goto! b 0 7)
      (pine.repl.command:run "eval-last-expression")
      (is (equal '((:after "=> 3" :face :comment))
                 (pine.edit.buffer:overlays-at b 0)))
      (is (search "=> 3" (first (pine:frame :width 40 :height 4)))
          "and it is drawn after the line rather than in the echo alone"))))

(defun %view-text (view)
  (format nil "~{~a~^~%~}" (mapcar #'car (pine.ui.node:view-rows view))))

(test a-window-shows-a-buffer-a-name-or-a-widget-tree
  (with-editor ()
    (let ((w (pine.edit.window:focused))
          (b (pine.edit.buffer:make-buffer "probe-pane")))
      (setf (pine.fs.node:contents b) "one line")
      (pine.edit.window:show! w "probe-pane")
      (is (eq b (pine.edit.window:buffer-of w)) "a name is the buffer it names")
      (is (search "one line" (%view-text (pine.edit.render:buffer-tree w))))
      (is-true (pine.edit.render:modelinep w))
      (pine.edit.window:show! w (pine.ui.build:label "a widget"))
      (is (search "a widget" (%view-text (pine.edit.render:buffer-tree w)))
          "a widget tree renders in a window like anything else")
      (is (null (pine.edit.render:modelinep w))
          "and has no line and column to put in a modeline")
      (is (eq b (pine.edit.buffer:current))
          "showing a widget does not make it the current buffer")
      (setf (pine.fs.node:contents (pine.edit.buffer:make-buffer "probe-other"))
            "elsewhere")
      (setf (pine.edit.buffer:current) (pine.edit.buffer:buffer-named "probe-other"))
      (is (typep (pine.edit.window:buffer-of w) 'pine.ui.node:node)
          "and the window holding it does not follow the current buffer")
      (setf (pine.fs.node:contents w) "probe-pane")
      (is (eq b (pine.edit.window:buffer-of w))
          "writing the window is showing what it names"))))

(test a-row-stands-for-a-place-and-the-rows-wrap
  (with-editor ()
    (pine.edit.buffer:make-buffer "probe-one")
    (pine.edit.buffer:make-buffer "probe-two")
    (pine.repl.command:run "list-buffers")
    (let* ((b (pine.edit.buffer:current))
           (n (length (pine.edit.listing:rows
                       (gethash "*buffers*" (pine.edit.listing:listings))))))
      (is (typep (pine.edit.listing:place b) 'pine.edit.buffer:buffer)
          "the row point is on stands for a buffer, not for its text")
      (is (eq (pine.edit.listing:place b)
              (pine.edit.buffer:setting b :selection))
          "and what it stands for is readable as a setting")
      (pine.edit.listing:step! -1 b)
      (is (= (1- n) (pine.edit.buffer:point-line b))
          "back from the first row is the last one")
      (pine.edit.listing:step! 1 b)
      (is (= 0 (pine.edit.buffer:point-line b)))
      (let ((was (pine.edit.listing:place b)))
        (pine.edit.key:dispatch nil (pine.edit.key:parse-key "n"))
        (is (not (eq was (pine.edit.listing:place b)))
            "n in a listing is the next row, not a letter typed into it"))
      (let ((stood-for (pine.edit.listing:place b)))
        (pine.repl.command:run "list-activate")
        (is (eq stood-for (pine.edit.buffer:current))
            "and what it opened is what the row stood for")))))
