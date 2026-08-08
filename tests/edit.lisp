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
    (setf pine.edit.render:*runtime* (pine.ts.runtime:make-ts-runtime))
    (pine.ts.runtime:ensure-ts pine.edit.render:*runtime*)
    (let ((found (pine.edit.render:highlights-for (b))))
      (is (consp found) "lisp mode names a grammar, so the buffer is parsed")
      (is (find :keyword found :key #'fourth)))))

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
