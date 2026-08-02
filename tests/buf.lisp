(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.buf :in :pine)

(defmacro with-buf (&body body)
  "A space with /buf served. No daemon, no actors: a buffer is paths, so an
edit is a write and it has landed when the write answers."
  `(pine.ns:with-space ()
     (pine.ns:raise :buf)
     (pine.ns:write /buf/scratch/text [""])
     (pine.ns:write /buf/scratch/point [0 0])
     ,@body))

(defun text-is (name string)
  (string= string (pine.ns:read (pine.buf:at name :text))))

;;;; reading a buffer is reading a path

(test every-buffer-is-under-buf
  (with-buf
    (pine.ns:write /buf/notes/text ["a"])
    (is (equal '("notes" "scratch") (pine.buf:names)))))

(test text-is-the-lines-joined
  (with-buf
    (pine.ns:write /buf/scratch/text ["one" "two" "three"])
    (is (text-is "scratch" (format nil "one~%two~%three")))
    (is (string= "two" (pine.ns:read /buf/scratch/line/1)))))

(test writing-text-lands-as-lines
  (with-buf
    (pine.ns:write /buf/scratch/text (format nil "one~%two"))
    (is (fset:equal? ["one" "two"] (pine.ns:held /buf/scratch/text)))))

(test a-window-reads-the-range-it-shows
  "The band is not a policy in the parser; it is what the window asked for."
  (with-buf
    (pine.ns:write /buf/scratch/text
                   (fset:convert 'fset:seq
                                 (loop :for i :below 50
                                       :collect (format nil "line ~d" i))))
    (let ((band (pine.ns:read /buf/scratch/line/10..14)))
      (is (= 5 (fset:size band)))
      (is (string= "line 10" (fset:lookup band 0)))
      (is (string= "line 14" (fset:lookup band 4))))))

(test a-range-past-the-end-is-what-is-there
  (with-buf
    (pine.ns:write /buf/scratch/text ["one"])
    (is (= 1 (fset:size (pine.ns:read /buf/scratch/line/0..99))))))

;;;; editing is writing, and it has landed when the write answers

(test insert-puts-text-at-point
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "hello"])
    (is (text-is "scratch" "hello"))
    (is (fset:equal? [0 5] (pine.ns:read /buf/scratch/point)))))

(test insert-carries-its-own-newlines
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert (format nil "one~%two")])
    (is (fset:equal? ["one" "two"] (pine.ns:held /buf/scratch/text)))
    (is (fset:equal? [1 3] (pine.ns:read /buf/scratch/point)))))

(test newline-splits-the-line-at-point
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "abcd"])
    (pine.ns:write /buf/scratch/point [0 2])
    (pine.ns:write /buf/scratch/text [:newline])
    (is (fset:equal? ["ab" "cd"] (pine.ns:held /buf/scratch/text)))
    (is (fset:equal? [1 0] (pine.ns:read /buf/scratch/point)))))

(test a-backspace-is-a-delete-of-the-character-before-point
  "There is no backspace verb. It is [:delete FROM TO] one character back."
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "abc"])
    (pine.ns:write /buf/scratch/text [:delete [0 2] [0 3]])
    (is (text-is "scratch" "ab"))
    (is (fset:equal? [0 2] (pine.ns:read /buf/scratch/point)))))

(test deleting-across-a-line-end-joins-the-lines
  (with-buf
    (pine.ns:write /buf/scratch/text ["ab" "cd"])
    (pine.ns:write /buf/scratch/text [:delete [0 2] [1 0]])
    (is (fset:equal? ["abcd"] (pine.ns:held /buf/scratch/text)))
    (is (fset:equal? [0 2] (pine.ns:read /buf/scratch/point)))))

(test a-kill-cuts-the-region-to-the-ring
  (with-buf
    (pine.ns:write /buf/scratch/text ["one" "two"])
    (pine.ns:write /buf/scratch/mark [0 1])
    (pine.ns:write /buf/scratch/point [1 2])
    (pine.ns:write /buf/scratch/text [:kill])
    (is (text-is "scratch" "oo"))
    (is (string= (format nil "ne~%tw") (pine.ns:read /kill)))))

(test reverting-reads-the-file-again
  (with-buf
    (pine.ns:raise :file)
    (let ((path "/tmp/pine-revert-probe.txt"))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "from disk~%"))
      (pine.ns:write /buf/scratch [:visit path])
      (pine.ns:write /buf/scratch/text [:insert "typed "])
      (is (string= (format nil "typed from disk~%") (pine.ns:read /buf/scratch/text)))
      (pine.ns:write /buf/scratch/text [:revert])
      (is (string= (format nil "from disk~%") (pine.ns:read /buf/scratch/text))))))

(test delete-takes-a-region
  (with-buf
    (pine.ns:write /buf/scratch/text ["one" "two" "three"])
    (pine.ns:write /buf/scratch/text [:delete [0 1] [2 2]])
    (is (fset:equal? ["oree"] (pine.ns:held /buf/scratch/text)))))

(test point-moves-by-a-unit
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "one two"])
    (pine.ns:write /buf/scratch/point [0 0])
    (pine.ns:write /buf/scratch/point [:move :word 1])
    (is (plusp (fset:lookup (pine.ns:read /buf/scratch/point) 1)))))

(test point-is-a-place-as-well-as-a-verb
  "A clause that declares only :verbs says what a path does, not what it holds."
  (with-buf
    (pine.ns:write /buf/scratch/point [3 4])
    (is (fset:equal? [3 4] (pine.ns:read /buf/scratch/point)))))

(test an-edit-moves-the-tick-and-a-motion-does-not
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "one"])
    (let ((tick (pine.ns:read /buf/scratch/tick)))
      (pine.ns:write /buf/scratch/point [:move :char -1])
      (is (= tick (pine.ns:read /buf/scratch/tick)))
      (pine.ns:write /buf/scratch/text [:insert "x"])
      (is (> (pine.ns:read /buf/scratch/tick) tick)))))

;;;; undo has no stacks: it is what the file remembers

(test undo-is-the-newest-change-under-this-buffer-put-back
  "Undo of a paste and undo of a window split are the same operation, so there
are no undo stacks anywhere."
  (pine.ns:with-space ()
    (let ((store (pine.store:open ":memory:")))
      (unwind-protect
           (progn
             (pine.ns:raise :buf)
             (pine.ns:write /buf/scratch/text ["one"])
             (pine.ns:write /buf/scratch/point [0 3])
             (pine.ns:write /buf/scratch/text [:insert "!"])
             (is (text-is "scratch" "one!"))
             (pine.ns:write /buf/scratch/text [:undo])
             (is (text-is "scratch" "one")))
        (pine.store:close store)))))

(test writing-a-line-replaces-it
  (with-buf
    (pine.ns:write /buf/scratch/text ["one" "two" "three"])
    (pine.ns:write /buf/scratch/line/1 "TWO")
    (is (fset:equal? ["one" "TWO" "three"] (pine.ns:held /buf/scratch/text)))))

(test modified-follows-the-file
  (with-buf
    (pine.ns:raise :file)
    (let ((path "/tmp/pine-modified-probe.txt"))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "on disk~%"))
      (pine.ns:write /buf/scratch [:visit path])
      (is (null (pine.ns:read /buf/scratch/modified)))
      (pine.ns:write /buf/scratch/text [:insert "x"])
      (is (pine.ns:read /buf/scratch/modified))
      (pine.ns:write /buf/scratch/text [:save])
      (is (null (pine.ns:read /buf/scratch/modified))))))

;;;; the buffer that is current

(test a-leaf-of-the-current-buffer-is-that-buffers-leaf
  "Nothing holds the current buffer. /buf/current names one, and a leaf under
it is that buffer's leaf."
  (with-buf
    (pine.ns:write /buf/notes/text ["hello"])
    (pine.ns:write /buf/current /buf/notes)
    (is (string= "hello" (pine.ns:read /buf/current/text)))
    (is (string= "hello" (pine.ns:read /buf/current/line/0)))
    (pine.ns:write /buf/current/text [:insert "oh "])
    (is (string= "oh hello" (pine.ns:read /buf/notes/text)))
    (pine.ns:write /buf/current /buf/scratch)
    (is (string= "" (pine.ns:read /buf/current/text)))))

(test the-current-buffer-is-not-a-buffer-of-its-own
  (with-buf
    (pine.ns:write /buf/current /buf/scratch)
    (is (equal '("scratch") (pine.buf:names)))))

;;;; locals, and the scope rule

(test a-buffer-local-is-a-place
  (with-buf
    (pine.ns:write /buf/scratch/tab-width 4)
    (is (= 4 (pine.ns:read /buf/scratch/tab-width)))))

(test a-leaf-a-buffer-does-not-have-falls-back-to-the-root
  "Buffer-local is not a mechanism. It is where the value is."
  (with-buf
    (pine.ns:write /tab-width 8)
    (is (= 8 (pine.ns:read /buf/scratch/tab-width)))
    (pine.ns:write /buf/scratch/tab-width 4)
    (is (= 4 (pine.ns:read /buf/scratch/tab-width)))
    (is (= 8 (pine.ns:read /tab-width))
        "setting it here did not set it everywhere")))

(test the-fallback-is-a-leaf-and-not-a-directory
  "/mode holds every mode there is, and a buffer with no mode has no mode."
  (with-buf
    (pine.ns:write /mode/lisp {:grammar :commonlisp})
    (is (null (pine.ns:read /buf/scratch/mode)))))

(test something-computed-from-a-local-follows-the-root
  "The fallback is a read like any other, so an expression over it is recomputed
when the root moves."
  (with-buf
    (pine.ns:write /tab-width 8)
    (pine.ns:write /width (pine.ns:read /buf/scratch/tab-width))
    (is (= 8 (pine.ns:read /width)))
    (pine.ns:write /tab-width 2)
    (is (= 2 (pine.ns:read /width)))))

;;;; what it is

(test a-buffer-with-no-file-has-held-text
  "Text is written as a string and held as its lines. It is still a value in
the tree, so it is what is stored."
  (with-buf
    (is (eq :held (pine.ns:kind /buf/scratch/text)))
    (is (eq :held (pine.ns:kind /buf/scratch/point)))))

(test text-reads-as-the-whole-string-and-is-held-as-lines
  (with-buf
    (pine.ns:write /buf/scratch/text (format nil "one~%two"))
    (is (string= (format nil "one~%two") (pine.ns:read /buf/scratch/text)))
    (is (fset:equal? ["one" "two"] (pine.ns:held /buf/scratch/text))
        "an edit shares the lines it did not touch, so the lines are the value")))

;;;; the parse is a watch, and it answers with a write

(test lines-moving-is-what-asks-for-a-parse
  "A buffer that names a grammar is parsed because its lines moved, not because
anything called the parser. The colours land as properties on the buffer."
  (with-fixture substrate ()
    (pine.ns:with-space ()
      (pine.ns:write /mode/lisp {:grammar :commonlisp})
      (pine.ns:raise :buf :system (pine.core.server:actor-system *server*)
                    :runtime (pine.core.server:ts-runtime *server*))
      (unwind-protect
           (progn
             (pine.ns:write /buf/probe/mode :lisp)
             (pine.buf:showing "probe" [0 200])
             (pine.ns:write /buf/probe/text ["(defun f (x) x)"])
             (is-true (wait-for (lambda ()
                                  (not (fset:empty? (pine.buf:properties "probe"))))
                                :seconds 15)
                      "no colours ever landed")
             (is (plusp (fset:size (pine.buf:properties "probe")))))
        (pine.ns:lower :buf)))))

(test a-parse-starts-when-one-is-asked-for-and-not-only-when-something-moved
  "The parse is not driven by a change alone. A buffer whose parser is gone --
because the text and the mode landed before anything watched them, or landed
again unchanged, which moves nothing -- still parses the moment a window says
what it is showing."
  (with-fixture substrate ()
    (within-seconds 40
      (let ((name "asked-probe"))
        (pine.editor.frame::make-buffer name :content "(defun f (x) x)")
        (pine.editor.frame::set-buffer-mode
         (pine.buf:live name) :lisp)
        ;; highlights exist to be painted, so a window has to be showing it
        (pine.buf:showing name (fset:seq 0 200))
        (is (wait-for (lambda () (not (fset:empty? (pine.buf:properties name))))
                      :seconds 20)
            "the first parse never landed")
        ;; the parser goes and nothing about the buffer moves after it
        (pine.buf:drop name)
        (pine.buf:clear-properties name)
        (sleep 0.1)
        (is (null (pine.buf:parser-of name)))
        (pine.buf:showing name (fset:seq 0 200))
        (is (wait-for (lambda () (not (fset:empty? (pine.buf:properties name))))
                      :seconds 20)
            "asking to see it did not start a parse")
        (pine.editor.frame::kill-buffer name)))))
