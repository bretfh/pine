(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.buf :in :pine)

(defmacro with-buf (&body body)
  "A space with /buf served. No daemon, no actors: a buffer is paths, so an
edit is a write and it has landed when the write answers."
  `(pine.ns:with-space ()
     (pine.buf:mount)
     (pine.ns:write /buf/scratch/lines [""])
     (pine.ns:write /buf/scratch/point [0 0])
     ,@body))

(defun text-is (name string)
  (string= string (pine.ns:read (pine.buf:at name :text))))

;;;; reading a buffer is reading a path

(test every-buffer-is-under-buf
  (with-buf
    (pine.ns:write /buf/notes/lines ["a"])
    (is (equal '("notes" "scratch") (pine.buf:names)))))

(test text-is-the-lines-joined
  (with-buf
    (pine.ns:write /buf/scratch/lines ["one" "two" "three"])
    (is (text-is "scratch" (format nil "one~%two~%three")))
    (is (string= "two" (pine.ns:read /buf/scratch/line/1)))))

(test writing-text-lands-as-lines
  (with-buf
    (pine.ns:write /buf/scratch/text (format nil "one~%two"))
    (is (fset:equal? ["one" "two"] (pine.ns:read /buf/scratch/lines)))))

(test a-window-reads-the-range-it-shows
  "The band is not a policy in the parser; it is what the window asked for."
  (with-buf
    (pine.ns:write /buf/scratch/lines
                   (fset:convert 'fset:seq
                                 (loop :for i :below 50
                                       :collect (format nil "line ~d" i))))
    (let ((band (pine.ns:read /buf/scratch/line/10..14)))
      (is (= 5 (fset:size band)))
      (is (string= "line 10" (fset:lookup band 0)))
      (is (string= "line 14" (fset:lookup band 4))))))

(test a-range-past-the-end-is-what-is-there
  (with-buf
    (pine.ns:write /buf/scratch/lines ["one"])
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
    (is (fset:equal? ["one" "two"] (pine.ns:read /buf/scratch/lines)))
    (is (fset:equal? [1 3] (pine.ns:read /buf/scratch/point)))))

(test newline-splits-the-line-at-point
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "abcd"])
    (pine.ns:write /buf/scratch/point [0 2])
    (pine.ns:write /buf/scratch/text [:newline])
    (is (fset:equal? ["ab" "cd"] (pine.ns:read /buf/scratch/lines)))
    (is (fset:equal? [1 0] (pine.ns:read /buf/scratch/point)))))

(test backspace-takes-the-character-before-point
  (with-buf
    (pine.ns:write /buf/scratch/text [:insert "abc"])
    (pine.ns:write /buf/scratch/text [:backspace])
    (is (text-is "scratch" "ab"))
    (is (fset:equal? [0 2] (pine.ns:read /buf/scratch/point)))))

(test backspace-at-a-line-start-joins-the-lines
  (with-buf
    (pine.ns:write /buf/scratch/lines ["ab" "cd"])
    (pine.ns:write /buf/scratch/point [1 0])
    (pine.ns:write /buf/scratch/text [:backspace])
    (is (fset:equal? ["abcd"] (pine.ns:read /buf/scratch/lines)))
    (is (fset:equal? [0 2] (pine.ns:read /buf/scratch/point)))))

(test delete-takes-a-region
  (with-buf
    (pine.ns:write /buf/scratch/lines ["one" "two" "three"])
    (pine.ns:write /buf/scratch/text [:delete [0 1] [2 2]])
    (is (fset:equal? ["oree"] (pine.ns:read /buf/scratch/lines)))))

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
             (pine.buf:mount)
             (pine.ns:write /buf/scratch/lines ["one"])
             (pine.ns:write /buf/scratch/point [0 3])
             (pine.ns:write /buf/scratch/text [:insert "!"])
             (is (text-is "scratch" "one!"))
             (pine.ns:write /buf/scratch/text [:undo])
             (is (text-is "scratch" "one")))
        (pine.store:close store)))))

;;;; locals, and the scope rule

(test a-buffer-local-is-a-place
  (with-buf
    (pine.ns:write /buf/scratch/tab-width 4)
    (is (= 4 (pine.ns:read /buf/scratch/tab-width)))))

;;;; what it is

(test text-is-live-because-it-is-computed-from-the-lines
  (with-buf
    (is (eq :live (pine.ns:kind /buf/scratch/text)))
    (is (eq :held (pine.ns:kind /buf/scratch/lines))
        "the lines are what someone wrote, so they are what is stored")))

;;;; the parse is a watch, and it answers with a write

(test lines-moving-is-what-asks-for-a-parse
  "A buffer that names a grammar is parsed because its lines moved, not because
anything called the parser. The colours land at /buf/?name/face."
  (with-fixture substrate ()
    (pine.ns:with-space ()
      (pine.ns:write /mode/lisp {:grammar :commonlisp})
      (pine.buf:mount :system (pine.core.server:actor-system *server*)
                      :runtime (pine.core.server:ts-runtime *server*))
      (unwind-protect
           (progn
             (pine.ns:write /buf/probe/mode :lisp)
             (pine.ns:write /buf/probe/viewport [0 200])
             (pine.ns:write /buf/probe/lines ["(defun f (x) x)"])
             (is-true (wait-for (lambda () (pine.ns:read /buf/probe/face))
                                :seconds 15)
                      "no colours ever landed")
             (is (plusp (length (pine.ns:read /buf/probe/face)))))
        (pine.buf:unmount)))))
