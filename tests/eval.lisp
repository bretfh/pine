(in-package :pine.test)

(def-suite* :pine.eval :in :pine)

(defparameter +probe-source+
  (format nil "(in-package :cl-user)~%(defun probe (x) (list x))~%(probe 41)~%"))

(defmacro with-lisp-buffer ((&key (text +probe-source+)) &body body)
  `(unwind-protect
        (progn
          (pine:start)
          (setf (pine.fs.node:contents (pine.edit.buffer:current)) ,text)
          (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 0)
          ,@body)
     (pine:stop)))

(test a-buffer-reads-in-the-package-it-declares
  (with-lisp-buffer ()
    (is (eq (find-package :cl-user)
            (pine.edit.eval:package-of (pine.edit.buffer:current))))))

(test the-image-says-where-a-symbol-is-defined
  (with-lisp-buffer ()
    (let ((found (pine.edit.eval:definition (pine.edit.buffer:current) "pine:start")))
      (is-true found "M-. has nothing to jump to")
      (is (search "boot.lisp" (first (first found))))
      (is (plusp (second (first found))) "and a line to land on"))))

(test the-image-says-what-a-call-takes-and-what-it-is-for
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (is (equal "list (&rest args)" (pine.edit.eval:arglist b "list")))
      (is (search "Return the 1st object"
                  (pine.edit.eval:documentation b "car"))))))

(test completing-a-name-asks-the-package-the-buffer-is-in
  (with-lisp-buffer ()
    (let ((found (pine.edit.eval:complete (pine.edit.buffer:current) "list-all-pack")))
      (is (equal '("list-all-packages") found)))))

(test the-form-before-point-is-the-one-that-evaluates
  (with-lisp-buffer ()
    (let* ((b (pine.edit.buffer:current))
           (text (pine.edit.buffer:text-of b)))
      (pine.edit.buffer:goto! b 2 10)
      (multiple-value-bind (from to)
          (pine.edit.eval:sexp-before text (pine.edit.eval:offset-of b))
        (is (equal "(probe 41)" (subseq text from to)))))))

(test the-definition-point-is-in-is-the-whole-toplevel-form
  (with-lisp-buffer ()
    (let* ((b (pine.edit.buffer:current))
           (text (pine.edit.buffer:text-of b)))
      (pine.edit.buffer:goto! b 1 12)
      (multiple-value-bind (from to)
          (pine.edit.eval:defun-around text (pine.edit.eval:offset-of b))
        (is (equal "(defun probe (x) (list x))" (subseq text from to)))))))

(test evaluating-the-form-before-point-defines-it-in-the-image
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 1 26)
      (pine.repl.command:run "eval-last-expression")
      (is-true (fboundp (find-symbol "PROBE" :cl-user))
               "C-x C-e did not reach the image")
      (is (equal '(41) (funcall (find-symbol "PROBE" :cl-user) 41)))
      (unintern (find-symbol "PROBE" :cl-user) :cl-user))))

(test M-dot-jumps-and-M-comma-comes-back
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 1 8)
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "M-."))
      (is (not (eq b (pine.edit.buffer:current)))
          "M-. on a symbol pine defines opens the file it is defined in")
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "M-,"))
      (is (eq b (pine.edit.buffer:current)) "M-, came back")
      (is (equal '(1 8) (pine.edit.buffer:point b)) "to where point was"))))

(test the-lisp-answers-are-the-modes-and-not-this-files
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (is-true (pine.repl.mode:handler b :definition))
      (is-true (pine.repl.mode:handler b :complete))
      (is (null (pine.repl.mode:handler (pine.repl.mode:mode-named "fundamental")
                                        :definition))
          "a mode that is not a lisp mode answers none of it"))))

(def-suite* :pine.parser :in :pine)

(test a-buffer-is-parsed-by-a-task-and-not-by-whoever-is-drawing
  (with-lisp-buffer (:text "(defun f (x) x)")
    (let ((p (pine.ts.parser:parser-for (pine.edit.buffer:current))))
      (is-true p "lisp mode names a grammar, so the buffer has a parser")
      (is (equal :commonlisp (pine.ts.parser:language-of p)))
      (is-true (pine.run.task:task-named "parse scratch")
               "the parse runs on its own task, off the typing thread"))))

(test the-parse-catches-up-with-the-buffer-it-is-for
  (with-lisp-buffer (:text "(defun f (x) x)")
    (let ((b (pine.edit.buffer:current)))
      (pine.ts.parser:wait b)
      (is (eql (pine.edit.buffer:tick b)
               (pine.run.cell:held
                (pine.ts.parser:parsed (pine.ts.parser:parser-for b)))))
      (pine.edit.buffer:goto! b 0 15)
      (pine.edit.buffer:insert! b " ")
      (pine.ts.parser:wait b)
      (is (eql (pine.edit.buffer:tick b)
               (pine.run.cell:held
                (pine.ts.parser:parsed (pine.ts.parser:parser-for b))))
          "an edit is followed by a re-parse, without anything asking for one"))))

(test asking-for-highlights-never-waits-for-the-parser
  (with-lisp-buffer (:text "(defun f (x) x)")
    (let ((b (pine.edit.buffer:current)))
      (pine.ts.parser:wait b)
      (pine.edit.buffer:goto! b 0 0)
      (pine.edit.buffer:insert! b ";")
      (let ((found (pine.edit.render:highlights-for b)))
        (is (hash-table-p found)
            "the frame is drawn from what the parser last said, stale or not")))))

(test a-buffer-that-is-killed-takes-its-parser-with-it
  (with-lisp-buffer (:text "(defun f (x) x)")
    (pine.ts.parser:parser-for (pine.edit.buffer:current))
    (is-true (pine.run.task:task-named "parse scratch"))
    (pine.repl.command:run "kill-buffer" (list "scratch"))
    (is (null (pine.ts.parser:parsers))
        "the foreign parser is freed rather than left to the image")))

(def-suite* :pine.motion :in :pine)

(test the-arrows-move-point
  (with-lisp-buffer (:text "one
two")
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 0 0)
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "Right"))
      (is (equal '(0 1) (pine.edit.buffer:point b)) "Right was unbound before")
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "Down"))
      (is (equal '(1 1) (pine.edit.buffer:point b)))
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "Home"))
      (is (equal '(1 0) (pine.edit.buffer:point b)))
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "End"))
      (is (equal '(1 3) (pine.edit.buffer:point b))))))

(test a-count-says-how-many-times-and-is-spent-once
  (with-lisp-buffer ()
    (pine.edit.motion:reset!)
    (is (eql 1 (pine.edit.motion:times)) "no count means once")
    (pine.edit.key:dispatch nil (pine.edit.key:parse-key "C-u"))
    (is (eql 4 (pine.edit.motion:times)) "C-u means four")
    (is (eql 1 (pine.edit.motion:times)) "and it is spent")
    (pine.edit.key:dispatch nil (pine.edit.key:parse-key "M-4"))
    (pine.edit.key:dispatch nil (pine.edit.key:parse-key "M-2"))
    (is (eql 42 (pine.edit.motion:times)) "the digits are the count")))

(test point-moves-over-a-form-because-the-parse-says-where-it-ends
  (with-lisp-buffer (:text "(defun f (x) x)
(probe 1)")
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 0 0)
      (pine.ts.parser:wait b)
      (is-true (pine.edit.motion:toward b :forward-sexp)
               "the parse is there and nothing was asking it")
      (pine.repl.command:run "forward-sexp")
      (is (equal '(0 15) (pine.edit.buffer:point b))
          "over the whole defun, not over a word")
      (pine.edit.buffer:goto! b 1 3)
      (pine.repl.command:run "beginning-of-defun")
      (is (equal '(1 0) (pine.edit.buffer:point b))))))

(test the-word-at-point-changes-case
  (with-lisp-buffer (:text "hello there")
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 0 0)
      (pine.repl.command:run "upcase-word")
      (is (equal "HELLO there" (pine.fs.node:contents b))))))

(test a-line-is-commented-and-uncommented-by-what-the-mode-says
  (with-lisp-buffer (:text "(probe)")
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 0 0)
      (pine.repl.command:run "comment-line")
      (is (equal "; (probe)" (pine.fs.node:contents b)))
      (pine.edit.buffer:goto! b 0 0)
      (pine.repl.command:run "comment-line")
      (is (equal "(probe)" (pine.fs.node:contents b))))))

(test help-goes-into-a-buffer-rather-than-the-echo-line
  (with-lisp-buffer ()
    (pine.repl.command:run "describe-bindings")
    (let ((b (pine.edit.buffer:buffer-named "*help*")))
      (is-true b "C-h b wrote a buffer")
      (is (search "C-x C-f" (pine.fs.node:contents b)))
      (is (search "find-file" (pine.fs.node:contents b))))))

(def-suite* :pine.repl-buffer :in :pine)

(test the-repl-is-a-buffer-whose-newline-submits
  (with-lisp-buffer ()
    (pine.repl.command:run "open-repl")
    (let ((b (pine.edit.buffer:buffer-named "*repl*")))
      (is-true b)
      (is (equal "repl" (pine.edit.buffer:mode-of b)))
      (pine.edit.buffer:move! b :buffer 1)
      (pine.edit.buffer:insert! b "(+ 1 2)")
      (pine.repl.command:run "newline")
      (is (search "3" (pine.fs.node:contents b))
          "the newline was the mode's, and it evaluated what was typed")
      (is (search "pine> " (pine.edit.buffer:line b (pine.edit.buffer:point-line b)))
          "and left a prompt to type at"))))

(test a-fault-can-be-a-buffer-with-its-restarts-in-it
  (with-lisp-buffer ()
    (handler-case (error "a probe")
      (error (e) (pine.edit.debugger:show e :restarts (list "abort" "retry"))))
    (let ((b (pine.edit.buffer:buffer-named "*debugger*")))
      (is-true b "the debugger is a buffer, not a line of text")
      (is (equal "debugger" (pine.edit.buffer:mode-of b)))
      (is (search "a probe" (pine.fs.node:contents b)))
      (is (search "0  abort" (pine.fs.node:contents b)))
      (is-true (pine.repl.mode:binding b "a") "and its keys are its mode's")
      (pine.repl.command:run "debugger-quit")
      (is (null (pine.edit.debugger:standing))))))

(test the-mode-chain-claims-a-verb-and-a-mode-that-does-not-falls-through
  (with-lisp-buffer (:text "(defun f (x)
  x)")
    (let ((b (pine.edit.buffer:current)))
      (pine.ts.parser:wait b)
      (pine.edit.buffer:goto! b 0 12)
      (pine.repl.command:run "newline")
      (is (eql 2 (pine.edit.buffer:point-col b))
          "lisp mode claimed the newline and indented, which the command does not do")
      (setf (pine.edit.buffer:mode-of b) "fundamental")
      (pine.repl.command:run "newline")
      (is (eql 0 (pine.edit.buffer:point-col b))
          "a mode that claims nothing leaves the plain newline"))))

(test a-big-buffer-is-parsed-and-painted-only-where-it-is-being-looked-at
  (with-lisp-buffer (:text "")
    (let ((b (pine.edit.buffer:current))
          (w (pine.edit.window:focused)))
      (setf (pine.fs.node:contents b)
            (with-output-to-string (s)
              (dotimes (i 6000) (format s "(defun f~d (x) x)~%" i))))
      (pine.ts.parser:wait b :seconds 30)
      (is (equal '(0 . 24) (pine.ts.parser:showing b)))
      (let ((found (pine.ts.parser:highlights b)))
        (is (< (length found) 500)
            "the walk covers the window, not six thousand lines")
        (is (every (lambda (run) (<= (first run) 24)) found)))
      (setf (pine.edit.window:scroll-of w) 3000)
      (pine.ts.parser:wait b :seconds 30)
      (is (equal '(3000 . 3024) (pine.ts.parser:showing b))
          "scrolling moves the band the parser is asked about")
      (let ((found (pine.ts.parser:highlights b)))
        (is (eql 3000 (first (first found)))
            "and the colours come back in the buffer's own line numbers")
        (is (<= (first (car (last found))) 3024))))))

(test an-edit-is-handed-to-the-parser-as-an-edit
  (with-lisp-buffer (:text "(defun f (x) x)")
    (let ((b (pine.edit.buffer:current)))
      (pine.ts.parser:wait b)
      (pine.edit.buffer:goto! b 0 15)
      (pine.edit.buffer:insert! b (format nil "~%(defun g (y) y)"))
      (destructuring-bind (what from) (pine.edit.buffer:edit-of b)
        (is (equal '(0 1 2 16) what)
            "at line 0, one line became two, and it grew by sixteen bytes")
        (is-true from "and it says which lines it was computed against"))
      (pine.ts.parser:wait b)
      (is (null (pine.edit.buffer:edit-of b))
          "the parser takes the edit rather than applying it twice"))))
