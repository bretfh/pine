(in-package :pine/test)

(def-suite* :pine/draw :in :pine)

(defun on-screen ()
  "What the editor surface holds, as text. Not what a function answers when asked
directly: what is on the screen."
  (let ((tree (node:contents (surface:named "editor"))))
    (with-output-to-string (out)
      (labels ((walk (w)
                 (when w
                   (when (typep w 'widget:cells)
                     (dolist (row (widget:by-row w))
                       (write-line (string-right-trim " " (car row)) out)))
                   (dolist (p (widget:parts w)) (walk p)))))
        (walk tree)))))

(defun on-screen-painted ()
  "The rows with their colours, so a selection moving is a change."
  (let ((tree (node:contents (surface:named "editor")))
        (out nil))
    (labels ((walk (w)
               (when w
                 (when (typep w 'widget:cells) (push (widget:by-row w) out))
                 (dolist (p (widget:parts w)) (walk p)))))
      (walk tree))
    (princ-to-string out)))

(defun quiet ()
  (loop :repeat 3 :while (prompt:askingp) :do (command:run "cancel")))

(test typing-shows-up-on-the-screen
  (editing)
  (quiet)
  (typed "h" "e" "l" "l" "o")
  (is (search "hello" (on-screen))))

(test what-is-typed-at-a-prompt-shows-up-on-the-screen
  "The frame follows what it read. A prompt keeps its text where nothing reads it,
and then nothing you type is drawn: the question stands there and the editor looks
dead. This is that, asserted against the screen."
  (editing)
  (quiet)
  (typed "h" "e" "l" "l" "o")
  (typed "M-x")
  (is (search "M-x" (on-screen)) "the question is drawn")
  (dolist (c '("b" "e" "g" "i"))
    (typed c)
    (is (search (format nil "M-x ~a" (prompt:so-far)) (on-screen))
        "~s is on the screen" (prompt:so-far)))
  (is (search "beginning-of-" (on-screen)) "and so are the candidates")
  (let ((before (on-screen-painted)))
    (typed "C-n")
    (is (not (equal before (on-screen-painted))) "moving the choice moves the screen"))
  (typed "TAB")
  (is (search (prompt:so-far) (on-screen)) "and so does filling it in")
  (typed "C-g")
  (is (search "hello" (on-screen)) "and cancelling puts the buffer back"))

(test a-prompt-inside-a-prompt-does-not-strand-you
  "Answering restores what was current. If that is the prompt itself, every key
after it edits a document nothing shows."
  (let ((doc (editing)))
    (quiet)
    (typed "M-x")
    (typed "C-x" "C-f")
    (typed "C-g")
    (quiet)
    (setf (node:contents doc) "")
    (doc:goto doc 0 0)
    (typed "o" "k")
    (is (search "ok" (on-screen)))))

(test a-surface-that-threw-once-comes-back
  "A derived that faults keeps no value. If a stir on a stale one stops there, the
screen is a picture until pine is restarted."
  (with-tree
    (let* ((said (cons nil nil))
           (broken (cons t nil))
           (n (tree:ensure nil "probe-src")))
      (setf (node:contents n) "first")
      (let ((s (surface:builds "probe-surface"
                               (lambda ()
                                 (let ((v (node:contents n)))
                                   (when (car broken) (error "on purpose"))
                                   (setf (car said) v)
                                   (build:label v)))
                               :as 'surface:panel :shown t)))
        (is (null (ignore-errors (node:contents s))))
        (setf (car broken) nil)
        (setf (node:contents n) "second")
        (ignore-errors (node:contents s))
        (is (equal "second" (car said)))))))

(test refresh-draws-everything-again
  (editing)
  (is (plusp (command:run "refresh"))))

(defun settled (doc &key (seconds 2))
  (let ((p (parser:note doc)))
    (when p
      (loop :repeat (round (/ seconds 0.01))
            :until (parser:currentp p)
            :do (sleep 0.01)))
    p))

(defun faces-on-line (doc line)
  (settled doc)
  (render:rows :cols 80 :lines 10)
  (sort (loop :for run :in (parser:highlights doc)
              :when (eql line (first run)) :collect (rest run))
        #'< :key #'first))

(test what-is-coloured-follows-what-is-typed
  "The colours on screen are the parse of the text that is there, not of the text
that was there a keystroke ago."
  (let ((doc (editing)))
    (quiet)
    (setf (node:contents doc) "(car (cdr xs))")
    (doc:goto doc 0 0)
    (is (member '(1 4 :builtin) (faces-on-line doc 0) :test #'equal)
        "~s" (faces-on-line doc 0))
    (doc:goto doc 0 3)
    (typed "q")
    (is (equal "(caqr (cdr xs))" (doc:text doc)))
    (is (member '(1 5 :function-call) (faces-on-line doc 0) :test #'equal)
        "a head nothing has heard of is a call, not a builtin: ~s"
        (faces-on-line doc 0))
    (typed "BackSpace")
    (is (member '(1 4 :builtin) (faces-on-line doc 0) :test #'equal)
        "and taking it back puts it back: ~s" (faces-on-line doc 0))))

(defun in-language (name text)
  (let ((doc (or (doc:named name)
                 (doc:make-document name :mode (make-instance 'mode:lisp)))))
    (setf (node:contents doc) text)
    (doc:goto doc 0 0)
    (window:show (window:focused) doc)
    (setf (doc:current) doc)
    (settled doc)
    doc))

(defun fresh-walk (text)
  (multiple-value-bind (lib fn) (pine/text/ts/syntax:grammar-of :commonlisp)
    (let ((ps (pine/text/ts/runtime:make-parse-state
               parser:*runtime* :commonlisp lib fn
               :syntax (pine/text/ts/syntax:for :commonlisp))))
      (when ps
        (unwind-protect
             (progn
               (pine/text/ts/runtime:parse-lines!
                ps (pine/text/lines:of text))
               (sort (pine/text/ts/highlight:parse-highlights ps) #'< :key #'first))
          (pine/text/ts/runtime:free-parse-state ps))))))

(test an-edit-is-answered-before-its-parse-is
  "The document commits on the thread that typed; the parse happens elsewhere. So
what is there is what was typed, whatever the colours are doing."
  (editing)
  (quiet)
  (let ((doc (in-language "async-edit" "(defun f (x) x)")))
    (doc:goto doc 0 15)
    (typed "y")
    (is (search "x)y" (doc:text doc)) "the edit landed at once")
    (doc:kill "async-edit")))

(test the-colours-that-settle-match-a-fresh-walk
  "A wrong edit descriptor makes a plausible wrong tree rather than an error, so
every verb is driven through a real document and what settles is compared with a
parse of the same lines from scratch."
  (editing)
  (quiet)
  (let ((doc (in-language "async-walk" (format nil "(defun f (x)~%  (+ x 1))~%(f 2)")))
        (state 20260816))
    (flet ((next (n)
             (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
             (mod state n)))
      (dotimes (i 12)
        (case (next 4)
          (0 (doc:insert doc "z"))
          (1 (doc:newline doc))
          (2 (doc:delete-back doc))
          (t (doc:insert doc "(g 1)")))))
    (settled doc)
    (let ((text (doc:text doc)))
      (is (until (lambda ()
                   (equal (fresh-walk text)
                          (sort (parser:highlights doc) #'< :key #'first)))
                 :seconds 5)
          "the colours that settled disagree with a fresh parse of~% ~s" text))
    (doc:kill "async-walk")))

(test every-parser-started-at-once-produces-a-tree
  "Starting a parser is a grammar load, a TSParser and a language claim, each on
whichever thread asked. A document opened at the same moment as nine others is the
case that has to hold."
  (editing)
  (quiet)
  (let ((names (loop :for i :from 0 :below 10
                     :collect (format nil "async-many-~d" i))))
    (dolist (name names)
      (let ((doc (doc:make-document name :mode (make-instance 'mode:lisp))))
        (setf (node:contents doc) "(defun f (x) x)")))
    (dolist (name names)
      (is (until (lambda () (parser:highlights (doc:named name))) :seconds 10)
          "~a never got colours" name))
    (dolist (name names) (doc:kill name))))

(test a-faulted-parser-leaves-the-document-editable
  "The isolation claim: the parser faults and the document goes on taking edits."
  (editing)
  (quiet)
  (let* ((doc (in-language "async-wedge" "(defun f (x) x)"))
         (p (parser:parser-for doc)))
    (is (not (null p)) "a lisp document should have a parser")
    (job:tell (parser:running p) '(:no-such-verb))
    (doc:goto doc 0 15)
    (dotimes (i 5) (doc:insert doc "q"))
    (is (until (lambda () (= 5 (count #\q (doc:text doc)))))
        "the document stopped editing after its parser faulted")
    (is (stringp (doc:text (doc:named "scratch")))
        "and the rest of pine stopped answering")
    (doc:kill "async-wedge")))

(test killing-a-document-takes-its-parser-with-it
  (editing)
  (quiet)
  (let ((before (length (sb-thread:list-all-threads))))
    (in-language "async-kill" "(defun f (x) x)")
    (is (not (null (parser:parser-for (doc:named "async-kill")))))
    (doc:kill "async-kill")
    (is (until (lambda () (<= (length (sb-thread:list-all-threads)) before))
               :seconds 5)
        "killing the document left its parser running")))

(test indenting-answers-without-waiting-for-the-parse
  "TAB tells the parser and comes back. The line takes its column when the parse
says what it is, so nothing sleeps on the thread that pressed the key."
  (editing)
  (quiet)
  (let ((doc (in-language "async-indent" (format nil "(defun f ()~%(bar))"))))
    (doc:goto doc 1 0)
    (typed "TAB")
    (is (until (lambda () (= 2 (doc:indent-of doc 1))) :seconds 5)
        "the line never took the column the parse said: ~s" (doc:line doc 1))
    (doc:kill "async-indent")))
