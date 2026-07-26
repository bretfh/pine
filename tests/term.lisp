(in-package :pine.test)

(def-suite* :pine.term :in :pine)

;;;; The drain is what stands between a flooding program and the UI thread, so
;;;; it is exercised with chunks pushed straight onto the terminal rather than
;;;; through a pty.

(defun detached-terminal (&key (width 20) (height 4))
  (pine.term::%make-terminal
   :buffer nil :fd -1 :pid -1
   :term (pine.vt:make-term :width width :height height)))

(defun push-pending (terminal &rest chunks)
  (dolist (c chunks terminal)
    (push c (pine.term::terminal-pending terminal))))

(defun terminal-row (terminal y)
  (string-right-trim " " (pine.vt:term-dump-row-string
                          (pine.term:terminal-term terminal) y)))

(test a-drain-feeds-the-pending-output-into-the-emulator
  (let ((terminal (detached-terminal)))
    (push-pending terminal "hello")
    (is-true (pine.term::%drain-one terminal))
    (is (string= "hello" (terminal-row terminal 0)))
    (is (null (pine.term::terminal-pending terminal)))))

(test a-drain-with-nothing-pending-reports-nothing
  (is-false (pine.term::%drain-one (detached-terminal))))

(test the-oldest-chunk-is-fed-first
  (let ((terminal (detached-terminal)))
    (push-pending terminal "one " "two")
    (pine.term::%drain-one terminal)
    (is (string= "one two" (terminal-row terminal 0)))))

(test a-drain-stops-at-its-budget-and-carries-the-rest
  (let ((terminal (detached-terminal))
        (pine.term::*drain-budget* 4))
    (push-pending terminal "abcde" "fghij")
    (pine.term::%drain-one terminal)
    (is (string= "abcde" (terminal-row terminal 0)))
    (is (equal '("fghij") (pine.term::terminal-carry terminal)))
    (pine.term::%drain-one terminal)
    (is (string= "abcdefghij" (terminal-row terminal 0)))
    (is (null (pine.term::terminal-carry terminal)))))

(test a-carried-backlog-past-the-cap-drops-its-oldest
  (let ((terminal (detached-terminal))
        (pine.term::*drain-budget* 1)
        (pine.term::*carry-cap* 6))
    (push-pending terminal "aaa" "bbb" "ccc" "ddd")
    (pine.term::%drain-one terminal)
    (is (string= "aaa" (terminal-row terminal 0)))
    (is (equal '("ccc" "ddd") (pine.term::terminal-carry terminal)))))

(test a-named-key-becomes-its-escape-sequence
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (is (string= (esc "[A")
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "Up"))))
    (is (string= (esc "[3~")
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "Delete"))))
    (is (string= (string #\Return)
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "Return"))))
    (is (string= (esc "[1;5A")
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "C-Up"))))))

(test a-control-key-becomes-its-control-character
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (is (string= (string (code-char 1))
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "C-a"))))
    (is (string= (string (code-char 3))
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "C-c"))))))

(test a-meta-key-is-escape-then-the-character
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (is (string= (esc "f")
                 (pine.term::key->pty-bytes term (pine.editor.key:parse-key "M-f"))))))

(test a-plain-key-is-itself-and-an-unknown-key-is-nothing
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (is (string= "a" (pine.term::key->pty-bytes term (pine.editor.key:parse-key "a"))))
    (is (null (pine.term::key->pty-bytes term (pine.editor.key:parse-key "F13"))))))

(test terminal-mode-answers-get-text-from-the-emulator-grid
  (let ((terminal (detached-terminal :width 6 :height 2)))
    (push-pending terminal "ab")
    (pine.term::%drain-one terminal)
    (is (string= (format nil "ab    ~%      ") (pine.term:gterm-text terminal)))))

(test a-buffer-with-no-terminal-has-none
  (with-fixture substrate ()
    (is (null (pine.term:terminal-for-buffer
               (pine.editor.frame::buffer "scratch"))))))
