(in-package :pine.test)

(def-suite* :pine.term :in :pine)

(defmacro with-terminal ((var &key (command "/bin/sh")) &body body)
  `(unwind-protect
        (progn
          (pine:start)
          (let ((,var (pine/edit/term:open-terminal "probe" :command ,command)))
            (unwind-protect (progn (sleep 0.6) ,@body)
              (pine/edit/term:close-terminal "probe"))))
     (pine:stop)))

(defun term-text ()
  (pine/fs/node:contents (pine/edit/buffer:buffer-named "probe")))

(defun settle (test &key (seconds 5))
  (loop :repeat (round (/ seconds 0.05))
        :when (funcall test) :do (return t)
        :do (sleep 0.05)))

(test a-terminal-is-a-buffer-and-the-shell-writes-into-it
  (with-terminal (tm)
    (is (typep tm 'pine/edit/term:terminal))
    (is (typep (pine/edit/buffer:buffer-named "probe") 'pine/edit/buffer:buffer))
    (is (equal "term" (pine/edit/buffer:mode-of (pine/edit/buffer:buffer-named "probe"))))
    (is-true (settle (lambda () (search "$" (term-text))))
             "the shell's prompt reached the buffer")))

(test what-is-typed-goes-to-the-pty-and-what-it-said-comes-back
  (with-terminal (tm)
    (pine/edit/term:send tm (format nil "echo probe-output~%"))
    (is-true (settle (lambda () (search "probe-output" (term-text)))))
    (is (search "echo probe-output" (term-text)) "the echoed line is there too")))

(test typing-in-a-term-buffer-reaches-the-shell-rather-than-the-buffer
  "The mode's :insert handler takes the key, which is the handler chain doing
its job: nothing in the key dispatcher knows what a terminal is."
  (with-terminal (tm)
    (settle (lambda () (search "$" (term-text))))
    (let ((before (term-text)))
      (loop :for ch :across "echo typed-through"
            :do (pine/edit/key:dispatch nil (pine/edit/key:make-key (string ch))))
      (pine/edit/key:dispatch nil (pine/edit/key:parse-key "RET"))
      (is-true (settle (lambda () (search "typed-through" (term-text)))))
      (is (not (equal before (term-text)))))))

(test a-control-key-crosses-as-its-escape-sequence
  (with-terminal (tm)
    (is (equal (string (code-char 3))
               (pine/edit/term:key->bytes tm (pine/edit/key:parse-key "C-c"))))
    (is (equal (string #\Return)
               (pine/edit/term:key->bytes tm (pine/edit/key:parse-key "RET"))))
    (is (equal (string #\Tab)
               (pine/edit/term:key->bytes tm (pine/edit/key:parse-key "TAB"))))))

(test the-screen-is-the-emulator-not-a-log-of-bytes
  (with-terminal (tm)
    (pine/edit/term:send tm (format nil "printf 'aaa\\rZ\\n'~%"))
    (is-true (settle (lambda () (search "Zaa" (term-text))))
             "the carriage return moved the cursor and Z overwrote the first a")))

(test closing-a-terminal-stops-its-reader-and-forgets-it
  (unwind-protect
       (progn
         (pine:start)
         (pine/edit/term:open-terminal "probe" :command "/bin/sh")
         (is (= 1 (length (pine/edit/term:terminals))))
         (pine/edit/term:close-terminal "probe")
         (is (null (pine/edit/term:terminals)))
         (is (null (pine/edit/term:terminal-for "probe"))))
    (pine:stop)))

(test the-terminal-is-a-command
  (unwind-protect
       (progn
         (pine:start)
         (let ((name (pine/repl/command:run "terminal" (list "probe-cmd"))))
           (is (equal "probe-cmd" name))
           (is (member "probe-cmd" (pine/repl/command:run "terminals") :test #'equal))
           (pine/repl/command:run "close-terminal" (list "probe-cmd"))
           (is (null (pine/repl/command:run "terminals")))))
    (pine:stop)))

(test a-terminal-takes-every-key-except-the-one-that-gets-you-out
  (unwind-protect
       (progn
         (pine:start)
         (pine/edit/term:open-terminal "probe" :command "/bin/sh")
         (sleep 0.5)
         (let ((b (pine/edit/buffer:buffer-named "probe")))
           (setf (pine/edit/buffer:current) b)
           (is-true (pine/edit/term:typing b (pine/edit/key:parse-key "C-c"))
                    "C-c reaches the shell rather than the keymap")
           (is-true (pine/edit/term:typing b (pine/edit/key:parse-key "Up")))
           (is (null (pine/edit/term:typing b (pine/edit/key:parse-key "C-x")))
               "and C-x is still the editor's, so you can leave")))
    (pine/edit/term:close-terminal "probe")
    (pine:stop)))

(test what-the-shell-says-faster-than-the-screen-can-show-is-bounded
  (unwind-protect
       (progn
         (pine:start)
         (let ((tm (pine/edit/term:open-terminal "probe" :command "/bin/sh"))
               (pine/edit/term:*carry* 64))
           (let ((b (pine/edit/buffer:buffer-named "probe")))
             (dotimes (i 20) (pine/edit/term::%carry tm (format nil "~40,'x<~d~>" i)))
             (is (<= (pine/data:held (pine/edit/term::carried tm))
                     (* 2 pine/edit/term:*carry*))
                 "it drops what is oldest rather than growing without end")
             (is (plusp (pine/data:held (pine/edit/term:dropped tm))))
             (pine/edit/term:drain tm b)
             (is-true b))))
    (pine/edit/term:close-terminal "probe")
    (pine:stop)))
