(in-package :pine.test)

(def-suite* :pine.prompt :in :pine)

(defmacro with-prompt ((&key (text "")) &body body)
  `(unwind-protect
        (progn
          (pine:start)
          (setf (pine.fs.node:contents (pine.edit.buffer:current)) ,text)
          (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 0)
          ,@body)
     (pine:stop)))

(defun typed (text)
  (loop :for ch :across text
        :do (pine.edit.key:dispatch nil (pine.edit.key:make-key (string ch)))))

(defun press (chord)
  (pine.edit.key:dispatch nil (pine.edit.key:parse-key chord)))

(test asking-puts-a-question-up-and-answering-takes-it-down
  (with-prompt ()
    (let ((got nil))
      (pine.edit.prompt:ask "Who? " :then (lambda (answer) (setf got answer)))
      (is-true (pine.edit.prompt:asking-p))
      (is (equal "Who? " (pine.edit.prompt:question (pine.edit.prompt:asking))))
      (typed "world")
      (is (equal "world" (pine.edit.prompt:said)))
      (is (equal "Who? world" (pine.edit.prompt:showing)))
      (pine.edit.prompt:answer!)
      (is (equal "world" got))
      (is-false (pine.edit.prompt:asking-p)))))

(test what-pine-said-is-shown-when-it-is-not-asking
  (with-prompt ()
    (is-false (pine.edit.prompt:asking-p))
    (pine.run.log:note "a probe said this")
    (is (equal "a probe said this" (pine.edit.prompt:showing))
        "pine saying is the log; pine asking is the prompt")))

(test cancelling-a-question-runs-nothing
  (with-prompt ()
    (let ((got :untouched))
      (pine.edit.prompt:ask "Who? " :then (lambda (answer) (setf got answer)))
      (typed "world")
      (pine.edit.prompt:cancel!)
      (is (eq :untouched got))
      (is-false (pine.edit.prompt:asking-p)))))

(test the-prompt-is-a-minor-mode-so-RET-answers-instead-of-inserting
  (with-prompt (:text "hello")
    (let ((got nil))
      (pine.edit.prompt:ask "Who? " :then (lambda (answer) (setf got answer)))
      (typed "you")
      (press "RET")
      (is (equal "you" got) "RET answered rather than breaking the line")
      (is-false (pine.edit.prompt:asking-p)))))

(defun named (candidates)
  (mapcar #'pine.edit.prompt:name-of candidates))

(test candidates-come-from-a-source-and-narrow-as-you-type
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (let ((all (named (pine.edit.prompt:matching))))
      (is (member "find-file" all :test #'equal))
      (is (member "save-buffer" all :test #'equal))
      (typed "split")
      (let ((found (named (pine.edit.prompt:matching))))
        (is (member "split-window-below" found :test #'equal))
        (is (null (member "find-file" found :test #'equal))
            "what does not match is not a candidate")))
    (pine.edit.prompt:cancel!)))

(test a-candidate-carries-what-it-is-for
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (typed "save-buffer")
    (let ((pick (first (pine.edit.prompt:matching))))
      (is (equal "save-buffer" (pine.edit.prompt:name-of pick)))
      (is (equal "write the buffer to its file"
                 (pine.edit.prompt:annotation pick)))
      (is (search "write the buffer" (pine.edit.prompt:shows pick 60))
          "the annotation is shown beside the name, not instead of it"))
    (pine.edit.prompt:cancel!)))

(test what-matches-from-the-start-is-offered-first
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (typed "kill")
    (let ((found (named (pine.edit.prompt:matching))))
      (is (member "backward-kill-word" found :test #'equal))
      (is (< (position "kill-line" found :test #'equal)
             (position "backward-kill-word" found :test #'equal))
          "a name beginning with what was typed comes before one merely containing it"))
    (pine.edit.prompt:cancel!)))

(test the-candidates-follow-what-is-typed-rather-than-the-question
  (with-prompt ()
    (pine.edit.prompt:ask "Buffer: " :category :buffer)
    (is (member "scratch" (named (pine.edit.prompt:matching)) :test #'equal))
    (pine.edit.buffer:make-buffer "later")
    (is (member "later" (named (pine.edit.prompt:matching)) :test #'equal)
        "a buffer made after the question was asked is still a candidate")
    (pine.edit.prompt:cancel!)))

(test choosing-and-completing-fills-the-answer
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (typed "split-window")
    (let ((first-choice (pine.edit.prompt:complete!)))
      (is (search "split-window" (pine.edit.prompt:name-of first-choice)))
      (is (equal (pine.edit.prompt:name-of first-choice)
                 (pine.edit.prompt:said))))
    (pine.edit.prompt:cancel!)
    (pine.edit.prompt:ask "M-x " :category :command)
    (typed "window")
    (is (< 1 (length (pine.edit.prompt:matching))) "several candidates match")
    (let ((first-pick (pine.edit.prompt:complete!)))
      (pine.edit.prompt:ask "M-x " :category :command)
      (typed "window")
      (pine.edit.prompt:choose! 1)
      (is (eql 1 (pine.edit.prompt:chosen (pine.edit.prompt:asking))))
      (is (not (equal first-pick (pine.edit.prompt:complete!)))
          "choosing moved to a different candidate"))
    (pine.edit.prompt:cancel!)))

(test typing-again-puts-the-choice-back-on-the-first-candidate
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (typed "window")
    (pine.edit.prompt:choose! 2)
    (is (eql 2 (pine.edit.prompt:chosen (pine.edit.prompt:asking))))
    (typed "-")
    (is (eql 0 (pine.edit.prompt:chosen (pine.edit.prompt:asking)))
        "a stale choice would complete to a candidate that is no longer there")
    (pine.edit.prompt:cancel!)))

(test m-x-runs-a-command-by-name
  (with-prompt ()
    (press "M-x")
    (is-true (pine.edit.prompt:asking-p))
    (typed "split-window-below")
    (press "RET")
    (is (= 2 (length (pine.edit.window:windows)))
        "the command M-x named actually ran")))

(test m-x-runs-the-candidate-in-front-when-the-name-is-half-typed
  (with-prompt ()
    (press "M-x")
    (typed "split-window-b")
    (press "RET")
    (is (= 2 (length (pine.edit.window:windows)))
        "a prompt that must match answers with the candidate, not the fragment")))

(test a-prompt-that-need-not-match-answers-with-what-was-typed
  (with-prompt ()
    (let ((got nil))
      (pine.edit.prompt:ask "New file: " :category :file
                            :then (lambda (answer) (setf got answer)))
      (typed "/tmp/pine-probe-not-there.txt")
      (press "RET")
      (is (equal "/tmp/pine-probe-not-there.txt" got)
          "a file prompt names files that do not exist yet"))))

(test a-command-that-asks-asks-at-the-prompt
  (with-prompt ()
    (let ((file (merge-pathnames "pine-probe-prompt.txt" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out file :direction :output :if-exists :supersede)
               (write-string "from disk" out))
             (pine.repl.command:run "find-file")
             (is-true (pine.edit.prompt:asking-p)
                      "the command did not read a stream, it raised a question")
             (is (equal "Find file: "
                        (pine.edit.prompt:question (pine.edit.prompt:asking))))
             (typed (namestring file))
             (press "RET")
             (is (equal "from disk"
                        (pine.fs.node:contents
                         (pine.edit.buffer:buffer-named "pine-probe-prompt.txt")))))
        (ignore-errors (delete-file file))))))

(test isearch-moves-point-and-marks-what-it-found
  (with-prompt (:text "one
two
three")
    (press "C-s")
    (typed "three")
    (press "RET")
    (is (equal '(2 0) (pine.edit.buffer:point (pine.edit.buffer:current))))
    (is (equal '((:face :match))
               (pine.edit.buffer:properties-at (pine.edit.buffer:current) 2 1)))))

(test the-caret-is-on-the-prompt-line-while-pine-is-asking
  (with-prompt (:text "hello")
    (press "M-x")
    (typed "spl")
    (let ((tree (pine.edit.render:frame-tree :cols 60 :rows 10)))
      (is (= -1 (pine.ui.node:view-crow (%classed tree "editor-view")))
          "the buffer shows no caret while the keyboard is at the prompt")
      (is (= 0 (pine.ui.node:view-crow (%classed tree "echo"))))
      (is (= 7 (pine.ui.node:view-ccol (%classed tree "echo")))
          "the caret sits after the question and what has been typed into it"))
    (pine.edit.prompt:cancel!)))

(test the-frame-shows-the-question-and-its-candidates
  (with-prompt ()
    (press "M-x")
    (typed "split")
    (let ((rows (pine:frame :width 60 :height 12)))
      (is (find-if (lambda (row) (search "M-x split" row)) rows)
          "the question and what is typed are the last line")
      (is (find-if (lambda (row) (search "split-window-below" row)) rows)
          "the candidates are above it"))
    (pine.edit.prompt:cancel!)))
