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

(test candidates-come-from-a-source-and-narrow-as-you-type
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (let ((all (pine.edit.prompt:matching)))
      (is (member "find-file" all :test #'equal))
      (is (member "save-buffer" all :test #'equal))
      (typed "split")
      (let ((found (pine.edit.prompt:matching)))
        (is (member "split-window-below" found :test #'equal))
        (is (null (member "find-file" found :test #'equal))
            "what does not match is not a candidate")))
    (pine.edit.prompt:cancel!)))

(test choosing-and-completing-fills-the-answer
  (with-prompt ()
    (pine.edit.prompt:ask "M-x " :category :command)
    (typed "split-window")
    (let ((first-choice (pine.edit.prompt:complete!)))
      (is (search "split-window" first-choice))
      (is (equal first-choice (pine.edit.prompt:said))))
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

(test m-x-runs-a-command-by-name
  (with-prompt ()
    (press "M-x")
    (is-true (pine.edit.prompt:asking-p))
    (typed "split-window-below")
    (press "RET")
    (is (= 2 (length (pine.edit.window:windows)))
        "the command M-x named actually ran")))

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
