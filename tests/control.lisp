(in-package :pine.test)

(def-suite* :pine.control :in :pine)

(defmacro with-pine (&body body)
  `(unwind-protect (progn (pine:start) ,@body) (pine:stop)))

(defun asked (&rest message)
  (pine.net.control:received message))

(test the-control-verbs-read-and-write-the-tree
  (with-pine
    (is (equal '(:ok "") (asked :write "/probe" "nil")))
    (is (equal '(:ok "41") (asked :write "/probe" "41")))
    (is (equal '(:ok "41") (asked :read "/probe")))
    (is (eq :no (first (asked :read "/nothing-here"))))
    (is (search "probe" (second (asked :ls "/"))))))

(test a-form-evaluates-in-the-daemon-and-comes-back-as-text
  (with-pine
    (is (equal '(:ok "3") (asked :eval "(+ 1 2)")))
    (is (eq :no (first (asked :eval "(error \"probe\")"))))
    (is (equal '(:ok "pong") (asked :ping)))))

(test a-command-runs-through-the-control-actor
  (with-pine
    (is (equal '(:ok "/") (asked :run "pwd")))
    (is (search "what a command is for" (second (asked :run "help" "help"))))
    (is (eq :no (first (asked :run "not-a-command"))))))

(test what-a-watcher-is-told-is-the-value-that-moved
  (with-pine
    (let ((heard nil)
          (n (pine/fs/tree:ensure (pine.world.world:root pine.world.world:*world*)
                                  "probe")))
      (pine/fs/watch:watch n (lambda (of value)
                               (push (list (pine/fs/node:name of) value) heard)))
      (setf (pine/fs/node:contents n) 1)
      (setf (pine/fs/node:contents n) 2)
      (is (equal '(("probe" 2) ("probe" 1)) heard))
      (setf (pine/fs/node:contents n) 2)
      (is (= 2 (length heard))
          "a write of what it already held is not a move"))))

(test a-watcher-that-is-dropped-hears-nothing-more
  (with-pine
    (let ((heard 0)
          (n (pine/fs/tree:ensure (pine.world.world:root pine.world.world:*world*)
                                  "probe")))
      (let ((w (pine/fs/watch:watch n (lambda (of value)
                                        (declare (ignore of value))
                                        (incf heard)))))
        (setf (pine/fs/node:contents n) 1)
        (pine/fs/watch:unwatch w)
        (setf (pine/fs/node:contents n) 2)
        (is (= 1 heard))
        (is (null (pine/fs/watch:watchers)))))))

(test the-cli-says-what-it-takes-and-answers-nothing-when-no-daemon-is-up
  (is (search "usage: pine" (pine.cli:usage)))
  (is (null (pine.cli:ask (list :ping) :port 17099))
      "no daemon on that port, and asking is not an error"))

(test what-is-under-one-place-and-not-the-other
  (with-pine
    (asked :write "/probe/a" "1")
    (asked :write "/probe/b" "2")
    (asked :write "/other/a" "1")
    (asked :write "/other/c" "3")
    (let ((said (second (asked :diff "/probe" "/other"))))
      (is (search "+ /probe/b" said) "here and not there")
      (is (search "- /other/c" said) "there and not here")
      (is (null (search "/probe/a" said)) "and what is the same is not said"))))

(test the-daemon-can-be-told-to-read-its-config-again
  (with-pine
    (is (equal '(:ok "RELOADED") (asked :reload)))))
