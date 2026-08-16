(in-package :pine/test)

(def-suite* :pine/host :in :pine)

(test a-device-is-rows-and-not-a-class-each
  (with-tree
    (let* ((held (list 40))
           (dev (device:device "probe"
                               (list (list "volume"
                                           (lambda () (first held))
                                           (lambda (v) (setf (first held) v)))
                                     (list "muted" (lambda () nil))))))
      (node:attach dev (tree:root))
      (is (equal '("volume" "muted") (node:contents dev)))
      (is (= 40 (node:contents (tree:at nil "probe/volume"))))
      (setf (node:contents (tree:at nil "probe/volume")) 55)
      (is (= 55 (first held)))
      (is (eq (tree:at nil "probe/volume") (tree:at nil "probe/volume"))
          "the same child every time"))))

(test a-device-says-what-it-wants-watched
  (with-tree
    (let ((dev (device:device "probe" (list (list "one" (constantly 1)))
                              :announces '("some stream") :refreshes 5)))
      (is (equal '("some stream") (node:announces dev)))
      (is (eql 5 (node:refreshes dev))))))

(test the-shell-answers-what-a-line-said
  (is (equal "hello" (sh:sh "echo hello")))
  (is (equal '("a" "b") (sh:lines (format nil "a~%b~%"))))
  (is (equal '("a" "b") (sh:words "a b")))
  (is (= 42 (sh:number-in "load 42 now")))
  (is (equal "a" (sh:firstp (format nil "a~%b")))))

(test an-answer-stands-for-a-breath
  (let ((pine/host/shell:*breath* 10))
    (let ((first-said (sh:sh "date +%s%N")))
      (is (equal first-said (sh:sh "date +%s%N"))
          "the same line asked twice in a breath forks once"))))

(test a-line-is-a-place-whether-or-not-it-was-asked-before
  (booted)
  (with-tree
    (sh:attach (tree:root))
    (is (equal "hello" (node:contents (tree:at nil "sh/echo hello"))))))

(test the-clock-is-the-time-as-paths
  (with-tree
    (let ((clock (device:clock)))
      (node:attach clock (tree:root))
      (device:tick)
      (is (integerp (node:contents (tree:at nil "clock/year"))))
      (is (stringp (node:contents (tree:at nil "clock/hour")))))))

(test the-environment-reads-and-writes-through
  (with-tree
    (let ((env (device:env)))
      (node:attach env (tree:root))
      (let ((n (node:resolve env "PATH")))
        (is (not (null n)))
        (is (equal (uiop:getenv "PATH") (node:contents n)))))))
