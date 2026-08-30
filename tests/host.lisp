(in-package :pine/test)

(def-suite* :pine/host :in :pine)

(test a-device-is-rows-and-not-a-class-each
  (with-tree
    (let* ((held (list 40))
           (dev (device:readings "probe"
                               (list (list "volume"
                                           (lambda () (first held))
                                           (lambda (v) (setf (first held) v)))
                                     (list "muted" (lambda () nil))))))
      (node:attach dev (tree:root))
      (is (equal '("volume" "muted") (node:contents dev)))
      (is (= 40 (node:contents (tree:at "/probe/volume"))))
      (setf (node:contents (tree:at "/probe/volume")) 55)
      (is (= 55 (first held)))
      (is (eq (tree:at "/probe/volume") (tree:at "/probe/volume"))
          "the same child every time"))))

(test a-device-says-what-it-wants-watched
  (with-tree
    (let ((dev (device:readings "probe" (list (list "one" (constantly 1)))
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
    (node:attach (sh:sh-node) (tree:root))
    (is (equal "hello" (node:contents (tree:at "/sh/echo hello"))))))

(test the-clock-is-the-time-as-paths
  (with-tree
    (let ((clock (device:clock)))
      (node:attach clock (tree:root))
      (device:tick)
      (is (integerp (node:contents (tree:at "/clock/year"))))
      (is (stringp (node:contents (tree:at "/clock/hour")))))))

(test the-environment-reads-and-writes-through
  (with-tree
    (let ((env (device:env)))
      (node:attach env (tree:root))
      (let ((n (node:resolve env "PATH")))
        (is (not (null n)))
        (is (equal (uiop:getenv "PATH") (node:contents n)))))))

(test a-device-told-the-world-moved-reads-it-again
  "A device is told the world behind it moved; what reads the world is its rows.
Stirring one without the other leaves every reading frozen at whatever it answered
the first time, which is a clock that never ticks."
  (with-tree
    (let* ((n (cons 0 nil))
           (dev (node:attach (device:readings "probe"
                                            (list (list "count"
                                                        (lambda ()
                                                          (d:swap (car n) #'1+)))))
                             (tree:root))))
      (is (eql 1 (node:contents (tree:at "/probe/count"))))
      (is (eql 1 (node:contents (tree:at "/probe/count")))
          "and it is remembered until something says otherwise")
      (node:moved dev)
      (is (eql 2 (node:contents (tree:at "/probe/count")))
          "the device moved, so the reading is read again"))))

(test the-desktop-clipboard-is-a-place
  "Copying is a write and pasting is a read. Without it an editor is an island:
nothing copied anywhere else can come in, and nothing killed here can go out."
  (with-tree
    (let ((dev (node:attach (device:clip) (tree:root))))
      (is (equal '("text") (node:contents dev)))
      (let ((text (node:resolve dev "text")))
        (is (not (null text)) "the row is a place under it")
        (is (not (null (node:reads text))) "it reads")
        (is (not (null (node:writes text))) "and it is written")))))
