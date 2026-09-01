(in-package :pine/test)

(def-suite* :pine/host :in :pine)

(test a-device-is-rows-and-not-a-class-each
  (with-tree
    (let ((held (list 40)))
      (declared:defdevice %probe-volume :describes "rows, not a class each")
      (declared:defbacking %probe-volume ()
        (volume :reads (first held) :writes (lambda (v) (setf (first held) v)))
        (muted  :reads nil))
      (let ((dev (node:attach (declared:made "%probe-volume") (tree:root))))
        (is (equal '("volume" "muted") (node:contents dev)))
        (is (= 40 (node:contents (tree:at "/%probe-volume/volume"))))
        (setf (node:contents (tree:at "/%probe-volume/volume")) 55)
        (is (= 55 (first held)))
        (is (eq (tree:at "/%probe-volume/volume")
                (tree:at "/%probe-volume/volume"))
            "the same child every time")))))

(test a-device-says-what-it-wants-watched
  (with-tree
    (declared:defdevice %probe-watched :describes "what it wants watched")
    (declared:defbacking %probe-watched (:announces '("some stream") :refreshes 5)
      (one :reads 1))
    (let ((dev (declared:made "%probe-watched")))
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

(test telling-the-machine-something-is-not-asking-it-something
  "An answer stands for a breath because two things reading /sys/cpu a moment
apart are asking about the same moment. A thing done twice is done twice, and a
write routed through the memo happened once however many times it was asked for:
muting twice inside a quarter of a second muted once, and two windows closed one
after the other closed one."
  (let ((pine/host/shell:*breath* 10))
    (is (equal (sh:sh "date +%s%N") (sh:sh "date +%s%N"))
        "asked twice in a breath, once")
    (is (not (equal (sh:did "date +%s%N") (sh:did "date +%s%N")))
        "told twice in a breath, twice")
    (is (not (equal (sh:argv "date" "+%s%N") (sh:argv "date" "+%s%N")))
        "and told twice as a program, twice")))

(test what-is-written-to-a-device-is-an-argument-and-not-a-line-of-shell
  "A sink is named by whoever named it and a network by whoever is broadcasting
it. Spliced into a line, either could say anything the shell can -- and quoting is
not an answer, because a double-quoted shell word still spells $(...)."
  (is (equal "$(id);x" (sh:argv "printf" "%s" "$(id);x"))
      "what a word says is what the program is given")
  (is (equal "a b" (sh:argv "printf" "%s" "a b"))
      "and a space in one is a space in one argument"))

(test a-line-is-a-place-whether-or-not-it-was-asked-before
  (booted)
  (with-tree
    (node:attach (sh:sh-node) (tree:root))
    (is (equal "hello" (node:contents (tree:at "/sh/echo hello"))))))

(test the-clock-is-the-time-as-paths
  (with-tree
    (let ((clock (declared:made "clock")))
      (node:attach clock (tree:root))
      (device:tick)
      (is (integerp (node:contents (tree:at "/clock/year"))))
      (is (stringp (node:contents (tree:at "/clock/hour")))))))

(test the-environment-reads-and-writes-through
  (with-tree
    (let ((env (declared:made "env")))
      (node:attach env (tree:root))
      (let ((n (node:resolve env "PATH")))
        (is (not (null n)))
        (is (equal (uiop:getenv "PATH") (node:contents n)))))))

(test a-device-told-the-world-moved-reads-it-again
  "A device is told the world behind it moved; what reads the world is its rows.
Stirring one without the other leaves every reading frozen at whatever it answered
the first time, which is a clock that never ticks."
  (with-tree
    (let ((n (cons 0 nil)))
      (declared:defdevice %probe-count :describes "counts every time it is read")
      (declared:defbacking %probe-count ()
        (count :reads (d:swap (car n) #'1+)))
      (let ((dev (node:attach (declared:made "%probe-count") (tree:root))))
        (is (eql 1 (node:contents (tree:at "/%probe-count/count"))))
        (is (eql 1 (node:contents (tree:at "/%probe-count/count")))
            "and it is remembered until something says otherwise")
        (node:moved dev)
        (is (eql 2 (node:contents (tree:at "/%probe-count/count")))
            "the device moved, so the reading is read again")))))

(test the-desktop-clipboard-is-a-place
  "Copying is a write and pasting is a read. Without it an editor is an island:
nothing copied anywhere else can come in, and nothing killed here can go out.

Which program does the copying is the machine's business: the reading stands either
way, and says :ABSENT where this machine has no way to answer it."
  (with-tree
    (let ((dev (node:attach (declared:made "clip") (tree:root))))
      (is (equal '("text") (node:contents dev)))
      (let ((text (node:resolve dev "text")))
        (is (not (null text)) "the row is a place under it")
        (if (declared:answering (declared:named "clip"))
            (progn
              (is (not (null (node:reads text))) "it reads")
              (is (not (null (node:writes text))) "and it is written"))
            (is (eq :absent (node:holding text))
                "and where nothing here can answer it, it says so"))))))

(test a-device-nothing-on-this-machine-can-answer-still-stands
  "A path that does not resolve is a surface that breaks. One that resolves and says
:ABSENT is a surface that shows a dash. NIL cannot be the answer to both, which is
why READ has always had three."
  (with-tree
    (declared:defdevice %probe-thermostat :describes "nothing here can answer this")
    (declared:defbacking %probe-thermostat (:needs "no-such-program-anywhere")
      (target :reads (sh:sh "no-such-program-anywhere get") :writes (sh:sh "x"))
      (mode   :reads (sh:sh "no-such-program-anywhere mode")))
    (let ((dev (node:attach (declared:made "%probe-thermostat")
                            (tree:ensure (tree:root) "dev"))))
      (is (null (declared:answering (declared:named "%probe-thermostat")))
          "no backing this machine can use")
      (is (equal '("target" "mode") (node:contents dev))
          "and it still says what it would answer")
      (is (not (null (tree:at "/dev/%probe-thermostat/target")))
          "the path resolves")
      (is (eq :absent (nth-value 1 (pine:read "/dev/%probe-thermostat/target")))
          "and reading it says :ABSENT, not NIL"))))

(test a-write-to-a-declared-reading-reaches-the-backing
  "The write is a function the row was given, not a form with the value bound behind
your back. A name a macro binds is a symbol in the macro's own package and the row is
read in the caller's, so the two are spelled the same and are not the same variable --
which every device write got wrong, silently, because nothing here wrote to one."
  (with-tree
    (let ((heard :nothing))
      (declared:defdevice %probe-lamp :describes "a lamp to write to")
      (declared:defbacking %probe-lamp ()
        (level :reads 0 :writes (lambda (said) (setf heard said) t)))
      (node:attach (declared:made "%probe-lamp") (tree:ensure (tree:root) "dev"))
      (pine:write "/dev/%probe-lamp/level" 42)
      (is (eql 42 heard) "what was written reached the backing"))))

(test a-backing-that-answers-some-readings-leaves-the-rest-absent
  "Two ways of asking one machine need not answer the same questions. The one that
knows less does not take the others away, or a surface written against the fuller
backing would break on a machine with the thinner one."
  (with-tree
    (declared:defdevice %probe-radio :describes "two ways, one thinner")
    (declared:defbacking %probe-radio (:needs "no-such-fat-program")
      (station :reads "fat") (signal :reads "fat") (preset :reads "fat"))
    (declared:defbacking %probe-radio ()
      (station :reads "thin"))
    (let ((dev (node:attach (declared:made "%probe-radio")
                            (tree:ensure (tree:root) "dev"))))
      (is (equal '("station" "signal" "preset") (node:contents dev))
          "every reading either backing declares")
      (is (equal "thin" (pine:read "/dev/%probe-radio/station"))
          "the backing that stands answers what it knows")
      (is (eq :absent (nth-value 1 (pine:read "/dev/%probe-radio/signal")))
          "and what it does not know stands and says so"))))

(test reading-a-line-under-sh-does-not-run-it
  "Every line is a place whether or not one has ever been run, and a read of one
used to run it -- so /sh was a shell anything that could reach the namespace could
type into by asking it a question. A read is the one thing every way in may always
do; running something is a write."
  (booted)
  (with-tree
    (node:attach (sh:sh-node) (tree:root))
    (let ((line "echo pine-probe-a-read-does-not-run"))
      (is (null (node:contents (tree:at (format nil "/sh/~a" line))))
          "a line nothing has run says nothing")
      (is (null (sh:last-said line))
          "and asking the place about it ran nothing")
      (sh:sh line)
      (is (equal "pine-probe-a-read-does-not-run"
                 (node:contents (tree:at (format nil "/sh/~a" line))))
          "and once something has run it, the place answers for it"))))

(test a-system-takes-its-paths-with-it
  "A system puts things on the tree and takes them off by what OWNED was told as
they went up. Attached by hand instead, dropping the host left /sh -- which runs
things -- and /file, which is the whole filesystem, standing for the life of the
image with nothing that named them."
  (booted)
  (with-tree
    (system:use "host")
    (is (not (null (tree:at "/sh"))) "it put /sh up")
    (is (not (null (tree:at "/file"))) "and the filesystem")
    (is (not (null (tree:at "/sys"))) "and the machine")
    (system:drop "host")
    (is (null (tree:at "/sh")) "and /sh goes with it")
    (is (null (tree:at "/file")) "and so does the filesystem")
    (is (null (tree:at "/sys")) "and so does the machine")))
