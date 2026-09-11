(in-package :pine/test)

(def-suite* :pine/fs :in :pine)

(test a-place-nobody-wrote-and-a-place-holding-nothing-are-different-questions
  "NIL is the answer to only one of them. READ says which, because a caller that
cannot tell them apart has to guess, and every one of them guessed the same way:
OR, which reads a written NIL as an absence."
  (with-tree
    (pine::write "/held" nil)
    (multiple-value-bind (value state) (pine::read "/held")
      (is (null value))
      (is (eq :file state) "somebody wrote NIL here, and that is what it holds"))
    (multiple-value-bind (value state) (pine::read "/nobody-wrote-this")
      (is (null value))
      (is (eq :absent state) "and nothing stands here at all"))
    (fs:mount (make-instance 'fs:mount) "/branch/under")
    (is (eq :dir (nth-value 1 (pine::read "/branch")))
        "a branch holds nothing by being one")
    (is (pine::standsp "/held"))
    (is (not (pine::standsp "/nobody-wrote-this")))))

(test else-answers-for-both-kinds-of-nothing
  "What a reader has to say instead is said once, where it is read, rather than as
an OR at every call site."
  (with-tree
    (pine::write "/held" nil)
    (is (eql 0 (pine::read "/held" :else 0)))
    (is (eql 0 (pine::read "/nobody-wrote-this" :else 0)))
    (pine::write "/held" 50)
    (is (eql 50 (pine::read "/held" :else 0)) "and stands aside for a real one")
    (is (eq :file (nth-value 1 (pine::read "/held" :else 0)))
        "ELSE says what to answer, not what was found")))

(test a-verb-is-a-word-in-lisp-and-not-only-on-the-shell
  "NODE:VERB could be reached only by writing a seq whose head is a keyword, which
pine write spelled and lisp had no way to say. So a config's mute button, which is
a on-click that writes T, could mute and never unmute."
  (with-tree
    (pine::write "/muted" nil)
    (pine::toggle "/muted")
    (is (eq t (pine::read "/muted")))
    (pine::toggle "/muted")
    (is (null (pine::read "/muted")) "and back again, which is what a button wants")
    (pine::include "/tags" "urgent")
    (pine::include "/tags" "later")
    (is (= 2 (d:size (pine::read "/tags"))))
    (pine::exclude "/tags" "later")
    (is (= 1 (d:size (pine::read "/tags"))))
    (pine::blend "/theme" (d:map :accent "red"))
    (pine::blend "/theme" (d:map :bg "black"))
    (is (equal "red" (d:lookup (pine::read "/theme") :accent))
        "a merge keeps what was already there")))

(test every-verb-names-a-place-the-same-way
  "WATCH took a node where the other three took a name, so the one verb a config
could not say was the one that follows something. LS was a command and not a word
at all."
  (with-tree
    (pine::write "/dev/audio/volume" 40)
    (is (equal '("audio") (pine::ls "/dev")))
    (is (null (pine::ls "/nobody-wrote-this")) "and nothing where nothing stands")
    (let ((w (pine::watch "/dev/audio/volume"
                          (lambda (n said) (declare (ignore n said))))))
      (is (eq (fs:at "/dev/audio/volume") (pine/run/watch::watches w))
          "a name reaches the same node the other three verbs reach")
      (watch:unwatch w))
    (signals fs:absent
      (pine::watch "/nobody-wrote-this"
                   (lambda (n said) (declare (ignore n said)))))))

(test watching-is-a-method-so-a-kind-can-say-how-it-is-followed
  "Three of the four verbs dispatched on the class and this one did not, so every
kind of node that could push had to build its own way round it: a device wired its
ANNOUNCES by hand somewhere else, and a place in another pine had a second path that
never met this one."
  (with-tree
    (let ((told nil))
      (defclass %loud (fs:mount) ((heard :initform nil :accessor heard)))
      (defmethod watch:watch ((n %loud) tells &key &allow-other-keys)
        (setf (heard n) tells)
        n)
      (let ((n (fs:mount (make-instance '%loud :name "loud") (fs:root))))
        (is (eq n (pine::watch "/loud" (lambda (of said)
                                         (declare (ignore of))
                                         (push said told))))
            "the class answered, and what came back is its to let go of")
        (funcall (heard n) n :moved)
        (is (equal '(:moved) told) "and telling goes where the class put it")))))

(test a-derived-node-follows-what-it-read
  (with-tree
    (let ((w (fs:mount (make-instance 'fs:value) "/window/width"))
          (runs 0))
      (setf (fs:contents w) 80)
      (let ((line (make-instance 'fs:derived :name "line" :recompute
                               (lambda ()
                                 (incf runs)
                                 (make-string (fs:contents w)
                                              :initial-element #\-)))))
        (fs:mount line (fs:root))
        (is (= 80 (length (fs:contents line))))
        (fs:contents line)
        (is (= 1 runs) "what it worked out is kept")
        (setf (fs:contents w) 20)
        (is (= 20 (length (fs:contents line)))
            "a write two levels height stirs it")
        (is (= 2 runs) "exactly once")))))

(test a-derived-node-takes-a-function-for-writing
  (with-tree
    (let ((held (list 41)))
      (let ((n (make-instance 'fs:derived :name "probe" :recompute (lambda () (first held))
                            :on-write (lambda (v) (setf (first held) (* 2 v))))))
        (fs:mount n (fs:root))
        (is (= 41 (fs:contents n)))
        (setf (fs:contents n) 10)
        (is (= 20 (fs:contents n)))))))

(test a-node-knows-where-it-is
  (with-tree
    (setf (fs:contents (fs:mount (make-instance 'fs:value) "/window/width")) 80)
    (is (equal "/window/width" (fs:full-name (fs:at "/window/width"))))
    (is (member "window" (mapcar #'fs:name (fs:children (fs:root))) :test #'equal))
    (is (null (fs:at "/window/nothing")))))

(test a-path-is-a-place
  "A path is one more thing AT and ENSURE take, not a second way to walk the tree."
  (with-tree
    (setf (fs:contents (fs:mount (make-instance 'fs:value) "/dev/audio/volume")) 40)
    (let ((p (path:path "/dev/audio/volume")))
      (is (= 40 (fs:contents (fs:at p))))
      (setf (fs:contents (fs:mount (make-instance 'fs:value) p)) 55)
      (is (= 55 (fs:contents (fs:at "/dev/audio/volume"))))
      (is (equal "volume" (path:leaf p))))))

(defun %exactly (s path)
  (sqlite:execute-to-list (pine/fs/store::db s)
                          "select path from node where path = ?" path))

(defun %paths (s like)
  (sqlite:execute-to-list (pine/fs/store::db s)
                          "select path from node where path like ?" like))

(defmacro with-store-file ((name) &body body)
  "A store file of its own, gone afterwards."
  `(let ((,name (merge-pathnames (format nil "pine-test-~36r.db" (random (expt 2 40)))
                                 (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,name)))))

(test what-was-written-comes-back-where-nothing-declares-it
  "The one thing a filesystem owes you. Nothing re-makes /notes on the second boot --
no config, no system, no code at all -- and the note is still there, because the name
is asked of the store when somebody asks for the name.

The store used to fill in only what already stood, so a value at a path nobody
declared could never come back, which is every value anybody ever wrote."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/notes/today" "it works")
        (store:close-store s)))
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (is (equal "it works" (pine::read "/notes/today"))
            "the note came back with nothing standing there to fill in")
        (is (equal '("today") (pine::ls "/notes"))
            "and the way to it was made as well as the leaf")
        (is (member "notes" (pine::ls "/") :test #'equal)
            "so it is listed from the root height")
        (store:close-store s)))))

(test a-declaration-is-not-somebody-s-data
  "A config says the same thing on every boot, so what it says is not kept. Written
height, the store put the old answer back over the config, and editing the config did
nothing for ever after."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (let ((fs:*declaring* t)) (pine::write "/wm/terminal" "alacritty"))
        (pine::write "/wm/chosen" "by hand")
        (is (equal "alacritty" (pine::read "/wm/terminal"))
            "it stands while this image runs")
        (store:close-store s)))
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (is (eq :absent (nth-value 1 (pine::read "/wm/terminal")))
            "and is gone next time, for the config to say again")
        (is (equal "by hand" (pine::read "/wm/chosen"))
            "while what somebody wrote is still there")
        (store:close-store s)))))

(test a-value-made-holding-something-is-not-a-value-somebody-wrote
  "A default is code saying something, not a person writing it height."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (fs:mount (make-instance 'fs:value :held :a-default) "/held/by-code")
        (store:close-store s)))
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (is (eq :absent (nth-value 1 (pine::read "/held/by-code")))
            "nothing kept it")
        (store:close-store s)))))

(test what-the-world-answers-for-is-never-kept
  "A device, a job, a command: the world says what they are, so the store has no
business holding an answer that was true once."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/here/mine" "kept")
        (is (equal '(("/here/mine")) (%paths s "/%"))
            "one row, and it is the one somebody wrote: ~a" (%paths s "/%"))
        (store:close-store s)))))

(test a-node-written-nil-comes-back-holding-nil
  "A node written NIL is one holding NIL, not one nobody ever wrote."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/held" nil)
        (store:close-store s)))
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (is (eq :file (nth-value 1 (pine::read "/held")))
            "it came back as a place holding NIL, not as one absent")
        (store:close-store s)))))

(test a-slot-is-kept-the-moment-it-is-written
  "Writing one has to reach the store as it happens, or what a crash costs is
everything since the image came up."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/held/state" :awake)
        (is (equal '(("/held/state")) (%paths s "/held%"))
            "written through, before any shutdown")
        (store:close-store s)))))

(test taking-a-name-off-the-tree-is-not-forgetting-what-it-held
  "A system going down takes its place off the tree. What somebody wrote into that
place is not the system's to take with it: DETACH is taking it off, ERASE-ENTRY is
the one that means it has gone."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/going/away" "kept")
        (fs:detach (fs:at "/going") "away")
        (is (equal '(("/going/away")) (%paths s "/going%"))
            "still kept, with the name off the tree")
        (is (equal "kept" (pine::read "/going/away"))
            "and asking for it again is how it comes back")
        (store:close-store s)))))

(test rm-is-the-one-that-means-it-has-gone
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/notes/today" "it works")
        (pine::write "/notes/other" "beside it")
        (fs:erase "/notes/today")
        (store:close-store s)))
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (is (eq :absent (nth-value 1 (pine::read "/notes/today"))) "gone for good")
        (is (equal "beside it" (pine::read "/notes/other")) "and only that one")
        (store:close-store s)))))

(test a-write-to-a-name-nothing-stands-at-goes-to-whatever-is-behind-it
  "Writing used to put a value in this image beside the backing, where nothing else
could ever find it. The mount above makes the name, so what is behind the mount is
what ends up holding it."
  (with-store-file (file)
    (with-tree
      (let ((s (store:open-store file)))
        (store:keeping s)
        (pine::write "/made/here" "by the mount")
        (is (equal '(("/made/here")) (%paths s "/made%"))
            "it reached the store rather than standing only in this image")
        (store:close-store s))))
  (with-tree
    (let ((where (merge-pathnames "pine-write-dir/" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (ensure-directories-exist where)
             (fs:mount where "/disk")
             (pine::write "/disk/fresh.txt" "onto the disk")
             (is (probe-file (merge-pathnames "fresh.txt" where))
                 "a name made under a directory is a file"))
        (ignore-errors (uiop:delete-directory-tree
                        where :validate (constantly t)))))))

(test a-walk-goes-all-the-way-down-and-stops-at-the-world
  (with-tree
    (pine::write "/deep/down/here" "value")
    (let ((seen nil))
      (fs:walk (fs:root) (lambda (n) (push (fs:full-name n) seen)))
      (is (member "/deep/down/here" seen :test #'equal)
          "a value three deep is reached: ~a" (reverse seen)))
    (is (not (fs:volatile-p (fs:at "/deep/down" "here")))
        "a value kept here is not live")
    (is (fs:volatile-p (make-instance 'fs:derived :name "somewhere" :live t))
        "and a place, which the world answers for, is")))

(test a-host-directory-reads-and-writes-through
  (with-tree
    (let ((where (merge-pathnames "pine-test-dir/" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (ensure-directories-exist where)
             (with-open-file (o (merge-pathnames "hello.txt" where)
                                :direction :output :if-exists :supersede)
               (write-string "from the disk" o))
             (fs:mount where "/file")
             (is (equal "from the disk"
                        (fs:contents (fs:at "/file/hello.txt"))))
             (setf (fs:contents (fs:at "/file/hello.txt")) "written back")
             (is (equal "written back"
                        (uiop:read-file-string
                         (merge-pathnames "hello.txt" where)))))
        (uiop:delete-directory-tree where :validate t
                                          :if-does-not-exist :ignore)))))

(test erasing-takes-a-node-off
  (with-tree
    (pine::write "/a/b" 1)
    (is (fs:at "/a/b"))
    (fs:erase "/a/b")
    (is (null (fs:at "/a/b")))))

(test a-place-to-erase-is-named-the-way-every-other-place-is
  "WHERE names a place the way AT does, so the name that goes may be the end of it.
Reading it as a node to walk from left the RM command taking a path apart from a
name it never had, and answering nothing while nothing went."
  (with-tree
    (pine::write "/a/b" 1)
    (fs:erase "/a/b")
    (is (null (fs:at "/a/b")) "one string, spelling the whole path")
    (pine::write "/a/b" 1)
    (is (command:run "rm" '("/a/b")))
    (is (null (fs:at "/a/b")) "which is what RM hands it")))

(test a-name-nothing-answers-for-is-not-kept
  "A child made once is made once; a child that was never made is not remembered as
nothing. The memo a walk of the children reads would have a hole in it, and a name
anybody can ask about would be a name anybody can grow it by."
  (with-tree
    (let ((p (make-instance 'fs:mount :name "empty" :names (lambda () nil)
                                 :each (lambda (name) (declare (ignore name)) nil))))
      (fs:mount p (fs:root))
      (is (null (fs:child p "nobody")))
      (is (null (d:keys (fs::dentries p))) "and nothing was kept saying so")
      (fs:mount p (fs:root))
      (is (equal "/empty" (fs:full-name p))
          "so renaming what is under it has something to rename"))))

(test a-name-means-the-same-place-wherever-it-is-said
  "There is nowhere to stand, so there is nothing for a name to be measured from.
A name is what it spells, in a config, at a prompt and on the wire alike."
  (with-tree
    (pine::write "/a/b" :at-root)
    (fs:mount (make-instance 'fs:mount) "/elsewhere")
    (is (eq :at-root (pine::read "a/b")) "with a leading / or without")
    (fs:erase "/a/b")
    (is (null (fs:at "/a/b")))))

(test nothing-is-not-a-place
  "It is what a place answers when there is none, so it cannot also be one. A miss
that flowed into another call used to read whatever the session stood on."
  (with-tree
    (signals fs:not-a-place (fs:at nil))
    (signals fs:not-a-place (fs:at nil "a" "b"))
    (signals fs:not-a-place (fs:mount (make-instance 'fs:mount) nil))
    (signals fs:not-a-place (fs:erase nil "a"))
    (signals fs:not-a-place (pine::read (fs:at "/nobody-wrote-this")))))

(test erase-takes-a-path-as-readily-as-a-name
  "ERASE names a place the way AT does, so every kind of place AT takes it takes."
  (with-tree
    (pine::write "/a/b" :hello)
    (fs:erase (pine/fs/path:path "/a/b"))
    (is (null (fs:at "/a/b")))))

(test a-write-under-a-worked-out-place-says-so
  "A place that works its children out has none to make. Attached anyway it would
sit where NODES and RESOLVE never look, and the write would be taken and not be
there to read."
  (with-tree
    (let ((p (make-instance 'fs:mount :name "p" :names (constantly nil)
                             :each (lambda (name) (declare (ignore name)) nil))))
      (fs:mount p (fs:root))
      (signals error (pine::write "/p/thing" :hello)))))

(test a-plain-branch-still-takes-a-write
  (with-tree
    (pine::write "/plain/deep/thing" :hello)
    (is (eq :hello (pine::read "/plain/deep/thing")))))

(test a-seq-that-begins-with-a-keyword-can-still-be-stored
  "A seq headed by a keyword is what writing a verb looks like, so storing one
stored the verb's argument instead. :QUOTED says this one is a value."
  (with-tree
    (pine::write "/data" nil)
    (setf (fs:contents (fs:at "/data")) (d:seq :quoted :alpha :beta))
    (let ((back (fs:contents (fs:at "/data"))))
      (is (d:seqp back))
      (is (equal '(:alpha :beta) (d:as :list back)))))
  (with-tree
    (pine::write "/flag" nil)
    (setf (fs:contents (fs:at "/flag")) (d:seq :toggle))
    (is (eq t (fs:contents (fs:at "/flag"))) "and a verb is still a verb")))

(test the-store-tells-its-own-words-from-somebody-elses
  "A map is written (:map ...), so a list that begins with :map came back a map it
never was."
  (dolist (each (list (list :map :a 1) (list :seq 1 2) (list :set 1)
                      (list :quoted 1) (list 1 (list :seq 2))))
    (let ((text (pine/fs/store::written each)))
      (is (equal each (pine/fs/store::read-back text))
          "~s came back as ~s" each (pine/fs/store::read-back text))))
  (dolist (each (list (d:map :a 1) (d:seq 1 2) (d:set 1 2) (d:map :a (d:seq 1 2))))
    (is (d:same each (pine/fs/store::read-back (pine/fs/store::written each)))
        "and a real one still round-trips")))

(test a-listener-that-breaks-does-not-break-the-write
  (with-tree
    (let ((heard nil))
      (pine::write "/x" 1)
      (setf (fs:on-commit :aaa-bad)
            (lambda (m) (declare (ignore m)) (error "listener broke")))
      (setf (fs:on-commit :zzz-good)
            (lambda (m) (declare (ignore m)) (setf heard t)))
      (unwind-protect
           (progn (finishes (setf (fs:contents (fs:at "/x")) 2))
                  (is (= 2 (fs:contents (fs:at "/x"))) "the value landed")
                  (is (not (null heard))
                      "and the other listener was still told"))
        (setf (fs:on-commit :aaa-bad) nil)
        (setf (fs:on-commit :zzz-good) nil)))))

(test a-derived-node-stops-reading-what-it-stopped-reading
  "SAW is recorded so this can be asked. Without it a node that once looked
somewhere is worked out for ever after whenever that place moves."
  (with-tree
    (let* ((a (fs:mount (make-instance 'fs:value :name "a") (fs:root)))
           (b (fs:mount (make-instance 'fs:value :name "b") (fs:root)))
           (which (list a))
           (dv (fs:mount (make-instance 'fs:derived :name "d" :recompute (lambda ()
                                               (fs:contents (first which))))
                            (fs:root))))
      (setf (fs:contents a) 1)
      (setf (fs:contents b) 2)
      (fs:contents dv)
      (setf which (list b))
      (fs:touch dv)
      (fs:contents dv)
      (is (zerop (d:size (pine/fs::dependents a))) "a is no longer read")
      (is (= 1 (d:size (pine/fs::dependents b))))
      (setf (fs:contents a) 99)
      (is (not (pine/fs::dirtyp dv)) "and moving it does not stir d")
      (setf (fs:contents b) 99)
      (is (pine/fs::dirtyp dv) "while moving what it does read still does"))))

(test two-nodes-that-read-each-other-do-not-run-the-stack-out
  (with-tree
    (let ((x (fs:mount (make-instance 'fs:value :name "x") (fs:root)))
          (y (fs:mount (make-instance 'fs:value :name "y") (fs:root))))
      (fs:depend x y)
      (fs:depend y x)
      (finishes (fs:touch x)))))

(test erasing-a-worked-out-child-leaves-nothing-behind
  "The memo is let go after the detach, not before: dropped first, the detach asks
for the child again and what is left is that second one with nothing over it."
  (with-tree
    (let ((p (make-instance 'fs:mount :name "p" :names (constantly (list "kid"))
                             :each (lambda (n) (make-instance 'fs:mount :name n)))))
      (fs:mount p (fs:root))
      (let ((before (fs:child p "kid")))
        (fs:unlink p "kid")
        (let ((after (fs:child p "kid")))
          (is (not (eq before after)) "what comes back is a fresh one")
          (is (eq p (fs:parent after)) "standing where it should")
          (is (equal "/p/kid" (fs:full-name after))))))))

(test a-working-out-that-throws-does-not-unwind-into-whoever-read
  "One surface breaking must not blank the frame. The fault is kept, the node
answers nothing, and what stands beside it is still worked out."
  (with-tree
    (fault:forget-faults)
    (let ((n (fs:mount (make-instance 'fs:value) "/probe-src"))
          (broken (make-instance 'fs:derived :name "broken" :recompute (lambda () (error "on purpose")))))
      (setf (fs:contents n) "still here")
      (fs:mount broken (fs:root))
      (let ((beside (make-instance 'fs:derived :name "beside" :recompute (lambda () (fs:contents n)))))
        (fs:mount beside (fs:root))
        (is (null (fs:contents broken))
            "it answers nothing rather than unwinding into the reader")
        (is (equal "still here" (fs:contents beside))
            "and what is beside it still answers")
        (is (find-if (lambda (f)
                       (search "on purpose"
                               (princ-to-string (fault:condition-of f))))
                     (fault:faults))
            "the fault is kept rather than swallowed")))))

(test a-node-that-threw-is-asked-again
  "It puts nothing height, so it is stale. What threw once because the world was
not ready answers the next time somebody asks, with nothing having stirred it."
  (with-tree
    (let ((broken (cons t nil))
          (runs 0))
      (let ((n (make-instance 'fs:derived :name "probe" :recompute
                            (lambda ()
                              (incf runs)
                              (when (car broken) (error "not yet"))
                              :ready))))
        (fs:mount n (fs:root))
        (is (null (fs:contents n)))
        (is (= 1 runs))
        (setf (car broken) nil)
        (is (eq :ready (fs:contents n)) "asked again")
        (is (= 2 runs) "and only once more")))))

(test nobody-waits-for-ever-on-somebody-elses-working-out
  "A READS is somebody else's code and it talks to the world. One that never
answers holds the claim while it runs, and without a deadline every reader of
that node waits behind it for the life of the image."
  (with-tree
    (let ((started (bordeaux-threads:make-semaphore))
          (go-on (bordeaux-threads:make-semaphore)))
      (let ((wedged (make-instance 'fs:derived
                     :name "wedged" :recompute
                     (lambda ()
                       (bordeaux-threads:signal-semaphore started)
                       (bordeaux-threads:wait-on-semaphore go-on :timeout 30)
                       :answered))))
        (fs:mount wedged (fs:root))
        (let ((holder (actors:blocking "wedged"
                                       (lambda () (fs:contents wedged)))))
          (is (bordeaux-threads:wait-on-semaphore started :timeout 5)
              "the other thread has the claim")
          (let ((fs:*retry-seconds* 1/10) (fs:*give-up-seconds* 1))
            (is (null (fs:contents wedged))
                "we give up rather than wait behind it"))
          (bordeaux-threads:signal-semaphore go-on)
          (actors:joined holder))))))

(test attach-lets-go-of-what-it-replaces
  "A node taken out of the tree and left in the reader sets of what it read is
worked out for ever after, every time any of that moves. DETACH is written to stop
that; ATTACH over a name is the same thing spelled the other way round, and it did
not. Declaring a surface twice leaked the first one and it went on being worked
out from every device it had ever read."
  (with-tree
    (let* ((r (fs:mount (make-instance 'fs:mount :name "r") (fs:root)))
           (src (fs:mount (make-instance 'fs:value :name "src") (fs:root)))
           (had (make-instance 'fs:derived :name "d" :recompute (lambda () (fs:contents src))))
           (fresh (make-instance 'fs:derived :name "d" :recompute (lambda () :fresh))))
      (setf (fs:contents src) 1)
      (fs:mount had r)
      (fs:contents had)
      (is (d:contains (fs::dependents src) had) "it read SRC")
      (fs:mount fresh r)
      (is (not (d:contains (fs::dependents src) had))
          "and it is out of SRC's readers once something stands in its place")
      (is (null (fs:parent had)) "and off the tree")
      (is (eq fresh (fs:child r "d")))
      (is (equal (list fresh) (fs:children r)) "listed once, not twice"))))

(test a-node-given-up-on-does-not-stand
  "The version a reading is checked against is exact, so a node that was given up
on and has not been worked out again has no answer to check. Saying it stood said
a node has what it would have if anybody asked -- and the two numberings met,
because MARK answers out of the version slot once there is no value to answer out
of."
  (with-tree
    (let* ((src (fs:mount (make-instance 'fs:value :name "src") (fs:root)))
           (d (make-instance 'fs:derived :name "d" :recompute (lambda () (fs:contents src)))))
      (setf (fs:contents src) 1)
      (fs:mount d (fs:root))
      (fs:contents d)
      (let ((at (fs::current-mtime d)))
        (is (fs::currentp d at) "it stands where it was worked out")
        (setf (fs:contents src) 2)
        (is (not (fs::currentp d at))
            "and does not once what it read has moved")))))

(test listing-a-branch-is-reading-it
  "What a branch holds is what is under it. Worked out of a listing and never told,
a surface over /proc showed what was running when it was first drawn, and a face
written at /face/keyword was one nothing was ever told about."
  (with-tree
    (let* ((r (fs:mount (make-instance 'fs:mount :name "r") (fs:root)))
           (runs 0)
           (n (make-instance 'fs:derived :name "n" :recompute (lambda () (incf runs) (length (fs:children r))))))
      (fs:mount n (fs:root))
      (is (eql 0 (fs:contents n)))
      (is (eql 1 runs))
      (is (eql 0 (fs:contents n)) "and is not worked out again for nothing")
      (is (eql 1 runs))
      (fs:mount (make-instance 'fs:value :name "one") r)
      (is (eql 1 (fs:contents n)) "attaching one works it out again")
      (fs:unlink r "one")
      (is (eql 0 (fs:contents n)) "and so does taking one off"))))

(test a-path-nothing-stands-at-is-still-a-reading
  "A read that answers nothing is a read of the place it looked. Without it, asking
before anything is there is a question nothing can ever answer again: a config
reading a device the host system has not put up yet never heard it arrive."
  (with-tree
    (let ((n (make-instance 'fs:derived :name "waiting" :recompute (lambda () (fs:at "/later/here")))))
      (fs:mount n (fs:root))
      (is (null (fs:contents n)) "nothing stands there yet")
      (setf (fs:contents (fs:mount (make-instance 'fs:value) "/later/here")) :arrived)
      (is (eq :arrived (fs:contents (fs:contents n)))
          "and the reader hears when it does"))))

(test what-the-store-keeps-comes-back-as-a-value-and-not-as-an-instruction
  "A seq beginning with a keyword is an instruction to VERB when it is written,
which is what lets a shell say (:toggle). Put back that way, /tags holding
[:urgent] came out of the store as an instruction to CONJ and what was kept was
lost on the way in. A name a node may be called is the other half: per cent and
underscore are what a LIKE pattern is written in, so erasing /a_b took /axb with
it."
  (let ((file (merge-pathnames "pine-test-values.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (progn
           (with-tree
             (let ((s (store:open-store file)))
               (store:keeping s)
               (pine::write "/tags" (d:seq :urgent :later))
               (pine::write "/a_b" "under a name a pattern is written in")
               (pine::write "/axb" "beside it")
               (store:close-store s)))
           (with-tree
             (let ((s (store:open-store file)))
               (store:keeping s)
               (is (d:same (d:seq :urgent :later) (fs:contents (fs:at "/tags")))
                   "what was kept is what came back")
               (fs:store-delete s "/a_b")
               (is (null (%exactly s "/a_b")) "the one named went")
               (is (%exactly s "/axb")
                   "and the one that only matched a pattern did not")
               (store:close-store s))))
      (ignore-errors (delete-file file)))))

(test a-seq-is-built-by-appending-whatever-is-in-it
  "Told to decide by what the thing looks like, (with (seq) 5) put NIL at index
five and padded the four before it: a seq of numbers was one WITH could not build,
and INCLUDE on one quietly wrecked it. Asked of what was handed over instead, the
way the NULL method already asked."
  (is (equal '(5) (d:as :list (d:with (d:seq) 5))))
  (is (equal '(1 2 5) (d:as :list (d:with (d:seq 1 2) 5))))
  (is (equal '(1 :x) (d:as :list (d:with (d:seq 1 2) 1 :x)))
      "and given a value it is still an index")
  (with-tree
    (pine::write "/nums" (d:seq))
    (pine::include "/nums" 3)
    (pine::include "/nums" 4)
    (is (equal '(3 4) (d:as :list (pine::read "/nums"))))))

(defclass %will-not-print () ())

(defmethod print-object ((it %will-not-print) stream)
  (declare (ignore stream))
  (error "this one will not print"))

(test a-file-is-written-beside-itself-and-then-over-itself
  "The thing being written over is the only copy. SUPERSEDE opens the file, says
nothing from that moment until it closes, and on a write that did not finish
deletes what it was making without putting back what was there -- measured, a
write that threw left no file at all."
  (let ((file (merge-pathnames "pine-test-atomic.txt" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (with-tree
           (fs:mount (uiop:temporary-directory) "/tmp")
           (with-open-file (o file :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "what was there before" o))
           (let ((it (fs:at "/tmp/pine-test-atomic.txt")))
             (is (equal "what was there before" (fs:contents it)))
             (handler-case (setf (fs:contents it) (make-instance '%will-not-print))
               (error () nil))
             (is (probe-file file) "a write that would not go left the file there")
             (is (equal "what was there before" (fs:contents it))
                 "holding exactly what it held before")
             (setf (fs:contents it) "the whole of something else")
             (is (equal "the whole of something else" (fs:contents it))
                 "and a write that went through is all of it")))
      (ignore-errors (delete-file file)))))

(test a-read-of-something-slow-does-not-wait-for-it
  "A working-out that talks to the world is a wait, and a read is a frame or a
keystroke. The first one pays for finding that out; every one after it is handed
what stands, and WATCH is how anybody hears the new answer."
  (booted)
  (with-tree
    (let* ((runs 0)
           (n (fs:mount
               (make-instance 'fs:derived :name "slow"
                              :recompute (lambda () (incf runs) (sleep 0.3) runs))
               (fs:root))))
      (is (eql 1 (fs:contents n)) "the first read waits, and is what says it is slow")
      (is (fs::waits-of n) "and the node knows it now")
      (fs:touch n)
      (let ((at (get-internal-real-time)))
        (is (eql 1 (fs:contents n)) "a stale read hands back what stood")
        (is (fs:pendingp n) "and says it is being worked out")
        (is (< (- (get-internal-real-time) at)
               (* 0.1 internal-time-units-per-second))
            "without waiting for it"))
      (is (until (lambda () (eql 2 (fs:contents n))) :seconds 5)
          "and the new answer lands on its own"))))

(test a-read-that-asked-to-wait-waits
  "The old behaviour, asked for rather than had. :AWAIT is the whole of it."
  (booted)
  (with-tree
    (let ((n (fs:mount
              (make-instance 'fs:derived :name "slow"
                             :recompute (lambda () (sleep 0.3) :answered))
              (fs:root))))
      (setf (fs::waits-of n) t)
      (is (eq :answered (pine::read "/slow" :await 5))
          "waited for, because it was asked to be")
      (is (not (nth-value 2 (pine::read "/slow")))
          "and it stands afterwards"))))

(test nothing-worked-out-yet-is-told-from-holding-nothing
  "A node nobody has worked out answers NIL, and a node holding NIL answers NIL.
Those are not the same news, and every caller guessed."
  (booted)
  (with-tree
    (let ((n (fs:mount
              (make-instance 'fs:derived :name "cold"
                             :waits t :recompute (lambda () nil))
              (fs:root))))
      (declare (ignore n))
      (multiple-value-bind (value kind pending) (pine::read "/cold")
        (is (null value) "nothing, because nothing has worked it out")
        (is (eq :dev kind) "it is answered from outside")
        (is (not (null pending)) "and it says that is what the nothing is"))
      (is (until (lambda () (not (nth-value 2 (pine::read "/cold"))))
                 :seconds 5)
          "and once it has been worked out it holds what it holds")
      (multiple-value-bind (value kind pending) (pine::read "/cold")
        (is (null value) "which is NIL")
        (is (eq :dev kind))
        (is (not pending) "said as a value this time, and not as an absence")))))

(test a-name-says-what-it-takes-and-refuses-the-rest
  "What a place holds it already answered; what a write must be it never did, so
every wrapper clamped its own argument after the tree had taken it."
  (with-tree
    (fs:mount (make-instance 'fs:value :name "volume" :takes '(:number 0 100))
              (fs:root))
    (pine::write "/volume" 40)
    (is (eql 40 (pine::read "/volume")))
    (signals fs:not-taken (pine::write "/volume" 400))
    (signals fs:not-taken (pine::write "/volume" "loud"))
    (is (eql 40 (pine::read "/volume")) "and what it took last still stands")))

(test what-a-name-takes-is-answered-like-anything-else-about-it
  (with-tree
    (fs:mount (make-instance 'fs:value :name "muted" :takes :flag) (fs:root))
    (is (equal :flag (getf (pine::describe "/muted") :takes)))
    (pine::write "/muted" t)
    (pine::toggle "/muted")
    (is (null (pine::read "/muted")) "a verb goes through the same gate")
    (signals fs:not-taken (pine::write "/muted" 7))))

(test a-name-that-said-nothing-about-it-takes-anything
  "Saying what a name is for is a thing a domain does; a name that has not said so
is not thereby closed."
  (with-tree
    (pine::write "/anything" 1)
    (pine::write "/anything" "words")
    (is (equal "words" (pine::read "/anything")))))

(test a-recompute-that-lands-on-the-same-value-does-not-move-its-version
  "Raising the version on every recompute is what makes one leaf write repaint
everything downstream of it."
  (with-tree
    (pine::write "/n" 1)
    (fs:mount (make-instance 'fs:derived :name "odd"
                             :recompute (lambda () (oddp (pine::read "/n"))))
              (fs:root))
    (let ((n (fs:at "/odd")))
      (is (eq t (fs:contents n)))
      (let ((at (fs::current-mtime n)))
        (pine::write "/n" 3)
        (is (eq t (fs:contents n)) "the same answer")
        (is (eql at (fs::current-mtime n))
            "standing at the version it stood at, so what read it is not stirred")))))

(test ten-readers-of-one-node-cause-one-working-out
  "A recompute talks to the world. Ten readers of one node in one frame is ten
shell-outs without the claim."
  (with-tree
    (let ((ran (cons 0 nil))
          (go-on (bordeaux-threads:make-semaphore)))
      (let ((n (make-instance 'fs:derived
                              :name "slow"
                              :recompute
                              (lambda ()
                                (sb-ext:atomic-update (car ran) (lambda (o) (1+ o)))
                                (bordeaux-threads:wait-on-semaphore go-on :timeout 10)
                                :answered))))
        (fs:mount n (fs:root))
        (let ((readers (loop :repeat 10
                             :collect (actors:blocking
                                       "reader" (lambda () (fs:contents n))))))
          (sleep 1/5)
          (dotimes (i 12) (bordeaux-threads:signal-semaphore go-on))
          (mapc #'actors:joined readers))
        (is (eql 1 (car ran)) "one working-out for ten readers")))))

(test a-working-out-whose-source-moved-under-it-is-given-up
  "Half one state and half another is not an answer. What it read is checked again
after it lands, and a value read before a write and stored after one is thrown."
  (with-tree
    (let ((ran (cons 0 nil))
          (started (bordeaux-threads:make-semaphore))
          (go-on (bordeaux-threads:make-semaphore)))
      (pine::write "/a" 1)
      (let ((n (make-instance 'fs:derived
                              :name "twice"
                              :recompute
                              (lambda ()
                                (let ((v (pine::read "/a")))
                                  (when (eql 1 (sb-ext:atomic-update
                                                (car ran) (lambda (o) (1+ o))))
                                    (bordeaux-threads:signal-semaphore started)
                                    (bordeaux-threads:wait-on-semaphore go-on :timeout 10))
                                  v)))))
        (fs:mount n (fs:root))
        (let ((reader (actors:blocking "reader" (lambda () (fs:contents n)))))
          (is (bordeaux-threads:wait-on-semaphore started :timeout 10)
              "it is inside the working-out")
          (pine::write "/a" 2)
          (bordeaux-threads:signal-semaphore go-on)
          (actors:joined reader))
        (is (eql 2 (fs:contents n)) "what it holds is what its source says now")
        (is (< 1 (car ran)) "because the first working-out was given up")))))
