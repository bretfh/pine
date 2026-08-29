(in-package :pine/test)

(def-suite* :pine/fs :in :pine)

(test a-derived-node-follows-what-it-read
  (with-tree
    (let ((w (tree:ensure nil "window" "width"))
          (runs 0))
      (setf (node:contents w) 80)
      (let ((line (node:derive "line"
                               (lambda ()
                                 (incf runs)
                                 (make-string (node:contents w)
                                              :initial-element #\-)))))
        (node:attach line (tree:root))
        (is (= 80 (length (node:contents line))))
        (node:contents line)
        (is (= 1 runs) "what it worked out is kept")
        (setf (node:contents w) 20)
        (is (= 20 (length (node:contents line)))
            "a write two levels down stirs it")
        (is (= 2 runs) "exactly once")))))

(test a-derived-node-takes-a-function-for-writing
  (with-tree
    (let ((held (list 41)))
      (let ((n (node:derive "probe" (lambda () (first held))
                            :writes (lambda (v) (setf (first held) (* 2 v))))))
        (node:attach n (tree:root))
        (is (= 41 (node:contents n)))
        (setf (node:contents n) 10)
        (is (= 20 (node:contents n)))))))

(test a-node-knows-where-it-is
  (with-tree
    (setf (node:contents (tree:ensure nil "window" "width")) 80)
    (is (equal "/window/width" (node:full-name (tree:at nil "window/width"))))
    (is (member "window" (tree:listing (tree:root)) :test #'equal))
    (is (null (tree:at nil "window/nothing")))))

(test a-path-is-a-place
  "A path is one more thing AT and ENSURE take, not a second way to walk the tree."
  (with-tree
    (setf (node:contents (tree:ensure nil "dev" "audio" "volume")) 40)
    (let ((p (path:path "/dev/audio/volume")))
      (is (= 40 (node:contents (tree:at p))))
      (setf (node:contents (tree:ensure p)) 55)
      (is (= 55 (node:contents (tree:at nil "dev/audio/volume"))))
      (is (equal "volume" (path:leaf p))))))

(defun %paths (s like)
  (sqlite:execute-to-list (pine/fs/store::db s)
                          "select path from node where path like ?" like))

(test a-store-writes-through-and-comes-back
  "Loading the code builds the shape and the store only fills it in. A path with
nothing standing at it is left in the store, not conjured as a plain value: what
it stood for is what knew how to read it."
  (let ((file (merge-pathnames "pine-test-store.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (progn
           (with-tree
             (let ((s (store:open-store file)))
               (store:keeping s)
               (tree:put nil '("kept" "note") "across")
               (tree:put nil '("gone" "away") "orphan")
               (store:close-store s)))
           (with-tree
             (let ((s (store:open-store file)))
               (tree:ensure nil "kept" "note")
               (is (eql 1 (store:restore s))
                   "one node stood there to be filled in")
               (is (equal "across" (node:contents (tree:at nil "kept/note"))))
               (is (null (tree:at nil "gone/away"))
                   "and nothing was conjured where nothing stands")
               (is (equal '("/gone/away") (store:stale s))
                   "which the store can say")
               (store:close-store s))))
      (ignore-errors (delete-file file)))))

(test a-slot-is-kept-the-moment-it-is-written
  "A slot says it is saved. Writing one has to reach the store as it happens, or
what a crash costs is everything since the image came up."
  (let ((file (merge-pathnames "pine-slot-store.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (with-tree
           (let ((s (store:open-store file))
                 (thing (make-instance 'job:thread :name "held" :restarts nil
                                                   :thunk (lambda () nil))))
             (store:keeping s)
             (node:attach thing (tree:root))
             (node:slots thing thing "state" 'job:state)
             (setf (node:contents (tree:at nil "held/state")) :awake)
             (is (equal '(("/held/state")) (%paths s "/held%"))
                 "written through, before any shutdown")
             (store:close-store s)))
      (ignore-errors (delete-file file)))))

(test a-walk-goes-all-the-way-down-and-stops-at-the-world
  "What the snapshot is written on. Every store test drives the write-through, so
a walk that stopped at the root would take all of them with it and none would
say so."
  (with-tree
    (tree:put nil '("deep" "down" "here") "value")
    (let ((seen nil))
      (tree:walk (tree:root) (lambda (n) (push (node:full-name n) seen)))
      (is (member "/deep/down/here" seen :test #'equal)
          "a value three deep is reached: ~a" (reverse seen)))
    (is (not (node:livep (tree:at nil "deep" "down" "here")))
        "a value kept here is not live")
    (is (node:livep (node:place "somewhere"))
        "and a place, which the world answers for, is")))

(test a-snapshot-writes-what-the-walk-reached
  (let ((file (merge-pathnames "pine-walk-store.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (with-tree
           (let ((s (store:open-store file)))
             (tree:put nil '("walked" "into" "it") "kept")
             (is (plusp (store:snapshot s)) "the snapshot found something")
             (is (equal '(("/walked/into/it")) (%paths s "/walked%")))
             (store:close-store s)))
      (ignore-errors (delete-file file)))))

(test a-snapshot-does-not-take-anything-out
  "The snapshot is belt and braces over the write-through. A system stopping takes
its nodes off the tree, and that is not a reason to forget what they held."
  (let ((file (merge-pathnames "pine-snap-store.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (with-tree
           (let ((s (store:open-store file)))
             (store:keeping s)
             (tree:put nil '("going" "away") "kept")
             (setf (commit:on-forget :store) nil)
             (tree:erase nil "going")
             (store:snapshot s)
             (is (equal '(("/going/away")) (%paths s "/going%"))
                 "still there after a snapshot with it off the tree")
             (store:close-store s)))
      (ignore-errors (delete-file file)))))

(test a-host-directory-reads-and-writes-through
  (with-tree
    (let ((where (merge-pathnames "pine-test-dir/" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (ensure-directories-exist where)
             (with-open-file (o (merge-pathnames "hello.txt" where)
                                :direction :output :if-exists :supersede)
               (write-string "from the disk" o))
             (mount:mount where (tree:root) "file")
             (is (equal "from the disk"
                        (node:contents (tree:at nil "file/hello.txt"))))
             (setf (node:contents (tree:at nil "file/hello.txt")) "written back")
             (is (equal "written back"
                        (uiop:read-file-string
                         (merge-pathnames "hello.txt" where)))))
        (uiop:delete-directory-tree where :validate t
                                          :if-does-not-exist :ignore)))))

(test erasing-takes-a-node-off
  (with-tree
    (tree:put nil '("a" "b") 1)
    (is (tree:at nil "a/b"))
    (tree:erase nil "a/b")
    (is (null (tree:at nil "a/b")))))

(test a-place-to-erase-is-named-the-way-every-other-place-is
  "WHERE names a place the way AT does, so the name that goes may be the end of it.
Reading it as a node to walk from left the RM command taking a path apart from a
name it never had, and answering nothing while nothing went."
  (with-tree
    (tree:put nil '("a" "b") 1)
    (tree:erase "/a/b")
    (is (null (tree:at nil "a/b")) "one string, spelling the whole path")
    (tree:put nil '("a" "b") 1)
    (is (command:run "rm" '("/a/b")))
    (is (null (tree:at nil "a/b")) "which is what RM hands it")))

(test a-name-nothing-answers-for-is-not-kept
  "A child made once is made once; a child that was never made is not remembered as
nothing. The memo a walk of the children reads would have a hole in it, and a name
anybody can ask about would be a name anybody can grow it by."
  (with-tree
    (let ((p (node:place "empty" :names (lambda () nil)
                                 :each (lambda (name) (declare (ignore name)) nil))))
      (node:attach p (tree:root))
      (is (null (node:resolve p "nobody")))
      (is (null (d:keys (d:all (node:memo p)))) "and nothing was kept saying so")
      (node:attach p (tree:root))
      (is (equal "/empty" (node:full-name p))
          "so renaming what is under it has something to rename"))))

(test erase-measures-an-absolute-name-from-the-root
  "AT does, and ERASE names a place the way AT does. Measured from where the
session stands, pine rm /a/b takes off whatever is at a/b under it instead."
  (with-tree
    (pine::write "/a/b" :at-root)
    (tree:ensure "/elsewhere")
    (let ((tree:*here* (tree:at "/elsewhere")))
      (pine::write "a/b" :under-elsewhere)
      (tree:erase "/a/b")
      (is (null (tree:at "/a/b")) "the one that was named")
      (is (not (null (tree:at "/elsewhere/a/b"))) "and not the one underfoot"))))

(test erase-still-measures-a-relative-name-from-here
  (with-tree
    (tree:ensure "/elsewhere")
    (let ((tree:*here* (tree:at "/elsewhere")))
      (pine::write "a/b" :under-elsewhere)
      (tree:erase "a/b")
      (is (null (tree:at "/elsewhere/a/b"))))))

(test a-write-under-a-worked-out-place-says-so
  "A place that works its children out has none to make. Attached anyway it would
sit where NODES and RESOLVE never look, and the write would be taken and not be
there to read."
  (with-tree
    (let ((p (node:place "p" :names (constantly nil)
                             :each (lambda (name) (declare (ignore name)) nil))))
      (node:attach p (tree:root))
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
    (setf (node:contents (tree:at "/data")) (d:seq :quoted :alpha :beta))
    (let ((back (node:contents (tree:at "/data"))))
      (is (d:seqp back))
      (is (equal '(:alpha :beta) (d:as :list back)))))
  (with-tree
    (pine::write "/flag" nil)
    (setf (node:contents (tree:at "/flag")) (d:seq :toggle))
    (is (eq t (node:contents (tree:at "/flag"))) "and a verb is still a verb")))

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
      (setf (commit:on-commit :aaa-bad)
            (lambda (m) (declare (ignore m)) (error "listener broke")))
      (setf (commit:on-commit :zzz-good)
            (lambda (m) (declare (ignore m)) (setf heard t)))
      (unwind-protect
           (progn (finishes (setf (node:contents (tree:at "/x")) 2))
                  (is (= 2 (node:contents (tree:at "/x"))) "the value landed")
                  (is (not (null heard))
                      "and the other listener was still told"))
        (setf (commit:on-commit :aaa-bad) nil)
        (setf (commit:on-commit :zzz-good) nil)))))

(test a-derived-node-stops-reading-what-it-stopped-reading
  "SAW is recorded so this can be asked. Without it a node that once looked
somewhere is worked out for ever after whenever that place moves."
  (with-tree
    (let* ((a (node:attach (node:make "a") (tree:root)))
           (b (node:attach (node:make "b") (tree:root)))
           (which (list a))
           (dv (node:attach (node:derive "d" (lambda ()
                                               (node:contents (first which))))
                            (tree:root))))
      (setf (node:contents a) 1)
      (setf (node:contents b) 2)
      (node:contents dv)
      (setf which (list b))
      (node:stir dv)
      (node:contents dv)
      (is (zerop (d:size (pine/fs/node::readers a))) "a is no longer read")
      (is (= 1 (d:size (pine/fs/node::readers b))))
      (setf (node:contents a) 99)
      (is (not (pine/fs/node::stalep dv)) "and moving it does not stir d")
      (setf (node:contents b) 99)
      (is (pine/fs/node::stalep dv) "while moving what it does read still does"))))

(test two-nodes-that-read-each-other-do-not-run-the-stack-out
  (with-tree
    (let ((x (node:attach (node:make "x") (tree:root)))
          (y (node:attach (node:make "y") (tree:root))))
      (node:depend x y)
      (node:depend y x)
      (finishes (node:stir x)))))

(test erasing-a-worked-out-child-leaves-nothing-behind
  "The memo is let go after the detach, not before: dropped first, the detach asks
for the child again and what is left is that second one with nothing over it."
  (with-tree
    (let ((p (node:place "p" :names (constantly (list "kid"))
                             :each (lambda (n) (node:place n)))))
      (node:attach p (tree:root))
      (let ((before (node:resolve p "kid")))
        (node:erase-child p "kid")
        (let ((after (node:resolve p "kid")))
          (is (not (eq before after)) "what comes back is a fresh one")
          (is (eq p (node:over after)) "standing where it should")
          (is (equal "/p/kid" (node:full-name after))))))))

(test a-working-out-that-throws-does-not-unwind-into-whoever-read
  "One surface breaking must not blank the frame. The fault is kept, the node
answers nothing, and what stands beside it is still worked out."
  (with-tree
    (fault:forget-faults)
    (let ((n (tree:ensure nil "probe-src"))
          (broken (node:derive "broken" (lambda () (error "on purpose")))))
      (setf (node:contents n) "still here")
      (node:attach broken (tree:root))
      (let ((beside (node:derive "beside" (lambda () (node:contents n)))))
        (node:attach beside (tree:root))
        (is (null (node:contents broken))
            "it answers nothing rather than unwinding into the reader")
        (is (equal "still here" (node:contents beside))
            "and what is beside it still answers")
        (is (find-if (lambda (f)
                       (search "on purpose"
                               (princ-to-string (fault:condition-of f))))
                     (fault:faults))
            "the fault is kept rather than swallowed")))))

(test a-node-that-threw-is-asked-again
  "It puts nothing down, so it is stale. What threw once because the world was
not ready answers the next time somebody asks, with nothing having stirred it."
  (with-tree
    (let ((broken (cons t nil))
          (runs 0))
      (let ((n (node:derive "probe"
                            (lambda ()
                              (incf runs)
                              (when (car broken) (error "not yet"))
                              :ready))))
        (node:attach n (tree:root))
        (is (null (node:contents n)))
        (is (= 1 runs))
        (setf (car broken) nil)
        (is (eq :ready (node:contents n)) "asked again")
        (is (= 2 runs) "and only once more")))))

(test nobody-waits-for-ever-on-somebody-elses-working-out
  "A READS is somebody else's code and it talks to the world. One that never
answers holds the claim while it runs, and without a deadline every reader of
that node waits behind it for the life of the image."
  (with-tree
    (let ((started (bordeaux-threads:make-semaphore))
          (go-on (bordeaux-threads:make-semaphore)))
      (let ((wedged (node:derive
                     "wedged"
                     (lambda ()
                       (bordeaux-threads:signal-semaphore started)
                       (bordeaux-threads:wait-on-semaphore go-on :timeout 30)
                       :answered))))
        (node:attach wedged (tree:root))
        (let ((holder (actors:blocking "wedged"
                                       (lambda () (node:contents wedged)))))
          (is (bordeaux-threads:wait-on-semaphore started :timeout 5)
              "the other thread has the claim")
          (let ((node:*waited* 1/10) (node:*waiting-on* 1))
            (is (null (node:contents wedged))
                "we give up rather than wait behind it"))
          (bordeaux-threads:signal-semaphore go-on)
          (actors:joined holder))))))
