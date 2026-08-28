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
