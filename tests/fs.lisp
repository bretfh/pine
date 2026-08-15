(in-package :pine/test)

(def-suite* :pine/fs :in :pine)

(defun fresh-tree ()
  (pine/fs/tree:root))

(test a-node-is-a-name-a-parent-and-what-is-under-it
  (let* ((root (fresh-tree))
         (buf (pine/fs/tree:ensure root "buf" "scratch")))
    (is (equal "scratch" (pine/fs/node:name buf)))
    (is (equal "/buf/scratch" (pine/fs/node:full-name buf)))
    (is (equal "buf" (pine/fs/node:name (pine/fs/node:parent buf))))
    (is-true (pine/fs/node:leafp buf))
    (is-false (pine/fs/node:leafp (pine/fs/node:parent buf)))))

(test contents-is-the-one-word-for-what-a-node-holds
  (let* ((root (fresh-tree))
         (n (pine/fs/tree:ensure root "buf" "scratch" "text")))
    (is (null (pine/fs/node:contents n)))
    (setf (pine/fs/node:contents n) "hello")
    (is (equal "hello" (pine/fs/node:contents n)))
    (is (equal "hello" (pine/fs/node:contents (pine/fs/tree:at root "buf/scratch/text"))))))

(test a-path-is-one-string-or-many-pieces-and-means-the-same-node
  (let ((root (fresh-tree)))
    (pine/fs/tree:ensure root "a" "b" "c")
    (is (eq (pine/fs/tree:at root "a/b/c")
            (pine/fs/tree:at root "a" "b" "c")))
    (is (eq (pine/fs/tree:at root "a/b/c")
            (pine/fs/tree:at root '|a| '|b| '|c|)))
    (is (null (pine/fs/tree:at root "a/b/absent")))))

(test detaching-takes-a-node-and-what-is-under-it
  (let ((root (fresh-tree)))
    (pine/fs/tree:ensure root "a" "b" "c")
    (pine/fs/tree:erase root "a/b")
    (is (null (pine/fs/tree:at root "a/b")))
    (is (null (pine/fs/tree:at root "a/b/c")))
    (is (equal '("a") (pine/fs/tree:listing root)))))

(test a-computed-node-answers-a-function-and-answers-it-once
  (let* ((root (fresh-tree))
         (runs 0)
         (n (pine/fs/computed:computed "twice" (lambda () (incf runs) 42))))
    (pine/fs/node:attach n root)
    (is (eql 42 (pine/fs/node:contents n)))
    (is (eql 42 (pine/fs/node:contents n)))
    (is (eql 1 runs) "a computed node is worked out once and kept")))

(test a-computed-node-follows-what-it-read-with-nothing-subscribing
  "This is what makes a surface follow the tree. Nobody registers anything: the
node records what it read while it was computing."
  (let* ((root (fresh-tree))
         (width (pine/fs/tree:ensure root "win" "width"))
         (runs 0))
    (setf (pine/fs/node:contents width) 80)
    (let ((line (pine/fs/computed:computed
                 "line"
                 (lambda ()
                   (incf runs)
                   (make-string (pine/fs/node:contents width)
                                :initial-element #\-)))))
      (pine/fs/node:attach line root)
      (is (eql 80 (length (pine/fs/node:contents line))))
      (is (eql 1 runs))
      (setf (pine/fs/node:contents width) 20)
      (is (eql 20 (length (pine/fs/node:contents line))))
      (is (eql 2 runs) "the write it read made it stale"))))

(test staleness-runs-the-whole-way-up
  (let* ((root (fresh-tree))
         (a (pine/fs/tree:ensure root "a")))
    (setf (pine/fs/node:contents a) 1)
    (let* ((b (pine/fs/computed:computed "b" (lambda () (* 10 (pine/fs/node:contents a)))))
           (c (pine/fs/computed:computed "c" (lambda () (+ 1 (pine/fs/node:contents b))))))
      (pine/fs/node:attach b root)
      (pine/fs/node:attach c root)
      (is (eql 11 (pine/fs/node:contents c)))
      (setf (pine/fs/node:contents a) 5)
      (is (eql 51 (pine/fs/node:contents c))
          "a moved two nodes below what was read"))))

(test a-node-that-holds-nothing-writable-says-so
  (let* ((root (fresh-tree))
         (n (make-instance 'pine/fs/node:node :name "bare")))
    (pine/fs/node:attach n root)
    (signals error (setf (pine/fs/node:contents n) 1))))

(test mounting-a-real-directory-reads-through-to-it
  (let* ((root (fresh-tree))
         (where (merge-pathnames "pine-probe-mount/" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist where)
           (with-open-file (out (merge-pathnames "hello.txt" where)
                                :direction :output :if-exists :supersede)
             (write-string "from the disk" out))
           (ensure-directories-exist (merge-pathnames "under/" where))
           (let ((m (pine/fs/mount:mount where root "disk")))
             (is (member "hello.txt" (pine/fs/tree:listing m) :test #'equal))
             (is (member "under" (pine/fs/tree:listing m) :test #'equal))
             (is (equal "from the disk"
                        (pine/fs/node:contents (pine/fs/tree:at root "disk/hello.txt"))))
             (setf (pine/fs/node:contents (pine/fs/tree:at root "disk/hello.txt"))
                   "written back")
             (is (equal "written back"
                        (uiop:read-file-string (merge-pathnames "hello.txt" where))))
             (is (null (pine/fs/node:persistp
                        (pine/fs/tree:at root "disk/hello.txt")))
                 "what the disk already keeps is not snapshotted")))
      (uiop:delete-directory-tree where :validate t :if-does-not-exist :ignore))))

(test walking-visits-everything-under-a-node
  (let ((root (fresh-tree)))
    (pine/fs/tree:ensure root "a" "b")
    (pine/fs/tree:ensure root "a" "c")
    (pine/fs/tree:ensure root "d")
    (is (equal '("/" "/a" "/a/b" "/a/c" "/d") (pine/fs/tree:names root)))))
