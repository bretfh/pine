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
  (with-tree
    (setf (node:contents (tree:ensure nil "dev" "audio" "volume")) 40)
    (let ((p (path:parse "/dev/audio/volume")))
      (is (= 40 (path:contents p)))
      (setf (path:contents p) 55)
      (is (= 55 (node:contents (tree:at nil "dev/audio/volume"))))
      (is (equal "volume" (path:leaf p))))))

(test a-store-writes-through-and-comes-back
  (let ((file (merge-pathnames "pine-test-store.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (progn
           (with-tree
             (let ((s (store:open-store file)))
               (store:keeping s)
               (tree:put nil '("kept" "note") "across")
               (store:close-store s)))
           (with-tree
             (let ((s (store:open-store file)))
               (store:restore s)
               (is (equal "across" (node:contents (tree:at nil "kept/note"))))
               (store:close-store s))))
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
