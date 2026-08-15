(in-package :pine/test)

(def-suite* :pine/world :in :pine)

(defmacro with-store ((var) &body body)
  `(let* ((file (merge-pathnames (format nil "pine-probe-~d.db" (random 100000))
                                 (uiop:temporary-directory)))
          (,var (pine/world/store:open-store (namestring file))))
     (unwind-protect (progn ,@body)
       (pine/world/store:close-store ,var)
       (ignore-errors (delete-file file)))))

(test a-node-has-an-identity-that-outlives-what-it-holds
  (let* ((w (pine/world/world:make-world))
         (n (pine/world/world:ensure w "buffer" "scratch")))
    (let ((id (pine/world/world:identity-of w n)))
      (is (integerp id))
      (setf (pine/fs/node:contents n) "one")
      (setf (pine/fs/node:contents n) "two")
      (is (eql id (pine/world/world:identity-of w n)))
      (is (eq n (pine/world/world:node-for-id w id))))))

(test what-persists-is-a-property-of-the-class
  (let* ((w (pine/world/world:make-world))
         (held (pine/world/world:ensure w "buffer" "scratch" "text"))
         (worked-out (pine/fs/computed:computed "sum" (lambda () 42))))
    (pine/fs/node:attach worked-out (pine/world/world:root w))
    (is-true (pine/fs/node:persistp held))
    (is-false (pine/fs/node:persistp worked-out)
              "what is worked out comes back by being worked out again")))

(test a-snapshot-comes-back-with-what-was-in-it
  (with-store (s)
    (let ((w (pine/world/world:make-world)))
      (pine/world/world:place w '("buffer" "scratch" "text") "hello")
      (pine/world/world:place w '("window" "width") 80)
      (pine/world/world:place w '("mode" "lisp" "indent") 2)
      (is (eql 3 (pine/world/store:snapshot w s))))
    (let ((back (pine/world/world:make-world)))
      (is (eql 3 (pine/world/store:restore back s)))
      (is (equal "hello" (pine/fs/node:contents
                          (pine/world/world:at back "buffer/scratch/text"))))
      (is (eql 80 (pine/fs/node:contents (pine/world/world:at back "window/width"))))
      (is (eql 2 (pine/fs/node:contents
                  (pine/world/world:at back "mode/lisp/indent")))))))

(test a-snapshot-keeps-what-was-defined-while-it-was-running
  "Modifying itself while it runs, and keeping it: a command made at the repl is
a node like any other, so it is in the snapshot."
  (with-store (s)
    (let ((w (pine/world/world:make-world)))
      (pine/world/world:place w '("cmd" "probe-live") "made at the repl")
      (pine/world/store:snapshot w s)
      (let ((back (pine/world/world:make-world)))
        (pine/world/store:restore back s)
        (is (equal "made at the repl"
                   (pine/fs/node:contents
                    (pine/world/world:at back "cmd/probe-live"))))))))

(test what-cannot-be-written-down-is-left-out-rather-than-signalling
  (with-store (s)
    (let ((w (pine/world/world:make-world)))
      (pine/world/world:place w '("ok") 1)
      (pine/world/world:place w '("live") (lambda () :a-closure))
      (is (eql 1 (pine/world/store:snapshot w s))))))

(test erasing-a-node-takes-what-it-held-out-of-the-store
  "A path erased here that came back on the next start would be a node nobody
asked for, and no way to be rid of it short of the file."
  (with-store (s)
    (let ((w (pine/world/world:make-world)))
      (unwind-protect
           (progn
             (pine/world/store:keeping s)
             (pine/world/world:place w '("kept" "note") "here")
             (is (= 1 (length (sqlite:execute-to-list
                               (pine/world/store::db s)
                               "select path from node where path = ?"
                               "/kept/note")))
                 "it is written through as it is written")
             (pine/fs/tree:erase (pine/world/world:root w) "kept" "note")
             (is (null (sqlite:execute-to-list
                        (pine/world/store::db s)
                        "select path from node where path = ?"
                        "/kept/note"))
                 "and erasing it takes the row with it"))
        (pine/world/store:keeping nil)))))

(test erasing-a-node-takes-what-stood-under-it-too
  (with-store (s)
    (let ((w (pine/world/world:make-world)))
      (unwind-protect
           (progn
             (pine/world/store:keeping s)
             (pine/world/world:place w '("kept" "a" "one") 1)
             (pine/world/world:place w '("kept" "a" "two") 2)
             (pine/world/world:place w '("kept" "b") 3)
             (pine/fs/tree:erase (pine/world/world:root w) "kept" "a")
             (is (= 1 (length (sqlite:execute-to-list
                               (pine/world/store::db s)
                               "select path from node")))
                 "what was under it went with it, and what was beside it stayed"))
        (pine/world/store:keeping nil)))))

(test what-comes-back-lands-on-the-node-that-was-already-there
  "The store fills leaves in; the shape is each install's to build.
/buffer/scratch/point-line is a slot on the scratch buffer, so what comes back
moves the buffer rather than standing beside it as a value nobody reads."
  (let ((file (namestring (merge-pathnames "pine-probe-point.db"
                                           (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (unwind-protect
                (progn
                  (pine:start :store file)
                  (setf (pine/fs/node:contents (pine/edit/buffer:current))
                        (format nil "one~%two~%three"))
                  (pine/edit/buffer:goto! (pine/edit/buffer:current) 2 1))
             (pine:stop))
           (unwind-protect
                (progn
                  (pine:start :store file)
                  (let ((n (pine/fs/tree:at
                            (pine/world/world:root pine/world/world:*world*)
                            "buffer" "scratch" "point-line")))
                    (is (typep n 'pine/fs/node:slot-node)
                        "the path is still the buffer's slot, not a fresh value")
                    (is (= 2 (pine/edit/buffer:point-line
                              (pine/edit/buffer:buffer-named "scratch")))
                        "and what came back moved the buffer itself")))
             (pine:stop)))
      (ignore-errors (delete-file file)))))

(test a-panel-that-was-up-is-up-again
  "A surface takes the name a config declares it under, and attaching one
replaces whatever stood there. Restoring before the config would hand the
surface a value node to throw away, so whether a panel was shown could never
survive a restart. The daemon reads the store after the config for that reason."
  (let ((file (namestring (merge-pathnames "pine-probe-panel.db"
                                           (uiop:temporary-directory))))
        (config (merge-pathnames "pine-probe-panel-init.lisp"
                                 (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out config :direction :output :if-exists :supersede)
             (write-string "(in-package :pine)
(pine/app/surface:surface \"probe-panel\" (lambda () nil) :as :panel)" out))
           (unwind-protect
                (progn
                  (pine:daemon :remoting 0 :store file :config config)
                  (is-false (pine/app/surface:shownp
                             (pine/app/surface:surface-named "probe-panel"))
                            "a panel starts down")
                  (pine/app/surface:show! "probe-panel"))
             (pine:stop))
           (unwind-protect
                (progn
                  (pine:daemon :remoting 0 :store file :config config)
                  (is-true (pine/app/surface:shownp
                            (pine/app/surface:surface-named "probe-panel"))
                           "and comes back up, on the surface and not beside it"))
             (pine:stop)))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file config)))))

(test where-a-pine-keeps-what-it-was-told-to-keep
  (is (search "pine" (namestring (pine:store-file)))
      "under a directory of its own, so nothing else in the share is disturbed")
  (is (equal "db" (pathname-type (pine:store-file)))))

(test a-structured-value-goes-into-the-store-and-comes-back-as-itself
  (let ((file (merge-pathnames "pine-probe-store.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (let ((held (pine/data:map :a (pine/data:seq 1 2 3)
                                    :b (pine/data:set :x :y))))
           (unwind-protect
                (progn
                  (pine:start :store file)
                  (setf (pine/fs/node:contents
                         (pine/world/world:ensure pine/world/world:*world* "probe"))
                        held))
             (pine:stop))
           (unwind-protect
                (progn
                  (pine:start :store file)
                  (let ((back (pine/fs/node:contents
                               (pine/fs/tree:at (pine/world/world:root
                                                 pine/world/world:*world*)
                                                "probe"))))
                    (is-true (pine/data:mapp back)
                             "a map goes in as a map and comes back as one")
                    (is (equal '(1 2 3) (pine/data:as :list (pine/data:at back :a))))
                    (is-true (pine/data:contains (pine/data:at back :b) :x))))
             (pine:stop)))
      (ignore-errors (delete-file file)))))

(test what-was-written-is-in-the-store-before-anything-stops
  (let ((file (merge-pathnames "pine-probe-crash.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (unwind-protect
         (progn
           (unwind-protect
                (progn
                  (pine:start :store file)
                  (setf (pine/fs/node:contents
                         (pine/world/world:ensure pine/world/world:*world* "probe"))
                        :written)
                  (let ((rows (sqlite:execute-to-list
                               (pine/world/store::db pine/world/store:*store*)
                               "select path, value from node where path = '/probe'")))
                    (is-true rows
                             "it is on disk already, not waiting for a clean stop")))
             (setf pine/fs/node:*on-write* nil)
             (ignore-errors (pine/world/store:close-store pine/world/store:*store*))
             (setf pine/world/store:*store* nil)
             (pine:stop))
           (unwind-protect
                (progn
                  (pine:start :store file)
                  (is (eq :written
                          (pine/fs/node:contents
                           (pine/fs/tree:at (pine/world/world:root
                                             pine/world/world:*world*)
                                            "probe")))
                      "so a crash before the snapshot costs nothing"))
             (pine:stop)))
      (ignore-errors (delete-file file)))))

(test what-a-killed-image-wrote-is-there-when-the-next-one-starts
  "Not a clean stop: the image is killed outright, so what comes back is what
the store had already been told, not what a snapshot would have written."
  (let ((file (merge-pathnames "pine-probe-killed.db" (uiop:temporary-directory))))
    (ignore-errors (delete-file file))
    (let ((p (make-instance 'pine/proc/lisp:lisp-process
                            :name "probe-killed" :restarts nil :systems '(:pine))))
      (unwind-protect
           (progn
             (pine/proc/process:start p)
             (is (equal '(:written)
                     (pine/proc/elsewhere:evaluate
                      p `(progn (pine:start :store ,(namestring file))
                                (setf (pine/fs/node:contents
                                       (pine/world/world:ensure
                                        pine/world/world:*world* "probe"))
                                      :written))
                      :timeout 120))
                 "the other image wrote it")
             (sb-posix:kill (uiop:process-info-pid (pine/proc/process:took p)) 9)
             (loop :repeat 200 :while (pine/proc/process:alivep p) :do (sleep 0.01))
             (is-false (pine/proc/process:alivep p) "and was killed outright"))
        (ignore-errors (pine/proc/process:stop p))))
    (unwind-protect
         (progn
           (pine:start :store file)
           (is (eq :written
                   (pine/fs/node:contents
                    (pine/fs/tree:at (pine/world/world:root pine/world/world:*world*)
                                     "probe")))
               "and it is there for the image that starts next"))
      (pine:stop)
      (ignore-errors (delete-file file)))))

(test what-the-daemon-is-doing-reads-through-the-tree
  "A number about the running daemon has to be reachable the way everything
else is: pine read /metric/frame/p95, or a widget in a bar."
  (unwind-protect
       (progn
         (pine:start)
         (pine/run/meter:reset)
         (let ((b (pine/edit/buffer:current)))
           (setf (pine/fs/node:contents b) "hello")
           (dotimes (n 5) (pine/edit/render:frame-tree :cols 40 :rows 10))
           (pine/edit/key:dispatch nil (pine/edit/key:make-key "x")))
         (let ((root (pine/world/world:root pine/world/world:*world*)))
           (is (member "frame" (pine/fs/tree:listing (pine/fs/tree:at root "metric"))
                       :test #'equal)
               "the frame is one of the instruments the tree lists")
           (is (= 5 (pine/fs/node:contents (pine/fs/tree:at root "metric/frame/count")))
               "five frames were built and five is what it says")
           (is (plusp (pine/fs/node:contents
                       (pine/fs/tree:at root "metric/frame/mean")))
               "and it took some milliseconds to do it")
           (is (= 1 (pine/fs/node:contents (pine/fs/tree:at root "metric/key/count")))
               "one key was dispatched")
           (is (null (pine/fs/tree:at root "metric/nothing-walked-this/count"))
               "an instrument nothing touched is not a zero in the tree")))
    (pine/run/meter:reset)
    (pine:stop)))

(test the-same-table-answers-a-command-and-a-workload
  "metrics is what a synthetic run prints and what a live daemon answers, which
is the only way one can be laid beside the other."
  (unwind-protect
       (progn
         (pine:start)
         (pine/run/meter:reset)
         (dotimes (n 3) (pine/edit/render:frame-tree :cols 40 :rows 10))
         (let ((rows (pine/repl/command:run "metrics")))
           (is (equal (mapcar (lambda (r) (getf r :name)) rows)
                      (mapcar (lambda (r) (getf r :name)) (pine/run/meter:said)))
               "the command answers the instruments themselves")
           (is-true (every (lambda (r) (and (consp r) (getf r :kind))) rows)
               "each one is the row the table keeps, not a line of text.
What a reading says of the clock -- seconds, per-second -- moves between two
reads, so the rows are the same instruments and not the same numbers.")
           (let ((frame (find :frame rows :key (lambda (r) (getf r :name)))))
             (is-true frame)
             (is (= 3 (getf frame :count))))
           (is (search "instrument"
                       (with-output-to-string (out)
                         (pine/run/meter:report rows :to out :about "a probe")))
               "and it prints through one report, with what it is about")))
    (pine/run/meter:reset)
    (pine:stop)))

(test a-workload-leaves-samples-on-what-it-says-it-drives
  "The point of a workload is that it walks the same paths a person does. If it
claims to drive the frame and the frame has no samples, the workload is lying
and the table would say nothing about pine."
  (unwind-protect
       (progn
         (pine:start)
         (pine/run/meter:reset)
         (let ((b (pine/edit/buffer:current))
               (w (pine/edit/window:focused)))
           (setf (pine/edit/buffer:mode-of b) "lisp")
           (setf (pine/fs/node:contents b) "(defun f (x) x)")
           (setf (pine/edit/window:width-of w) 40
                 (pine/edit/window:height-of w) 10)
           (dotimes (n 10)
             (pine/edit/key:dispatch nil (pine/edit/key:make-key "x"))
             (pine/edit/render:frame-tree :cols 40 :rows 12)))
         (dolist (name '(:key :frame))
           (let ((said (pine/run/meter:reading name)))
             (is-true said "~a has no samples, so nothing drove it" name)
             (is (= 10 (getf said :count)) "~a" name)))
         (let ((rows (pine/run/meter:said)))
           (is (every (lambda (row) (getf row :name)) rows)
               "every row says which instrument it is")
           (is (every (lambda (row) (plusp (getf row :count))) rows)
               "and none of them is an instrument with no samples")))
    (pine/run/meter:reset)
    (pine:stop)))
