(in-package :pine.test)

(def-suite* :pine.world :in :pine)

(defmacro with-store ((var) &body body)
  `(let* ((file (merge-pathnames (format nil "pine-probe-~d.db" (random 100000))
                                 (uiop:temporary-directory)))
          (,var (pine.world.store:open-store (namestring file))))
     (unwind-protect (progn ,@body)
       (pine.world.store:close-store ,var)
       (ignore-errors (delete-file file)))))

(test a-node-has-an-identity-that-outlives-what-it-holds
  (let* ((w (pine.world.world:make-world))
         (n (pine.world.world:ensure w "buf" "scratch")))
    (let ((id (pine.world.world:identity-of w n)))
      (is (integerp id))
      (setf (pine.fs.node:contents n) "one")
      (setf (pine.fs.node:contents n) "two")
      (is (eql id (pine.world.world:identity-of w n)))
      (is (eq n (pine.world.world:node-for-id w id))))))

(test what-persists-is-a-property-of-the-class
  (let* ((w (pine.world.world:make-world))
         (held (pine.world.world:ensure w "buf" "scratch" "text"))
         (worked-out (pine.fs.computed:computed "sum" (lambda () 42))))
    (pine.fs.node:attach worked-out (pine.world.world:root w))
    (is-true (pine.fs.node:persistp held))
    (is-false (pine.fs.node:persistp worked-out)
              "what is worked out comes back by being worked out again")))

(test a-snapshot-comes-back-with-what-was-in-it
  (with-store (s)
    (let ((w (pine.world.world:make-world)))
      (pine.world.world:place w '("buf" "scratch" "text") "hello")
      (pine.world.world:place w '("win" "width") 80)
      (pine.world.world:place w '("mode" "lisp" "indent") 2)
      (is (eql 3 (pine.world.store:snapshot w s))))
    (let ((back (pine.world.world:make-world)))
      (is (eql 3 (pine.world.store:restore back s)))
      (is (equal "hello" (pine.fs.node:contents
                          (pine.world.world:at back "buf/scratch/text"))))
      (is (eql 80 (pine.fs.node:contents (pine.world.world:at back "win/width"))))
      (is (eql 2 (pine.fs.node:contents
                  (pine.world.world:at back "mode/lisp/indent")))))))

(test a-snapshot-keeps-what-was-defined-while-it-was-running
  "Modifying itself while it runs, and keeping it: a command made at the repl is
a node like any other, so it is in the snapshot."
  (with-store (s)
    (let ((w (pine.world.world:make-world)))
      (pine.world.world:place w '("cmd" "probe-live") "made at the repl")
      (pine.world.store:snapshot w s)
      (let ((back (pine.world.world:make-world)))
        (pine.world.store:restore back s)
        (is (equal "made at the repl"
                   (pine.fs.node:contents
                    (pine.world.world:at back "cmd/probe-live"))))))))

(test what-cannot-be-written-down-is-left-out-rather-than-signalling
  (with-store (s)
    (let ((w (pine.world.world:make-world)))
      (pine.world.world:place w '("ok") 1)
      (pine.world.world:place w '("live") (lambda () :a-closure))
      (is (eql 1 (pine.world.store:snapshot w s))))))
