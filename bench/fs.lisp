(require :asdf)
(asdf:load-system :pine/place)

(defpackage #:pine/bench/fs
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:commit #:pine/fs/commit))
  (:export #:main))
(in-package #:pine/bench/fs)

(defvar *runs* (or (ignore-errors (parse-integer (uiop:getenv "RUNS"))) 1000000))

(defun cost (label n thunk)
  (sb-ext:gc :full t)
  (let ((before (sb-ext:get-bytes-consed))
        (at (get-internal-real-time)))
    (dotimes (i n) (funcall thunk))
    (let ((secs (max 1d-6 (/ (- (get-internal-real-time) at)
                             (float internal-time-units-per-second))))
          (bytes (- (sb-ext:get-bytes-consed) before)))
      (format t "~&~46@a ~9,0f ns ~8,0f bytes~%"
              label (/ (* secs 1e9) n) (/ bytes n))
      (force-output))))

(defun fresh () (tree:make-root) (commit:forget-listeners))

(defun with-kids (n)
  "A branch with N children, to see what finding one among them costs."
  (let ((under (tree:ensure "/many")))
    (dotimes (i n) (node:attach (node:make (format nil "kid~d" i)) under))
    under))

(defun main ()
  (format t "~&~%pine as it stands: what the namespace costs~%~%")

  (fresh)
  (tree:put "/dev/audio/volume" nil 50)
  (cost "read a value three deep" *runs*
        (lambda () (node:contents (tree:at "/dev/audio/volume"))))
  (cost "walk to it, without reading" *runs*
        (lambda () (tree:at "/dev/audio/volume")))
  (let ((it (tree:at "/dev/audio/volume")))
    (cost "read it, already in hand" *runs*
          (lambda () (node:contents it)))
    (cost "write it, already in hand" *runs*
          (lambda () (setf (node:contents it) 50))))

  (format t "~&~%finding one child among many~%~%")
  (dolist (n '(1 10 100 1000))
    (fresh)
    (with-kids n)
    (cost (format nil "resolve one of ~d" n) (max 1000 (floor *runs* (* 2 n)))
          (lambda () (tree:at "/many/kid0"))))

  (format t "~&~%what a write costs with somewhere to keep it~%~%")
  (fresh)
  (let ((where "/tmp/pine-fs-bench.db"))
    (ignore-errors (delete-file where))
    (let ((s (pine/fs/store:open-store where)))
      (pine/fs/store:keeping s)
      (tree:put "/kept" nil 0)
      (let ((n (tree:at nil "kept")))
        (cost "write a kept node, store on" (floor *runs* 200)
              (lambda () (setf (node:contents n) (random 1000)))))
      (pine/fs/store:keeping nil)
      (let ((n (tree:at nil "kept")))
        (cost "write a kept node, store off" (floor *runs* 200)
              (lambda () (setf (node:contents n) (random 1000)))))
      (ignore-errors (pine/fs/store:close-store s))
      (ignore-errors (delete-file where))))

  (format t "~&~%what is worked out~%~%")
  (fresh)
  (tree:put "/n" nil 1)
  (let ((twice (node:attach (node:derive "twice"
                                         (lambda ()
                                           (* 2 (node:contents (tree:at "/n")))))
                            (tree:root)))
        (n (tree:at "/n")))
    (node:contents twice)
    (cost "read one, nothing moved" *runs*
          (lambda () (node:contents twice)))
    (cost "write what it reads, then read it" (floor *runs* 20)
          (lambda () (setf (node:contents n) 2) (node:contents twice))))

  (format t "~&~%a branch of a hundred thousand~%~%")
  (fresh)
  (let ((at (get-internal-real-time)))
    (dotimes (i 100000)
      (tree:put (format nil "/big/~d/~d" (mod i 100) i) nil i))
    (format t "~&~46@a ~,2f s~%" "put 100,000 of them"
            (/ (- (get-internal-real-time) at)
               (float internal-time-units-per-second))))
  (cost "read one of a hundred thousand" (floor *runs* 10)
        (lambda () (node:contents (tree:at "/big/50/50050"))))
  (sb-ext:gc :full t)
  (format t "~&~46@a ~,1f MB~%" "and what it weighs, once swept"
          (/ (sb-kernel:dynamic-usage) 1024.0 1024.0)))

(main)
(sb-ext:exit)
