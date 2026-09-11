(require :asdf)
(asdf:load-system :pine/fs)

(defpackage #:pine/bench/fs
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
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

(defun fresh () (fs:make-root) (fs:forget-listeners))

(defun with-kids (n)
  "A branch with N children, to see what finding one among them costs."
  (let ((under (fs:ensure "/many")))
    (dotimes (i n) (fs:attach (make-instance 'fs:value :name (format nil "kid~d" i)) under))
    under))

(defun main ()
  (format t "~&~%pine as it stands: what the namespace costs~%~%")

  (fresh)
  (pine::write "/dev/audio/volume" 50)
  (cost "read a value three deep" *runs*
        (lambda () (fs:contents (fs:at "/dev/audio/volume"))))
  (cost "walk to it, without reading" *runs*
        (lambda () (fs:at "/dev/audio/volume")))
  (let ((it (fs:at "/dev/audio/volume")))
    (cost "read it, already in hand" *runs*
          (lambda () (fs:contents it)))
    (cost "write it, already in hand" *runs*
          (lambda () (setf (fs:contents it) 50))))

  (format t "~&~%finding one child among many~%~%")
  (dolist (n '(1 10 100 1000))
    (fresh)
    (with-kids n)
    (cost (format nil "resolve one of ~d" n) (max 1000 (floor *runs* (* 2 n)))
          (lambda () (fs:at "/many/kid0"))))

  (format t "~&~%what a write costs with somewhere to keep it~%~%")
  (fresh)
  (let ((where "/tmp/pine-fs-bench.db"))
    (ignore-errors (delete-file where))
    (let ((s (pine/fs/store:open-store where)))
      (pine/fs/store:keeping s)
      (pine::write "/kept" 0)
      (let ((n (fs:at "kept")))
        (cost "write a kept node, store on" (floor *runs* 200)
              (lambda () (setf (fs:contents n) (random 1000)))))
      (pine/fs/store:keeping nil)
      (let ((n (fs:at "kept")))
        (cost "write a kept node, store off" (floor *runs* 200)
              (lambda () (setf (fs:contents n) (random 1000)))))
      (ignore-errors (pine/fs/store:close-store s))
      (ignore-errors (delete-file where))))

  (format t "~&~%what is worked out~%~%")
  (fresh)
  (pine::write "/n" 1)
  (let ((twice (fs:attach (make-instance 'fs:derived :name "twice" :recompute
                                         (lambda ()
                                           (* 2 (fs:contents (fs:at "/n")))))
                            (fs:root)))
        (n (fs:at "/n")))
    (fs:contents twice)
    (cost "read one, nothing moved" *runs*
          (lambda () (fs:contents twice)))
    (cost "write what it reads, then read it" (floor *runs* 20)
          (lambda () (setf (fs:contents n) 2) (fs:contents twice))))

  (format t "~&~%a branch of a hundred thousand~%~%")
  (fresh)
  (let ((at (get-internal-real-time)))
    (dotimes (i 100000)
      (pine::write (format nil "/big/~d/~d" (mod i 100) i) i))
    (format t "~&~46@a ~,2f s~%" "put 100,000 of them"
            (/ (- (get-internal-real-time) at)
               (float internal-time-units-per-second))))
  (cost "read one of a hundred thousand" (floor *runs* 10)
        (lambda () (fs:contents (fs:at "/big/50/50050"))))
  (sb-ext:gc :full t)
  (format t "~&~46@a ~,1f MB~%" "and what it weighs, once swept"
          (/ (sb-kernel:dynamic-usage) 1024.0 1024.0)))

(main)
(sb-ext:exit)
